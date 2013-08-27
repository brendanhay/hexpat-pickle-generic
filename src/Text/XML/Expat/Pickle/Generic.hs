{-# LANGUAGE DefaultSignatures               #-}
{-# LANGUAGE DeriveGeneric                   #-}
{-# LANGUAGE FlexibleContexts                #-}
{-# LANGUAGE FlexibleInstances               #-}
{-# LANGUAGE MultiParamTypeClasses           #-}
{-# LANGUAGE Rank2Types                      #-}
{-# LANGUAGE OverlappingInstances            #-}
{-# LANGUAGE OverloadedStrings               #-}
{-# LANGUAGE ScopedTypeVariables             #-}
{-# LANGUAGE TypeOperators                   #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- Module      : Text.XML.Expat.Pickle.Generic
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               Berkeley Software Distribution License, v. 3.0.
--               You can obtain it at
--               http://http://opensource.org/licenses/BSD-3-Clause.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.XML.Expat.Pickle.Generic
    (
    -- * Class
      IsXML      (..)

    -- * Functions
    , toXML
    , toIndentedXML
    , fromXML

    -- * Data Types
    , XMLPU      (..)

    -- * Options
    , XMLOptions (..)
    , defaultXMLOptions

    -- * Generics
    , XMLGeneric
    , genericXMLPickler

    -- * Combinators
    , xpWrap
    , xpList
    , xpElemList
    , xpElem
    , xpSum
    , xpEither
    , xpPrim
    , xpEmpty
    , xpOption
    , xpPair
    , xpTriple
    , xp4Tuple
    , xp5Tuple
    , xp6Tuple
    , xpUnit
    , xpLift
    , xpText
    , xpText0
    , xpContent

    -- * Re-exported Tag Helpers
    , module Text.XML.Expat.Internal.Namespaced
    , module Text.XML.Expat.Internal.Qualified
    ) where

import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Char8              as BS
import           Data.Char                          (isLower, isSpace)
import           Data.Either
import           Data.Maybe
import           Data.Monoid
import           GHC.Generics
import           Text.XML.Expat.Format
import           Text.XML.Expat.Internal.Namespaced
import           Text.XML.Expat.Internal.Qualified  hiding (fromQualified)
import           Text.XML.Expat.Tree                hiding (Node, fromQualified)

--
-- Class
--

type Node = NNode ByteString

data XMLPU t a = XMLPU
    { pickleTree   :: a -> t
    , unpickleTree :: t -> Either String a
    , root         :: Maybe (NName ByteString)
    }

type PU = XMLPU

class IsXML a where
    xmlPickler :: PU [Node] a

    default xmlPickler :: (Generic a, GIsXML (Rep a)) => PU [Node] a
    xmlPickler = genericXMLPickler defaultXMLOptions

--
-- Functions
--

toXML :: IsXML a => a -> ByteString
toXML = format'
    . fromQualified
    . fromNamespaced
    . maybe head (\n -> Element n []) (root pu)
    . pickleTree pu
  where
    pu = xmlPickler

toIndentedXML :: IsXML a => Int -> a -> ByteString
toIndentedXML i = format'
    . indent i
    . fromQualified
    . fromNamespaced
    . maybe head (\n -> Element n []) (root pu)
    . pickleTree pu
  where
    pu = xmlPickler

fromXML :: IsXML a => ByteString -> Either String a
fromXML = either (Left . show) (unwrap . toNamespaced . toQualified)
    . parse' defaultParseOptions
  where
    unwrap e@(Element n _ cs) = case root pu of
        Just x | x == n    -> unpickleTree pu cs
               | otherwise -> Left $
            "expected root: " ++ show x ++ ", got: " ++ show n
        Nothing -> unpickleTree pu [e]
    unwrap t = Left $ "unexpected root: " ++ show t

    pu = xmlPickler

--
-- Options
--

data XMLOptions = XMLOptions
    { xmlCtorModifier  :: String -> NName ByteString
      -- ^ Function applied to constructor tags.
    , xmlFieldModifier :: String -> NName ByteString
      -- ^ Function applied to record field labels.
    , xmlListElement   :: NName ByteString
      -- ^ Default element name to wrap list items with.
    }

defaultXMLOptions :: XMLOptions
defaultXMLOptions = XMLOptions
    { xmlCtorModifier  = mkAnNName . BS.pack
    , xmlFieldModifier = mkAnNName . BS.pack . dropWhile isLower
    , xmlListElement   = mkAnNName "Value"
    }

--
-- Generics
--

type XMLGeneric a = (Generic a, GIsXML (Rep a)) => PU [Node] a

genericXMLPickler opts =
    (to, from) `xpWrap` (gXMLPickler opts) (genericXMLPickler opts)

class GIsXML f where
    gXMLPickler :: XMLOptions -> PU [Node] a -> PU [Node] (f a)

instance IsXML a => GIsXML (K1 i a) where
    gXMLPickler _ _ = (K1, unK1) `xpWrap` xmlPickler

instance (GIsXML a, GIsXML b) => GIsXML (a :+: b) where
    gXMLPickler opts f = gXMLPickler opts f `xpSum` gXMLPickler opts f

instance (GIsXML a, GIsXML b) => GIsXML (a :*: b) where
    gXMLPickler opts f = xpWrap
        (uncurry (:*:), \(a :*: b) -> (a, b))
        (gXMLPickler opts f `xpPair` gXMLPickler opts f)

instance (Datatype d, GIsXML a) => GIsXML (D1 d a) where
    gXMLPickler opts = xpWrap (M1, unM1) . gXMLPickler opts

instance (Constructor c, GIsXML a) => GIsXML (C1 c a) where
    gXMLPickler opts f = (xpWrap (M1, unM1) $ gXMLPickler opts f)
        { root = Just . xmlCtorModifier opts $ conName (undefined :: C1 c a p)
        }

instance (Selector s, GIsXML a) => GIsXML (S1 s a) where
    gXMLPickler opts f = xpElem
        (xmlFieldModifier opts $ selName (undefined :: S1 s a p))
        ((M1, unM1) `xpWrap` gXMLPickler opts f)

instance (Selector s, IsXML a) => GIsXML (S1 s (K1 i [a])) where
    gXMLPickler opts _ = xpElem
        (xmlFieldModifier opts $ selName (undefined :: t s (K1 i [a]) p))
        ((M1 . K1, unK1 . unM1) `xpWrap` xpList (xpElem key pu))
      where
        key = fromMaybe (xmlListElement opts) $ root pu
        pu  = xmlPickler

instance (Selector s, IsXML a) => GIsXML (S1 s (K1 i (Maybe a))) where
    gXMLPickler opts _ =
        (M1 . K1, unK1 . unM1) `xpWrap` xpOption (xpElem name xmlPickler)
      where
        name = xmlFieldModifier opts $ selName (undefined :: t s (K1 i (Maybe a)) p)

--
-- Combinators
--

xpWrap :: (a -> b, b -> a) -> PU [n] a -> PU [n] b
xpWrap (f, g) pu = XMLPU
    { pickleTree   = pickleTree pu . g
    , unpickleTree = fmap f . unpickleTree pu
    , root         = root pu
    }

xpElemList :: NName ByteString -> PU [Node] a -> PU [Node] [a]
xpElemList name = xpList . xpElem name

xpList :: PU [Node] a -> PU [Node] [a]
xpList pu = XMLPU
    { pickleTree   = concatMap (pickleTree pu)
    , unpickleTree = concatEithers . unpickle
    , root         = root pu
    }
  where
    unpickle (e@(Element _ _ _):es) = unpickleTree pu [e] : unpickle es
    unpickle (_:es)                 = unpickle es
    unpickle []                     = []

    concatEithers xs = case partitionEithers xs of
        ([], rs) -> Right rs
        (l:_, _) -> Left l

xpElem :: NName ByteString -> PU [Node] a -> PU [Node] a
xpElem name pu = XMLPU
    { pickleTree   = \x -> [Element name [] (pickleTree pu x)]
    , unpickleTree = \t ->
          let children = map matching t
          in case catMaybes children of
                 []    -> Left $ "can't find " ++ tag
                 (x:_) -> case x of
                     Left e -> Left $ "in " ++ tag ++ ", " ++ e
                     r      -> r
    , root = Just name
    }
  where
    matching (Element n _ cs)
        | n == name = Just $ unpickleTree pu cs
    matching _      = Nothing

    tag = "<" ++ show name ++ ">"

xpSum :: PU [t] (f r) -> PU [t] (g r) -> PU [t] ((f :+: g) r)
xpSum left right = (inp, out) `xpWrap` xpEither left right
  where
    inp (Left  x) = L1 x
    inp (Right x) = R1 x

    out (L1 x) = Left x
    out (R1 x) = Right x

xpEither :: PU [t] a -> PU [t] b -> PU [t] (Either a b)
xpEither pa pb = XMLPU
    { pickleTree   = either (pickleTree pa) (pickleTree pb)
    , unpickleTree = \t -> case unpickleTree pa t of
          Right x -> Right . Left $ x
          Left  _ -> Right `fmap` unpickleTree pb t
    , root     = listToMaybe $ catMaybes [root pa, root pb]
    }


xpPrim :: (Read a, Show a) => PU ByteString a
xpPrim = XMLPU
    { pickleTree   = BS.pack . show
    , unpickleTree = \t ->
        let s = BS.unpack t
        in case reads s of
               [(x, "")] -> Right x
               _         -> Left $ "failed to read text: " ++ s
    , root         = Nothing
    }

xpEmpty :: (Read a, Show a) => PU [Node] a
xpEmpty = XMLPU
    { pickleTree   = \x -> [Element (mkAnNName . BS.pack $ show x) [] []]
    , unpickleTree = \t -> case t of
          [(Element n _ _)] -> let s = BS.unpack $ nnLocalPart n
                               in case reads s of
              [(x, "")] -> Right x
              _         -> Left $ "failed to read: " ++ s
          l                 -> Left $ "expected empty element, got: " ++ show l
    , root        = Nothing
    }

xpOption :: PU [n] a -> PU [n] (Maybe a)
xpOption pu = XMLPU
    { pickleTree   = maybe [] (pickleTree pu)
    , unpickleTree = Right . either (const Nothing) Just . unpickleTree pu
    , root     = root pu
    }

xpPair :: PU [n] a -> PU [n] b -> PU [n] (a, b)
xpPair pa pb = XMLPU
    { pickleTree   = \(a, b) -> concat [pickleTree pa a, pickleTree pb b]
    , unpickleTree = \t ->
          case (unpickleTree pa t, unpickleTree pb t) of
              (Right a, Right b) -> Right (a, b)
              (Left e,  _)       -> Left $ "in 1st of pair, " ++ e
              (_,       Left e)  -> Left $ "in 2nd of pair, " ++ e
    , root = listToMaybe $ catMaybes [root pa, root pb]
    }

xpTriple :: PU [n] a -> PU [n] b -> PU [n] c -> PU [n] (a, b, c)
xpTriple pa pb pc = XMLPU
     { pickleTree   = \(a, b, c) ->
           concat [pickleTree pa a, pickleTree pb b, pickleTree pc c]
     , unpickleTree = \t ->
           case (unpickleTree pa t, unpickleTree pb t, unpickleTree pc t) of
                (Right a, Right b, Right c) -> Right (a, b, c)
                (Left e, _, _) -> Left $ "in 1st of triple, " ++ e
                (_, Left e, _) -> Left $ "in 2nd of triple, " ++ e
                (_, _, Left e) -> Left $ "in 3rd of triple, " ++ e
    , root = listToMaybe $ catMaybes [root pa, root pb, root pc]
    }

xp4Tuple :: PU [n] a -> PU [n] b -> PU [n] c -> PU [n] d -> PU [n] (a, b, c, d)
xp4Tuple pa pb pc pd = XMLPU
     { pickleTree   = \(a, b, c, d) ->
           concat [pickleTree pa a, pickleTree pb b, pickleTree pc c, pickleTree pd d]
     , unpickleTree = \t ->
           case (unpickleTree pa t, unpickleTree pb t, unpickleTree pc t,
                 unpickleTree pd t) of
               (Right a, Right b, Right c, Right d) -> Right (a, b, c, d)
               (Left e, _, _, _) -> Left $ "in 1st of 4-tuple, " ++ e
               (_, Left e, _, _) -> Left $ "in 2nd of 4-tuple, " ++ e
               (_, _, Left e, _) -> Left $ "in 3rd of 4-tuple, " ++ e
               (_, _, _, Left e) -> Left $ "in 4th of 4-tuple, " ++ e
    , root = listToMaybe $ catMaybes [root pa, root pb, root pc, root pd]
    }

xp5Tuple :: PU [n] a -> PU [n] b -> PU [n] c -> PU [n] d -> PU [n] e -> PU [n] (a, b, c, d, e)
xp5Tuple pa pb pc pd pe = XMLPU
    { pickleTree   = \(a, b, c, d, e) ->
           concat [pickleTree pa a, pickleTree pb b, pickleTree pc c, pickleTree pd d, pickleTree pe e]
    , unpickleTree = \t ->
          case (unpickleTree pa t, unpickleTree pb t, unpickleTree pc t,
                unpickleTree pd t, unpickleTree pe t) of
              (Right a, Right b, Right c, Right d, Right e) -> Right (a, b, c, d, e)
              (Left e, _, _, _, _) -> Left $ "in 1st of 5-tuple, " ++ e
              (_, Left e, _, _, _) -> Left $ "in 2nd of 5-tuple, " ++ e
              (_, _, Left e, _, _) -> Left $ "in 3rd of 5-tuple, " ++ e
              (_, _, _, Left e, _) -> Left $ "in 4th of 5-tuple, " ++ e
              (_, _, _, _, Left e) -> Left $ "in 5th of 5-tuple, " ++ e
    , root = listToMaybe $ catMaybes [root pa, root pb, root pc, root pd, root pe]
    }

xp6Tuple :: PU [n] a -> PU [n] b -> PU [n] c -> PU [n] d -> PU [n] e -> PU [n] f -> PU [n] (a, b, c, d, e, f)
xp6Tuple pa pb pc pd pe pf = XMLPU
    { pickleTree   = \(a, b, c, d, e, f) ->
           concat [pickleTree pa a, pickleTree pb b, pickleTree pc c, pickleTree pd d, pickleTree pe e, pickleTree pf f]
    , unpickleTree = \t ->
          case (unpickleTree pa t, unpickleTree pb t, unpickleTree pc t,
                unpickleTree pd t, unpickleTree pe t, unpickleTree pf t) of
              (Right a, Right b, Right c, Right d, Right e', Right f) -> Right (a, b, c, d, e', f)
              (Left e, _, _, _, _, _) -> Left $ "in 1st of 6-tuple, " ++ e
              (_, Left e, _, _, _, _) -> Left $ "in 2nd of 6-tuple, " ++ e
              (_, _, Left e, _, _, _) -> Left $ "in 3rd of 6-tuple, " ++ e
              (_, _, _, Left e, _, _) -> Left $ "in 4th of 6-tuple, " ++ e
              (_, _, _, _, Left e, _) -> Left $ "in 5th of 6-tuple, " ++ e
              (_, _, _, _, _, Left e) -> Left $ "in 6th of 6-tuple, " ++ e
    , root = listToMaybe $ catMaybes [root pa, root pb, root pc, root pd, root pe, root pf]
    }

xpUnit :: PU [n] ()
xpUnit = xpLift ()

xpLift :: a -> PU [n] a
xpLift a = XMLPU
    { pickleTree   = const []
    , unpickleTree = const $ Right a
    , root         = Nothing
    }

xpText :: PU ByteString ByteString
xpText = XMLPU
    { pickleTree   = id
    , unpickleTree = \t -> if BS.null t then Left "empty text" else Right t
    , root         = Nothing
    }

xpText0 :: PU ByteString ByteString
xpText0 = XMLPU
    { pickleTree   = id
    , unpickleTree = Right
    , root         = Nothing
    }

xpContent :: PU ByteString a -> PU [Node] a
xpContent pu = XMLPU
    { pickleTree   = \t ->
          let txt = pickleTree pu t
          in if gxNullString txt then [] else [Text txt]
    , unpickleTree = unpickleTree pu . mconcat . map extract
    , root     = root pu
    }
  where
    extract (Element _ _ cs) = strip . mconcat $ map extract cs
    extract (Text txt)       = strip txt

    strip = snd . BS.break valid . fst . BS.breakEnd valid

    valid c
        | isSpace c = False
        | c == '\r' = False
        | c == '\n' = False
        | otherwise = True

--
-- Instances
--

instance IsXML a => IsXML (Maybe a) where
    xmlPickler = xpOption xmlPickler

instance (IsXML a, IsXML b) => IsXML (Either a b) where
    xmlPickler = xmlPickler `xpEither` xmlPickler

instance IsXML Int where
    xmlPickler = xpContent xpPrim

instance IsXML Integer where
    xmlPickler = xpContent xpPrim

instance IsXML Double where
    xmlPickler = xpContent xpPrim

instance IsXML Float where
    xmlPickler = xpContent xpPrim

instance IsXML ByteString where
    xmlPickler = xpContent xpText

--
-- Qualified Prefix Hack
--

fromQualified :: (NodeClass n c, GenericXMLString text)
              => n c (QName text) text
              -> n c text text
fromQualified = mapAllTags tag
  where
    tag (QName Nothing   local)     = local
    tag (QName (Just prefix) local)
        | prefix == xmlns           = prefix
        | otherwise                 = local
