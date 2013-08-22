{-# LANGUAGE DefaultSignatures               #-}
{-# LANGUAGE DefaultSignatures               #-}
{-# LANGUAGE DeriveGeneric                   #-}
{-# LANGUAGE FlexibleContexts                #-}
{-# LANGUAGE FlexibleInstances               #-}
{-# LANGUAGE FunctionalDependencies          #-}
{-# LANGUAGE KindSignatures                  #-}
{-# LANGUAGE MultiParamTypeClasses           #-}
{-# LANGUAGE OverlappingInstances            #-}
{-# LANGUAGE OverloadedStrings               #-}
{-# LANGUAGE ScopedTypeVariables             #-}
{-# LANGUAGE TypeOperators                   #-}
{-# LANGUAGE UndecidableInstances            #-}
{-# LANGUAGE ViewPatterns                    #-}

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
    , xpUnit
    , xpLift
    , xpText
    , xpText0
    , xpContent
    ) where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Char             (isLower, isSpace)
import           Data.Either
import           Data.Maybe
import           Data.Monoid
import           GHC.Generics
import           Text.XML.Expat.Format
import           Text.XML.Expat.Tree   hiding (Node)

--
-- Class
--

type Node = UNode ByteString

data XMLPU t a = XMLPU
    { pickleTree   :: a -> t
    , unpickleTree :: t -> Either String a
    , root     :: Maybe ByteString
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
toXML = format' . maybe head (\n -> Element n []) (root pu) . pickleTree pu
  where
    pu = xmlPickler

toIndentedXML :: IsXML a => Int -> a -> ByteString
toIndentedXML i = format'
    . indent i
    . maybe head (\n -> Element n []) (root pu)
    . pickleTree pu
  where
    pu = xmlPickler

fromXML :: IsXML a => ByteString -> Either String a
fromXML = either (Left . show) unwrap . parse' defaultParseOptions
  where
    unwrap e@(Element n _ cs) = case root pu of
        Just x | x == n -> unpickleTree pu cs
        Just _          -> Left "Unexpected root element"
        Nothing         -> unpickleTree pu [e]
    unwrap                  _ = Left "Unexpected root element"

    pu = xmlPickler

--
-- Defining Picklers
--

data XMLOptions = XMLOptions
    { xmlCtorModifier  :: String -> ByteString
      -- ^ Function applied to constructor tags.
    , xmlFieldModifier :: String -> ByteString
      -- ^ Function applied to record field labels.
    , xmlListElement   :: ByteString
      -- ^ Default element name to wrap list items with.
    }

type Options = XMLOptions

defaultXMLOptions :: Options
defaultXMLOptions = XMLOptions BS.pack (BS.pack . dropWhile isLower) "Value"

--
-- Generics
--

genericXMLPickler opts =
    (to, from) `xpWrap` (gXMLPickler opts) (genericXMLPickler opts)

class GIsXML f where
    gXMLPickler :: Options -> PU [Node] a -> PU [Node] (f a)

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

--
-- Combinators
--

xpWrap :: (a -> b, b -> a) -> PU [n] a -> PU [n] b
xpWrap (f, g) pu = XMLPU
    { pickleTree   = pickleTree pu . g
    , unpickleTree = fmap f . unpickleTree pu
    , root         = root pu
    }

xpElemList :: ByteString -> PU [Node] a -> PU [Node] [a]
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

xpElem :: ByteString -> PU [Node] a -> PU [Node] a
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

    tag = "<" ++ gxToString name ++ ">"

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
    { pickleTree   = \x -> [Element (BS.pack $ show x) [] []]
    , unpickleTree = \t -> case t of
          [(Element n [] [])] -> let s = BS.unpack n
                                 in case reads s of
              [(x, "")] -> Right x
              _         -> Left $ "failed to read text: " ++ s
          _ -> Left "Expected empty element"
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
    { pickleTree   = \(a, b) -> pickleTree pa a ++ pickleTree pb b
    , unpickleTree = \t ->
          case (unpickleTree pa t, unpickleTree pb t) of
              (Right a, Right b) -> Right (a, b)
              (Left e,  _)       -> Left $ "in 1st of pair, " ++ e
              (_,       Left e)  -> Left $ "in 2nd of pair, " ++ e
    , root     = listToMaybe $ catMaybes [root pa, root pb]
    }

xpUnit :: PU [n] ()
xpUnit = xpLift ()

xpLift :: a -> PU [n] a
xpLift a = XMLPU
    { pickleTree   = const []
    , unpickleTree = const $ Right a
    , root     = Nothing
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
