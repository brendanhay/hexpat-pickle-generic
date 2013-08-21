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
    , xpSum
    , xpEither
    , xpPrim
    , xpElem
    , xpOption
    , xpPair
    , xpWrap
    , xpUnit
    , xpLift
    , xpText
    , xpContent
    , xpList
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

import System.IO.Unsafe

--
-- Class
--

type Node = UNode ByteString

data XMLPU t a = XMLPU
    { pickleTree   :: a -> t
    , unpickleTree :: t -> Either String a
    , root         :: Maybe ByteString
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
    pu = unsafePerformIO $ print (root pu') >> return pu'

    pu' = xmlPickler

toIndentedXML :: IsXML a => Int -> a -> ByteString
toIndentedXML i = format'
    . indent i
    . maybe head (\n -> Element n []) (root pu)
    . pickleTree pu
  where
    pu = unsafePerformIO $ print (root pu') >> return pu'

    pu' = xmlPickler

fromXML :: IsXML a => ByteString -> Either String a
fromXML = either (Left . show) unwrap . parse' defaultParseOptions
  where
    unwrap e@(Element n _ cs) = case root pu of
        Just x | x == n -> unpickleTree pu cs
        Just _          -> Left "Unexpected root element"
        Nothing         -> unpickleTree pu [e]
    unwrap _                  = Left "Unexpected root element"

    pu = unsafePerformIO $ print (root pu') >> return pu'

    pu' = xmlPickler

--
-- Defining Picklers
--

data XMLOptions = XMLOptions
    { xmlCtorModifier  :: String -> String
      -- ^ Function applied to constructor tags.
    , xmlFieldModifier :: String -> String
      -- ^ Function applied to record field labels.
    }

type Options = XMLOptions

defaultXMLOptions :: Options
defaultXMLOptions = XMLOptions id (dropWhile isLower)

--
-- Generics
--

genericXMLPickler opts =
    (to, from) `xpWrap` (gXMLPickler opts) (genericXMLPickler opts)

class GIsXML f where
    gXMLPickler :: XMLOptions -> PU [Node] a -> PU [Node] (f a)

instance IsXML a => GIsXML (K1 i a) where
    -- Constants
    gXMLPickler _ _ = (K1, unK1) `xpWrap` xmlPickler

instance GIsXML U1 where
    -- Empty Constructors Parameters
    gXMLPickler _ _ = (const U1, const ()) `xpWrap` xpUnit

instance GIsXML a => GIsXML (M1 i d a) where
    -- Discard Metadata
     gXMLPickler opts = xpWrap (M1, unM1) . gXMLPickler opts

instance (Constructor c, CtorIsXML a) => GIsXML (C1 c a) where
    -- Constructor Encoding
    gXMLPickler opts = xpWrap (M1, unM1) . ctorXMLPickler opts

instance ( AllNullary (a :+: b) allNullary
         , NullIsXML  (a :+: b) allNullary
         ) => GIsXML  (a :+: b) where
    -- Nullary Constructors
    gXMLPickler opts =
        (unTagged :: Tagged allNullary (PU t ((a :+: b) d)) -> (PU t ((a :+: b) d)))
            . nullXMLPickler opts

--
-- Nullary
--

class NullIsXML f allNullary where
    nullXMLPickler :: XMLOptions -> PU [Node] a -> Tagged allNullary (PU [Node] (f a))

instance SumIsXML (a :+: b) => NullIsXML (a :+: b) True where
    nullXMLPickler opts _ = Tagged $ sumXMLPickler opts

class SumIsXML f where
    sumXMLPickler :: XMLOptions -> PU [Node] (f a)

instance (SumIsXML a, SumIsXML b) => SumIsXML (a :+: b) where
    sumXMLPickler opts = sumXMLPickler opts `xpSum` sumXMLPickler opts

instance Constructor c => SumIsXML (C1 c U1) where
    sumXMLPickler opts = xpContent $ XMLPU
        { pickleTree   = const name
        , unpickleTree = \t ->
              if t == name
                  then Right $ M1 U1
                  else Left "Unable to read nullary ctor"
        , root         = Just name
        }
      where
        name = BS.pack . xmlCtorModifier opts $ conName (undefined :: t c U1 p)

--
-- Records
--

class CtorIsXML f where
    ctorXMLPickler :: XMLOptions -> PU [Node] a -> PU [Node] (f a)

class CtorIsXML' f isRecord where
    ctorXMLPickler' :: XMLOptions -> PU [Node] a -> Tagged isRecord (PU [Node] (f a))

instance (IsRecord f isRecord, CtorIsXML' f isRecord) => CtorIsXML f where
    ctorXMLPickler opts = (unTagged :: Tagged isRecord (PU t (f a)) -> PU t (f a))
        . ctorXMLPickler' opts

instance RecIsXML f => CtorIsXML' f True where
    ctorXMLPickler' opts = Tagged . recXMLPickler opts

instance GIsXML f => CtorIsXML' f False where
    ctorXMLPickler' opts = Tagged . gXMLPickler opts

class RecIsXML f where
    recXMLPickler :: XMLOptions -> PU [Node] a -> PU [Node] (f a)

instance (RecIsXML a, RecIsXML b) => RecIsXML (a :*: b) where
    recXMLPickler opts f = xpWrap
        (uncurry (:*:), \(a :*: b) -> (a, b))
        (recXMLPickler opts f `xpPair` recXMLPickler opts f)

instance (Selector s, GIsXML a) => RecIsXML (S1 s a) where
    recXMLPickler opts f = xpElem
        (BS.pack . xmlFieldModifier opts $ selName (undefined :: S1 s a r))
        ((M1, unM1) `xpWrap` gXMLPickler opts f)

instance (Selector s, IsXML a) => RecIsXML (S1 s (K1 i (Maybe a))) where
    recXMLPickler opts _ =
        (M1 . K1, unK1 . unM1) `xpWrap` xpOption (xpElem name xmlPickler)
      where
        name = BS.pack . xmlFieldModifier opts
            $ selName (undefined :: t s (K1 i (Maybe a)) p)

instance (Selector s, IsXML a) => RecIsXML (S1 s (K1 i [a])) where
    recXMLPickler opts _ =
        ((M1 . K1, unK1 . unM1) `xpWrap` xpList (xpElem name xmlPickler))
      where
        name = BS.pack . xmlFieldModifier opts
            $ selName (undefined :: t s (K1 i [a]) p)


--
-- Tagging
--

class IsRecord (f :: * -> *) isRecord | f -> isRecord

instance (IsRecord f isRecord) => IsRecord (f :*: g) isRecord
instance IsRecord (M1 S NoSelector f) False
instance (IsRecord f isRecord) => IsRecord (M1 S c f) isRecord
instance IsRecord (K1 i c) True
instance IsRecord U1 False

class AllNullary (f :: * -> *) allNullary | f -> allNullary

instance ( AllNullary a allNullaryL
         , AllNullary b allNullaryR
         , And allNullaryL allNullaryR allNullary
         ) => AllNullary (a :+: b) allNullary
instance AllNullary a allNullary => AllNullary (M1 i c a) allNullary
instance AllNullary (a :*: b) False
instance AllNullary (K1 i c) False
instance AllNullary U1 True

data True
data False

class And bool1 bool2 bool3 | bool1 bool2 -> bool3

instance And True  True  True
instance And False False False
instance And False True  False
instance And True  False False

newtype Tagged s b = Tagged { unTagged :: b }

--
-- Combinators
--

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
    , root         = listToMaybe $ catMaybes [root pa, root pb]
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
    , root     = Just name
    }
  where
    matching (Element n _ cs)
        | n == name = Just $ unpickleTree pu cs
    matching _      = Nothing

    tag = "<" ++ gxToString name ++ ">"

xpOption :: PU [n] a -> PU [n] (Maybe a)
xpOption pu = XMLPU
    { pickleTree   = maybe [] (pickleTree pu)
    , unpickleTree = Right . either (const Nothing) Just . unpickleTree pu
    , root         = root pu
    }

xpPair :: PU [n] a -> PU [n] b -> PU [n] (a, b)
xpPair pa pb = XMLPU
    { pickleTree   = \(a, b) -> pickleTree pa a ++ pickleTree pb b
    , unpickleTree = \t ->
          case (unpickleTree pa t, unpickleTree pb t) of
              (Right a, Right b) -> Right (a, b)
              (Left e,  _)       -> Left $ "in 1st of pair, " ++ e
              (_,       Left e)  -> Left $ "in 2nd of pair, " ++ e
    , root         = listToMaybe $ catMaybes [root pa, root pb]
    }

xpWrap :: (a -> b, b -> a) -> PU [n] a -> PU [n] b
xpWrap (f, g) pu = XMLPU
    { pickleTree   = pickleTree pu . g
    , unpickleTree = fmap f . unpickleTree pu
    , root         = root pu
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
    , unpickleTree = Right
    , root         = Nothing
    }

xpContent :: PU ByteString a -> PU [Node] a
xpContent pu = XMLPU
    { pickleTree   = \t ->
          let txt = pickleTree pu t
          in if gxNullString txt then [] else [Text txt]
    , unpickleTree = unpickleTree pu . mconcat . map extract
    , root         = root pu
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

xpList :: PU [Node] a -> PU [Node] [a]
xpList pu = XMLPU
    { pickleTree   = mconcat . map (pickleTree pu)
    , unpickleTree = \t ->
            let munge [] = []
                munge (elt@(Element _ _ _):es) =
                    case unpickleTree pu [elt] of
                        Right val -> Right val:munge es
                        Left err  -> [Left $ "in list, "++err]
                munge (_:es) = munge es  -- ignore text nodes
                m = munge t
            in  case m of
                    [] -> Right []
                    _  ->
                        case last m of
                                Left err -> Left err
                                Right _ -> Right $ rights m
    , root = root pu
    }

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
