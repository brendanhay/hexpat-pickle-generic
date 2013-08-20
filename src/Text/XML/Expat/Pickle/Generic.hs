{-# LANGUAGE DefaultSignatures               #-}
{-# LANGUAGE FlexibleContexts                #-}
{-# LANGUAGE FlexibleInstances               #-}
{-# LANGUAGE MultiParamTypeClasses           #-}
{-# LANGUAGE OverlappingInstances            #-}
{-# LANGUAGE OverloadedStrings               #-}
{-# LANGUAGE ScopedTypeVariables             #-}
{-# LANGUAGE TypeOperators                   #-}
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
    , fromXML

    -- * Re-exported Data Types
    , PU         (..)

    -- * Options
    , XMLOptions (..)
    , defaultXMLOptions

    -- * Generics
    , genericXMLPickler

    -- * Combinators
    , xpSum
    , xpEither
    , xpGenericString

    -- * Re-exported Combinators
    , xpRoot
    , xpContent
    , xpText0
    , xpText
--    , xpList
    , xpPrim
    , xpWrap
    , xpOption
    ) where

import           Data.ByteString       (ByteString)
import           Data.Char             (isLower)
import           Data.Either
import           Data.Monoid
import           Data.Text             (Text)
import           GHC.Generics
import qualified Text.XML.Expat.Pickle as Pickle
import           Text.XML.Expat.Pickle hiding (xpList, xpPrim)
import           Text.XML.Expat.Tree

import System.IO.Unsafe

--
-- Class
--

class IsXML a where
    xmlPickler :: PU [UNode ByteString] a

    default xmlPickler :: (Generic a, GIsXML [UNode ByteString] (Rep a))
                      => PU [UNode ByteString] a
    xmlPickler = genericXMLPickler defaultXMLOptions

--
-- Functions
--

toXML :: IsXML a => a -> ByteString
toXML = pickleXML' (xpRoot xmlPickler)

fromXML :: IsXML a => ByteString -> Either String a
fromXML = unpickleXML' defaultParseOptions (xpRoot xmlPickler)

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

class GIsXML t f where
    gXMLPickler :: Options -> PU t a -> PU t (f a)

instance IsXML a => GIsXML [UNode ByteString] (K1 i a) where
    gXMLPickler _ _ = (K1, unK1) `xpWrap` xmlPickler

instance GIsXML [t] U1 where
    gXMLPickler _ _ = (const U1, const ()) `xpWrap` xpUnit

instance (GIsXML t a, GIsXML t b) => GIsXML t (a :+: b) where
    gXMLPickler opts f = gXMLPickler opts f `xpSum` gXMLPickler opts f

instance (GIsXML [t] a, GIsXML [t] b) => GIsXML [t] (a :*: b) where
    gXMLPickler opts f = xpWrap
        (uncurry (:*:), \(a :*: b) -> (a, b))
        (gXMLPickler opts f `xpPair` gXMLPickler opts f)

instance (Datatype d, GIsXML t a) => GIsXML t (D1 d a) where
    gXMLPickler opts = xpWrap (M1, unM1) . gXMLPickler opts

instance (Constructor c, GIsXML [UNode ByteString] a)
         => GIsXML [UNode ByteString] (C1 c a) where
    gXMLPickler opts f = xpElemNodes
        (gxFromString . xmlCtorModifier opts $ conName (undefined :: M1 C c a r))
        ((M1, unM1) `xpWrap` gXMLPickler opts f)

instance (Selector s, GIsXML [UNode ByteString] a)
         => GIsXML [UNode ByteString] (S1 s a) where
    gXMLPickler opts f = xpElemNodes
        (gxFromString . xmlFieldModifier opts $ selName (undefined :: M1 S s a r))
        ((M1, unM1) `xpWrap` gXMLPickler opts f)

instance (Selector s, Show a, IsXML a)
         => GIsXML [UNode ByteString] (S1 s (K1 i [a])) where
    gXMLPickler opts _ =
        ((M1 . K1, unK1 . unM1) `xpWrap` xpList0 (xpElemNodes name xmlPickler))
      where
        name = gxFromString
            . xmlFieldModifier opts
            $ selName (undefined :: t s (K1 i [a]) p)

--
-- Combinators
--

xpPrim :: (Read b, Show b, GenericXMLString t) => PU [Node a t] b
xpPrim = xpContent Pickle.xpPrim

xpSum :: PU t (f r) -> PU t (g r) -> PU t ((f :+: g) r)
xpSum left right = (inp, out) `xpWrap` xpEither left right
  where
    inp (Left  x) = L1 x
    inp (Right x) = R1 x

    out (L1 x) = Left x
    out (R1 x) = Right x

xpEither :: PU t a -> PU t b -> PU t (Either a b)
xpEither ~(PU _ uea pa) ~(PU ub ueb pb) =
    PU unpickle unpickleEither pickle
  where
    unpickle t = case uea t of
        Right x -> Left x
        Left  _ -> Right $ ub t

    unpickleEither t = case uea t of
        Right x -> Right . Left $ x
        Left  _ -> Right `fmap` ueb t

    pickle (Left x)  = pa x
    pickle (Right y) = pb y

xpGenericString :: GenericXMLString t => PU [UNode ByteString] t
xpGenericString = (gxFromByteString, gxToByteString) `xpWrap` xpContent xpText0

--
-- Instances
--

instance IsXML Int where
    xmlPickler = xpPrim

instance IsXML Integer where
    xmlPickler = xpPrim

instance IsXML Text where
    xmlPickler = xpGenericString

instance IsXML ByteString where
    xmlPickler = xpGenericString

instance IsXML a => IsXML (Maybe a) where
    xmlPickler = xpOption xmlPickler

instance (IsXML a, IsXML b) => IsXML (Either a b) where
    xmlPickler = xmlPickler `xpEither` xmlPickler
