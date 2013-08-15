{-# LANGUAGE DefaultSignatures               #-}
{-# LANGUAGE FlexibleContexts                #-}
{-# LANGUAGE FlexibleInstances               #-}
{-# LANGUAGE MultiParamTypeClasses           #-}
{-# LANGUAGE OverloadedStrings               #-}
{-# LANGUAGE ScopedTypeVariables             #-}
{-# LANGUAGE TypeOperators                   #-}
{-# LANGUAGE ViewPatterns                    #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- |
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
      IsXML   (..)

    -- * Options
    , Options (..)
    , defaultOptions

    -- * Functions
    , genericXMLPickler
    , encode
    , decode
    , decodeEither

    -- * Combinators
    , xpSum
    , xpEither

    -- * Re-exported Combinators
    , xpText0
    , xpText
    , xpList0
    , xpList
    , xpContent
    , xpPrim
    , xpWrap
    ) where

import           Data.ByteString       (ByteString)
import           Data.Char             (isLower)
import           Data.Text             (Text)
import           GHC.Generics
import qualified Text.XML.Expat.Pickle as Pickle
import           Text.XML.Expat.Pickle hiding (xpPrim)

--
-- Class
--

class IsXML a where
    xmlPickler :: PU [UNode ByteString] a

    default xmlPickler :: (Generic a, GIsXML [UNode ByteString] (Rep a))
                      => PU [UNode ByteString] a
    xmlPickler = genericXMLPickler defaultOptions

--
-- Functions
--

encode :: IsXML a => a -> ByteString
encode = pickleXML' (xpRoot xmlPickler)

decode :: IsXML a => ByteString -> Maybe a
decode = either (const Nothing) Just . decodeEither

decodeEither :: IsXML a => ByteString -> Either String a
decodeEither = unpickleXML' defaultParseOptions (xpRoot xmlPickler)

--
-- Defining Picklers
--

data Options = Options
    { constructorTagModifier :: String -> String
      -- ^ Function applied to constructor tags.
    , fieldLabelModifier     :: String -> String
      -- ^ Function applied to record field labels.
    }

defaultOptions :: Options
defaultOptions = Options id (dropWhile isLower)

genericXMLPickler opts =
    (to, from) `xpWrap` (gXMLPickler opts) (genericXMLPickler opts)

genericXMLStringPickler :: GenericXMLString t => PU [UNode ByteString] t
genericXMLStringPickler =
    (gxFromByteString, gxToByteString) `xpWrap` xpContent xpText0

--
-- Instances
--

instance IsXML Int where
    xmlPickler = xpPrim

instance IsXML Integer where
    xmlPickler = xpPrim

instance IsXML Text where
    xmlPickler = genericXMLStringPickler

instance IsXML ByteString where
    xmlPickler = genericXMLStringPickler

instance IsXML a => IsXML (Maybe a) where
    xmlPickler = xpOption xmlPickler

instance IsXML a => IsXML [a] where
    xmlPickler = xpList0 xmlPickler

--
-- Generics
--

class GIsXML t f where
    gXMLPickler :: Options -> PU t a -> PU t (f a)

instance IsXML a => GIsXML [UNode ByteString] (K1 i a) where
    gXMLPickler _ _ = (K1, unK1) `xpWrap` xmlPickler

instance GIsXML [t] U1 where
    gXMLPickler _ _ = (const U1, const ()) `xpWrap` xpUnit

instance (GIsXML t f, GIsXML t g) => GIsXML t (f :+: g) where
    gXMLPickler opts f = gXMLPickler opts f `xpSum` gXMLPickler opts f

instance (GIsXML [t] f, GIsXML [t] g) => GIsXML [t] (f :*: g) where
    gXMLPickler opts f = xpWrap
        (uncurry (:*:), \(a :*: b) -> (a, b))
        (gXMLPickler opts f `xpPair` gXMLPickler opts f)

instance (Datatype d, GIsXML t f) => GIsXML t (M1 D d f) where
    gXMLPickler opts = xpWrap (M1, unM1) . gXMLPickler opts

instance (Constructor c, GIsXML [UNode ByteString] f)
         => GIsXML [UNode ByteString] (M1 C c f) where
    gXMLPickler opts f = xpElemNodes
        (gxFromString . constructorTagModifier opts $ conName (undefined :: M1 C c f r))
        ((M1, unM1) `xpWrap` gXMLPickler opts f)

instance (Selector s, GIsXML [UNode ByteString] f)
         => GIsXML [UNode ByteString] (M1 S s f) where
    gXMLPickler opts f = xpElemNodes
        (gxFromString . fieldLabelModifier opts $ selName (undefined :: M1 S s f r))
        ((M1, unM1) `xpWrap` gXMLPickler opts f)

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
