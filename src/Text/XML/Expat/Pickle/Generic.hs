{-# LANGUAGE DefaultSignatures               #-}
{-# LANGUAGE FlexibleContexts                #-}
{-# LANGUAGE FlexibleInstances               #-}
{-# LANGUAGE OverloadedStrings               #-}
{-# LANGUAGE RecordWildCards                 #-}
{-# LANGUAGE ScopedTypeVariables             #-}
{-# LANGUAGE TypeOperators                   #-}
{-# LANGUAGE MultiParamTypeClasses           #-}

-- {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans            #-}

{-# LANGUAGE DeriveGeneric #-}

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

module Text.XML.Expat.Pickle.Generic where

import Data.ByteString       (ByteString)
import Data.Char             (isLower)
import GHC.Generics
import Text.XML.Expat.Pickle

data Options = Options
    { constructorTagModifier :: String -> String
      -- ^ Function applied to constructor tags.
    , fieldLabelModifier     :: String -> String
      -- ^ Function applied to record field labels.
    }

defaultOptions :: Options
defaultOptions = Options id (dropWhile isLower)

class IsXML a where
    xmlPickler :: PU (UNode ByteString) a

    default xmlPickler :: (Generic a, GIsXML [UNode ByteString] (Rep a))
                      => PU (UNode ByteString) a
    xmlPickler = xpRoot $ genericXMLPickler defaultOptions

encode :: IsXML a => a -> ByteString
encode = pickleXML' xmlPickler

decode :: IsXML a => ByteString -> Maybe a
decode = either (const Nothing) Just . decodeEither

decodeEither :: IsXML a => ByteString -> Either String a
decodeEither = unpickleXML' defaultParseOptions xmlPickler


data Thing = Thing { thingInt :: Int } deriving (Show, Generic)

thing = Thing 2

instance IsXML Thing


genericXMLPickler opts =
    (to, from) `xpWrap` (gXMLPickler opts) (genericXMLPickler opts)

class GIsXML t f where
    gXMLPickler :: Options -> PU t a -> PU t (f a)

instance XmlPickler t a => GIsXML t (K1 i a) where
    gXMLPickler _ _ = (K1, unK1) `xpWrap` xpickle

-- instance GIsXML t a => GIsXML t (M1 i c a) where
--     gXMLPickler opts = xpWrap (M1, unM1) . gXMLPickler opts

instance GIsXML [t] U1 where
    gXMLPickler _ _ = (const U1, const ()) `xpWrap` xpUnit

-- instance (GIsXML t f, GIsXML t g) => GIsXML t (f :+: g) where
--     gXMLPickler opts f = gXMLPickler opts f `xpSum` gXMLPickler opts f

-- instance (GIsXML [t] f, GIsXML [t] g) => GIsXML [t] (f :*: g) where
--     gXMLPickler opts f = xpWrap
--         (uncurry (:*:), \(a :*: b) -> (a, b))
--         (gXMLPickler opts f `xpPair` gXMLPickler opts f)

instance (Datatype d, GIsXML t f) => GIsXML t (M1 D d f) where
    gXMLPickler opts = xpWrap (M1, unM1) . gXMLPickler opts

-- Only supports child nodes, not attributes
instance (Constructor c, GIsXML [UNode ByteString] f)
         => GIsXML [UNode ByteString] (M1 C c f) where
    gXMLPickler opts@Options{..} f = xpElemNodes
        (gxFromString . constructorTagModifier $ conName (undefined :: M1 C c f r))
        ((M1, unM1) `xpWrap` gXMLPickler opts f)

-- Only supports child nodes, not attributes
instance (Selector s, GIsXML [UNode ByteString] f)
         => GIsXML [UNode ByteString] (M1 S s f) where
    gXMLPickler opts@Options{..} f = xpElemNodes
        (gxFromString . fieldLabelModifier $ selName (undefined :: M1 S s f r))
        ((M1, unM1) `xpWrap` gXMLPickler opts f)

-- xpElemNodes :: (Eq tag, Show tag) => tag -> PU [Node tag text] a -> PU [Node tag text] a



instance XmlPickler [t] () where
    xpickle = xpUnit

instance (XmlPickler [t] a, XmlPickler [t] b) => XmlPickler [t] (a, b) where
    xpickle = xpPair xpickle xpickle

instance ( XmlPickler [t] a
         , XmlPickler [t] b
         , XmlPickler [t] c
         ) => XmlPickler [t] (a, b, c) where
    xpickle = xpTriple xpickle xpickle xpickle

instance ( XmlPickler [t] a
         , XmlPickler [t] b
         , XmlPickler [t] c
         , XmlPickler [t] d
         ) => XmlPickler [t] (a, b, c, d) where
    xpickle = xp4Tuple xpickle xpickle xpickle xpickle

instance ( XmlPickler [t] a
         , XmlPickler [t] b
         , XmlPickler [t] c
         , XmlPickler [t] d
         , XmlPickler [t] e
         ) => XmlPickler [t] (a, b, c, d, e) where
    xpickle = xp5Tuple xpickle xpickle xpickle xpickle xpickle

instance XmlPickler [t] a => XmlPickler [t] (Maybe a) where
    xpickle = xpOption xpickle



-- instance XmlPickler t Bool where
--     xpickle = (inp, out) `xpWrap` xpText
--       where
--         inp (lower -> "true")  = True
--         inp (lower -> "false") = False
--         inp _ = error "No parse for bool in toBool (XmlPickler)."

--         out True  = "true"
--         out False = "false"

--         lower = map toLower

-- instance GIsXML t (K1 i String) where
--     gXMLPickler _ _ = (K1, unK1) `xpWrap` xpText0

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
