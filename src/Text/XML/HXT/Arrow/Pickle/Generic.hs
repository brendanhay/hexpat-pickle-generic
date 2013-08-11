{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- |
-- Module      : Text.XML.HXT.Arrow.Pickle.Generic
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               Berkeley Software Distribution License, v. 3.0.
--               You can obtain it at
--               http://http://opensource.org/licenses/BSD-3-Clause.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.XML.HXT.Arrow.Pickle.Generic where

import Data.Char                        (toLower)
import GHC.Generics
import Text.XML.HXT.Arrow.Pickle
import Text.XML.HXT.Arrow.Pickle.Schema
import Text.XML.HXT.Arrow.Pickle.Xml

data Options = Options

class IsXML a where
    pickleXML :: Options -> PU a

    default pickleXML :: (Generic a, GIsXML (Rep a)) => PU a
    pickleXML = genericPickleXML defaultOptions

genericPickleXML :: (Generic a, GIsXML (Rep a)) => Options -> PU a
genericPickleXML opts =
    (to, from) `xpWrap` (gPickleXML opts) (genericPickleXML opts)

defaultOptions :: Options
defaultOptions = Options

class GIsXML f where
    gPickleXML :: Options -> PU p -> PU (f p)

instance GIsXML a => GIsXML (M1 i c a) where
    -- Meta-information, which is not handled elsewhere
    gPickleXML opts = xpWrap (M1, unM1) . gPickleXML opts

instance XmlPickler a => GIsXML (K1 i a) where
    -- Constant values are decoded using their XmlPickler instance
    gPickleXML _ _ = (K1, unK1) `xpWrap` xpickle

instance GIsXML U1 where
    -- Empty constructors
    gPickleXML _ _ = (const U1, const ()) `xpWrap` xpUnit

instance (GIsXML f, GIsXML g) => GIsXML (f :+: g) where
    -- Sums: Encode choice between constructors
    gPickleXML opts f = gPickleXML opts f `xpSum` gPickleXML opts f

instance (GIsXML f, GIsXML g) => GIsXML (f :*: g) where
    -- Products: Encode multiple argument constructors
    gPickleXML opts f = xpWrap
        (uncurry (:*:), \(a :*: b) -> (a, b))
        (gPickleXML opts f `xpPair` gPickleXML opts f)

instance (Constructor c, GIsXML f) => GIsXML (M1 C c f) where
    gPickleXML opts f = xpElem
        (map toLower $ conName (undefined :: M1 C c f r))
        ((M1, unM1) `xpWrap` (gPickleXML opts f))

instance (Selector s, GIsXML f) => GIsXML (M1 S s f) where
    gPickleXML opts f = xpElem
        (map toLower $ selName (undefined :: M1 S s f r))
        ((M1, unM1) `xpWrap` gPickleXML opts f)

--
-- Pickler Combinators
--

xpSum :: PU (f r) -> PU (g r) -> PU ((f :+: g) r)
xpSum l r = (i, o) `xpWrap` xpEither l r
  where
    i (Left  x) = L1 x
    i (Right x) = R1 x

    o (L1 x) = Left x
    o (R1 x) = Right x

xpEither :: PU a -> PU b -> PU (Either a b)
xpEither ~(PU fl tl sa) ~(PU fr tr sb) = PU pickle unpickle schema
  where
    pickle (Left x)  = fl x
    pickle (Right y) = fr y

    unpickle = UP $ \x ->
        case runUP tl x of
            (Left err, _) -> lmap (fmap Right) (runUP tr x)
            r             -> lmap (fmap Left) r

    lmap f (a, b) = (f a, b)

    schema = sa `scAlt` sb

--
-- Instances
--

instance XmlPickler Bool where
    xpickle = (toBool, fromBool) `xpWrap` xpText
      where
        toBool "true"  = True
        toBool "false" = False
        toBool _       = error "No parse for bool in toBool (XmlPickler)."

        fromBool True  = "true"
        fromBool False = "false"

instance GIsXML (K1 i String) where
    gPickleXML _ _ = (K1, unK1) `xpWrap` xpText0
