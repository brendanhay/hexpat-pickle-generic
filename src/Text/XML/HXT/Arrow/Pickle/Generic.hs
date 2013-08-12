{-# LANGUAGE DefaultSignatures               #-}
{-# LANGUAGE FlexibleContexts                #-}
{-# LANGUAGE FlexibleInstances               #-}
{-# LANGUAGE OverloadedStrings               #-}
{-# LANGUAGE RecordWildCards                 #-}
{-# LANGUAGE ScopedTypeVariables             #-}
{-# LANGUAGE TypeOperators                   #-}
{-# LANGUAGE ViewPatterns                    #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans            #-}


{-# LANGUAGE MultiParamTypeClasses                    #-}

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

import Data.ByteString       (ByteString)
import Data.Char             (isLower, toLower)
import GHC.Generics
import Text.XML.Expat.Pickle

--
-- Options
--

data Options = Options
    { constructorTagModifier :: String -> String
      -- ^ Function applied to constructor tags.
    , fieldLabelModifier     :: String -> String
      -- ^ Function applied to record field labels.
    }

defaultOptions :: Options
defaultOptions = Options id (dropWhile isLower)

--
-- IsXML
--

class IsXML t a where
    pickleXML :: Options -> PU t a

    -- default pickleXML :: (Generic a, GIsXML (Rep a)) => PU t a
    -- pickleXML = genericPickleXML defaultOptions

class GIsXML t f where
    gPickleXML :: Options -> PU t a -> PU t (f a)

-- genericPickleXML opts =
--     (to, from) `xpWrap` (gPickleXML opts) (genericPickleXML opts)

--
-- Generic Instances
--

instance GIsXML t a => GIsXML t (M1 i c a) where
    gPickleXML opts = xpWrap (M1, unM1) . gPickleXML opts

instance XmlPickler t a => GIsXML t (K1 i a) where
    gPickleXML _ _ = (K1, unK1) `xpWrap` xpickle

instance GIsXML [t] U1 where
    gPickleXML _ _ = (const U1, const ()) `xpWrap` xpUnit

instance (GIsXML t f, GIsXML t g) => GIsXML t (f :+: g) where
    gPickleXML opts f = gPickleXML opts f `xpSum` gPickleXML opts f

instance (GIsXML [t] f, GIsXML [t] g) => GIsXML [t] (f :*: g) where
    gPickleXML opts f = xpWrap
        (uncurry (:*:), \(a :*: b) -> (a, b))
        (gPickleXML opts f `xpPair` gPickleXML opts f)

instance (Constructor c, GIsXML [Node String t] f) => GIsXML [Node String t] (M1 C c f) where
    gPickleXML opts@Options{..} f = xpElem
        (constructorTagModifier $ conName (undefined :: M1 C c f r))
        ((M1, unM1) `xpWrap` (gPickleXML opts f))
        ((M1, unM1) `xpWrap` (gPickleXML opts f))

-- instance (Selector s, GIsXML t f) => GIsXML t (M1 S s f) where
--     gPickleXML opts@Options{..} f = xpElem
--         (fieldLabelModifier $ selName (undefined :: M1 S s f r))
--         ((M1, unM1) `xpWrap` gPickleXML opts f)

--
-- Concrete Instances
--

-- instance XmlPickler Bool where
--     xpickle = (inp, out) `xpWrap` xpText
--       where
--         inp (lower -> "true")  = True
--         inp (lower -> "false") = False
--         inp _ = error "No parse for bool in toBool (XmlPickler)."

--         out True  = "true"
--         out False = "false"

--         lower = map toLower

-- instance GIsXML t (K1 i String) where
--     gPickleXML _ _ = (K1, unK1) `xpWrap` xpText0

--
-- Combinators
--

xpSum :: PU t (f r) -> PU t (g r) -> PU t ((f :+: g) r)
xpSum left right = (inp, out) `xpWrap` xpEither left right
  where
    inp (Left  x) = L1 x
    inp (Right x) = R1 x

    out (L1 x) = Left x
    out (R1 x) = Right x

xpEither :: PU t a -> PU t b -> PU t (Either a b)
xpEither ~(PU _ uea pa) ~(PU ub ueb pb) = PU unpickle unpickleEither pickle
  where
    unpickle t = case uea t of
        Right x -> Left x
        Left  _ -> Right $ ub t

    unpickleEither t = case uea t of
        Right x -> Right . Left $ x
        Left  _ -> Right `fmap` ueb t

    pickle (Left x)  = pa x
    pickle (Right y) = pb y
