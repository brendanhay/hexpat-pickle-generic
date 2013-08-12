{-# LANGUAGE DefaultSignatures               #-}
{-# LANGUAGE FlexibleContexts                #-}
{-# LANGUAGE FlexibleInstances               #-}
{-# LANGUAGE OverlappingInstances            #-}
{-# LANGUAGE OverloadedStrings               #-}
{-# LANGUAGE RecordWildCards                 #-}
{-# LANGUAGE ScopedTypeVariables             #-}
{-# LANGUAGE TypeOperators                   #-}
{-# LANGUAGE ViewPatterns                    #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans            #-}

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

import Data.Char                        (isLower, toLower)
import Data.Maybe
import GHC.Generics
import Text.XML.HXT.Arrow.Pickle.Schema
import Text.XML.HXT.Arrow.Pickle.Xml
import Text.XML.HXT.Core

data Options = Options
    { constructorTagModifier :: String -> String
      -- ^ Function applied to constructor tags.
    , fieldLabelModifier     :: String -> String
      -- ^ Function applied to record field labels.
    }

defaultOptions :: Options
defaultOptions = Options id (dropWhile isLower)

class IsXML a where
    pickleXML :: PU a

    default pickleXML :: (Generic a, GIsXML (Rep a)) => PU a
    pickleXML = genericPickleXML defaultOptions

class GIsXML f where
    gPickleXML :: Options -> PU a -> PU (f a)

genericPickleXML opts =
    (to, from) `xpWrap` (gPickleXML opts) (genericPickleXML opts)

encode :: IsXML a => a -> String
encode = concat . (pickleDoc pickleXML >>> runLA (writeDocumentToString sys))
  where
    sys = [ withValidate no
          , withCheckNamespaces no
          , withRemoveWS yes
          ]

decode :: IsXML a => String -> Either String a
decode = fromMaybe (Left "Failed to parse anything from input")
    . listToMaybe
    . runLA (xread >>> arr (unpickleDoc' pickleXML))

instance GIsXML a => GIsXML (M1 i c a) where
    gPickleXML opts = xpWrap (M1, unM1) . gPickleXML opts

instance XmlPickler a => GIsXML (K1 i a) where
    gPickleXML _ _ = (K1, unK1) `xpWrap` xpickle

instance GIsXML (K1 i String) where
    gPickleXML _ _ = (K1, unK1) `xpWrap` xpText0

instance GIsXML U1 where
    gPickleXML _ _ = (const U1, const ()) `xpWrap` xpUnit

instance (GIsXML f, GIsXML g) => GIsXML (f :+: g) where
    gPickleXML opts f = gPickleXML opts f `xpSum` gPickleXML opts f

instance (GIsXML f, GIsXML g) => GIsXML (f :*: g) where
    gPickleXML opts f = xpWrap
        (uncurry (:*:), \(a :*: b) -> (a, b))
        (gPickleXML opts f `xpPair` gPickleXML opts f)

instance (Constructor c, GIsXML f) => GIsXML (M1 C c f) where
    gPickleXML opts@Options{..} f = xpElem
        (constructorTagModifier $ conName (undefined :: M1 C c f r))
        ((M1, unM1) `xpWrap` (gPickleXML opts f))

instance (Selector s, GIsXML f) => GIsXML (M1 S s f) where
    gPickleXML opts@Options{..} f = xpElem
        (fieldLabelModifier $ selName (undefined :: M1 S s f r))
        ((M1, unM1) `xpWrap` gPickleXML opts f)

instance XmlPickler Bool where
    xpickle = (inp, out) `xpWrap` xpText
      where
        inp (lower -> "true")  = True
        inp (lower -> "false") = False
        inp _ = error "No parse for bool in toBool (XmlPickler)."

        out True  = "true"
        out False = "false"

        lower = map toLower

xpSum :: PU (f r) -> PU (g r) -> PU ((f :+: g) r)
xpSum l r = (inp, out) `xpWrap` xpEither l r
  where
    inp (Left  x) = L1 x
    inp (Right x) = R1 x

    out (L1 x) = Left x
    out (R1 x) = Right x

xpEither :: PU a -> PU b -> PU (Either a b)
xpEither ~(PU fl tl sa) ~(PU fr tr sb) = PU pickle unpickle schema
  where
    pickle (Left x)  = fl x
    pickle (Right y) = fr y

    unpickle = UP $ \x ->
        case runUP tl x of
            (Left _, _) -> lmap (fmap Right) (runUP tr x)
            r           -> lmap (fmap Left) r

    lmap f (a, b) = (f a, b)

    schema = sa `scAlt` sb
