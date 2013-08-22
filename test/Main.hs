{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}

{-# OPTIONS_GHC -fno-warn-orphans      #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Main
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import           Control.Applicative
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Char8                as BS
import           Data.List                            (stripPrefix)
import           Data.Maybe
import           Data.String
import           GHC.Generics
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import           Text.XML.Expat.Pickle.Generic

main :: IO ()
main = defaultMain
    [ testGroup "Isomorphisms"
        [ testProperty "Flat"    (xml :: Iso Flat)
        , testProperty "Nested"  (xml :: Iso Nested)
        , testProperty "Maybe"   (xml :: Iso MaybeRec)
        , testProperty "Complex" (xml :: Iso Complex)
        , testProperty "List"    (xml :: Iso ListRec)
        , testProperty "Nullary" (xml :: Iso Nullary)
        ]
    , testGroup "Generic Option Modifiers"
        [ testProperty "Constructors" (xml :: Iso Ctors)
        , testProperty "Fields"       (xml :: Iso Fields)
        ]
    ]

data Flat = Flat
    { fooInt        :: Int
    , fooByteString :: ByteString
    } deriving (Eq, Show, Generic)

instance IsXML Flat

instance Arbitrary Flat where
    arbitrary = Flat <$> arbitrary <*> arbitrary

data Nested = Nested
    { barInt     :: Int
    , barInteger :: Integer
    , barFlat     :: Flat
    } deriving (Eq, Show, Generic)

instance IsXML Nested

instance Arbitrary Nested where
    arbitrary = Nested <$> arbitrary <*> arbitrary <*> arbitrary

data MaybeRec = MaybeRec
    { bazFlat :: Maybe Flat
    , bazInt :: Int
    } deriving (Eq, Show, Generic)

instance IsXML MaybeRec

instance Arbitrary MaybeRec where
    arbitrary = MaybeRec <$> arbitrary <*> arbitrary

data Complex = Complex
    { waldoMaybeRec   :: MaybeRec
    , waldoMaybe :: Maybe Flat
    } deriving (Eq, Show, Generic)

instance IsXML Complex

instance Arbitrary Complex where
    arbitrary = Complex
        <$> arbitrary
        <*> arbitrary

data ListRec = ListRec
    { wibList :: [Flat]
    } deriving (Eq, Show, Generic)

instance IsXML ListRec

instance Arbitrary ListRec where
    arbitrary = ListRec <$> arbitrary

data Nullary = PrefixXyzzy | PrefixThud
    deriving (Eq, Read, Show, Generic)

instance IsXML Nullary where
    xmlPickler = (xpContent xpPrim)
        { root = Just "Nullary"
        }

instance Arbitrary Nullary where
    arbitrary = elements [PrefixXyzzy, PrefixThud]

data Fields = Fields
    { thisPrefixInt :: Int
    , thisPrefixFlat :: Flat
    } deriving (Eq, Show, Generic)

instance IsXML Fields where
    xmlPickler = genericXMLPickler $ defaultXMLOptions
        { xmlFieldModifier = reverse
        }

instance Arbitrary Fields where
    arbitrary = Fields <$> arbitrary <*> arbitrary

data Ctors = PrefixCtors
    { ctorInt  :: Int
    , ctorFlat :: Flat
    } deriving (Eq, Show, Generic)

instance IsXML Ctors where
    xmlPickler = genericXMLPickler $ defaultXMLOptions
        { xmlCtorModifier = \s -> fromMaybe s $ stripPrefix "Prefix" s
        }

instance Arbitrary Ctors where
    arbitrary = PrefixCtors <$> arbitrary <*> arbitrary

instance Arbitrary ByteString where
    arbitrary = fmap fromString . listOf1 $ oneof
        [ choose ('\48', '\57')
        , choose ('\65', '\90')
        , choose ('\97', '\122')
        ]

type Iso a = Isomorphism a -> Bool

data Isomorphism a = Iso
    { domain   :: a
    , codomain :: ByteString
    , identity :: Either String a
    }

instance Show a => Show (Isomorphism a) where
    show Iso{..} = unlines
        [ "[Input]"
        , show domain
        , ""
        , "[Encoded]"
        , BS.unpack codomain
        , ""
        , "[Parsed]"
        , show identity
        ]

instance (Eq a, Arbitrary a, Show a, IsXML a) => Arbitrary (Isomorphism a) where
    arbitrary = do
        inp <- arbitrary
        let enc = toIndentedXML 2 inp
        return . Iso inp enc $ fromXML enc

xml :: (Eq a, Arbitrary a, IsXML a) => Isomorphism a -> Bool
xml (Iso d _ i) = either (const False) (== d) i
