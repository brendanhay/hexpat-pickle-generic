{-# LANGUAGE DeriveGeneric             #-}
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
import System.IO.Unsafe

main :: IO ()
main = defaultMain
    [ testGroup "Isomorphisms"
        [ -- testProperty "Flat"    (xml :: Iso Foo)
        testProperty "Nested"  (xml :: Iso Bar)
        -- , testProperty "Maybe"   (xml :: Iso Baz)
        -- , testProperty "Complex" (xml :: Iso Waldo)
        -- , testProperty "List"    (xml :: Iso Wibble)
        ]
    -- , testGroup "Generic Option Modifiers"
    --     [ testProperty "Constructors" (xml :: Iso Fred)
    --     , testProperty "Fields"       (xml :: Iso Plugh)
    --     ]
    ]

data Foo = Foo
    { fooInt        :: Int
    , fooByteString :: ByteString
    } deriving (Eq, Show, Generic)

instance IsXML Foo

instance Arbitrary Foo where
    arbitrary = Foo <$> arbitrary <*> arbitrary

data Bar = Bar
    { barInt     :: Int
    , barInteger :: Integer
    , barFoo     :: Foo
    } deriving (Eq, Show, Generic)

instance IsXML Bar

instance Arbitrary Bar where
    arbitrary = Bar <$> arbitrary <*> arbitrary <*> arbitrary

data Baz = Baz
    { bazFoo :: Maybe Foo
    , bazInt :: Int
    } deriving (Eq, Show, Generic)

instance IsXML Baz

instance Arbitrary Baz where
    arbitrary = Baz <$> arbitrary <*> arbitrary

data Waldo = Waldo
    { waldoBaz   :: Baz
    , waldoMaybe :: Maybe Foo
    } deriving (Eq, Show, Generic)

instance IsXML Waldo

instance Arbitrary Waldo where
    arbitrary = Waldo
        <$> arbitrary
        <*> arbitrary

data Wibble = Wibble
    { wibList :: [Int]
    } deriving (Eq, Show, Generic)

instance IsXML Wibble

instance Arbitrary Wibble where
    arbitrary = Wibble <$> arbitrary

data Fred = PrefixXyzzy | PrefixThud
    deriving (Eq, Show, Generic)

instance IsXML Fred where
    xmlPickler = genericXMLPickler $ defaultXMLOptions
        { xmlCtorModifier = \s -> fromMaybe s $ stripPrefix "Prefix" s
        }

instance Arbitrary Fred where
    arbitrary = elements [PrefixXyzzy, PrefixThud]

data Plugh = Plugh
    { thisPrefixInt :: Int
    , thisPrefixFoo :: Foo
    } deriving (Eq, Show, Generic)

instance IsXML Plugh where
    xmlPickler = genericXMLPickler $ defaultXMLOptions
        { xmlFieldModifier = reverse
        }

instance Arbitrary Plugh where
    arbitrary = Plugh <$> arbitrary <*> arbitrary

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

        return . unsafePerformIO $ do
            BS.putStrLn enc
            return . Iso inp enc $ fromXML enc

xml :: (Eq a, Arbitrary a, IsXML a) => Isomorphism a -> Bool
xml (Iso d _ i) = either (const False) (== d) i
