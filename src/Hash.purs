module Hash where

import Prelude

import Data.Array (foldl, nubByEq, length)
import Data.Char (toCharCode)
import Data.Function (on)
import Data.String.CodeUnits (toCharArray)

newtype HashCode = HashCode Int

instance Eq HashCode where
    eq (HashCode a) (HashCode b) =
        a == b

instance Show HashCode where
    show (HashCode h) = show h

hashcode :: Int -> HashCode
hashcode h = HashCode (h `mod` 65535)

class Eq a <= Hashable a where
    hash :: a -> HashCode

combinehashes :: HashCode -> HashCode -> HashCode
combinehashes (HashCode a) (HashCode b) =
    hashcode (73 * a + 51 * b)

hashequal :: forall a. Hashable a => a -> a -> Boolean
hashequal = eq `on` hash

instance Hashable Int where
    hash = hashcode

instance Hashable Boolean where
    hash false = hashcode 0
    hash true  = hashcode 1

instance Hashable Char where
    hash = toCharCode >>> hash

instance Hashable a => Hashable (Array a) where
    hash =
        map hash >>> foldl combinehashes (hashcode 0)

instance Hashable String where
    hash = toCharArray >>> hash

arrayhasduplicates :: forall a. Hashable a => Array a -> Boolean
arrayhasduplicates arr =
    let
        aretrueduplicate :: a -> a -> Boolean
        aretrueduplicate x y = hashequal x y && x == y
        uniquearr = nubByEq aretrueduplicate arr
     in length uniquearr < length arr
