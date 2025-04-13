module Main where

import Prelude

import Data.Bounded (Ordering(..))
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, over2)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (logShow)

newtype Point
    = Point
    { x :: Number
    , y :: Number
    }

derive instance Newtype Point _
derive instance Eq      Point

instance Show Point where
    show (Point {x, y}) =
        -- "(Point {x: " <> show x <> ", y: " <> show y <> "})"
        "("<>show x<>", "<>show y<>")"

origin :: Point
origin = Point { x: 0.0, y: 0.0 }

newtype Complex
    = Complex
    { real      :: Number
    , imaginary :: Number
    }

derive instance Newtype Complex _
derive instance Eq      Complex

derive newtype instance Ring Complex

instance Show Complex where
    show (Complex {real, imaginary}) =
        show real
        <> whenMonoid (imaginary >= 0.0) "+"
        <> show imaginary
        <> "i"

instance Semiring Complex where
    zero = Complex { real: 0.0, imaginary: 0.0}
    one  = Complex { real: 1.0, imaginary: 0.0}
    add  = over2 Complex add
    mul  = over2 Complex mul

whenMonoid :: forall a. Monoid a => Boolean -> a -> a
whenMonoid b a = if b then a else mempty

data Shape
    = Circle    Point Number
    | Rectangle Point Number Number
    | Line      Point Point
    | Text      Point String

derive instance Generic Shape _

instance Show Shape where
    show = genericShow

--- Non Empty Array ---
data NonEmpty a = NonEmpty a (Array a)

derive instance Eq a => Eq (NonEmpty a)

instance Semigroup (NonEmpty a) where
    append (NonEmpty a arr) (NonEmpty b brr) =
        NonEmpty a (append arr (append [b] brr))

instance Functor NonEmpty where
    map f (NonEmpty a arr) =
        NonEmpty (f a) (map f arr)

instance Foldable NonEmpty

--- Infinite from Ord ---
data Extended a = Infinite | Finite a

derive instance Eq a => Eq (Extended a)

instance Ord a => Ord (Extended a) where
    compare Infinite Infinite     = EQ
    compare Infinite _            = GT

    compare (Finite a) (Finite b) = compare a b
    compare (Finite _) Infinite   = LT

main :: Effect Unit
main = do
    logShow origin
    logShow (Circle origin 10.0)
