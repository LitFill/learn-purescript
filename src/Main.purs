module Main where

import Prelude
import Hash

import Data.Monoid (power)
import Data.Array (nub, nubEq)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximum)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
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
derive instance Ord     Point

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
derive instance Eq      Shape
derive instance Ord     Shape

instance Show Shape where
    show = genericShow

dedupShapes :: Array Shape -> Array Shape
dedupShapes = nubEq

dedupShapesFast :: Array Shape -> Array Shape
dedupShapesFast = nub

--- Non Empty Array ---
data NonEmpty a = NonEmpty a (Array a)

derive instance Eq a => Eq (NonEmpty a)

instance Semigroup (NonEmpty a) where
    append (NonEmpty a arr) (NonEmpty b brr) =
        NonEmpty a (append arr (append [b] brr))

instance Functor NonEmpty where
    map f (NonEmpty a arr) =
        NonEmpty (f a) (map f arr)

instance Foldable NonEmpty where
    foldl f i (NonEmpty a arr) = f (foldl f i arr) a
    foldr f i (NonEmpty a arr) = f a (foldr f i arr)
    foldMap f (NonEmpty a arr) = f a <> foldMap f arr

--- Infinite from Ord ---
data Extended a = Infinite | Finite a

derive instance Eq a => Eq (Extended a)

instance Ord a => Ord (Extended a) where
    compare Infinite Infinite     = EQ
    compare Infinite _            = GT

    compare (Finite a) (Finite b) = compare a b
    compare (Finite _) Infinite   = LT

--- One More Container ---
data OneMore f a = OneMore a (f a)

instance Foldable f => Foldable (OneMore f) where
    foldl f i (OneMore a as) = f (foldl f i as) a
    foldr f i (OneMore a as) = f a (foldr f i as)
    foldMap f (OneMore a as) = f a <> foldMap f as

--- Unsafe Maximum ---
unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum xs =
    case maximum xs of Just n -> n

--- Action Class ---
class Monoid m <= Action m a where
    act :: m -> a -> a
--| Law:
-- act mempty     a ≡ a
-- act (m1 <> m2) a ≡ act m1 (act m2 a)

newtype Multiply = Multiply Int

derive instance Eq Multiply

instance Semigroup Multiply where
    append (Multiply n) (Multiply m) =
        Multiply (n * m)

instance Monoid Multiply where
    mempty = Multiply 1

instance Action Multiply Int where
    act (Multiply n) a = n * a

instance Action Multiply String where
    act (Multiply n) s = power s n

instance Action m a => Action m (Array a) where
    act m = map (act m)

newtype Self m = Self m

instance Monoid m => Action m (Self m) where
    act m (Self n) = Self (m <> n)

main :: Effect Unit
main = do
    logShow origin
    logShow (Circle origin 10.0)
    logShow (hash "1")
