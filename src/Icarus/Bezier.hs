module Icarus.Bezier (Point(..),
                      Despair(..),
                      pointToList,
                      pointToTuple, tupleToPoint,
                      bezier,
                      line1d',
                      cubic, cubicSeq, trange) where

import Control.Monad (zipWithM)


-------------------------------------------------------------------------------
-- http://mathfaculty.fullerton.edu/mathews/n2003/BezierCurveMod.html
-------------------------------------------------------------------------------

newtype Despair a = Despair { getDespair :: (a, a) } deriving (Show, Eq, Ord)

-- -- To apply a function per coord the Despair signature should be `Despair a`
-- -- where `a` would be a tuple.
instance Functor Despair where
  fmap f (Despair (x, y)) = Despair (f x, f y)

-- instance Applicative Despair where
--   pure = Despair
--   Despair (f, g) <*> Despair (x, y) = Despair ((f x), (g y))



-- TODO: Should be restricted to numbers.
data Point a = Point { getCoordX :: a,
                       getCoordY :: a } deriving (Show, Eq, Ord)

-- Let's opperate on Points without manual unwrapping.
--
--    fmap (* 2) (Point 3 4)  -- => Point 6 8
instance Functor Point where
  fmap f (Point x y) = Point (f x) (f y)

-- Applicative functors too
--
--    (Point (* 2) (* 3)) <*> (Point 2 4) -- => Point 4 12
--    pure (+) <*> Point 1 1 <*> (Point 1 2) -- => Point 2 3
--    (+) <$> Point 1 1 <*> (Point 1 2) -- => Point 2 3
instance Applicative Point where
  pure a = Point a a
  Point f g <*> Point x y = Point (f x) (g y)


instance Num a => Monoid (Point a) where
  mempty = Point 0 0
  Point x0 y0 `mappend` Point x1 y1 = Point (x0 + x1) (y0 + y1)


-- Notes/Ideas
--
-- Is it possible to create a function with `sequenceA` that gathers all points
-- and applies the interpolation function? is it idiotic?
-- maybe it is a matter of using `map` or `zipWith`.

tupleToPoint :: (a, a) -> Point a
tupleToPoint (x, y) = Point x y

pointToList :: Point a -> [a]
pointToList (Point x y) = [x, y]

pointToTuple :: Point a -> (a, a)
pointToTuple (Point x y) = (x, y)

cubicSeq :: Point Float -> Point Float -> Point Float -> Point Float
         -> Float -> Float
         -> [Point Float]
cubicSeq p0 p1 p2 p3
         t0 t1 = map (cubic p0 p1 p2 p3) $ trange t0 t1

-- cubicSeq helper
trange :: Float -> Float -> [Float]
trange a b = [x | x <- xs, x >= a,
                           x <= b]
  where xs = [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]

-- p = P0 B1(t) + P1 B2(t) + P2 B3(t) + P3 B4(t)
-- where:
--   Pi are the control points
--   Bi are the Beziér functions
--   t is a percentage of the distance along the curve (between 0 and 1)
--   p is the point in 2D space
cubic :: Point Float -> Point Float -> Point Float -> Point Float
      -> Float
      -> Point Float
cubic (Point x0 y0)
      (Point x1 y1)
      (Point x2 y2)
      (Point x3 y3) t = Point (coord x0 x1 x2 x3 t)
                              (coord y0 y1 y2 y3 t)

-- parametric equation
coord :: Float -> Float -> Float -> Float -> Float -> Float
coord x1 x2 x3 x4 t = x1 * b1 t
                    + x2 * b2 t
                    + x3 * b3 t
                    + x4 * b4 t

b1 :: Float -> Float
b1 t = (1 - t) ** 3

b2 :: Float -> Float
b2 t = 3 * (1 - t) ** 2 * t

b3 :: Float -> Float
b3 t = 3 * (1 - t) * t ** 2

b4 :: Float -> Float
b4 t = t ** 3


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- REVIEW Functors, Applicatives and Monads
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- bezier' :: [Point] -> Point
-- bezier' [p] = const p
-- bezier' ps  = do l <- bezier' (init ps)
--                  r <- bezier' (tail ps)
--                  line' l r

-- Line between two points:
-- line' :: Point -> Point -> Point
-- line' (Point x1 y1)
--       (Point x2 y2) = toPoint $ zipWithM line1d' [x1, y1] [x2, y2]

-- toPoint :: [Float] -> Point
-- toPoint [x, y] = Point x y

-- Linear interpolation between two numbers
line1d' :: Float -> Float -> Float -> Float
line1d' x y t = (1 - t) * x + t * y



-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- https://github.com/hrldcpr/Bezier.hs/blob/master/Bezier.hs
-------------------------------------------------------------------------------
-- bezier of one point is fixed at that point, and bezier of N points is the
-- linear interpolation between bezier of first N-1 points and bezier of last
-- N-1 points.
type BPoint = [Float]
type Parametric a = Float -> a


bezier :: [BPoint] -> Parametric BPoint
bezier [p] = const p
bezier ps  = do l <- bezier (init ps)
                r <- bezier (tail ps)
                line l r

-- line between two points:
line :: BPoint -> BPoint -> Parametric BPoint
line = zipWithM line1d

-- linear interpolation between two numbers:
line1d :: Float -> Float -> Parametric Float
line1d a b t = (1 - t) * a + t * b
