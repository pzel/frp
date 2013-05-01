{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, TupleSections  #-}
module Main where

import Control.Applicative
import Data.Function
import Data.Ratio
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Function

newtype Time = Time { getT :: Float } deriving (Eq,Num,Ord,Show)
instance Fractional Time where
  t1 / t2 = Time $ (getT t1) / (getT t2)
  fromRational = undefined

instance Enum Time where
  fromEnum = round . (1000 *) . getT
  toEnum = Time . fromIntegral . (1000 *)

timeToInt :: Time -> Int
timeToInt = round . getT

data Behavior a = Behavior { at :: Time -> a }
instance Functor Behavior where
  fmap f b = Behavior { at = \t -> f (at b t) }

instance Applicative Behavior where
  pure = mkB . const
  ba <*> bb = mkB $ \t -> (at ba) t (at bb t)

mkB :: (Time -> a) -> Behavior a
mkB f = Behavior { at = f }

lift0 :: a -> Behavior a
lift0 = pure

lift1 :: (a -> b) -> Behavior a -> Behavior b
lift1 = fmap

lift2 :: (a -> b -> c) -> Behavior a -> Behavior b -> Behavior c
lift2 f ba bb = f <$> ba <*> bb

integral :: Num a => Behavior a -> Time -> Behavior a
integral b from = mkB (\to-> sum (map (at b) [from .. to]))


-- Some behaviors

timeTransform :: Behavior a -> Behavior Time -> Behavior a
timeTransform b tb = mkB (\t -> at b (at tb t))

time :: Behavior Time 
time = mkB id

ints :: Behavior Int
ints = mkB timeToInt 

{- Test-related code & properties -}

instance Arbitrary Time where
  arbitrary = Time <$> arbitrary

{- 
The Time datatype has been simplified and the below no longer apply

prop_time_ordering = \tvx tvy -> tvx <= tvy ==>   At tvx <= AtLeast tvy
prop_time_bottoms = \tvx tvy -> isBottom         (AtLeast tvx <= At tvy) &&
                 		    	     	isBottom         (AtLeast tvx <= AtLeast tvy) &&
				                        (not . isBottom) (At tvx <= At tvy)
 -}

prop_timebehavior_id = \t -> at time t == t

prop_lift0 v = \t -> at (lift0 v) t == v 
  where types = (v :: Int)

prop_lift1_plus c v = \t-> at (lift1 (+c) (lift0 v)) t == (+c) (at (lift0 v) t)
  where types = (c::Int,v::Int)

prop_lift1_tupl c v = \t-> at (lift1 (c,) (lift0 v)) t == (c,) (at (lift0 v) t)
  where types = (c::Int,v::Int)

prop_lift2_tupl v1 v2 = \t-> at (lift2 (,) (lift0 v1) (lift0 v2)) t == (,) (at (lift0 v1) t) (at (lift0 v2) t)
  where types = (v1::Int, v2::Int)

prop_behavior_is_a_functor v = \t-> at (fmap (+1) (lift0 v)) t == (+1) (at (lift0 v) t)
  where types = (v::Int)

prop_behavior_is_applicative_pure v = \t-> at ((+1) <$> (pure v)) t == (+1) (at (pure v) t)
  where types = (v::Int)

prop_behavior_is_applicative_star v1 v2 = \t-> at ((+) <$> (pure v1) <*> (pure v2)) t == (+) (at (pure v1) t) (at (pure v2) t)
  where types = (v1::Int, v2::Int)

-- silly, but I wanted to see with my own eyes
prop_behavior_is_applicative_star5 v1 v2 v3 v4 v5 = \t->
  let f5 = \a b c d e -> sum [a,b,c,d,e]
      (p1:p2:p3:p4:p5:[]) = map pure [v1,v2,v3,v4,v5]
  in  at (f5 <$> p1 <*> p2 <*> p3 <*> p4 <*> p5) t == 
      f5 (at p1 t) (at p2 t) (at p3 t) (at p4 t) (at p5 t)
 where types = (v1::Int)

prop_show_time t = at (show <$> time) t == show t
 where types = (t::Time)

prop_ints t1 t2 =  t1 <= t2 ==> at ints (t1) <= at ints (t2)

prop_add_ints t = at ((+) <$> ints <*> ints) t == (timeToInt t) * 2

prop_time_transform t = let timeInHalf = (/) <$> time <*> pure 2
                        in at (timeTransform ints  timeInHalf) t ==
                    ((at ints) . (at timeInHalf)) t

prop_integral t0 t1 = (abs (t0 - t1)) < 15 ==>
              at (integral time t0) t1 == sum [t0..t1]

main :: IO Bool
main = do
     $forAllProperties quickCheckResult --verboseCheckResult