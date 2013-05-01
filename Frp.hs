{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, TupleSections  #-}

module Frp (
  Behavior
 ,Event
 ,Time(..)
 ,at
 ,integral
 ,mkB
 ,mkE
 ,occ
 ,runTests
 ,timeTransform
 ,time
 ,untilB
 ) where

import Control.Applicative
import Data.Function
import Data.Ratio
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Function

type Time = Double

data Behavior a = Behavior { at :: Time -> a }
data Event a = Event { occ :: (Time, a) } deriving (Eq,Ord,Show)

instance Functor Behavior where
  fmap f b = Behavior { at = \t -> f (at b t) }

instance Applicative Behavior where
  pure = mkB . const
  ba <*> bb = mkB $ \t -> (at ba) t (at bb t)

mkB :: (Time -> a) -> Behavior a
mkB f = Behavior { at = f }

mkE :: Time -> a -> Event a
mkE t a = Event { occ=(t,a) }

integral :: Num a => Behavior a -> Time -> Behavior a
integral b from = mkB (\to-> sum (map (at b) [from .. to]))

timeTransform :: Behavior a -> Behavior Time -> Behavior a
timeTransform b tb = mkB (\t -> at b (at tb t))

untilB :: Behavior a -> Event (Behavior a) -> Behavior a
untilB b1 e = mkB
 (\t -> let t2 = fst (occ e)
            b2 = snd (occ e)
        in  if t < t2
            then at b1 t
            else at b2 t)

time :: Behavior Time 
time = mkB id

ints :: Behavior Int
ints = mkB round

lift0 :: a -> Behavior a
lift0 x = mkB (const x)

{- 
The Time datatype has been simplified and the below no longer apply

prop_time_ordering = \tvx tvy -> tvx <= tvy ==>   At tvx <= AtLeast tvy
prop_time_bottoms = \tvx tvy -> isBottom         (AtLeast tvx <= At tvy) &&
                 		    	     	isBottom         (AtLeast tvx <= AtLeast tvy) &&
				                        (not . isBottom) (At tvx <= At tvy)
 -}

prop_timebehavior_id = \t -> at time t == t

prop_behavior_is_a_functor v = \t-> at (fmap (+1) (lift0 v)) t == (+1) (at (lift0 v) t)
  where types = (v::Int)

prop_behavior_is_applicative_pure v = \t-> at ((+1) <$> (pure v)) t == (+1) (at (pure v) t)
  where types = (v::Int)

prop_behavior_is_applicative_star v1 v2 = \t-> at ((+) <$> (pure v1) <*> (pure v2)) t == (+) (at (pure v1) t) (at (pure v2) t)
  where types = (v1::Int, v2::Int)

prop_show_time t = at (show <$> time) t == show t
 where types = (t::Time)

prop_ints t1 t2 =  t1 <= t2 ==> at ints (t1) <= at ints (t2)

prop_add_ints t = at ((+) <$> ints <*> ints) t == (round t) * 2

prop_time_transform t = let timeInHalf = (/) <$> time <*> pure 2
                        in at (timeTransform ints  timeInHalf) t ==
                    ((at ints) . (at timeInHalf)) t

prop_integral t0 t1 = (abs (t0 - t1)) < 100 ==>
              at (integral time t0) t1 == sum [t0..t1]

runTests :: IO Bool
runTests = $forAllProperties quickCheckResult