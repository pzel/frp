{-# LANGUAGE Rank2Types, RecordWildCards, TemplateHaskell  #-}
module Main where

import Control.Applicative
import Test.ChasingBottoms (isBottom)
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Function

data TimeValue = NegInf | TimeV Float | PosInf deriving (Eq,Show)  
instance Ord TimeValue where
  NegInf `compare` NegInf       = EQ
  NegInf `compare` _            = LT
  PosInf `compare` PosInf       = EQ
  PosInf `compare` _            = GT
  (TimeV x) `compare` NegInf    = GT
  (TimeV x) `compare` PosInf    = LT
  (TimeV x) `compare` (TimeV y) = x `compare` y

data Time = At TimeValue | AtLeast TimeValue deriving (Eq,Show)  -- Time = 𝕽 + 𝕽
instance Ord Time where
  (At x) <= (At y)      = x <= y
  (At x) <= (AtLeast y) = x <= y
  _      <= _           = undefined

{- dropping typeclasses, as shown at: http://www.haskellforall.com/2012/05/scrap-your-type-classes.html -}

data Behavior a = Behavior {
  at :: (Behavior a) -> Time -> a
} 

at' :: Behavior a -> Time -> a
at' b = (at b) b               -- This is just syntax sugar

time :: Behavior Time 
time = Behavior { at = const id }

lift0 :: a -> Behavior a
lift0 v = Behavior { at = const . const $ v }

lift1 :: (a -> b) -> Behavior a -> Behavior b
lift1 f ba@Behavior{..} = Behavior { at = \b t-> f (at ba t) }

lift2 :: (a -> b -> c) -> Behavior a -> Behavior b -> Behavior c
lift2 f ba bb = Behavior { at = \b t -> f (at' ba t) (at' bb t) }

{- Test-related code & properties -}
instance Arbitrary TimeValue where
  arbitrary = oneof [ return NegInf, TimeV <$> arbitrary, return PosInf ]

instance Arbitrary Time where
  arbitrary = oneof [ At <$> arbitrary, AtLeast <$> arbitrary ]

prop_time_ordering = \tvx tvy -> tvx <= tvy ==>   At tvx <= AtLeast tvy
prop_time_bottoms = \tvx tvy -> isBottom         (AtLeast tvx <= At tvy) &&
		    	     	isBottom         (AtLeast tvx <= AtLeast tvy) &&
				(not . isBottom) (At tvx <= At tvy)

prop_timebehavior_id0 = \tv0 -> (at time) time (At tv0) == (At tv0)
prop_timebehavior_id1 = \tv0 -> (at time) time (AtLeast tv0) == (AtLeast tv0)

prop_lift0 v = \t -> at' (lift0 v) t == v 
  where types = (v :: Int)

prop_lift1_plus c v = \t-> let f = (+ c)
	                    in at' (lift1 f (lift0 v)) t == f (at' (lift0 v) t)
  where types = (c::Int,v::Int)

prop_lift1_tupl c v = \t-> let f x = (c,x)
	                    in at' (lift1 f (lift0 v)) t == f (at' (lift0 v) t)
  where types = (c::Int,v::Int)

prop_lift2_tupl v1 v2 = \t-> let f = (,)
	                       in at' (lift2 f (lift0 v1) (lift0 v2)) t == f (at' (lift0 v1) t) (at' (lift0 v2) t)
   where types = (v1::Int, v2::Int)


runTests = $quickCheckAll
main :: IO Bool
main = runTests
