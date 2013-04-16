{-# LANGUAGE Rank2Types, TemplateHaskell, TupleSections  #-}
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

tvToInt :: TimeValue -> Int
tvToInt (TimeV x) = round x
tvToInt _       = undefined

data Time = At TimeValue | AtLeast TimeValue deriving (Eq,Show)  -- Time = 𝕽 + 𝕽
instance Ord Time where
  (At x) <= (At y)      = x <= y
  (At x) <= (AtLeast y) = x <= y
  _      <= _           = undefined

timeValue :: Time -> TimeValue
timeValue (At x) = x
timeValue (AtLeast x) = x

data Behavior a = Behavior { at :: Time -> a }

mkB :: (Time -> a) -> Behavior a
mkB f = Behavior { at = f }

instance Functor Behavior where
  fmap f b = Behavior { at = \t -> f (at b t) }

instance Applicative Behavior where
  pure = mkB . const
  ba <*> bb = mkB $ \t -> (at ba) t (at bb t)

time :: Behavior Time 
time = mkB id

ints :: Behavior Int
ints = mkB (tvToInt . timeValue) where

lift0 :: a -> Behavior a
lift0 = pure

lift1 :: (a -> b) -> Behavior a -> Behavior b
lift1 = fmap

lift2 :: (a -> b -> c) -> Behavior a -> Behavior b -> Behavior c
lift2 f ba bb = f <$> ba <*> bb

{- Test-related code & properties -}
instance Arbitrary TimeValue where
  arbitrary = oneof [ return NegInf, TimeV <$> arbitrary, return PosInf ]

instance Arbitrary Time where
  arbitrary = oneof [ At <$> arbitrary, AtLeast <$> arbitrary ]

nonInfTV :: Gen TimeValue
nonInfTV = TimeV <$> arbitrary

prop_time_ordering = \tvx tvy -> tvx <= tvy ==>   At tvx <= AtLeast tvy
prop_time_bottoms = \tvx tvy -> isBottom         (AtLeast tvx <= At tvy) &&
		    	     	isBottom         (AtLeast tvx <= AtLeast tvy) &&
				(not . isBottom) (At tvx <= At tvy)

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

prop_ints = forAll nonInfTV $ \tv1->
	    forAll nonInfTV $ \tv2-> tv1 <= tv2 ==> at ints (At tv1) <= at ints (At tv2)

prop_add_ints = forAll nonInfTV $ \tv->
                 at ((+) <$> ints <*> ints) (At tv) == (tvToInt tv) * 2

main :: IO Bool
main = do
     $forAllProperties quickCheckResult --verboseCheckResult