module Main where

import Control.Applicative
import Frp
import System.CPUTime

clear :: IO ()
clear = putStr (take 50 $ repeat '\b')
now :: IO Time
now = ((/1000000000.0). fromIntegral) <$> getCPUTime 

runBehavior :: Show a => Behavior a -> IO ()
runBehavior b = clear >> now >>= showB b >> runBehavior b

showB :: Show a => Behavior a -> Time -> IO ()
showB b = putStr . show . at b

switcheroo :: Behavior Time
switcheroo = time `untilB` (mkE 1000 ((*(-1)) <$> time))

main :: IO ()
main = runBehavior switcheroo
