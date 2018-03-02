module Main where

import           RoundRobin

main :: IO ()
main = putStrLn $ show $ roundRobin generateSurveys
