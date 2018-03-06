{-# LANGUAGE ImplicitParams #-}

module Main where

import           System.Environment
import           Text.Read

import           RoundRobin

main :: IO ()
main = do
  args <- getArgs
  checkArgs args

  let ?select = selectMethod (args !! 1)
  let respondents = getRespondents (args !! 0)

  testRR respondents

checkArgs :: [String] -> IO ()
checkArgs args
  | length args /= 2 = error "Incorrect args"
  | otherwise = return ()

selectMethod :: String -> ([(Double, Survey)] -> IO Survey)
selectMethod "random" = selectSurvey
selectMethod "fixed"  = selectSurveyMax
selectMethod _        = error "Incorrect method"

getRespondents :: String -> Int
getRespondents r =
  case readMaybe r of
    Nothing -> error "Incorrect respondents"
    Just x  -> x
