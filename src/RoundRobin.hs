{-# LANGUAGE RecordWildCards #-}

module RoundRobin where

import           Control.Monad
import           Data.Ord

data Survey = Survey
  { surveyId                :: Int
  , surveyScore             :: Double
  , surveyOverallQuota      :: Int
  , surveyRespondentStarted :: Int
  , surveyRank              :: Int
  } deriving (Show, Eq, Ord)

roundRobin :: [Survey] -> Survey
roundRobin l = snd $ maximum $ map (\s -> (getStaticWeight (length l + 1) s * getDynamicWeight s, s)) l

getStaticWeight :: Int -> Survey -> Double
getStaticWeight l Survey{..} = fromIntegral $ l - (if surveyRank == 0 then l else surveyRank)

getDynamicWeight :: Survey -> Double
getDynamicWeight Survey{..} = fromIntegral surveyRespondentStarted / fromIntegral surveyOverallQuota


generateSurveys :: [Survey]
generateSurveys =
  [ Survey
    { surveyId                = 1
    , surveyScore             = 1.1
    , surveyOverallQuota      = 100
    , surveyRespondentStarted = 0
    , surveyRank              = 2
    }
  , Survey
    { surveyId                = 2
    , surveyScore             = 1.2
    , surveyOverallQuota      = 100
    , surveyRespondentStarted = 0
    , surveyRank              = 1
    }
  , Survey
    { surveyId                = 3
    , surveyScore             = 1.1
    , surveyOverallQuota      = 80
    , surveyRespondentStarted = 0
    , surveyRank              = 3
    }
  , Survey
    { surveyId                = 4
    , surveyScore             = 1.1
    , surveyOverallQuota      = 150
    , surveyRespondentStarted = 0
    , surveyRank              = 0
    }
  ]

randomSurvey :: IO Survey
randomSurvey = undefined
