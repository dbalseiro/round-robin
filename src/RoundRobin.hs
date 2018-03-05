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
roundRobin l = snd . maximum $ map foo l
  where
    totalQuota = sum $ map surveyOverallQuota l
    totalRank = sum $ map surveyRank l
    foo s@Survey{..} = (w * a, s)
      where
        surveyQuotaLeft = 1 - (fromIntegral surveyRespondentStarted / fromIntegral surveyOverallQuota)
        w = surveyQuotaLeft * (fromIntegral surveyOverallQuota / fromIntegral totalQuota)
        a = fromIntegral surveyRank / fromIntegral totalRank

updateSurvey :: Survey -> Survey
updateSurvey Survey{..} = Survey
  surveyId
  surveyScore
  surveyOverallQuota
  (surveyRespondentStarted + 1)
  surveyRank

testRR :: [Int]
-- testRR = rrSurveys 480 generateSurveys
testRR = rrSurveys 800 generateSurveys

rrSurveys :: Int -> [Survey] -> [Int]
rrSurveys 0 _ = []
rrSurveys _ [] = []
rrSurveys n surveys = surveyId rrSurvey : rrSurveys (n - 1) updatedSurveys
  where
    rrSurvey = roundRobin surveys
    updatedSurveys = updateSurvey rrSurvey : filter foo surveys
    foo survey = rrSurvey /= survey

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
    , surveyScore             = 0.7
    , surveyOverallQuota      = 200
    , surveyRespondentStarted = 0
    , surveyRank              = 4
    }
  ]

randomSurvey :: IO Survey
randomSurvey = undefined



{-
totalQuota = sum . map surveyOverallQuota surveys
totalLoad = sum . map surveyRank surveys

for survey in surveys:
  surveyQuotaLeft = 1 - (surveyRespondentStarted survey / surveyOverallQuota survey)
  w = surveyQuotaLeft * (surveyOverallQuota survey / totalQuota)
  a = rank survey / totalRank
  wc = w * a

maximum wc
-}
