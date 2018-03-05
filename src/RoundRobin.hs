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
    foo s = (staticWeight * dynamicWeight, s)
      where
        staticWeight = getStaticWeight (length l + 1) s
        dynamicWeight = getDynamicWeight s

-- l = 5
-- r = 2, 1, 3, 0
--   = 3, 4, 2, 1
getStaticWeight :: Int -> Survey -> Double
getStaticWeight n Survey{..} = fromIntegral $ n - rank
  where
    rank = if surveyRank /= 0 then surveyRank else n - 1

-- q = 100, 100,  80, 150
-- s =   0,   0,   0,   0
--   =   1,   1,   1,   1
getDynamicWeight :: Survey -> Double
getDynamicWeight Survey{..} =
  let n = fromIntegral surveyRespondentStarted / fromIntegral surveyOverallQuota
  in if n == 0 then 0.001 else
     if n == 1 then 0 else n

updateSurvey :: Survey -> Survey
updateSurvey Survey{..} = Survey
  surveyId
  surveyScore
  surveyOverallQuota
  (surveyRespondentStarted + 1)
  surveyRank

testRR :: [Int]
testRR = rrSurveys 300 generateSurveys

rrSurveys :: Int -> [Survey] -> [Int]
rrSurveys 0 _ = []
rrSurveys n surveys =
  let rrSurvey = roundRobin surveys
      updatedSurveys = updateSurvey rrSurvey : filter (rrSurvey /=) surveys
  in surveyId rrSurvey : rrSurveys (n - 1) updatedSurveys

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
    , surveyOverallQuota      = 150
    , surveyRespondentStarted = 0
    , surveyRank              = 0
    }
  ]

randomSurvey :: IO Survey
randomSurvey = undefined
