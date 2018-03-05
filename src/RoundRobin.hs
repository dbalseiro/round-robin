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
  , surveyWeight            :: Maybe Double
  } deriving (Show, Eq, Ord)

roundRobin :: [Survey] -> Survey
roundRobin l = snd $ maximum $ map (\s -> (getStaticWeight (length l + 1) s * getDynamicWeight s, s)) l

getStaticWeight :: Int -> Survey -> Double
getStaticWeight l Survey{..} = fromIntegral $ l - (if surveyRank == 0 then l else surveyRank)

getDynamicWeight :: Survey -> Double
getDynamicWeight Survey{..} = 1 - fromIntegral surveyRespondentStarted / fromIntegral surveyOverallQuota

roundRobin2 :: [Survey] -> Survey
roundRobin2 =
  let totalRank = sum (map surveyRank generateSurveys)
      totalQuotaPercentage s = (fromIntegral $ surveyOverallQuota s) / fromIntegral (sum (map surveyOverallQuota generateSurveys))
      weight = getStaticWeight (length generateSurveys + 1) / fromIntegral totalRank
      load s = (getDynamicWeight s * totalQuotaPercentage)
   in snd . maximum . map (\s ->
                              let w = load s * weight s
                               in (w, s { surveyWeight = Just w
                                        , surveyRespondentStarted = surveyRespondentStarted s + 1
                                        }
                              ))


prettyPrintSurvey :: Survey -> IO ()
prettyPrintSurvey Survey{..} = do
  putStrLn
    $  "ID="
    ++ show surveyId
    ++ " Corrected="
    ++ show surveyWeight

rrSurveys :: Int -> [Survey] -> IO [Survey]
rrSurveys 0 _ = return []
rrSurveys i ss = do
  let rrs = roundRobin2 ss
      rest = filter ((/= surveyId rrs) . surveyId) ss

  mapM_ prettyPrintSurvey ss
  putStrLn "-------------------------------------------------"
  rrSurveys (i - 1) (rrs:rest)

updateSurvey :: Survey -> Survey
updateSurvey s = s
  { surveyRespondentStarted = surveyRespondentStarted s + 1 }

testRR :: Int -> IO ()
testRR n = void $ rrSurveys n generateSurveys

prettyPrint :: Survey -> IO ()
prettyPrint survey = return ()
    {-putStrLn-}
      {-$  "ID="-}
      {-++ show (surveyId survey)-}



generateSurveys :: [Survey]
generateSurveys =
  [ Survey
    { surveyId                = 1
    , surveyScore             = 1.1
    , surveyOverallQuota      = 100
    , surveyRespondentStarted = 0
    , surveyRank              = 2
    , surveyWeight            = Nothing
    }
  , Survey
    { surveyId                = 2
    , surveyScore             = 1.2
    , surveyOverallQuota      = 100
    , surveyRespondentStarted = 0
    , surveyRank              = 1
    , surveyWeight            = Nothing
    }
  , Survey
    { surveyId                = 3
    , surveyScore             = 1.1
    , surveyOverallQuota      = 100
    , surveyRespondentStarted = 0
    , surveyRank              = 3
    , surveyWeight            = Nothing
    }
  ]
  {-
  , Survey
    { surveyId                = 4
    , surveyScore             = 1.1
    , surveyOverallQuota      = 150
    , surveyRespondentStarted = 0
    , surveyRank              = 4
    }
  , Survey
    { surveyId                = 5
    , surveyScore             = 1.1
    , surveyOverallQuota      = 150
    , surveyRespondentStarted = 0
    , surveyRank              = 5
    }
  ]
  -}
randomSurvey :: IO Survey
randomSurvey = undefined
