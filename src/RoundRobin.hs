{-# LANGUAGE ImplicitParams  #-}
{-# LANGUAGE RecordWildCards #-}

module RoundRobin where

import           Control.Monad
import           Data.Ord
import           System.Random

data Survey = Survey
  { surveyId                :: Int
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

roundRobin2 :: (?select :: [(Double, Survey)] -> IO Survey) => [Survey] -> IO Survey
roundRobin2 = do
  let totalRank = sum (map surveyRank generateSurveys)
      weight s = (getStaticWeight (length generateSurveys + 1) s) / fromIntegral totalRank
      load s = getDynamicWeight s

  ?select . map (\s ->
                              let w = weight s * load s
                               in (w, s { surveyWeight = Just w
                                      , surveyRespondentStarted = surveyRespondentStarted s + 1
                                      }
                                )
                     )

selectSurveyMax :: [(Double, Survey)] -> IO Survey
selectSurveyMax = return . snd . maximum

selectSurvey :: [(Double, Survey)] -> IO Survey
selectSurvey surveys = do
  r <- randomRIO (0, sum (map fst surveys))
  return $ selectSurvey' r 0 surveys

selectSurvey' :: Double -> Double -> [(Double, Survey)] -> Survey
selectSurvey' _ _ [] = error "No more surveys"
selectSurvey' rnd acum ((weight, survey):xs)
  | acum + weight > rnd = survey
  | otherwise = selectSurvey' rnd (acum + weight) xs


prettyPrintSurvey :: Survey -> IO ()
prettyPrintSurvey Survey{..} = do
  putStrLn
    $  "ID="
    ++ show surveyId
    ++ "\tRespondents="
    ++ show surveyRespondentStarted

rrSurveys :: (?select :: [(Double, Survey)] -> IO Survey) => Int -> [Survey] -> IO [Survey]
rrSurveys 0 x = return x
rrSurveys i ss = do
  rrs <- roundRobin2 ss
  let rest = filter ((/= surveyId rrs) . surveyId) ss

  putStrLn $ (show $ surveyId rrs) ++ "\t"
  rrSurveys (i - 1) (rrs:rest)

updateSurvey :: Survey -> Survey
updateSurvey s = s
  { surveyRespondentStarted = surveyRespondentStarted s + 1 }

testRR :: (?select :: [(Double, Survey)] -> IO Survey) => Int -> IO ()
testRR n = void $ rrSurveys n generateSurveys

generateSurveys :: [Survey]
generateSurveys = map mkSurvey [1..20]

mkSurvey :: Int -> Survey
mkSurvey i = Survey
  { surveyId                = i
  , surveyOverallQuota      = 100
  , surveyRespondentStarted = 0
  , surveyRank              = i
  , surveyWeight            = Nothing
  }
