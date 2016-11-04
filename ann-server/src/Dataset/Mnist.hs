{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Dataset.Mnist
    ( trainingPipeline
    , testPipeline
    , showError
    ) where

import AI.Fann (Fann, InputData, OutputData, run, train)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit (Sink, ($$), (=$=), await, awaitForever)
import Data.Conduit.Combinators (sourceFileBS)
import Data.Csv ( FromRecord (..), HasHeader (NoHeader), Parser
                , defaultDecodeOptions, parseField
                )
import Data.Csv.Conduit (CsvParseError, fromCsv)
import Text.Printf (printf)

import Data.ByteString.Char8 as BS
import qualified Data.Vector as Vec
import qualified Data.Vector.Storable as SVec

newtype Bundle = Bundle (InputData, OutputData)
    deriving Show

instance FromRecord Bundle where
    parseRecord line
        | Vec.length line == 785 = do
            digit <- parseField (Vec.head line)
            when (digit < 0 || digit > 9) $
                fail "Digit field must be (0 - 1)"

            inputData <- Vec.convert <$> (parseInput $ Vec.drop 1 line)
            return $ Bundle (inputData, genOutput digit)

        | otherwise              = fail "CSV line length error (shall be 785)"

trainingPipeline :: (MonadError CsvParseError m, MonadResource m)
                 => FilePath -> Fann -> m()
trainingPipeline file fann =
    (sourceFileBS file =$= fromCsv defaultDecodeOptions NoHeader)
        $$ trainNetwork fann

trainNetwork :: (MonadIO m) => Fann -> Sink Bundle m ()
trainNetwork fann =
    awaitForever $ \(Bundle (input, output)) -> do
        liftIO $ train fann input output
        trainNetwork fann

testPipeline :: (MonadError CsvParseError m, MonadResource m)
             => FilePath -> Fann -> m (Int, Int)
testPipeline file fann =
    (sourceFileBS file =$= fromCsv defaultDecodeOptions NoHeader)
        $$ testNetwork fann

testNetwork :: (MonadIO m) => Fann -> Sink Bundle m (Int, Int)
testNetwork fann = go (0, 0)
  where
    go :: (MonadIO m) => (Int, Int) -> Sink Bundle m (Int, Int)
    go cnt = maybe (return cnt) (handleBundle cnt) =<< await

    handleBundle :: (MonadIO m) => (Int, Int) -> Bundle
                 -> Sink Bundle m (Int, Int)
    handleBundle (total, err) (Bundle (input, expect)) = do
        output <- liftIO $ run fann input
        let expectD = identifyDigit expect
            outputD = identifyDigit output
        if expectD /= outputD
            then do
                --liftIO $ printf "Err: exp %s <-> out %s\n"
                --                (show expectD) (show outputD)
                --liftIO $ printf "==> %s\n" (show output)
                (go (total + 1, err + 1))
            else (go (total + 1, err))

showError :: CsvParseError -> String
showError _ = "baah"

identifyDigit :: OutputData -> Maybe Int
identifyDigit = SVec.findIndex (\x -> x > 0.5)

parseInput :: Vec.Vector BS.ByteString -> Parser (Vec.Vector Float)
parseInput = Vec.mapM parseField

genOutput :: Int -> OutputData
genOutput n = SVec.generate 10 (\i -> if i == n then 1 else 0)
