{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Trans.Either (bimapEitherT, runEitherT)
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import Data.Csv
import Data.Csv.Conduit
import Data.Conduit
import Data.Conduit.Combinators

import Dataset.Types
import Dataset.Mnist

main :: IO ()
main = do
    --runConduitRes $ (sourceFileBS "datasets/mnist_train.csv") $$ (sinkFileBS "/tmp/csvcopy.csv")
    res <- runEitherT $ bimapEitherT showError id $ runResourceT trainingPipeline
    either putStrLn return res

trainingPipeline :: (MonadError CsvParseError m, MonadResource m) => m ()
trainingPipeline =
    (sourceFileBS "datasets/mnist_mini.csv" =$= fromCsv defaultDecodeOptions NoHeader) $$ sinkBundles

showError :: CsvParseError -> String
showError _ = "baah"

sinkBundles :: (MonadIO m) => Sink Bundle m ()
sinkBundles =
    awaitForever $ \b -> do
        liftIO $ putStrLn "---"
        liftIO $ Prelude.print b
        liftIO $ putStrLn "---"
        sinkBundles
