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

import Dataset.Mnist

main :: IO ()
main = do
    --runConduitRes $ (sourceFileBS "datasets/mnist_train.csv") $$ (sinkFileBS "/tmp/csvcopy.csv")
    res <- runEitherT $ bimapEitherT showError id $ runResourceT (trainingPipeline "datasets/mnist_mini.csv")
    either putStrLn return res
