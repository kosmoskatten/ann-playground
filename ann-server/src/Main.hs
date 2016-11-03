{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.Trans.Either (bimapEitherT, runEitherT)
import Control.Monad.Trans.Resource (runResourceT)

import Dataset.Mnist

main :: IO ()
main = do
    --runConduitRes $ (sourceFileBS "datasets/mnist_train.csv") $$ (sinkFileBS "/tmp/csvcopy.csv")
    res <- runEitherT $ bimapEitherT showError id $ runResourceT (trainingPipeline "datasets/mnist_mini.csv")
    either putStrLn return res
