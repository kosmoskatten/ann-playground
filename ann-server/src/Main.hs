{-# LANGUAGE FlexibleContexts #-}
module Main where

import AI.Fann ( Fann, ActivationFunction (..), createStandard'3L
               , setActivationFunctionHidden, setActivationFunctionOutput, mse
               )
import Control.Monad (sequence, replicateM)
import Control.Monad.Trans.Either (bimapEitherT, runEitherT)
import Control.Monad.Trans.Resource (runResourceT)
import Text.Printf (printf)

import Dataset.Mnist (trainingPipeline, showError)

main :: IO ()
main = do
    fann <- createStandard'3L 784 1000 10
    setActivationFunctionHidden fann Sigmoid
    setActivationFunctionOutput fann Sigmoid

    res <- replicateM 1 (performTraining fann)

    case sequence res of
        Right _  -> do
            printf "Training complete. MSE=%f\n" (mse fann)
        Left err -> putStrLn err

performTraining :: Fann -> IO (Either String ())
performTraining fann =
    runEitherT $ bimapEitherT showError id $
        runResourceT (trainingPipeline "datasets/mnist_train.csv" fann)
