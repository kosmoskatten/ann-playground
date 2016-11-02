module Main where

import Data.Conduit
import Data.Conduit.Combinators

main :: IO ()
main =
    runConduitRes $ (sourceFileBS "datasets/mnist_train.csv") $$ (sinkFileBS "/tmp/csvcopy.csv")
