{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Dataset.Mnist
    ( trainingPipeline
    , showError
    ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Csv.Conduit
import Data.Conduit
import Data.Conduit.Combinators (sourceFileBS)

import Data.Csv
import Data.ByteString.Char8 as BS
import qualified Data.Vector as Vec
import qualified Data.Vector.Storable as SVec

-- TODO: Should come from hfann
type InputData  = SVec.Vector Float
type OutputData = SVec.Vector Float

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
                 => FilePath -> m()
trainingPipeline file =
    (sourceFileBS file =$= fromCsv defaultDecodeOptions NoHeader) $$ sinkBundles

showError :: CsvParseError -> String
showError _ = "baah"

sinkBundles :: (MonadIO m) => Sink Bundle m ()
sinkBundles =
    awaitForever $ \b -> do
        liftIO $ Prelude.putStrLn "---"
        liftIO $ Prelude.print b
        liftIO $ Prelude.putStrLn "---"
        sinkBundles

parseInput :: Vec.Vector BS.ByteString -> Parser (Vec.Vector Float)
parseInput = Vec.mapM parseField

genOutput :: Int -> OutputData
genOutput n = SVec.generate 10 (\i -> if i == n then 1 else 0)
