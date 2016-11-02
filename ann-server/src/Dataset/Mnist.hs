{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Dataset.Mnist
    ( mnistFromFile
    ) where

import Control.Monad (when)
import Data.Csv
import Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Vector as Vec
import qualified Data.Vector.Storable as SVec

import Dataset.Types (Bundle (..), OutputData)

instance FromRecord Bundle where
    parseRecord line
        | Vec.length line == 785 = do
            digit <- parseField (Vec.head line)
            when (digit < 0 || digit > 9) $
                fail "Digit field must be (0 - 1)"

            inputData <- Vec.convert <$> (parseInput $ Vec.drop 1 line)
            return $ Bundle (inputData, genOutput digit)

        | otherwise              = fail "CSV line length error (shall be 785)"

mnistFromFile :: FilePath -> IO (Either String (Vec.Vector Bundle))
mnistFromFile file = do
    content <- LBS.readFile file
    case decode NoHeader content of
        Right xs -> return $ Right xs
        Left err -> return $ Left err

parseInput :: Vec.Vector BS.ByteString -> Parser (Vec.Vector Float)
parseInput = Vec.mapM parseField

genOutput :: Int -> OutputData
genOutput n = SVec.generate 10 (\i -> if i == n then 1 else 0)
