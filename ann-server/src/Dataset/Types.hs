module Dataset.Types
    ( InputData
    , OutputData
    , Bundle (..)
    ) where

import qualified Data.Vector as Vec
import qualified Data.Vector.Storable as SVec

type InputData  = SVec.Vector Float
type OutputData = SVec.Vector Float

newtype Bundle = Bundle (InputData, OutputData)
    deriving Show
