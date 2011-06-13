{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module SVM (train,saveModel,predict) where

import Bindings.SVM
import System.IO.Unsafe
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Utils as U
import Foreign.Ptr
import qualified Data.Vector.Storable.Mutable as MVec
import qualified Data.Vector.Storable as SVec
import Foreign.Marshal.Array as MA
import Data.List.Split

svm_parameters = C'svm_parameter
  { c'svm_parameter'svm_type = 0
  , c'svm_parameter'kernel_type = 2
  , c'svm_parameter'degree = 3
  , c'svm_parameter'gamma = 0.5
  , c'svm_parameter'coef0 = 0.0
  , c'svm_parameter'cache_size = 100.0
  , c'svm_parameter'eps = 0.1
  , c'svm_parameter'C = 1.0
  , c'svm_parameter'nr_weight = 1
  , c'svm_parameter'weight_label = unsafePerformIO $ new (1::CInt)
  , c'svm_parameter'weight = unsafePerformIO $ new (1.0::CDouble)
  , c'svm_parameter'nu = 0.5
  , c'svm_parameter'p = 0.1
  , c'svm_parameter'shrinking = 1
  , c'svm_parameter'probability = 0 
  }

node = SVec.unsafeToForeignPtr $ SVec.fromList ([1,1,-1,-1]::[CDouble])

type Model = Ptr C'svm_model
type Nodes = Ptr C'svm_node

parseEntryClass :: String -> Double
parseEntryClass row = eval $ classStr row
  where
    classStr s = (splitOn " " s) !! 0
    read' s    = (read s) :: Double
    eval s     = if ((s !! 0) == '+') then (read' $ tail $ s) else (read' s)

parseEntryNodes :: String -> [(Int,Double)]
parseEntryNodes row = pairs
  where
    cellStrs = init $ tail $ splitOn " " row -- Last split empty
    pairs    = map (\i->(read (i!!0)::Int, read (i!!1)::Double)) $ map (\s->splitOn ":" s) cellStrs

-- Indices in ascending order, ending -1 index is added
-- TODO: enforce ordering
createEntryCNodes :: [(Int,Double)] -> Nodes
createEntryCNodes pairs = unsafePerformIO $ MA.newArray $ (map createCNode pairs)++[endingNode]
  where
    endingNode = createCNode (-1,0) -- spec: last node must be (-1,?)

parseTrainEntry :: String -> (Double, [(Int, Double)])
parseTrainEntry row = (class_, nodes_)
  where 
    class_ = parseEntryClass row
    nodes_ = parseEntryNodes row

createCNode :: (Int, Double) -> C'svm_node
createCNode (index,value) = C'svm_node
  { c'svm_node'index     = (fromIntegral index) :: CInt
  , c'svm_node'value     = realToFrac value :: CDouble
  }

train :: [(Double, [(Int, Double)])] -> Model 
train trainData = unsafePerformIO $ do
  classes <- MA.newArray $ (map (realToFrac.fst) trainData :: [CDouble])
  allNodeArrays <- MA.newArray $ map (createEntryCNodes.snd) trainData
  svm_problem <- return $ C'svm_problem
    { c'svm_problem'l = (fromIntegral $ length trainData) :: CInt
    , c'svm_problem'y = classes
    , c'svm_problem'x = allNodeArrays
    }
  with svm_problem $ \svm_problem' ->
    with svm_parameters $ \svm_parameters' ->
      c'svm_train svm_problem' svm_parameters'

saveModel :: Model -> String -> IO ()
saveModel model file = do
  outputfile <- newCString file
  _ <- c'svm_save_model outputfile model
  return ()

predict :: Model -> [(Int,Double)] -> Double
predict model nodes' = realToFrac $ unsafePerformIO $ c'svm_predict model nodes
  where
    nodes = createEntryCNodes nodes'

