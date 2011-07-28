module Scaling where

-- Vectors -> (min,max) for each attribute
calcRanges :: [[Double]] -> [(Double,Double)]
calcRanges datas = foldl (\acc i->acc++[calcRange i]) [] [0..(attrN-1)]
  where
    calcRange i  = (minimum $ attrVals i, maximum $ attrVals i)
    attrVals i   = map (!!i) datas
    attrN        = length $ head datas

-- Vectors -> attribute-vice (min,max)'s -> scaled Vectors
scaleUsingRanges :: [[Double]] -> [(Double,Double)] -> [[Double]]
scaleUsingRanges dataz ranges = map applyScales dataz
  where
    applyScales values = foldr (\i acc->(scaleV i (values !! i)):acc) [] [0..(attrN-1)] 
    scaleV i val       = (val-(fst $ ranges !! i)) / ( (snd $ ranges !! i)-(fst $ ranges !! i))
    attrN              = length $ head dataz

