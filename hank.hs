module Main where

import System
import System.FilePath
import CV.Image
import CV.Textures
import CV.ImageOp
import CV.Drawing

import SVM

createVector :: HaralickFeatures -> [(Int, Double)]
createVector hf = concat [asmV, entropyV, contrastV]--, correlationV]
  where
    asmV         = zip [1..4]   $ asms hf
    entropyV     = zip [5..8]   $ entropies hf
    contrastV    = zip [8..12]  $ contrasts hf
--    correlationV = zip [13..16] $ correlations hf

createGPlotStr :: HaralickFeatures -> String
createGPlotStr hf = (show asmV)++" "++(show contrastV) --, correlationV]
  where
    asmV         = 0.25 * (sum $ asms hf)
    contrastV    = 0.25 * (sum $ contrasts hf)

main' = do
    args <- getArgs
    Just im <- loadImage (args !! 0 :: FilePath)
    coords <- return $ [ (x,y) | y <- [0,10..90], x <- [0,10..90] ]
    skyCoords <- return $ take 10 coords
    forestCoords <- return $ take 10 $ drop 70 coords

    skyRegions <- return $ map (\(x,y)->getRegion (fromIntegral x, fromIntegral y) (10,10) im) skyCoords
    forestRegions <- return $ map (\(x,y)->getRegion (fromIntegral x, fromIntegral y) (10,10) im) forestCoords

    skyData    <- return $ zip (repeat 1)    $ map (createVector.calculateHaralickFeatures) skyRegions
    forestData <- return $ zip (repeat (-1)) $ map (createVector.calculateHaralickFeatures) forestRegions
    trainData <- return $ concat [skyData, forestData]
    model <- return $ train trainData
    saveModel model ((args !! 0)++".model")
    regions <- return $ map (\(x,y)->getRegion (fromIntegral x, fromIntegral y) (10,10) im) coords
    haralickVals <- return $ map (\i->calculateHaralickFeatures i) regions
    predictions <- return $ map ((predict model).createVector) haralickVals
    montages <- return $ map (\v->(empty (10,10) :: Image GrayScale D32) <# rectOp (realToFrac v) (-1) (0,0) (10,10) ) predictions
    saveImage ((args !! 0)++".predict.jpg") $ montage (10,10) 0 montages
    return ()

{-
mainDatas = do
    args <- getArgs
    Just im <- loadImage (args !! 0 :: FilePath)
    coords <- return $ [ (x,y) | y <- [0,10..90], x <- [0,10..90] ]
    skyCoords <- return $ take 10 coords
    forestCoords <- return $ take 10 $ drop 70 coords

    skyRegions <- return $ map (\(x,y)->getRegion (fromIntegral x, fromIntegral y) (10,10) im) skyCoords
    forestRegions <- return $ map (\(x,y)->getRegion (fromIntegral x, fromIntegral y) (10,10) im) forestCoords

    skyData    <- return $ map ("1 "++)    $ map (createGPlotStr.calculateHaralickFeatures) skyRegions
    forestData <- return $ map ("-1 "++)   $ map (createGPlotStr.calculateHaralickFeatures) forestRegions
    trainData <- return $ concat [skyData, forestData]
    mapM_ putStrLn trainData
    return ()
-}
main = main'
