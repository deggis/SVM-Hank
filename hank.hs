module Main where

import System
import System.FilePath
import CV.Image
import CV.Textures
import CV.Haralick
import CV.ImageOp
import CV.Drawing
import CV.ColourUtils
import qualified CV.ImageMath as CVIM
import Data.List.Split
import System.IO.Unsafe

import SVM

createVector :: [Double] -> [(Int, Double)]
createVector features = zip [1..] features

readCoords :: String -> IO [(Int,Int)]
readCoords file = do
    file <- readFile file
    lns <- return $ lines file
    return $ map read' lns 
  where
    read' s = read s :: (Int,Int)

imageFeatures :: Image GrayScale D32 -> [Double]
imageFeatures im = unsafePerformIO $ do
    co_occ <- return $ coOccurenceMatrix (0,0) $ unsafeImageTo8Bit $ im
    contr <- contrast co_occ 256
    ang <- angularSecondMoment co_occ 256
    avg <- return $ (realToFrac.CVIM.average) im
    return $ [contr, ang, avg]

main' = do
    args <- getArgs
    fn <- return $ args !! 0
    Just im' <- loadImage fn
    im <- return $ stretchHistogram im'
    coords <- return $ [ (x,y) | y <- [0,10..90], x <- [0,10..90] ]
    class1Coords <- readCoords $ fn++".class1.txt"
    class2Coords <- readCoords $ fn++".class2.txt"

    class1_regions <- return $ map (\(x,y)->getRegion (fromIntegral x, fromIntegral y) (10,10) im) class1Coords
    class2_regions <- return $ map (\(x,y)->getRegion (fromIntegral x, fromIntegral y) (10,10) im) class2Coords

    saveImage (fn++".hank.class1.jpg") $ montage (length class1_regions,1) 1 class1_regions
    saveImage (fn++".hank.class2.jpg") $ montage (length class2_regions,1) 1 class2_regions

    class1_data <- packFeatures 1    class1_regions
    class2_data <- packFeatures (-1) class2_regions

    trainData <- return $ simpleScaleWClasses $ concat [class1_data, class2_data]

    model <- return $ train trainData
    saveModel model (fn++".hank.model.txt")
    regions <- return $ map (\(x,y)->getRegion (fromIntegral x, fromIntegral y) (10,10) im) coords
    vals <- return $ simpleScale $ map (createVector.imageFeatures) regions
    writeFile (fn++".hank.values.txt") $ concat $ map ((++"\n").createGPlotStr) vals
    predictions <- return $ map (predict model) $ vals
    montages <- return $ map (\v->(empty (10,10) :: Image GrayScale D32) <# rectOp (realToFrac v) (-1) (0,0) (10,10) ) predictions
    saveImage (fn++".hank.predict.jpg") $ montage (10,10) 0 montages
    return ()
  where
    saveMontages fn ms       = saveImage fn $ montage (length ms,1) 1 ms
    packFeatures cls regions = return $ zip (repeat (cls::Double)) $ map (createVector.imageFeatures) regions
    createGPlotStr vals      = tail $ foldl (\acc (k,v)->concat [acc, " ", show v]) "" vals

main = main'
