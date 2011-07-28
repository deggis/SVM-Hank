module Main where

import System
import System.FilePath
import System.IO.Unsafe
import Data.List.Split
import Foreign.Marshal.Array
import qualified Data.Vector.Storable as V
import qualified Data.Array.CArray as CA
import Data.Array.IArray

import AI.SVM.Simple
import AI.SVM.Base

import CV.Image
import CV.Textures
import CV.Conversions
import qualified CV.Transforms as CV
import CV.Haralick
import CV.ImageOp
import CV.Drawing
import CV.ColourUtils
import qualified CV.ImageMath as CVIM

import qualified Scaling as S

imageFeatures :: Image GrayScale D32 -> [Double]
imageFeatures im = unsafePerformIO $ do
    do let coOccMs = map (`coOccurenceMatrix` i') ds
       let contrs  = map (`contrast` 256) coOccMs
       let angs    = map (`angularSecondMoment` 256) coOccMs
       let avg     = [(realToFrac.CVIM.average) im]
       return $ concat [avg, contrs, angs]
  where
    i' = unsafeImageTo8Bit im
    ds = [(1,0),(1,1),(0,1),(-1,1)] --0,45,90,135'


getRegions :: Int -> [(Int,Int)] -> Image GrayScale D32 -> [Image GrayScale D32]
getRegions d regions i = map (\(x,y)->getRegion (fromIntegral x, fromIntegral y) (d,d) i) regions

dataRowToStr :: (Int,V.Vector Double) -> String
dataRowToStr (classN,vec) = unwords $ show classN : dataString
  where
    datas = zip [1..] $ V.toList vec
    dataString = map (\(i,v)->show i++":"++show v) datas

printStats :: [Int] -> [Int] -> FilePath -> String -> IO ()
printStats predictions corrects fn method = putStrLn $ unwords ["Image", fn, "classification correctness with", method, ":", show amount, "%"]
  where
    nCorrect = fromIntegral $ foldl (\a (p,c)->if p == c then a + 1 else a) 0 $ zip predictions corrects
    amount   = nCorrect / fromIntegral (length predictions) * 100


main = do
    args <- getArgs
    let fn = head args
    Just im' <- loadImage fn
    let im = stretchHistogram im'

    (ranges,trainData) <- let bgClassCoords   = [ (x,y) | y <- [0,10..90], x <- [0,10..90], not (x<50 && y>40) ]
                              boxClassCoords  = [ (x,y) | y <- [0,10..90], x <- [0,10..90], x<50 && y>40 ]
                              bgClassRegions  = getRegions 10 bgClassCoords im
                              boxClassRegions = getRegions 10 boxClassCoords im
                              bgClassData'    = extractFeatures bgClassRegions
                              boxClassData'   = extractFeatures boxClassRegions
                              ranges          = S.calcRanges $ bgClassData' ++ boxClassData'
                              bgClassData     = S.scaleUsingRanges bgClassData' ranges
                              boxClassData    = S.scaleUsingRanges boxClassData' ranges
                              bgClassVectors  = createSVMVectors  1   bgClassData
                              boxClassVectors = createSVMVectors (-1) boxClassData
                              trainData       = take 10 bgClassVectors ++ take 10 boxClassVectors
                          in return (ranges,trainData)
--    mapM_ (putStrLn.dataRowToStr) trainData

    (c,gamma) <- return (2048.0, 0.0078125)
--    (c,gamma) <- return (1,1)
    (msg,svm) <- return $ trainClassifier (C c) (RBF gamma) trainData
    putStrLn msg

    -- method 1
    _ <- let regions         = getRegions 10 [(x,y) | y <- [0,10..90], x <- [0,10..90]] im
             features        = extractFeatures regions
             scaledFeatures  = S.scaleUsingRanges features ranges
             vectors         = map V.fromList scaledFeatures
             predictions     = map (classify svm) vectors
             colorSq c       = rectOp (realToFrac c) (-1) (0,0) (10,10) 
             montages        = map (\v->(empty (10,10) :: Image GrayScale D32) <# colorSq v) predictions
             image           = montage (10,10) 0 montages
             fn1             = dropExtension fn ++ "_method1.bmp"
             correctValues   = [ if x < 50 && y > 40 then (-1) else 1 | y <- [0,10..90], x <- [0,10..90]]
          in (saveImage fn1 image) >> printStats predictions correctValues fn1 "method 1"

    -- method 2
    _ <- let regions         = getRegions 11 [(x,y) | y <- [0..89], x <- [0..89]] im
             features        = extractFeatures regions
             scaledFeatures  = S.scaleUsingRanges features ranges
             vectors         = map V.fromList scaledFeatures
             predictions :: [Int]
             predictions     = map (classify svm) vectors
             arska           = listArray ((0,0),(89,89)) $ map fromIntegral predictions
             image           = CV.flip CV.Vertical $ CV.flip CV.Horizontal $ copyCArrayToImage arska
             fn2             = dropExtension fn ++ "_method2.bmp"
             correctValues   = [ if x < 45 && y > 44 then (-1) else 1 | y <- [0..89], x <- [0..89]]
          in saveImage fn2 image >> printStats predictions correctValues fn2 "method 2"

    return ()
  where
    extractFeatures :: [Image GrayScale D32] -> [[Double]]
    extractFeatures = map imageFeatures
    createSVMVectors :: Int -> [[Double]] -> [(Int, V.Vector Double)]
    createSVMVectors classN datas = zip (repeat classN) $ map V.fromList datas

