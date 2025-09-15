{-
nodes representing individual datasets from files, and things to create them 
-}

module DataSets where

import qualified CdlProcessing as Cdl
import qualified DataClumps
import qualified DataGraph

import Data.Ratio((%))
import qualified Data.Map as Map

type Series = [(Double, Double)]

---- utility functions follow

-- convert degrees to radians
degToRad :: Floating a => a -> a
degToRad x = x * pi / 180

-- calculate a number proportional (where longitudinal width is held constant) to the area of a latitude-longitude grid square of the given latitudinal height and latitude, derived from https://www.johndcook.com/blog/2023/02/21/sphere-grid-area/
uniformGridAreaProportion :: Floating a => a -> a -> a
uniformGridAreaProportion height cenLat = abs $ (sin $ degToRad $ cenLat + height / 2) - (sin $ degToRad $ cenLat - height / 2)

-- weighted mean of a list of value-weight pairs
weightedMean :: RealFrac a => [(a, a)] -> a
weightedMean xs = sumWeightedVals / sumWeights
  where
    (sumWeightedVals, sumWeights) = f 0 0 xs
    f accWeightedVals accWeights [] = (accWeightedVals, accWeights)
    f accWeightedVals accWeights ((value, weight):ys) = f (accWeightedVals + value * weight) (accWeights + weight) ys

-- average a series into boxes with given boundaries
intervalMeans :: [Double] -> Series -> Series
intervalMeans (boundary1:boundaries) xs' = f boundary1 boundaries $ dropWhile (\(x,_) -> x < boundary1) xs'
  where
    f b1 (b2 : bounds) xs@(_:_) =
      let (ys, zs) = span (\(x, _) -> x <= b2) xs in
        case ys of
          [] -> f b2 bounds xs
          _ -> ((b1 + b2) / 2, (sum (map snd ys) / fromIntegral (length ys))) : f b2 bounds zs
    f _ _ _ = []
intervalMeans [] _ = error "no interval boundaries provided"

---- nodes follow

-- temperature series from Ljungqvist (2010)
ljungqvist2010Reconstruction :: DataGraph.Node [(Double, Double, Double, Double)]
ljungqvist2010Reconstruction = DataGraph.memoise DataGraph.ljungqvist2010Reconstruction $ do
  x <- DataClumps.ljungqvist2010
  pure $ f x
  where
    f = map (\(s,e,t,u,l,_) -> ((fromIntegral s + fromIntegral e) / 2, fromRational l, fromRational t, fromRational u))

-- decadal means for temperature over 30-90 degrees North, from HadCRUT3v
hadcrut3v30_90n_decadal :: DataGraph.Node Series
hadcrut3v30_90n_decadal = DataGraph.memoise DataGraph.hadcrut3v_30_90n_decadal $ do
  x <- DataClumps.hadcrut3v
  pure $ f x
  where
    f (_, las, tis, tempTiLaLo) = intervalMeans [1800,1810..2020] $ g (map fromRational las, map fromRational tis, (map $ map $ map $ fmap fromRational) tempTiLaLo)
    g (las, tis, tempTiLaLo) = h tis $ map (zipWith (\la tempLo -> if la > 30 then map (weigh la) tempLo else []) las) tempTiLaLo
    weigh la temp = case temp of
      Nothing -> (0, 0)
      Just t -> (t, uniformGridAreaProportion 5 la)
    h tis tempTiLaLo = zip tis $ map weightedMean $ map concat (tempTiLaLo :: [[[(Double, Double)]]])

-- decadal means for temperature over 30-90 degrees North, from HadCRUT3v
hadcrut5_30_90n_decadal :: DataGraph.Node Series
hadcrut5_30_90n_decadal = DataGraph.memoise DataGraph.hadcrut5_30_90n_decadal $ do
  x <- DataClumps.hadcrut5
  pure $ f x
  where
    f (_, las, tis, tempTiLaLo) = intervalMeans [1800,1810..2030] $ g (map fromRational las, map fromRational tis, (map $ map $ map $ fmap fromRational) tempTiLaLo)
    g (las, tis, tempTiLaLo) = h tis $ map (zipWith (\la tempLo -> if la > 30 then map (weigh la) tempLo else []) las) tempTiLaLo
    weigh la temp = case temp of
      Nothing -> (0, 0)
      Just t -> (t, uniformGridAreaProportion 5 la)
    h tis tempTiLaLo = zip tis $ map weightedMean $ map concat (tempTiLaLo :: [[[(Double, Double)]]])

-- coastline line segments from GSHHG
world_map :: DataGraph.Node [[(Rational, Rational)]]
world_map = DataGraph.memoise DataGraph.world_map $ do
    x <- DataClumps.binned_GSHHS_c
    pure $ procMap x
  where
    procMap :: Cdl.Group -> [[(Rational, Rational)]]
    procMap (Cdl.Group _ _ _ _ _ dataMap _) =
        let numSegmentsInBins = extractData $ dataMap Map.! "N_segments_in_a_bin"
            firstPointsOfSegments = extractData $ dataMap Map.! "Id_of_first_point_in_a_segment"
            xs = extractData $ dataMap Map.! "Relative_longitude_from_SW_corner_of_bin"
            ys = extractData $ dataMap Map.! "Relative_latitude_from_SW_corner_of_bin"
        in let firstPointsOfBins = findFirstPointsOfBins numSegmentsInBins firstPointsOfSegments
        in let scaledPoints = zip (convertScale xs) (convertScale ys)
        in let convertedPoints = convertOffset 0 0 (-20) 90 firstPointsOfBins scaledPoints
        in segmentify 0 convertedPoints $ drop 1 firstPointsOfSegments
        
    extractData :: Cdl.DataDecl -> [Integer]
    extractData (Cdl.DataDecl _ xs) = map extractDatum xs
    
    extractDatum :: Cdl.DataItem -> Integer
    extractDatum (Cdl.Datum (Cdl.SimpleDatum (Cdl.IntConstant x))) = x
    extractDatum _ = error "wrong format"

    -- each point's position in a bin is stored as a pair of int16s where bin edges are min and max int16 value, we need it as lat/long
    convertScale :: [Integer] -> [Rational]
    convertScale xs = map (\x -> (fromIntegral x * (20 % 65535)) :: Rational) $ map (\x -> if x < 0 then x + 65536 else x) $ xs

    -- each bin contains a given number of segments, each with a specified first point, and we need to know at which point each bin starts
    findFirstPointsOfBins :: [Integer] -> [Integer] -> [Integer]
    findFirstPointsOfBins = f 0
      where
        -- no segments left in this bin, this bin started at this point and the next will start at the start of the next segment
        f currentPoint (0:segsInBins) (pointOfCurrentSegment:pointsOfSegments) = currentPoint : f pointOfCurrentSegment segsInBins (pointOfCurrentSegment:pointsOfSegments)
        -- the next segment is in this bin, so ignore it
        f currentPoint (segsInCurrentBin:segsInBins) (_:pointsOfSegments) = f currentPoint (segsInCurrentBin - 1 : segsInBins) pointsOfSegments
        -- out of bins
        f _ [] _ = []
        f currentPoint [0] _ = [currentPoint]
        f _ _ [] = error "ran out of segments before bins"

    -- each bin has its own latitude and longitude offset, which can be found by wrapping them around the earth
    -- the bins form a 9x18 lat/long grid, starting at 0E90N
    convertOffset :: Integer -> Integer -> Rational -> Rational -> [Integer] -> [(Rational, Rational)] -> [(Rational, Rational)]
    convertOffset currentPoint currentBin xOff yOff (currentBinFirstPoint:binFirstPoints) ((x, y) : ps)
      -- when we reach 180E, continue from 180W
      | currentPoint == currentBinFirstPoint && currentBin `mod` 18 == 9 = let xOff' = xOff - 340 in
                                           let yOff' = yOff in
                                             convertOffset currentPoint (currentBin + 1) xOff' yOff' binFirstPoints ((x, y) : ps)
      -- when we reach 0E, shift by 20S
      | currentPoint == currentBinFirstPoint && currentBin `mod` 18 == 0 = let xOff' = xOff + 20 in
                                           let yOff' = yOff - 20 in
                                             convertOffset currentPoint (currentBin + 1) xOff' yOff' binFirstPoints ((x, y) : ps)
      -- any other time we hit a new bin, shift by 20E
      | currentPoint == currentBinFirstPoint = let xOff' = xOff + 20 in
                      let yOff' = yOff in
                        convertOffset currentPoint (currentBin+1) xOff' yOff' binFirstPoints ((x, y) : ps)
      -- if not a new bin, process the next point
      | otherwise = let xOff' = xOff in
                      let yOff' = yOff in
                        (x + xOff, y + yOff) : convertOffset (currentPoint + 1) currentBin xOff' yOff' (currentBinFirstPoint:binFirstPoints) ps
    -- when out of bins, use the current offset for the rest of the points
    convertOffset currentPoint currentBin xOff yOff [] ((x, y) : ps) = (x + xOff, y + yOff) : convertOffset (currentPoint + 1) currentBin xOff yOff [] ps
    convertOffset _ _ _ _ _ _ = []

    -- associate the points into their segments
    segmentify :: Integer -> [(Rational, Rational)] -> [Integer] -> [[(Rational, Rational)]]
    segmentify currentPoint points (segPoint:segPoints) = let (seg, ps') = splitAt (fromIntegral (segPoint - currentPoint)) points in seg : segmentify segPoint ps' segPoints
    segmentify _ points [] = [points]
