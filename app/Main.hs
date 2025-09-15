{-
basic user interface for executing processing nodes
-}

module Main where

import qualified DataGraph
import qualified Figures

-- directly executable figure-creating nodes
figs :: [(String, String, String -> DataGraph.Node ())]
figs = [
  ("output/ad1_2024_30_90n.svg", "Ljungqvist comparison", Figures.ad1_2024_30_90n),
  ("output/ljungqvist_2010_coverage.svg", "Ljungqvist coverage", Figures.ljungqvist_2010_coverage)]

execFig :: (String, String, String -> DataGraph.Node ()) -> IO ()
execFig (fileName, _, act) = DataGraph.execute $ act fileName

catalogue :: String
catalogue = concat $ zipWith f figs [0..]
  where
    f (file, desc, _) n = '\n' : show (n :: Integer) ++ " - " ++ file ++ " - " ++ desc

main :: IO ()
main = do
  putStrLn catalogue
  i <- fmap read getLine
  execFig $ figs !! i
