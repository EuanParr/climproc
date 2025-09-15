{-
processing nodes that create charts/figures, and utility functions for them
-}

module Figures where

import qualified Graphics.Rendering.Chart.Backend.Diagrams as ChartDiagrams
import qualified Graphics.Rendering.Chart.Easy as ChartEasy
import qualified Graphics.Rendering.Chart.Layout as ChartLayout
import qualified Graphics.Rendering.Chart.Plot as ChartPlot
import qualified Graphics.Rendering.Chart.State as ChartState

import qualified DataSets
import qualified DataGraph
import qualified Globe

---- utility functions follow

-- shade the area between 2 lines in a line chart, useful for uncertainty intervals
fillBetween :: String -> [(x1, (y1, y1))] -> ChartState.EC (ChartLayout.Layout Double Double) (ChartPlot.PlotFillBetween x1 y1)
fillBetween title signal = ChartEasy.liftEC $ do
  colour <- ChartEasy.takeColor
  ChartEasy.plot_fillbetween_title ChartEasy..= title
  ChartEasy.plot_fillbetween_style ChartEasy..= ChartEasy.solidFillStyle colour
  ChartEasy.plot_fillbetween_values ChartEasy..= signal

-- plot a line with uncertainty intervals
rangeLine :: String -> [(Double, Double, Double, Double)] -> ChartState.EC (ChartLayout.Layout Double Double) ()
rangeLine title signal = do
  ChartEasy.plot $ fillBetween title $ map (\(t,l,_,u) -> (t, (l, u))) signal
  ChartEasy.plot $ ChartEasy.line title $ [map (\(t,_,v,_) -> (t,v)) signal]

---- nodes follow

-- decadal temperatures over 1-2024 AD over the 30-90 degrees North area of Earth's surface, according to Ljungqvist (2010) and HadCRUT (3v and 5)
ad1_2024_30_90n :: String -> DataGraph.Node ()
ad1_2024_30_90n fileName = do
  ljun <- DataSets.ljungqvist2010Reconstruction
  hadcrut3v <- DataSets.hadcrut3v30_90n_decadal
  hadcrut5 <- DataSets.hadcrut5_30_90n_decadal
  DataGraph.liftIO $ ChartDiagrams.toFile ChartEasy.def fileName $ fig ljun hadcrut3v hadcrut5
  where
    fig ljun hadcrut3v hadcrut5 =
      do
        ChartEasy.layout_title ChartEasy..= "90-30\x00B0N"
        ChartLayout.layout_y_axis . ChartLayout.laxis_title ChartEasy..= "Temperature (\x00B0\&C) anomaly relative to 1961-90 mean"
        ChartLayout.layout_x_axis . ChartLayout.laxis_title ChartEasy..= "Year AD"
        ChartEasy.setColors [ChartEasy.opaque ChartEasy.gray, ChartEasy.opaque ChartEasy.black, ChartEasy.opaque ChartEasy.blue, ChartEasy.opaque ChartEasy.red]
        rangeLine "Ljungqvist reconstruction" ljun
        ChartEasy.plot (ChartEasy.line "HadCRUT3v" [hadcrut3v])
        ChartEasy.plot (ChartEasy.line "HadCRUT5" [hadcrut5])

-- the parts of Earth's surface covered by Ljungqvist (2010)
ljungqvist_2010_coverage :: String -> DataGraph.Node ()
ljungqvist_2010_coverage fileName = do
  globeGraph <- Globe.equalAreaGlobeGraph
  DataGraph.liftIO $ ChartDiagrams.toFile ChartEasy.def fileName $ f globeGraph
  where
    f globeGraph = globeGraph $
      do
        ChartEasy.setColors [ChartEasy.withOpacity ChartEasy.blue 0.5]
        ChartEasy.plot $ Figures.fillBetween "Ljungqvist Coverage" [(-180, (30, 90)), (180, (30, 90))]
  
