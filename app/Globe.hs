{-
things useful for creating lat/long graphs
-}

module Globe where

import qualified DataGraph
import qualified DataSets

import qualified Graphics.Rendering.Chart.Axis.Types as ChartAT
import qualified Graphics.Rendering.Chart.Easy as ChartEasy
import qualified Graphics.Rendering.Chart.Layout as ChartLayout

longitudeAxis :: ChartAT.AxisData Double
longitudeAxis = ChartAT.AxisData
  {
    ChartAT._axis_visibility = ChartEasy.def,
    ChartAT._axis_viewport = ChartAT.vmap (-180, 180),
    ChartAT._axis_tropweiv = ChartAT.invmap (-180, 180),
    ChartAT._axis_ticks = map (\x -> (x, 2)) [-180, -150 .. 180],
    ChartAT._axis_labels = [map (\x -> (x, show x)) [-180, -150 .. 180]],
    ChartAT._axis_grid = [-180, -150 .. 180]
  }

-- latitude axis using the given scaling function and its inverse
latitudeAxis :: (Double -> Double) -> (Double -> Double) -> ChartAT.AxisData Double
latitudeAxis pf fp = ChartAT.AxisData
  {
    ChartAT._axis_visibility = ChartEasy.def,
    ChartAT._axis_viewport = pvmap,
    ChartAT._axis_tropweiv = invpvmap,
    ChartAT._axis_ticks = map (\x -> (x, 2)) [-90, -60 .. 90],
    ChartAT._axis_labels = [map (\x -> (x, show x)) [-90, -60 .. 90]],
    ChartAT._axis_grid = [-90, -60 .. 90]
  }
  where
    pvmap range x = ChartAT.vmap (pf (-90), pf 90) range (pf x)
    invpvmap range x = fp $ ChartAT.invmap (pf (-90), pf 90) range x

-- an incomplete chart showing an equal-area rectangular projection of Earth, by using the appropriate non-linear scaling on the latitude axis
equalAreaGlobeGraph :: DataGraph.Node (ChartEasy.EC (ChartLayout.Layout Double Double) () -> ChartEasy.EC (ChartLayout.Layout Double Double) ())
equalAreaGlobeGraph = DataGraph.memoise DataGraph.equalAreaGlobeGraph f
  where
    f = do
      segments <- DataSets.world_map
      pure $ g segments
    g :: [[(Rational, Rational)]] -> ChartEasy.EC (ChartLayout.Layout Double Double) () -> ChartEasy.EC (ChartLayout.Layout Double Double) ()
    g segments plots = do
      ChartEasy.layout_title ChartEasy..= "Map"
      ChartLayout.layout_y_axis . ChartLayout.laxis_title ChartEasy..= "Latitude"
      ChartLayout.layout_x_axis . ChartLayout.laxis_title ChartEasy..= "Longitude"
      ChartLayout.layout_y_axis . ChartLayout.laxis_generate ChartEasy..= const (latitudeAxis (\x -> sin (x * pi / 180)) (\x -> asin x * 180 / pi))
      ChartLayout.layout_x_axis . ChartLayout.laxis_generate ChartEasy..= const longitudeAxis
      plots
      ChartEasy.setColors [ChartEasy.opaque ChartEasy.black]
      ChartEasy.plot (ChartEasy.line "" $ map (map (\(x, y) -> (fromRational x, fromRational y))) segments)
