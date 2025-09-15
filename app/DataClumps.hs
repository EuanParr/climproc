{-
nodes that load and parse files, and things for making them
-}

module DataClumps where

import qualified Data.Time.Calendar.OrdinalDate as TD
import qualified Data.Time.Clock as TC
import qualified Data.Time.LocalTime as LT

import qualified CdlProcessing as Cdl
import qualified Csv
import qualified DataGraph
import Parsing

---- utility functions follow

-- split a list into segments n elements long
unconcat :: Int -> [a] -> [[a]]
unconcat _ [] = []
unconcat n xs =
  let (ts, ds) = splitAt n xs in
  ts : unconcat n ds

-- convert a time given in days since a particular year and day to a time on the year scale
dayToYearFrac :: Integer -> Int -> Rational -> Rational
dayToYearFrac startYear startDay deltaDays = fromIntegral year + (if TD.isLeapYear year then dayWithTimeOfYear / 366 else dayWithTimeOfYear / 365)
  where
    dayWithTimeOfYear = fromRational $ fromIntegral dayOfYear + LT.timeOfDayToDayFraction localTimeOfDay
    (year, dayOfYear) = TD.toOrdinalDate localDay
    (LT.LocalTime localDay localTimeOfDay) = LT.utcToLocalTime LT.utc $ (TC.addUTCTime ((fromRational deltaDays :: TC.NominalDiffTime) * TC.nominalDay) $ TC.UTCTime (TD.fromOrdinalDate startYear startDay) 0)

---- nodes follow

-- Ljungqvist (2010)'s temperature reconstruction, from the supplementary data retrieved from https://www.ncei.noaa.gov/pub/data/paleo/contributions_by_author/ljungqvist2010/ljungqvist2010.txt
ljungqvist2010 :: DataGraph.Node [(Integer,Integer,Rational,Rational,Rational,Rational)]
ljungqvist2010 = DataGraph.memoise DataGraph.ljungqvist2010 $ DataGraph.liftIO $ parseFileUtf8 p "data/ljungqvist2010.txt"
  where
    p = dropP 9448 >> repeatP row >>= \rows -> popSeq "\n\n" >> return rows
    row = do
      startYear <- natP
      _ <- popToken '\x00FB'
      endYear <- natP
      anyWhitespace
      reconTemp <- ratP
      anyWhitespace
      upperErr <- ratP
      anyWhitespace
      lowerErr <- ratP
      anyWhitespace
      instTemp <- ratP
      _ <- popToken '\n'
      return (startYear, endYear, reconTemp, upperErr, lowerErr, instTemp)

-- the HadCRUT3v record, converted to cdl with ncdump from HadCRUT3v.nc retrieved from https://crudata.uea.ac.uk/cru/data/crutem3/HadCRUT3v.nc
hadcrut3v :: DataGraph.Node ([Rational], [Rational], [Rational], [[[Maybe Rational]]])
hadcrut3v = DataGraph.memoise DataGraph.hadcrut3v $ DataGraph.liftIO $ parseFileUtf8 p "data/HadCRUT3v.cdl"
  where
    p = do
      _ <- popSeq "netcdf HadCRUT3v {\ndimensions:\n\tlongitude = 72 ;\n\tlatitude = 36 ;\n\tunspecified = 1 ;\n\tt = UNLIMITED ; // (1973 currently)\nvariables:\n\tfloat longitude(longitude) ;\n\t\tlongitude:units = \"degrees_east\" ;\n\t\tlongitude:point_spacing = \"even\" ;\n\t\tlongitude:modulo = \" \" ;\n\tfloat latitude(latitude) ;\n\t\tlatitude:units = \"degrees_north\" ;\n\t\tlatitude:point_spacing = \"even\" ;\n\tfloat unspecified(unspecified) ;\n\t\tunspecified:units = \" \" ;\n\t\tunspecified:positive = \"up\" ;\n\tfloat t(t) ;\n\t\tt:units = \"days since 1850-01-01 00:00:00\" ;\n\t\tt:time_origin = \"01-JAN-1850:00:00:00\" ;\n\tfloat temp(t, unspecified, latitude, longitude) ;\n\t\ttemp:source = \"No field processing\" ;\n\t\ttemp:name = \"temp\" ;\n\t\ttemp:title = \"Temperature T\" ;\n\t\ttemp:date = \"01/01/50\" ;\n\t\ttemp:time = \"00:00\" ;\n\t\ttemp:long_name = \"Temperature T\" ;\n\t\ttemp:units = \"K\" ;\n\t\ttemp:missing_value = 2.e+20f ;\n\t\ttemp:_FillValue = 2.e+20f ;\n\t\ttemp:valid_min = -18.97005f ;\n\t\ttemp:valid_max = 22.44921f ;\n\n// global attributes:\n\t\t:history = \"Thu Jun 26 18:43:32 BST 2014 - CONVSH V1.91 16-February-2006\" ;\ndata:\n\n longitude = "
      longitudes <- repeatSep ratP (popToken ',' >> anyWhitespace)
      _ <- popSeq " ;\n\n latitude = "
      latitudes <- repeatSep ratP (popToken ',' >> anyWhitespace)
      _ <- popSeq " ;\n\n unspecified = 0 ;\n\n t = "
      times <- repeatSep ratP (popToken ',' >> anyWhitespace)
      _ <- popSeq " ;\n\n temp =\n  "
      tempSeq <- repeatSep (fmap Just expP `backup` (popToken '_' >> return Nothing)) (popToken ',' >> anyWhitespace)
      tempLo <- return $ unconcat (length longitudes) tempSeq
      tempTiLaLo <- return $ unconcat (length latitudes) tempLo
      --_ <- popSeq " ;\n}\n"
      return (longitudes, latitudes, map (dayToYearFrac 1850 0) times, tempTiLaLo)

-- the HadCRUT5 non-infilled ensemble means, converted to cdl with ncdump from HadCRUT.5.0.2.0.anomalies.ensemble_mean.nc retrieved from https://www.metoffice.gov.uk/hadobs/hadcrut5/data/HadCRUT.5.0.2.0/non-infilled/HadCRUT.5.0.2.0.anomalies.ensemble_mean.nc
hadcrut5 :: DataGraph.Node ([Rational], [Rational], [Rational], [[[Maybe Rational]]])
hadcrut5 = DataGraph.memoise DataGraph.hadcrut5 $ DataGraph.liftIO $ parseFileUtf8 p "data/HadCRUT.5.0.2.0.anomalies.ensemble_mean.cdl"
  where
    p = do
      _ <- popSeq "netcdf HadCRUT.5.0.2.0.anomalies.ensemble_mean {\ndimensions:\n\ttime = 2095 ;\n\tlatitude = 36 ;\n\tlongitude = 72 ;\n\tbnds = 2 ;\nvariables:\n\tdouble tas_mean(time, latitude, longitude) ;\n\t\ttas_mean:_FillValue = -1.e+30 ;\n\t\ttas_mean:long_name = \"blended air_temperature_anomaly over land with sea_water_temperature_anomaly\" ;\n\t\ttas_mean:units = \"K\" ;\n\t\ttas_mean:cell_methods = \"area: mean (interval: 5.0 degrees_north 5.0 degrees_east) time: mean (interval: 1 month) realization: mean\" ;\n\t\ttas_mean:coordinates = \"realization\" ;\n\tdouble time(time) ;\n\t\ttime:axis = \"T\" ;\n\t\ttime:bounds = \"time_bnds\" ;\n\t\ttime:units = \"days since 1850-01-01 00:00:00\" ;\n\t\ttime:standard_name = \"time\" ;\n\t\ttime:long_name = \"time\" ;\n\t\ttime:calendar = \"standard\" ;\n\tint64 time_bnds(time, bnds) ;\n\tdouble latitude(latitude) ;\n\t\tlatitude:axis = \"Y\" ;\n\t\tlatitude:bounds = \"latitude_bnds\" ;\n\t\tlatitude:units = \"degrees_north\" ;\n\t\tlatitude:standard_name = \"latitude\" ;\n\t\tlatitude:long_name = \"latitude\" ;\n\tdouble latitude_bnds(latitude, bnds) ;\n\tdouble longitude(longitude) ;\n\t\tlongitude:axis = \"X\" ;\n\t\tlongitude:bounds = \"longitude_bnds\" ;\n\t\tlongitude:units = \"degrees_east\" ;\n\t\tlongitude:standard_name = \"longitude\" ;\n\t\tlongitude:long_name = \"longitude\" ;\n\tdouble longitude_bnds(longitude, bnds) ;\n\tint64 realization ;\n\t\trealization:bounds = \"realization_bnds\" ;\n\t\trealization:units = \"1\" ;\n\t\trealization:standard_name = \"realization\" ;\n\tint64 realization_bnds(bnds) ;\n\n// global attributes:\n\t\t:comment = \"2m air temperature over land blended with sea water temperature at a depth of 20cm expressed as monthly anomalies relative to 1961-1990 climatology.\" ;\n\t\t:history = \"Data set built at: 2024-09-02T14:38:01+00:00\" ;\n\t\t:institution = \"Met Office Hadley Centre / Climatic Research Unit, University of East Anglia\" ;\n\t\t:licence = \"HadCRUT5 is licensed under the Open Government Licence v3.0 except where otherwise stated. To view this licence, visit https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3\" ;\n\t\t:reference = \"C. P. Morice, J. J. Kennedy, N. A. Rayner, J. P. Winn, E. Hogan, R. E. Killick, R. J. H. Dunn, T. J. Osborn, P. D. Jones and I. R. Simpson (2021), An updated assessment of near-surface temperature change from 1850: the HadCRUT5 data set, Journal of Geophysical Research: Atmospheres, 126, e2019JD032361. https://doi.org/10.1029/2019JD032361\" ;\n\t\t:source = \"CRUTEM.5.0.2.0 HadSST.4.0.1.0\" ;\n\t\t:title = \"HadCRUT.5.0.2.0 blended land air temperature and sea-surface temperature anomaly data set\" ;\n\t\t:version = \"HadCRUT.5.0.2.0\" ;\n\t\t:Conventions = \"CF-1.7\" ;\ndata:\n\n tas_mean =\n  "
      tempSeq <- repeatSep (fmap Just expP `backup` (popToken '_' >> return Nothing)) (popToken ',' >> anyWhitespace)
      _ <- popSeq " ;\n\n time = "
      times <- repeatSep ratP (popToken ',' >> anyWhitespace)
      _ <- popSeq " ;\n\n time_bnds =\n  "
      _ <- repeatSep ratP (popToken ',' >> anyWhitespace)
      _ <- popSeq " ;\n\n latitude = "
      latitudes <- repeatSep ratP (popToken ',' >> anyWhitespace)
      _ <- popSeq " ;\n\n latitude_bnds =\n  "
      _ <- repeatSep ratP (popToken ',' >> anyWhitespace)
      _ <- popSeq " ;\n\n longitude = "
      longitudes <- repeatSep ratP (popToken ',' >> anyWhitespace)
      _ <- popSeq " ;\n\n longitude_bnds =\n  "
      _ <- repeatSep ratP (popToken ',' >> anyWhitespace)
      _ <- popSeq " ;\n\n realization = 100 ;\n\n realization_bnds = 1, 200 ;\n}\n"
      tempLo <- return $ unconcat (length longitudes) tempSeq
      tempTiLaLo <- return $ unconcat (length latitudes) tempLo
      return (longitudes, latitudes, map (dayToYearFrac 1850 1 . fromRational) times, tempTiLaLo)

-- global coastlines from binned_GSHHS_c.cdl, converted with ncdump from binned_GSHHS_c.nc, unzipped from gshhg-gmt-2.3.7.tar.gz, retrieved from http://www.soest.hawaii.edu/pwessel/gshhg/gshhg-gmt-2.3.7.tar.gz
binned_GSHHS_c :: DataGraph.Node Cdl.Group
binned_GSHHS_c = DataGraph.memoise DataGraph.binned_GSHHS_c $ DataGraph.liftIO $ fmap Cdl.parse $ readFileUtf16Le "data/binned_GSHHS_c.cdl"

-- HadSST4 data, WIP
hadsst4nh :: IO Csv.RawCsv
hadsst4nh = parseFileUtf8 Csv.csvParser "data/HadSST.4.1.1.0_annual_GLOBE.csv"
