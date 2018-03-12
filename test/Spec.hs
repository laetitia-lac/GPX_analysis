{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Data.Char

import GPXGarmin
import Haversine
import GPXhandling

import System.IO.Unsafe
import System.Directory

getGPXPart part fname -- how to test results of I/O actions
 = unsafePerformIO $
     do txt <- readFile fname
        let gpx = parseGarmin txt
        return $ part gpx

firstPoint = Trkpt 46.992480 2.409190 9.56 46
secondPoint = Trkpt 46.981472 2.419833 9.57 59
thirdPoint = Trkpt 46.983346 2.425670 8.99 72
fourthPoint = Trkpt 46.978193 2.423438 8.99 86
fifthPoint = Trkpt 46.992480 2.409190 9.56 110

{- HUnit Tests -------------------- -}

{- Parsing tests -}
simple_parsing_short = "\tName : \"RITD16 (short)\"\n\tType : \"street_running\"\n\tTrack : [Trkpt {lat = 53.34834856912494, lon = -6.251975242048502, ele = 9.0, time = 4986044333000},Trkpt {lat = 53.348345048725605, lon = -6.251754546537995, ele = 8.800000190734863, time = 4986044338000},Trkpt {lat = 53.34833348169923, lon = -6.251550866290927, ele = 8.399999618530273, time = 4986044343000},Trkpt {lat = 53.348312359303236, lon = -6.2513315957039595, ele = 8.800000190734863, time = 4986044347000},Trkpt {lat = 53.34829894825816, lon = -6.251205364242196, ele = 9.399999618530273, time = 4986044349000},Trkpt {lat = 53.348242957144976, lon = -6.250886432826519, ele = 10.600000381469727, time = 4986044354000},Trkpt {lat = 53.348218984901905, lon = -6.250669173896313, ele = 10.800000190734863, time = 4986044358000},Trkpt {lat = 53.348226863890886, lon = -6.250411765649915, ele = 12.0, time = 4986044362000},Trkpt {lat = 53.348222840577364, lon = -6.250115046277642, ele = 13.399999618530273, time = 4986044367000},Trkpt {lat = 53.348222337663174, lon = -6.2498557940125465, ele = 14.199999809265137, time = 4986044372000}]\n"
simple_parsing_little = "\tName : \"Little Test\"\n\tType : \"car\"\n\tTrack : [(823,425000)]\n"

test_get_short_name  = getGPXPart name "data/short.gpx" @?= "RITD16 (short)"
test_get_run_name  = getGPXPart name "data/run.gpx" @?= "RunInTheDark 2016"
test_get_short_simple_parsing  = simpleParsing "data/short.gpx" @?= simple_parsing_short
test_get_little_simple_parsing  = simpleParsing "data/little.gpx" @?= simple_parsing_little
test_get_short_set_distances_and_times = showGPXasDistanceAndTime "data/short.gpx" @?= [(15,5000),(14,5000),(15,4000),(9,2000),(22,5000),(15,4000),(17,4000),(20,5000),(17,5000)]
test_get_little_set_distances_and_times = showGPXasDistanceAndTime "data/little.gpx" @?= [(823,425000)]

test_parsing
 = testGroup "\nParsing GPX Files"
      [ testCase "Get short.gpx name" $ test_get_short_name,
       testCase "Get run.gpx name" $ test_get_run_name, 
       testCase "Get short.gpx simple parsing" $ test_get_short_simple_parsing, 
       testCase "Get little.gpx simple parsing" $ test_get_short_simple_parsing, 
       testCase "Get short.gpx parsing as set of distances and times" $ test_get_short_set_distances_and_times,
       testCase "Get little.gpx parsing as set of distances and times" $ test_get_little_set_distances_and_times
      ]

test_convert_two_points = convertGPXtoDistanceAndTime [firstPoint, secondPoint] [] @?= [(1466,13)]
test_convert_three_points = convertGPXtoDistanceAndTime [firstPoint, secondPoint, thirdPoint] [] @?= [(1466,13),(489,13)]
test_convert_four_points = convertGPXtoDistanceAndTime [firstPoint, secondPoint, thirdPoint, fourthPoint] [] @?= [(1466,13),(489,13),(597,14)]
test_convert_empty_list = convertGPXtoDistanceAndTime [] [] @?= []

test_convert_gpx_set_distances_and_times
 = testGroup "\nConvert GPX Files as a set of distances and times"
      [ testCase "Convert a list of two points" $ test_convert_two_points, 
       testCase "Convert a list of three points" $ test_convert_three_points, 
       testCase "Convert a list of four points" $ test_convert_four_points, 
       testCase "Convert an empty list" $ test_convert_empty_list
      ]

{- Summary tests -}
test_compute_distance_one_point = computeSumDistance [firstPoint] 0 @?= 0
test_compute_distance_two_points = computeSumDistance [firstPoint, secondPoint] 0 @?= 1.4662883996328782
test_compute_distance_three_points =  computeSumDistance [firstPoint, secondPoint, thirdPoint] 0 @?= 1.955663250015882
test_compute_distance_four_points = computeSumDistance [firstPoint, secondPoint, thirdPoint, fourthPoint] 0 @?= 2.5531456964216113
test_compute_distance_fifth_points = computeSumDistance [firstPoint, secondPoint, thirdPoint, fourthPoint, fifthPoint] 0 @?= 4.474574960017367

test_summary_compute_total_distance
 = testGroup "\nSummary-Compute total distance"
      [ testCase "Compute total distance for a list of two points" $ test_compute_distance_two_points, 
       testCase "Compute total distance for a list of three points" $ test_compute_distance_three_points, 
       testCase "Compute total distance for a list of four points" $ test_compute_distance_four_points, 
       testCase "Compute total distance for a list of fifth points (with same point for departure and arrival)" $ test_compute_distance_fifth_points, 
       testCase "Compute total distance for one point" $ test_compute_distance_one_point,
       testCase "Compute total distance of an empty list" $ computeSumDistance [] 0 @?= 0
      ]

test_compute_time_one_point = computeSumTime [firstPoint] @?= 0
test_compute_time_two_points = computeSumTime [firstPoint, secondPoint] @?= 13
test_compute_time_three_points =  computeSumTime [firstPoint, secondPoint, thirdPoint] @?= 26
test_compute_time_four_points = computeSumTime [firstPoint, secondPoint, thirdPoint, fourthPoint] @?= 40
test_compute_time_fifth_points = computeSumTime [firstPoint, secondPoint, thirdPoint, fourthPoint, fifthPoint] @?= 64

test_summary_compute_total_time
 = testGroup "\nSummary-Compute total time"
      [ testCase "Compute total time for a list of two points" $ test_compute_time_two_points, 
       testCase "Compute total time for a list of three points" $ test_compute_time_three_points, 
       testCase "Compute total time for a list of four points" $ test_compute_time_four_points, 
       testCase "Compute total time for a list of fifth points (with same point for departure and arrival)" $ test_compute_time_fifth_points, 
       testCase "Compute total time for one point" $ test_compute_time_one_point, 
       testCase "Compute total time of an empty list" $ computeSumTime [] @?= 0
      ]

test_compute_pace_one_point = computeSumPace [firstPoint] @?= 0
test_compute_pace_two_points = computeSumPace [firstPoint, secondPoint] @?= 1.4776538279980567e-4
test_compute_pace_three_points =  computeSumPace [firstPoint, secondPoint, thirdPoint] @?= 2.2157870652312673e-4
test_compute_pace_four_points = computeSumPace [firstPoint, secondPoint, thirdPoint, fourthPoint] @?= 2.6111579437124974e-4
test_compute_pace_fifth_points = computeSumPace [firstPoint, secondPoint, thirdPoint, fourthPoint, fifthPoint] @?= 2.38383908236622e-4

test_summary_compute_total_pace
 = testGroup "\nSummary-Compute total pace"
      [ testCase "Compute total pace for a list of two points" $ test_compute_pace_two_points, 
       testCase "Compute total pace for a list of three points" $ test_compute_pace_three_points, 
       testCase "Compute total pace for a list of four points" $ test_compute_pace_four_points, 
       testCase "Compute total pace for a list of fifth points (with same point for departure and arrival)" $ test_compute_pace_fifth_points, 
       testCase "Compute total pace for a one point" $ test_compute_pace_one_point, 
       testCase "Compute total pace of an empty list" $ computeSumPace [] @?= 0
      ]

test_summary_run_gpx = summarizeFile "data/run.gpx" @?= "\n\tTotal distance is 10.145512236833222(in km) \n\tTotal time is 0:55:53 (in hr:min:s) \n\tTotal pace is 5.508182537146742 (in mins/km)\n"
test_summary_short_gpx = summarizeFile "data/short.gpx" @?= "\n\tTotal distance is 0.14223883872428575(in km) \n\tTotal time is 0:0:39 (in hr:min:s) \n\tTotal pace is 4.569778590923068 (in mins/km)\n"
test_summary_little_gpx = summarizeFile "data/little.gpx" @?= "\n\tTotal distance is 0.8234332712158375(in km) \n\tTotal time is 0:7:5 (in hr:min:s) \n\tTotal pace is 8.602194714423504 (in mins/km)\n"

test_summary_gpx_files
 = testGroup "\nSummary for a specific GPX file"
      [ testCase "Summary for run.gpx" $ test_summary_run_gpx, 
       testCase "Summary for short.gpx" $ test_summary_short_gpx, 
       testCase "Summary for little.gpx" $ test_summary_little_gpx
      ]

{-Specify a distance and find the fastest subsequence of the track - tests -}

test_fin_subsequence_two_track_points = show (findSubsequence [(1466,13)] [] [firstPoint, secondPoint] []) @?= show [firstPoint, secondPoint]
test_fin_subsequence_three_track_points = show (findSubsequence [(1466,13),(489,13)] [] [firstPoint, secondPoint, thirdPoint] []) @?= show [firstPoint, secondPoint, thirdPoint]
test_fin_subsequence_four_track_points =  show (findSubsequence [(1466,13),(489,13),(597,14)] [] [firstPoint, secondPoint, thirdPoint, fourthPoint] []) @?= show [firstPoint, secondPoint, thirdPoint, fourthPoint]
test_fin_subsequence_five_track_points = show (findSubsequence [(1466,13),(489,13),(597,14),(1921,24)] [] [firstPoint, secondPoint, thirdPoint, fourthPoint, fifthPoint] []) @?= show [firstPoint, secondPoint, thirdPoint, fourthPoint, fifthPoint]

test_analysis
 = testGroup "\nAnalysis"
      [ testCase "Find two track points thanks to a set of distances and times" $ test_fin_subsequence_two_track_points,
       testCase "Find three track points thanks to a set of distances and times" $ test_fin_subsequence_three_track_points, 
       testCase "Find four track points thanks to a set of distances and times" $ test_fin_subsequence_four_track_points, 
       testCase "Find five track points thanks to a set of distances and times" $ test_fin_subsequence_five_track_points
      ]

{- Directory listing tests -}

test_directory_listing_current_directory = unsafePerformIO (getDirectoryContents ".") @?= ["test","stack.yaml","src","Setup.hs","README.md","LICENSE","gpxanalysis.cabal","data",".stack-work","..","."]
test_directory_listing_data = unsafePerformIO (getDirectoryContents "data") @?= ["short.gpx","run.gpx","little.gpx","..","."]

test_directory_listing
 = testGroup "\nDirectory listing - ! Be careful ! Tests are ok only if the executable is in 'VisitingExtra' folder"
      [ testCase "Current directory" $ test_directory_listing_current_directory, 
       testCase "Data directory" $ test_directory_listing_data
      ]

{- To exit tests-}

test_help_facilities_exit = unsafePerformIO (executeCommand "exit") @?= True
test_help_facilities_inexistant_command = unsafePerformIO (executeCommand "inexistant-command") @?= False

test_help_facilities
 = testGroup "\nHelp facilities and exit"
      [ testCase "Exit" $ test_help_facilities_exit, 
       testCase "Inexistant command gives a loop" $ test_help_facilities_inexistant_command
      ]

{-Teacher tests -}

test_fasterThan
  = testGroup "faster-than"
      [ testCase "9/3 > 6/3"   ( (9,3)  `fasterThan` (6,3)  @?= True )
      , testCase "7/3 > 6/3"   ( (7,3)  `fasterThan` (6,3)  @?= True )
      , testCase "6/3 > 6/3"   ( (6,3)  `fasterThan` (6,3)  @?= False )
      , testCase "5/3 > 6/3"   ( (5,3)  `fasterThan` (6,3)  @?= False )
      , testCase "6/18 > 6/23" ( (6,18) `fasterThan` (6,23) @?= True )
      ]

test_stuff
 = testGroup "\nTesting Stuff"
     [ testCase "1+1=2" ( 1+1 @?= 2 )
     , testCase "2+2=4" ( 2+2 @?= 4 )
     , testCase "measured prefix"
        ( getMeasuredPrefix 6 0 0 []
                 [(1,10),(2,3),(1,2),(2,8),(1,5),(2,12),(1,5)]
           @?= ( [(1,10),(2,3),(1,2),(2,8)]
               , 6, 23
               , [(1,5),(2,12),(1,5)] ) )
     , testCase "next current"
        ( nextCurrent 6 5 13 (reverse [(2,3),(1,2),(2,8)])
                             [(1,5),(2,12),(1,5)]
          @?= ( 6, 18
              , [(2,3),(1,2),(2,8),(1,5)]
              , [(2,12),(1,5)] ) )
     , test_fasterThan
     , testCase "fastest - example"
        ( fastest 6 [(1,10),(2,3),(1,2),(2,8),(1,5),(2,12),(1,5)]
           @?= [(2,3),(1,2),(2,8),(1,5)] )
     , testCase "fastest - null" ( fastest 42 [] @?= [] )
     , testCase "fastest - short"
        ( fastest 20 [(1,10),(2,3),(1,2),(2,8),(1,5),(2,12),(1,5)]
           @?= [(1,10),(2,3),(1,2),(2,8),(1,5),(2,12),(1,5)] )
     , testCase "fastest - one pair"
        ( fastest 1 [(1,100),(2,30),(1,20),(2,2),(1,50),(2,120),(1,50)]
           @?= [(2,2)] )
     , testCase "fastest - reverse"
        ( fastest 6 (reverse [(1,10),(2,3),(1,2),(2,8),(1,5),(2,12),(1,5)])
           @?= reverse (fastest 6 [(1,10),(2,3),(1,2),(2,8),(1,5),(2,12),(1,5)]) )
     ]

{- Test Main ---------------------- -}

main = defaultMain tests

tests :: [TF.Test]
tests
  = [ test_parsing,
      test_convert_gpx_set_distances_and_times,
      test_summary_compute_total_distance,
      test_summary_compute_total_time,
      test_summary_compute_total_pace,
      test_summary_gpx_files,
      test_analysis,
      test_directory_listing,
      test_help_facilities,
      test_stuff
    ]
