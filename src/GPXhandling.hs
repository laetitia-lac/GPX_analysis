module GPXhandling where
import System.Directory
import System.IO.Unsafe
import GPXGarmin
import Haversine
import REPL
import Data.Char

{-
   use this module to add your own code,
-}

{-
     Assume we have lists pairs of integers (distance, time),
     where both components are non-zero and positive.
     Given a distance, the following function returns
        the fastest subsequence close to that distance.

  So given [(1,10),(2,3),(1,2),(2,8),(1,5),(2,12),(1,5)]
  if we specify distance 6,
  we have the following subsequences of length 6
        [(1,10),(2,3),(1,2),(2,8) ] -- time = 23
        [(2,3), (1,2),(2,8),(1,5) ] -- time = 18
        [(1,2), (2,8),(1,5),(2,12)] -- time = 27
        [(2,8), (1,5),(2,12),(1,5)] -- time = 30

  So the fastest subsequence of length 6 has time 18
  and  is [(2,3),(1,2),(2,8),(1,5)]

  If the lengths don't add-up precisely,
  we compare speeds (dist / time).

  This can be tricky to do with Ints in Haskell.
  You have to work with Doubles, not Ints, which are much simpler.

-}

fastest
  :: Int         -- udist : user requested distance
  -> [(Int,Int)] -- pairs : full sequence
  -> [(Int,Int)]  -- fastest subsequence

fastest udist pairs
 -- first find shortest prefix whose distance is equal or greater
 -- to that suggested by the user (if it exists)
  = let
      (subseq1,dist,time,rest) = getMeasuredPrefix udist 0 0 [] pairs
    in findFastestSeq udist dist time subseq1 dist time subseq1 rest

getMeasuredPrefix
  :: Int           -- udist : user requested distance
  -> Int           -- dsofar : distance of subsequence so far
  -> Int           -- tsofar : time of subsequence so far
  -> [(Int,Int)]   -- ss : subsequence so far (reverse order)
  -> [(Int,Int)]   -- pairs : remainder of full sequence
  -> ( [(Int,Int)]   -- shortest prefix of at least required distance
     , Int           -- distance of shortest prefix
     , Int           -- time of shortest prefix
     , [(Int,Int)] ) -- rest of full sequence
getMeasuredPrefix udist dsofar tsofar ss []
  -- no more full sequence, just return what we have (it might be short!)
  = (reverse ss,dsofar,tsofar,[])
getMeasuredPrefix udist dsofar tsofar ss (p@(d,t):pairs)
  | dsofar' >= udist  =  (reverse (p:ss),dsofar',tsofar',pairs)
  | otherwise = getMeasuredPrefix udist dsofar' tsofar' (p:ss) pairs
  where
   dsofar' = dsofar + d
   tsofar' = tsofar + t

findFastestSeq
  :: Int         -- udist : user requested distance
  -> Int         -- fdist : distance of fastest subseq so far
  -> Int         -- ftime : time of fastest subseq so far
  -> [(Int,Int)] -- fastest : fastest subseq so far
  -> Int         -- cdist : distance of current/candidate subseq
  -> Int         -- ctime : time of current/candidate subseq
  -> [(Int,Int)] -- current : current candidate subsequence
  -> [(Int,Int)] -- pairs: rest of full sequence
  -> [(Int,Int)] -- fastest subsequence
findFastestSeq udist fdist ftime fastest cdist ctime current [] = fastest
-- we assume at this point that the current subseq is no better than the fastest
-- otherwise we would have recorded it as the fastest
-- we also assume that current is not null
findFastestSeq udist fdist ftime fastest cdist ctime ((cd,ct):current) pairs
  -- next current is too short, we are done
  | cdist' < udist  =  fastest
    -- we check if current is faster than fastest, and update accordingly
  | (cdist',ctime') `fasterThan` (fdist,ftime)
     = findFastestSeq udist cdist' ctime' current' cdist' ctime' current' pairs'
  | otherwise
     = findFastestSeq udist fdist  ftime  fastest  cdist' ctime' current' pairs'
  where
    -- we remove one element from start of current
    -- and then while cdist < udist we add to the end of current from pairs
   (cdist',ctime',current',pairs')
       = nextCurrent udist (cdist-cd) (ctime-ct) (reverse current) pairs

nextCurrent
  :: Int         -- udist : user requested distance
  -> Int         -- cdist : distance of current subseq
  -> Int         -- ctime : time of current subseq
  -> [(Int,Int)] -- current : current (short) subsequence (reverse order)
  -> [(Int,Int)] -- pairs : rest of full sequence
  -> ( Int            -- distance of current subseq
     , Int            -- time of current subseq
     , [(Int,Int)]    -- current subseq with distance >= udist
     , [(Int,Int)] )  -- rest of full sequence
nextCurrent udist cdist ctime current [] = (cdist,ctime,reverse current,[])
nextCurrent udist cdist ctime current (p@(d,t):pairs)
 = let
     cdist' = cdist + d
     ctime' = ctime + t
     current' = p:current
   in if cdist' >= udist
       then (cdist',ctime',reverse current',pairs)
       else nextCurrent udist cdist' ctime' current' pairs

fasterThan
  :: ( Int  -- cdist : current distance
     , Int  -- ctime : current time
     )
  -> ( Int  -- fdist : fastest distance
     , Int  -- ftime : fastest time
     )
  -> Bool  -- True if current is faster than fastest
(cdist,ctime) `fasterThan` (fdist,ftime)  =  cdist * ftime > fdist * ctime
-- not so tricky, as we can avoid division !


--
--
--my own program --
--
--
--
getGPX part fname -- how to test results of I/O actions
 = unsafePerformIO $
     do txt <- readFile fname
        let gpx = parseGarmin txt
        return $ part gpx

--
--1st question -- ability to load and parse a GPX file
--
showGPXasDistanceAndTime :: String -> [(Int,Int)]
showGPXasDistanceAndTime pathFile = let 
                trackFile = getGPX track pathFile
                result = convertGPXtoDistanceAndTime trackFile []
              in result

showGPXasTrackList :: String -> [Trkpt]
showGPXasTrackList pathFile = getGPX track pathFile

--given a list of Trkpt we will obtain a set of distances and times
convertGPXtoDistanceAndTime :: [Trkpt] -> [(Int,Int)] -> [(Int,Int)]
convertGPXtoDistanceAndTime (l:[]) listInt = reverse listInt
convertGPXtoDistanceAndTime (l:(l':list)) listInt = let 
                    distanceBetweenTwoPoints = round (eHaversine (lat l,lon l) (lat l',lon l'))
                    timeBetweenTwoPoints = computeTime l l'
                  in convertGPXtoDistanceAndTime (l':list) ((distanceBetweenTwoPoints, timeBetweenTwoPoints):listInt)
convertGPXtoDistanceAndTime [] _ = []

--execute a parsing of a gpx file (indicate by a path) such as the track is defined as a table of distances and times
distanceAndTimeParsing :: String -> String
distanceAndTimeParsing pathFile = let 
              nameFile = show (getGPX name pathFile)
              typeFile = show (getGPX ttype pathFile)
              trackFile = show (showGPXasDistanceAndTime pathFile)
            in "\tName : "++ nameFile ++"\n\tType : "++ typeFile ++"\n\tTrack : "++ trackFile ++"\n"

--execute a parsing of a gpx file (indicate by a path) --> indicates the name, type and untreated track
simpleParsing :: String -> String
simpleParsing pathFile = let 
              nameFile = show (getGPX name pathFile)
              typeFile = show (getGPX ttype pathFile)
              trackFile = show (getGPX track pathFile)
            in "\tName : "++ nameFile ++"\n\tType : "++ typeFile ++"\n\tTrack : "++ trackFile ++"\n"

showParseGPXFile :: String -> String
showParseGPXFile pathFile = let 
              simple = simpleParsing pathFile
              distanceAndTimeTable = distanceAndTimeParsing pathFile
            in "\n\tSimple parsing : \n"++ simple ++"\n" ++ "\tParsing as set of distances (in m) and times (in ms) : \n"++ distanceAndTimeTable

--
--2nd question -- summarize content of track file
--
summarizeFile :: String -> String
summarizeFile pathFile = let
              trackFile = getGPX track pathFile
              totalDistance = displaySumDistance trackFile
              totalTime = displaySumTime trackFile
              totalPace = displaySumPace trackFile
            in totalDistance ++  totalTime ++ totalPace

displaySumDistance :: [Trkpt] -> String
displaySumDistance list = "\n\tTotal distance is " ++ show(computeSumDistance list 0) ++ "(in km) \n"

-- compute the total distance for a list of track
computeSumDistance :: [Trkpt] -> Double -> Double
computeSumDistance (l:(l':list)) oldDistance = let 
                        distanceBetweenTwoPoints = eHaversine (lat l,lon l) (lat l',lon l')
                        distance = oldDistance + distanceBetweenTwoPoints
                      in computeSumDistance (l':list) distance
computeSumDistance (l:[]) distance = distance/1000 --in km
computeSumDistance [] _ = 0

displaySumTime :: [Trkpt] -> String
displaySumTime list = let 
          totalTime = computeSumTime list
          hours = totalTime `div` 3600000 
          minutes = (totalTime - hours*3600000) `div` 60000
          seconds = (totalTime - hours*3600000 -  minutes*60000) `div` 1000
        in "\tTotal time is " ++ show (hours) ++ ":" ++ show (minutes) ++ ":" ++ show (seconds) ++ " (in hr:min:s) \n"

-- compute the total time for a list of track
computeSumTime :: [Trkpt] -> Int
computeSumTime (l:list) = case list of 
                            [] -> 0
                            _ -> let 
                                  (l':_) = reverse list
                                in computeTime l l'
computeSumTime [] = 0

computeTime :: Trkpt -> Trkpt -> Int
computeTime point point' = (time point') - (time point) --in ms

displaySumPace :: [Trkpt] -> String
displaySumPace list = "\tTotal pace is " ++ show (computeSumPace list) ++ " (in mins/km)\n"

-- compute the total pace for a list of track
computeSumPace :: [Trkpt] -> Double
computeSumPace list = let 
        timeInSec = fromIntegral (computeSumTime list)
        timeInMins = timeInSec / 60000
        distance = computeSumDistance list 0
      in case distance of 
          0 -> 0
          _ -> timeInMins / distance

--
--3rd question 
--
getAnalysis :: String -> String -> String
getAnalysis nameFile distanceWished = let 
                                        listIntFound = fastest (read distanceWished) (showGPXasDistanceAndTime nameFile)
                                        listTrack = showGPXasTrackList nameFile
                                    in "\n\t" ++ show listIntFound ++ " meaning : " ++ displayTrackSet (findSubsequence listIntFound [] listTrack []) ""

displayTrackSet :: [Trkpt] -> String -> String
displayTrackSet [] response = response
displayTrackSet (l:listTrack) oldResponse = let 
                                              response = "\n\t\tTrack point with lat " ++ show (lat l) ++ " and lon " ++ show (lon l) ++ " and ele (in m) " ++ show (ele l) ++ " and time (in ms) " ++ show (time l)
                                            in displayTrackSet listTrack (oldResponse ++ response)

findPair :: Trkpt -> Trkpt -> (Int, Int) -> Bool
findPair l l' (distanceToFind, timeToFind) = let 
                              distanceBetweenTwoPoints = round(eHaversine (lat l,lon l) (lat l',lon l'))
                              timeBetweenTwoPoints = computeTime l l'
                              response = distanceBetweenTwoPoints==distanceToFind && timeBetweenTwoPoints==timeToFind
                          in response
               
--parameters : listIntToFind listIntAlreadyFound listTrackToExamine listTrackAlreadyFound  
findSubsequence :: [(Int,Int)] -> [(Int,Int)] -> [Trkpt] -> [Trkpt] -> [Trkpt]
findSubsequence [] _ _ listTrack = reverse listTrack
findSubsequence ((foundThisDistance, foundThisTime):listIntToFind) listIntFound (t:(t':(listTrack))) []  = let 
                                                                                                  result = findPair t t' (foundThisDistance, foundThisTime)
                                                                                                  lengthInt = length ((foundThisDistance, foundThisTime):listIntToFind)
                                                                                                  lengthTrak = length ((t:(t':(listTrack))))
                                                                                              in case result of
                                                                                                             True -> findSubsequence listIntToFind ((foundThisDistance, foundThisTime):listIntFound) (t':listTrack) (t':t:[])
                                                                                                             False -> findSubsequence ((foundThisDistance, foundThisTime):listIntToFind) [] (t':listTrack) []
findSubsequence ((foundThisDistance, foundThisTime):listIntToFind) listIntAlreadyFound (t:(t':(listTrack))) listTrackAlreadyFound = let 
                                                                                                                                       result = findPair t t' (foundThisDistance, foundThisTime)
                                                                                                                                   in case result of
                                                                                                                                         True -> findSubsequence listIntToFind ((foundThisDistance, foundThisTime):listIntAlreadyFound) (t':listTrack) (t':listTrackAlreadyFound)
                                                                                                                                         False -> let
                                                                                                                                                    partOfListIntToFind = (foundThisDistance, foundThisTime):listIntToFind
                                                                                                                                                    listOriginalIntToFind = (reverse listIntAlreadyFound)++partOfListIntToFind
                                                                                                                                                    (trackToThrowAway : listTrackPossibleForAnalysis) = (reverse listTrackAlreadyFound)++(t:(t':(listTrack)))
                                                                                                                                                  in findSubsequence listOriginalIntToFind [] listTrackPossibleForAnalysis []
-- 
--main program
--
firstDisplay = "\tTap your command. \n\tRemember : tap 'help' to display help about the available commands."
executeCommand command
  = if null command
    then do putStrLn "\tGood Bye."
            return True
    else case command of 
            "help" -> do putStrLn ("\thelp : \t\tDisplays this message to understand available commands.")
                         putStrLn ("\tparse : \tDisplays the content of a specific GPX file.")
                         putStrLn ("\tlisting : \tDisplays the listing in the file 'data'.")
                         putStrLn ("\tsummary : \tDisplays total distance, total time and average pace for a specific GPX file.")
                         putStrLn ("\tanalysis : \tSpecify a distance and we will try to find the fastest subsequence of a specific GPX file.")
                         putStrLn ("\tcomplete_analysis : Launch a summary and an analysis.")
                         putStrLn ("\texit : \t\tStop the program.\n")
                         return False
            "parse" -> do rexpl displayParsing executeParsing
                          return False
            "listing" -> do rexpl displayListing executeListing
                            return False
            "summary" -> do rexpl displaySummary executeSummary
                            return False
            "analysis" -> do rexpl displayAnalysis executeAnalysis
                             return False
            "complete_analysis" -> do rexpl displayCompleteA executeCompleteA
                                      return False
            "exit" -> do putStrLn "\tGood Bye."
                         return True
            otherwise -> do putStrLn ("\tYou said : "++ command++", this command is not available.")
                            return False



displayParsing = "\n\tGive me the name of the GPX file to parse. Example : data/run.gpx. \n\tOtherwise, tap enter to return to the main menu."
executeParsing nameFile 
  = if null nameFile
    then do return True
    else do let result = showParseGPXFile nameFile
            putStrLn(result)
            return True

displayListing = "\n\tYou can obtain a directory listing. Indicate the filepath. Example : data. \n\tOtherwise, tap enter to return to the main menu."
executeListing pathName 
  = if null pathName
    then do return True
    else do {-handle (\(e :: IOException) -> print e >> return False) $ do -}
            putStrLn ("\n\tIt contains : " ++ show (unsafePerformIO (getDirectoryContents pathName)) ++ "\n")
            return True

displaySummary = "\n\tGive me the name of the GPX file to summarize. Example : data/run.gpx. \n\tOtherwise, tap enter to return to the main menu."
executeSummary nameFile 
  = if null nameFile
    then do return True
    else do let result = summarizeFile nameFile
            putStrLn(result)
            return True

displayAnalysis = "\n\tGive me the name of the GPX file to analyse. Example : data/run.gpx. \n\tOtherwise, tap enter to return to the main menu."
executeAnalysis nameFile
  = if null nameFile
    then do return True
    else do putStrLn ("\n\tGive me a distance. Otherwise, tap enter to return to the main menu.")
            distanceWished <- getLine
            if null distanceWished
            then do return True
            else do putStrLn ("\n\tThe fastest subsequence available in "++ show (nameFile) ++ " is " ++ (getAnalysis nameFile distanceWished) ++ "\n")     
                    return True

displayCompleteA = "\n\tGive me the name of the GPX file to execute a complete analysis. Example : data/run.gpx. \n\tOtherwise, tap enter to return to the main menu."
executeCompleteA nameFile 
  = if null nameFile
    then do return True
    else do putStrLn ("\n\tGive me a distance. Otherwise, tap enter to return to the main menu.")
            distanceWished <- getLine
            if null distanceWished
            then do return True
            else do let result = summarizeFile nameFile
                    putStrLn ("\n\tSummary :"++result)
                    putStrLn ("\tThe fastest subsequence available in "++ show (nameFile) ++ " is " ++ (getAnalysis nameFile distanceWished) ++ "\n")      
                    return True









