module REPL where
import Data.Char

shout = do putStr "Say something: "
           utterance <- getLine
           putStrLn ("You said : "++map toUpper utterance)
           if null utterance then return () else shout

shout2
 = do putStr "Say something: "
      said <- getLine
      if null said
      then putStrLn "I CAN'T HEAR YOU! I'M OFF !!"
      else do putStrLn ("You said : "++map toUpper said)
              shout2

revpl prompt eval print done
 = do putStrLn prompt
      userinp <- getLine
      let result = eval userinp
      print result
      if done result
       then return ()
       else revpl prompt eval print done

shout1 = revpl "Say something :"
               (map toUpper)
               print1
               null

print1 res = putStrLn ("You said : "++res)

rexpl prompt execute
 = do putStrLn prompt
      usercmd <- getLine
      done <- execute usercmd
      if done then return ()
              else rexpl prompt execute

shout3 = rexpl "Say something :"  doshout3

doshout3 utt
  = if null utt
    then do putStrLn "I CAN'T HEAR YOU! I'M OFF !!"
            return True
    else do putStrLn ("You said : "++map toUpper utt)
            return False

totup = dototting 0.0

dototting tot
 = do putStr("["++show tot++"]\n:- ")
      numtxt <- getLine
      if null numtxt
       then putStrLn ("\nTotal = "++show tot)
       else dototting (tot+read numtxt) -- state update!


srepl prompt done exit execute state
 = do prompt state
      cmd <- getLine
      if done cmd
       then exit state
       else
        let state' = execute cmd state
        in srepl prompt done exit execute state'


totpr tot = putStr("["++show tot++"]\n:- ")

totxit tot = putStrLn ("\nTotal = "++show tot)

totexe cmd tot = tot+read cmd

totup2 = srepl totpr null totxit totexe 0.0
