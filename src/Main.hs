module Main where
import System.Directory

import GPXGarmin
import Haversine
import REPL
import GPXhandling

import System.IO.Unsafe
import System.IO
import Control.Exception

version = "0.1.0.0"
main
 = do putStrLn ("\n\tGPX Analysis v"++version++"\n")
      rexpl firstDisplay executeCommand
