## GPX Analysis 

### Task

Develop a program that allows GPS data to be read and
analysed.

###  Program Features

The program mainline will be a REPL with state which provides
the following features:

1. the ability to load and parse a GPX file.
2. summarise the contents of a GPX track as follows :
  1. show total distance in km.
  2. show total time in hr:min:sec format.
  3. show average pace in mins/km.
3. allow the user to specify a distance and then search for the fastest subsequence of the track of that distance.
4. provide an ability to do a directory listing.
5. provide help facilites and a means to exit the program.

### Provided Resources

Furnished modules by the professor : 

module `GPXGarmin` provides a datatype `GPX` to represent a GPX track, as well as a function `parseGarmin` that parses the GPX file data.
module `Haversine` provides a way (function `eHaversine`) to compute the distance between two points specified by latitude and longitude coordinates in degrees.
module `REPL` is the REPL module from the class lectures.

Modified modules : 

module `GPXhandling` to add the code allowing program features.
module `test/Spec.hs` to test all the created functions.

## Running the executable

Build the exectuable:  `stack build`

Run the executable: `stack exec gpxanalysis`

Run the tests: `stack test`

## Running the Interpeter

You can also type : `stack ghci` to startup the GHCi interpeter.
Type `:help` to get help.

## Contribution

This was an assignment. The instructions were provided by Andrew Butterfield (Professor in School of Computer Science and Statistics, Trinity College Dublin, Ireland).