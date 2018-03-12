# CS3016 Visiting Students Extra Work

## Task

Develop a program that allows GPS data to be read and
analysed.

###  Program Features

The program mainline will be a REPL with state which provides
the following features:

1. the ability to load and parse a GPX file (10 marks)
2. summarise the contents of a GPX track as follows :
  1. show total distance in km (6 marks)
  2. show total time in hr:min:sec format (8 marks)
  3. show average pace in mins/km (6 marks)
3. allow the user to specify a distance and then search for the fastest subsequence of the track of that distance (25 marks)
4. provide an ability to do a directory listing (10 marks)
5. provide help facilites and a means to exit the program (10 marks)

Note: 3 is challenging --- read comments in `src/GPXhandling.hs` carefully

### Grading

This exercise substitutes for the end-of-year exam, and so is worth 75% of the total marks for the course.
It will be marked out of 75.

Marks here therefore, bear no relation to the marks used for Exercises Zero through Two, 
where those 260 marks contribute to 25% of the module total.

### Function Tests

In addition, you should write at least one test (in `test/Spec.hs`) for every major function you write. Whether or not a function is "major" is left to your own discretion. However there should be at least one test for each feature above. 1 of the marks for each feature above will depend on the tests.

A sample test involving file reading/parsing
is given in `test/Spec.hs`

It shows how to use `unsafePerformIO` to do such tests.

### Provided Resources

Module `GPXGarmin` provides a datatype `GPX` to represent
a GPX track, as well as a function `parseGarmin` that
parses the GPX file data.

Module `Haversine` provides a way (function `eHaversine`) to compute the distance between two points specified by latitude and longitude coordinates in degrees.

Module `REPL` is the REPL module from the class lectures.

Module `GPXhandling` is provided for you to add code as appropriate.
Note that it is easy to write tests in `test/Spec.hs` for functions in this module.

**Note:** you need to pay close attention the units being used in various datatypes -- read the comments carefully!

Two sample GPX files are provided in the `data` subdirectory: `run.gpx `and `short.gpx `
-- the latter is just the first 10 track points of the former.

### Coding your solution

You can modify `src/Main.hs`, `src/GPXhandling.hs` and `test/Spec.hs` or add in your own extra  modules. If you do the latter, you may find that stack complains (mildly) that the module is not "listed in gpxanalysis.cabal", and suggests to add it to "other-modules". You can safely ignore this warning, or, for extra excitement, try to fix things so the warning does not occur. Backing-up the `.cabal` file beforehand is advisable.

Note: do not modify `GPXGarmin`, `Haversine` or `REPL`!

The main module `src/Main.hs` imports `System.Directory`, which provides, among others, the IO action:

`getDirectoryContents :: FilePath -> IO [FilePath]`

This might come in handy!

## Grading

The main method for grading will be to run the executable and attempt to exercise all the features described above, on both `data/run.gpx `and `data/short.gpx`

The tests will also be run and assessed for usefulness.

## Running the executable

Build the exectuable:  `stack build`

Run the executable: `stack exec gpxanalysis`

## Running Tests

Unlike the labs, your code and tests, will be graded manually.

As with previous labs, you need to type
`stack test`
to run the tests.

## Submitting Work

Archive the `VisitingExtra` folder using zip/tar or similar
and submit this by email to Andrew.Butterfield@scss.tcd.ie

Submission to Blackboard *may* be available.

Currently supported archive formats are `.zip`, `.tar`, `.tar.gz` and  `.rar`.


## Running the Interpeter

You can also type

`stack ghci` 

to startup the GHCi interpeter.
Type `:help` to get help.

Once the interpeter is loaded,
enter the command

`:l ????`

which will load up the Lab04.hs file so you can experiment.

Simply type an expression at the prompt and GHCi will evaluate it.

The command 

`:browse`

will show all the values currently defined in that file.

You can exit the interpeter by typing

`:q`




