# Revision history for mmsyn7h

## 0.1.0.0 -- 2019-12-24

* First version. Released on an unsuspecting world.

## 0.2.0.0 -- 2019-12-25

* Second version. Changed the behaviour of the program.

## 0.2.1.0 -- 2019-12-25

* Second version revised A. Program now is a library and a executable, it has another module structure. Some minor documentation improvements.

## 0.2.2.0 -- 2019-12-28

* Second version revised B. Fixed an issue with the null command line arguments and with playing of the created file.

## 0.2.2.1 -- 2019-12-28

* Second version revised C. Some minor documentation improvements.

## 0.3.0.0 -- 2020-01-05

* Third version. Added support for Windows playing the resulting file. Fixed an issue with the deprecated documentation.

## 0.3.0.1 -- 2020-01-05

* Third version revised A. Fixed an issue with the not compiling code.

## 0.4.0.0 -- 2020-01-10

* Fourth version. Added the possibility to work with not the full set of Ukrainian sounds representations, but with some of them. 
The program now can use more command line arguments. Added new functions and dependencies.

## 0.4.0.1 -- 2020-01-10

* Fourth version revised A. Fixed an issue with the more than three command line arguments. The program now plays the file in such a
case and does not remove any resulting sound files.

## 0.4.0.2 -- 2020-01-10

* Fourth version revised B. Fixed an issue with documentation for the appendS16LEFileList function.

## 0.4.0.3 -- 2020-01-10

* Fourth version revised C. Fixed an issue with documentation for the appendS16LEFileList function.

## 0.4.1.0 -- 2020-01-10

* Fourth version revised D. Fixed issues with package not being compiled. 
Improved informational messages. 

## 0.5.0.0 -- 2020-01-14

* Fifth version. Added more command line options like "-h" and "-v". New module 
dependencies. Fixed issues with the deprecated wrong documentation.

## 0.5.1.0 -- 2020-01-16

* Fifth version revised A. Fixed issue with threads. Now, mmsyn7h must run without blocking the other Haskell threads.

## 0.6.0.0 -- 2020-01-24

* Sixth version. Changed the behaviour for the giving information. Now the package uses 'catchEnd' function from the mmsyn7ukr package.
Besides, the last one now is a dependency for the package.

## 0.6.0.1 -- 2020-01-24

* Sixth version revised A. Fixed version information. Changed the information being displayed in case of obtaining informational messages.

## 0.6.1.0 -- 2020-01-24

* Sixth version revised B. Fixed an issue with being not compiled dependency mmsyn7ukr for GHC 7.8.4.

## 0.6.2.0 -- 2020-01-28

* Sixth version revised C. Changed the imported modules and dependency bounds.

## 0.7.0.0 -- 2020-01-28

* Seventh version. Changed the behaviour of the mmsyn7h function. Fixed issues with program being executed further after the giving the needed information.
Some minor documentation improvements.

## 0.7.0.1 -- 2020-01-28

* Seventh version revised A. Fixed issues with unexact documentation (e. g. README file). 

## 0.7.0.2 -- 2020-01-28

* Seventh version revised B. Removed exceptions in case of being given information for "-h" or "-v" command line arguments.
Some documentation imrovements.

## 0.7.1.0 -- 2020-01-28

* Seventh version revised C. Changed dependencies for modules. Changed errors to exceptions.

## 0.7.1.1 -- 2020-01-28

* Seventh version revised D. Fixed issue with the wrong version number.

## 0.7.2.0 -- 2020-01-30

* Seventh version revised E. Changed the README file to the README.markdown. 

## 0.7.3.0 -- 2020-03-11

* Seventh version revised G. Fixed issue with being removed all sound files with extensions in the current directory in case of being executed without
third and more command line arguments (no matter whether you specify to remove them or not). Changed the 'main7h' function and its realization.
The possibility to clean sound files is now controlled by the separate prompt after inputing the text to be sounded. For this,
a new function 'defineClean' is used.

## 0.7.3.1 -- 2020-03-11

* Seventh version revised H. Fixed issue with deprecated information in the README.markdown file.

## 0.7.4.0 -- 2020-03-11

* Seventh version revised I. Fixed issue with being not compiled because of the syntactic mistake.

## 0.7.5.0 -- 2020-03-11

* Seventh version revised J. Fixed issue with being not compiled because of the syntactic mistake in the do-block. Simplified the 'main7h' function code being written. 

## 0.7.6.0 -- 2020-05-14

* Seventh version revised K. Changed bounds for dependencies so that now also GHC 8.10* series are supported.

## 0.7.7.0 -- 2020-06-24

* Seventh version revised J. Changed bounds for dependencies so that now it is consistent with new dobutokO packages. Some minor documentation improvements.

## 0.8.0.0 -- 2020-08-16

* Eighth version. Removed mmsyn7s from the dependencies, instead use a mmsyn6ukr version more than 0.8.0.0.

## 0.8.1.0 -- 2020-12-03

* Eighth version revised A. Fixed issues with being not compiled for some versions. Changed the dependency 
boundaries.
