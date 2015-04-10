########################################################################
##
## ReadMe.txt : build instructions and other notes
##
## This project contains the code and examples for the book
##
## Etienne Forest
## From Tracking Code to Analysis
## Generalised Courant-Snyder theory for realistic accelerator models
## Springer, 2015.
##
## (c) E. Forest, 2015
##
########################################################################

 M A N I F E S T
-----------------

This project includes the following directories and files:

  ReadMe.txt      this ReadMe file

  fpp_ptc         directory for the FPP/PTC library source files
  book_examples   directory for all the example source files
  libug_examples  directory for additional example source files

  CMakeLists.txt  top-level file used by the CMake build system

  blank.txt       blank file (is this needed??)
  intro.txt       instructions that likely refer to the next two files
  make.bat          and are probably for
  terminal.lnk      building on Windows


 B U I L D   A N D   I N S T A L L
-----------------------------------

The build instructions given here have been tested only on
  Mac OS 10.9.5 and Red Hat Linux 4.4.7.
With any luck, these instructions will work on other systems.

To build the code for this project, you can use the cross-platform build
system CMake. This build system uses the CMakeLists.txt file found in
this directory and various sub-directories to control the build.

If you do not already have CMake installed, see the webpages at
  http://www.cmake.org/download/
  http://www.cmake.org/install/
In most cases, you should be able to use a pre-compiled binary.

With CMake installed, do the following at a command-line prompt:

  1. Navigate to the fpp_book project:
       $ cd _path_to_/fpp_book

  2. Create a build directory, and change into that dirctory:
       $ mkdir build
       $ cd build

  3. Configure the build:
       $ cmake ..

     The default location for installed binaries will be (assuming you
     have sufficient privileges) /usr/local/PTC/bin/; for installed
     libraries, it will be /usr/local/PTC/lib/. If you wish to modify
     this location, do so by setting the variable CMAKE_INSTALL_PREFIX
     on the cmake command line. Thus, for example, configure the build
     by executing
       $ cmake -DCMAKE_INSTALL_PREFIX=~/Projects/fpp_book ..
     to have the binaries installed in the directory
       ~/Projects/fpp_book/PTC/bin
     and correspondingly for libraries.

     By default, the libraries created will all be static libraries.
     To have shared libraries built, add
       -DBUILD_SHARED_LIBS=ON
     to the cmake command line.

  4. Build the libraries and executables:
       $ make

  5. (Optional) Install the libraries and executables:
       $ make install
     or, if necessary,
       $ sudo make install

     If you do not perform the installation, you will find the binaries
     in the directories
       _path_to_/fpp_book/build/book_examples/
       _path_to_/fpp_book/build/usrguide_examples/
     and the libraries in the directories
       _path_to_/fpp_book/build/fpp_ptc/
       _path_to_/fpp_book/build/book_examples/als/
       _path_to_/fpp_book/build/usrguide_examples/lattices/

