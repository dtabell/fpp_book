#######################################################################
##
## CMakeLists.txt : top level
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
#######################################################################

cmake_minimum_required (VERSION 2.8.12)
enable_language(Fortran)

project (fpp_book)

include_directories(
  ${CMAKE_CURRENT_BINARY_DIR}/fpp_ptc
)

add_subdirectory(fpp_ptc)
add_subdirectory(book_examples)

