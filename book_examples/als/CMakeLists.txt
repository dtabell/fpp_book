#######################################################################
##
## CMakeLists.txt : lattice source files for PTC example executables
##
## This project contains the source code and examples for the book
##
## Etienne Forest
## From Tracking Code to Analysis
## Generalised Courant-Snyder theory for realistic accelerator models
## Springer, 2015.
##
## (c) E. Forest, 2015
##
#######################################################################

set(PTC_LATTICE_FILES
  z_als_lattice.f90
)

add_library(ptclattices
  ${PTC_LATTICE_FILES}
)

target_link_libraries(ptclattices ptc)

install(
  TARGETS ptclattices LIBRARY
  DESTINATION PTC/lib
  ARCHIVE DESTINATION PTC/lib
)

