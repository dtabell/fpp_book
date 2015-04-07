#######################################################################
##
## CMakeLists.txt : lattice source files for additional examples
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

set(USRGUIDE_LATTICE_FILES
  z_psr_lattice.f90
)

add_library(usrguide_lattices
  ${USRGUIDE_LATTICE_FILES}
)

target_link_libraries(usrguide_lattices ptc)

install(
  TARGETS usrguide_lattices LIBRARY
  DESTINATION PTC/lib
  ARCHIVE DESTINATION PTC/lib
)

