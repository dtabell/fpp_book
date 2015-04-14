#######################################################################
##
## CMakeLists.txt : lattice source files for user guide examples
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

set(USRGUIDE_LATTICE_FILES
  z_psr_lattice.f90
)

add_library(ptc_uglattices
  ${USRGUIDE_LATTICE_FILES}
)

target_link_libraries(ptc_uglattices ptc)

install(
  TARGETS ptc_uglattices LIBRARY
  DESTINATION PTC/lib
  ARCHIVE DESTINATION PTC/lib
)

