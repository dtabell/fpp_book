#######################################################################
##
## CMakeLists.txt : utility source files for user guide examples
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

set(USRGUIDE_UTILITY_FILES
  strings.f90
)

add_library(ptc_ugutils
  ${USRGUIDE_UTILITY_FILES}
)

#target_link_libraries(ptc_ugutils ptc)

install(
  TARGETS ptc_ugutils LIBRARY
  DESTINATION PTC/lib
  ARCHIVE DESTINATION PTC/lib
)

