#######################################################################
##
## CMakeLists.txt : PTC executables for user guide examples
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

set(USRGUIDE_PROGRAMS
  psr_chromaticity
  psr_tracking
  ptc_geometry
  ptc_splitting
)

foreach (p ${USRGUIDE_PROGRAMS})
  add_executable(${p} "${p}.f90")
  target_link_libraries(${p} ptc ptc_uglattices ptc_ugutils)
endforeach (p)

add_subdirectory(lattices)
add_subdirectory(utilities)

install(
  TARGETS ${USRGUIDE_PROGRAMS}
  DESTINATION PTC/bin
)

