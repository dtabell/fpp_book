#######################################################################
##
## CMakeLists.txt : PTC executables for additional examples
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

set(USRGUIDE_PROGRAMS
  psr_chromaticity
  ptc_geometry
  ptc_splitting
)

foreach (p ${USRGUIDE_PROGRAMS})
  add_executable(${p} "${p}.f90")
  target_link_libraries(${p} ptc usrguide_lattices)
endforeach (p)

add_subdirectory(lattices)

install(
  TARGETS ${USRGUIDE_PROGRAMS}
  DESTINATION PTC/bin
)

