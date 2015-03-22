#######################################################################
##
## CMakeLists.txt : PTC executables
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

set(PTC_PROGRAMS
  hamiltonian_guignard
  hamiltonian_guignard_1df
  hamiltonian_guignard_1df_x
  hamiltonian_guignard_cs
  modulated_map
  modulated_map_jordan
  one_resonance_map
  one_turn_cavity_map
  one_turn_orbital_map
  one_turn_orbital_map_normal_form_2d
  one_turn_orbital_map_phase_ad
  pendulum
  radiation_map
  spin_phase_advance_isf
  standard_map
  very_damped_map
)

foreach (p ${PTC_PROGRAMS})
  add_executable(${p} "${p}.f90")
  target_link_libraries(${p} ptc ptclattices)
endforeach (p)

add_subdirectory(als)

install(
  TARGETS ${PTC_PROGRAMS}
  DESTINATION PTC/bin
)

