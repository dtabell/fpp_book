#######################################################################
##
## CMakeLists.txt : fpp_ptc
##
## Source files for FPP/PTC, including complex FPP.
##
## Etienne Forest
## From Tracking Code to Analysis
## Generalised Courant-Snyder theory for realistic accelerator models
## Springer, 2015.
##
## (c) E. Forest, 2015
##
#######################################################################

set(PTC_INCLUDE_FILES
  a_def_all_kind.inc
  a_def_element_fibre_layout.inc
  a_def_frame_patch_chart.inc
  a_def_sagan.inc
  a_def_worm.inc
  a_namelists.inc
)

set(FPP_LIB_FILES
  a_scratch_size.f90
  b_da_arrays_all.f90
  c_dabnew.f90
  d_lielib.f90
  h_definition.f90
  i_tpsa.f90
  j_tpsalie.f90
  k_tpsalie_analysis.f90
  l_complex_taylor.f90
  m_real_polymorph.f90
  n_complex_polymorph.f90
  o_tree_element.f90
)

# To ensure proper dependency checking, the source files
# in complex/. need to be visible at this level.
set(COMPLEX_FPP_LIB_FILES
  complex/cb_da_arrays_all.f90
  complex/cc_dabnew.f90
  complex/Ci_tpsa.f90
)

set(PTC_LIB_FILES
  Sa_extend_poly.f90
  Sb_sagan_pol_arbitrary.f90
  Sc_euclidean.f90
  Sd_frame.f90
  Se_status.f90
  Sf_def_all_kinds.f90
  Sg_sagan_wiggler.f90
  Sh_def_kind.f90
  Si_def_element.f90
  Sk_link_list.f90
  Sl_family.f90
  Sm_tracking.f90
  Sma0_beam_beam_ptc.f90
  Sma_multiparticle.f90
  Sn_mad_like.f90
  So_fitting.f90
  Sp_keywords.f90
  Spb_fake_gino_sub.f90
  Sq_orbit_ptc.f90
  Sr_spin.f90
  Sra_fitting.f90
  Ss_fake_mad.f90
  St_pointers.f90
)

add_library(ptc SHARED
  ${PTC_INCLUDE_FILES}
  ${PTC_LIB_FILES}
  ${FPP_LIB_FILES}
  ${COMPLEX_FPP_LIB_FILES}
  zzy_run_madx.f90
)

