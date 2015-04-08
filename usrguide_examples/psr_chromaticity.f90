program psr_chromaticity
use madx_ptc_module
use pointer_lattice
use c_tpsa
implicit none

interface
subroutine build_full_PSR(PSR, useRBends, sextupolesON)
  use madx_ptc_module
  use pointer_lattice
  implicit none
  type(layout), target :: PSR
  logical(lp), intent(in), optional :: useRBends
  logical(lp), intent(in), optional :: sextupolesON
end subroutine build_full_PSR
end interface

type(layout), pointer :: PSR
type(internal_state), target :: state
type(c_damap) :: id, one_turn_map
type(real_8), dimension(6) :: y
real(dp), dimension(6) :: closed_orbit
real(dp), dimension(6,6) :: matrix_part
real(dp) :: relpdev, pprec
integer :: i, mapOrder

!-----------------------------------
state = nocavity0
mapOrder = 2
pprec = 1.d-6  ! print precision

call ptc_ini_no_append
call append_empty_layout(m_u)
PSR => m_u%start

call build_full_PSR(PSR, useRBends=.false., sextupolesON=.false.)

call init_all(state, mapOrder, 0)
call alloc(id)
call alloc(one_turn_map)
call alloc(y)

relpdev = 0.0
closed_orbit(5) = relpdev
call find_orbit_x(PSR, closed_orbit, state, 1.e-5_dp, fibre1=1)

write(6,*) " closed orbit at delta = ", relpdev
write(6,*) closed_orbit(1:6)

id = 1  ! identity map
write(6,*) " id%v(1) "
call print(id%v(1),6)

y = closed_orbit + id
write(6,*) " y(1) = closed_orbit(1) + id%v(1) "
call print(y(1), 6)
call propagate(PSR, y, state, fibre1=1)

one_turn_map = y  ! promote the six polymorphs to Taylor maps

call print(one_turn_map, 6, pprec)

closed_orbit = y
matrix_part = one_turn_map

write(6,*) " ";
write(6,*) " === constant and linear parts of the map ===";
write(6,'(a16,6(1x,g12.5))') " closed orbit = ", closed_orbit(1:6)
do i = 1, 6
  write(6,'(a5,i1,a6,6(1x,g12.5))') " row(",i,") --> ", matrix_part(i, 1:6)
enddo

call kill(id)
call kill(one_turn_map)
call kill(y)

call ptc_end
end program psr_chromaticity

