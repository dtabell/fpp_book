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
logical, external :: is_yes

type(layout), pointer :: PSR
type(internal_state), target :: state
type(c_damap) :: id, one_turn_map
type(c_normal_form) :: normal_form
type(real_8), dimension(6) :: y
real(dp), dimension(6) :: closed_orbit
real(dp), dimension(6,6) :: matrix_part
real(dp), dimension(5) :: dval
real(dp), dimension(2,5) :: zco, nu
real(dp), dimension(2) :: chrom1, chrom2
real(dp), dimension(2) :: C100, C101, C102, C010
real(dp) :: circumference, pprec
integer :: i, j, ndeltas, mapOrder
character(len=8) :: answer
character(len=2) :: rs, wwo
logical(lp) :: useRB, sextON

!-----------------------------------
state = nocavity0
mapOrder = 3
pprec = 1.d-6  ! print precision

! values of \delta
ndeltas = 5
dval = zero
dval(1) =  0.d+0
dval(2) =  1.d-3
dval(3) = -1.d-3
dval(4) =  1.d-4
dval(5) = -1.d-4

call ptc_ini_no_append
call append_empty_layout(m_u)
PSR => m_u%start

! Construct the PSR with rectangular bends, or sector bends?
write(*, '(a)', advance='no') &
  "Use rectangular bends (instead of sector bends)?: "
read (*, '(a)') answer
if (is_yes(answer)) then
  useRB = .true.
  rs = "r"
  write(*, '(a)') "  Will use rectangular bends."
else
  useRB = .false.
  rs = "s"
  write(*, '(a)') "  Will use sector bends."
endif

! Turn on the sextupoles?
write(*, '(a)', advance='no') "Turn on the sextupoles?: "
read (*, '(a)') answer
if (is_yes(answer)) then
  sextON = .true.
  wwo = "w/"
  write(*, '(a)') "  Will turn on the sextupoles."
else
  sextON = .false.
  wwo = "no"
  write(*, '(a)') "  Will turn off the sextupoles."
endif
write(*,*) ""

call build_full_PSR(PSR, useRBends=useRB, sextupolesON=sextON)

call init_all(state, mapOrder, 0)
call alloc(id)
call alloc(one_turn_map)
call alloc(normal_form)
call alloc(y)

id = 1  ! identity map

closed_orbit(1:6) = zero
call find_orbit_x(PSR, closed_orbit, state, 1.e-9_dp, fibre1=1)
circumference = PSR%T%end%s(1)

! loop over designated values of the relative momentum deviation
do j = 1, ndeltas
  write(6,*) ""
  write(6,'(a,es8.2e1,a)') " ====================== delta = ", dval(j)," ======================"
  closed_orbit(5) = dval(j)
  ! compute the closed orbit
  call find_orbit_x(PSR, closed_orbit, state, 1.e-9_dp, fibre1=1)
  zco(:,j) = closed_orbit(1:2)
  write(6,'(a,2(2x,es14.7e2),2x,es8.2e1)') " closed orbit: ", closed_orbit(1:2), closed_orbit(5)
  ! compute the one-turn map
  y = closed_orbit + id
  call propagate(PSR, y, state, fibre1=1)
  one_turn_map = y  ! promote the six polymorphs to a (6-D) Taylor map
  ! convert to normal form
  call c_normal(one_turn_map, normal_form)
  nu(:,j) = normal_form%tune(1:2)
  if (j .eq. 1) then
    !call print(normal_form%n, 6, pprec)
    !call print(normal_form%n%v(1), 6, pprec)
    !call print(normal_form%n%v(3), 6, pprec)
    C100(1) = normal_form%n%v(1).sub."10000"
    C101(1) = normal_form%n%v(1).sub."10001"
    C102(1) = normal_form%n%v(1).sub."10002"
    C010(1) = normal_form%n%v(1).sub."01000"
    C100(2) = normal_form%n%v(3).sub."00100"
    C101(2) = normal_form%n%v(3).sub."00101"
    C102(2) = normal_form%n%v(3).sub."00102"
    C010(2) = normal_form%n%v(3).sub."00010"
  endif
enddo

!call print(one_turn_map, 6, pprec)

closed_orbit = y
matrix_part = one_turn_map

call kill(id)
call kill(one_turn_map)
call kill(normal_form)
call kill(y)

! =============== write out summary of results ===============
! header
write(6,*) ""
write(6,'(a)', advance='no') " ========================= "
write(6,'(a1,a,a2,a)', advance='no') rs, "bends, ", wwo, " sextupoles"
write(6,'(a)') " ========================="
write(6,'(4x,a,6x,a,11x,a,12x,a,11x,a)') "delta", "co_X / C", "co_Px", "nu_X", "nu_Y"
!
! report delta, closed-orbit, and tunes
do j = 1, 5
  write(6,'(2x,es8.2e1,2(2x,e15.8e2),2(2x,e13.7e1))') &
    dval(j), zco(1,j) / circumference, zco(2,j), nu(:,j)
enddo
!
! report numerically computed chromaticities
!   first-order
do i = 1, 2
  chrom1(i) = (nu(i,2) - nu(i,3)) / (dval(2) - dval(3))
enddo
!   second-order
do i = 1, 2
  chrom2(i) = (nu(i,2) + nu(i,3) - 2 * nu(i,1)) / (dval(2) ** 2)
enddo
write(6,'(a)') ""
write(6,'(a)') " chromaticities (computed numerically)"
write(6,'(a,1x,f8.4,2x,f8.4)') "   order 1:", chrom1(:)
write(6,'(a,1x,f6.2,4x,f6.2)') "   order 2:", chrom2(:)
!
! report chromaticities computed from the map
!   first-order
do i = 1, 2
  chrom1(i) = -C101(i) / C010(i) / twopi
enddo
!   second-order
do i = 1, 2
  chrom2(i) = -(C102(i) + 0.5 * C100(i) * (C101(i) / C010(i))**2) &
               / (pi * C010(i))
enddo
write(6,'(a)') ""
write(6,'(a)') " chromaticities (computed from the map)"
write(6,'(a,1x,f8.4,2x,f8.4)') "   order 1:", chrom1(:)
if (mapOrder > 2) then
  write(6,'(a,1x,f6.2,4x,f6.2)') "   order 2:", chrom2(:)
endif
write(6,'(a)') " =========================================================================="


call ptc_end
end program psr_chromaticity

