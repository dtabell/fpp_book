subroutine  build_PSR(PSR)
use madx_ptc_module
use pointer_lattice
implicit none

type(layout), target :: PSR

real(dp) :: ang, brho, kd, kf, Larc
type(fibre) :: b, d1, d2, qd, qf
type(layout) :: cell
!-----------------------------------

call make_states(.false.)
exact_model = .true.
!default = default + nocavity + exactmis
default = default + nocavity
call update_states
madlength = .false.

ang = (twopi * 36.d0 / 360.d0)
Larc = 2.54948d0
brho = 1.2d0 * (Larc / ang)
call set_mad(brho = brho, method = 2, step = 10)
madkind2 = drift_kick_drift

kf =  2.72d0 / brho
kd = -1.92d0 / brho

d1 = drift("D1", 2.28646d0)
d2 = drift("D2", 0.45d0)
qf = quadrupole("QF", 0.5d0, kf)
qd = quadrupole("QD", 0.5d0, kd)
b  = rbend("B", Larc, ang)
cell = d1 + qd + d2 + b + d2 + qf + d1

PSR = 10 * cell
PSR = .ring.PSR

call survey(PSR)
end subroutine build_PSR


!=================================================================
! Build the *full* PSR lattice as described in the paper "Exact
! numerical calculation of chromaticity in small rings" by Alex J.
! Dragt: Part. Accel. 12:205--218 (1982).
!
subroutine  build_full_PSR(PSR, useRBends, sextupolesON)
use madx_ptc_module
use pointer_lattice
implicit none

type(layout), target :: PSR
logical(lp), intent(in), optional :: useRBends
logical(lp), intent(in), optional :: sextupolesON

logical(lp) :: useRB, sextON
real(dp) :: ang, B0, brho, Larc
real(dp) :: kqd, kqf, ksd, ksf
type(fibre) :: b, d1, d2, d3, d4, qd, qf, sd, sf
type(layout) :: cell0, cellT, cellL
!-----------------------------------

useRB = .false.
if (present(useRBends)) then
  useRB = useRBends
endif

sextON = .false.
if (present(sextupolesON)) then
  sextON = sextupolesON
endif

call make_states(.false.)
exact_model = .true.
!default = default + nocavity + exactmis
default = default + nocavity
call update_states
madlength = .false.

ang = (twopi * 36.d0 / 360.d0)
Larc = 2.54948d0
B0 = 1.2d0
brho = B0 * (Larc / ang)
call set_mad(brho = brho, method = 2, step = 10)
madkind2 = drift_kick_drift

if (useRB) then
  kqd = -1.92d0 / brho
  kqf =  2.72d0 / brho
  ksd = -3.50d0 / brho
  ksf =  1.70d0 / brho
else
  kqd = -2.68d0 / brho
  kqf =  1.95d0 / brho
  ksd = -2.40d0 / brho
  ksf =  1.60d0 / brho
endif

d1 = drift("D1", 2.28646d0)
d2 = drift("D2", 0.45d0)
d3 = drift("D3", 1.48646d0)
d4 = drift("D4", 0.30d0)
qf = quadrupole("QF", 0.5d0, kqf)
qd = quadrupole("QD", 0.5d0, kqd)
sf = quadrupole("SF", 0.5d0, ksf)
sd = quadrupole("SD", 0.5d0, ksd)
if (useRB) then
  b  = rbend("B", Larc, ang)
else
  b  = sbend("B", Larc, ang)
endif

! lattice period with no sextupole
cell0 = d1 + qd + d2 + b + d2 + qf + d1
! lattice period with a trailing sextupole
cellT = d1 + qd + d2 + b + d2 + qf + d4 + sf + d3
! lattice period with a leading sextupole
cellL = d3 + sd + d4 + qd + d2 + b + d2 + qf + d1

if (sextON) then
  PSR = cell0 + cellT + cellL + 3 * cell0 + cellT + cellL  + 2 * cell0
else
  PSR = 10 * cell0
endif
PSR = .ring.PSR

call survey(PSR)
end subroutine build_full_PSR


!=================================================================

subroutine  build_PSR_minus(PSR)
use madx_ptc_module
use pointer_lattice
implicit none

type(layout), target :: PSR

real(dp) :: ang, brho, kd, kf, Larc
type(fibre) :: b, d1, d2, qd, qf
type(layout) :: cell
!-----------------------------------

call make_states(.false.)
exact_model = .true.
!default = default + nocavity + exactmis
default = default + nocavity
call update_states
madlength = .false.

ang = (twopi * 36.d0 / 360.d0)
Larc = 2.54948d0
brho = 1.2d0 * (Larc / ang)
call set_mad(brho = brho, method = 6, step = 10)
madkind2 = drift_kick_drift

kf =  2.72d0 / brho
kd = -1.92d0 / brho

d1 = drift("D1", 2.28646d0)
d2 = drift("D2", 0.45d0)
qf = quadrupole("QF", 0.5d0, kf)
qd = quadrupole("QD", 0.5d0, kd)
b  = rbend("B", Larc, ang)
cell = d1 + qd + d2 + b + d2 + qf + d1

PSR = b + d2 + qf + d1 + 8 * cell + d1 + qd + d2 + b
PSR = .ring.PSR

call survey(PSR)
end subroutine build_PSR_minus


!=================================================================

subroutine  build_Quad_for_Bend(PSR)
use madx_ptc_module
use pointer_lattice
implicit none

type(layout),target :: PSR

real(dp) :: ang, ang2, brho, b1, Larc, Lq
type(fibre) :: b
!-----------------------------------

call make_states(.false.)
exact_model = .true.
!default = default + nocavity + exactmis
default = default + nocavity
call update_states
madlength = .false.

ang = (twopi * 36.d0 / 360.d0)
Larc = 2.54948d0
brho = 1.2d0 * (Larc / ang)
call set_mad(brho = brho, method = 6, step = 10)
madkind2 = drift_kick_drift

ang2 = ang / two
b1 = ang / Larc
Lq = Larc * sin(ang2) / ang2

b = quadrupole("B_QUAD", Lq, 0.d0);
call add(b, 1, 0, b1)
!b%mag%permfringe = .true.
!b%magp%permfringe = .true.
!b%mag%p%bend_fringe = .true.
!b%magp%p%bend_fringe = .true.

PSR = 10 * b
PSR = .ring.PSR

call survey(PSR)
end subroutine build_Quad_for_Bend


!=================================================================

subroutine build_PSR_xTF(PSR, exactTF, imethod)
use run_madx
use pointer_lattice
implicit none

type(layout), target :: PSR
logical(lp) :: exactTF
integer :: imethod

real(dp) :: ang, brho, kd, kf, larc, lq
type(fibre) :: b, d1, d2, qd, qf
type(layout) :: cell
!-----------------------------------

call make_states(.false.)
!default = default + nocavity + exactmis
default = default + nocavity
call update_states
madlength = .false.

exact_model = exactTF

ang = (twopi * 36.d0 / 360.d0)
larc = 2.54948d0
brho = 1.2d0 * (larc / ang)
call set_mad(brho = brho, method = 6, step = 100)
madkind2 = imethod

kf =  2.72d0 / brho
kd = -1.92d0 / brho
lq = 0.5d0
write(6,'(a)') "kf * lq, kd * lq :"
write(6,*) kf * lq, kd * lq

d1 = drift("D1", 2.28646d0)
d2 = drift("D2", 0.45d0)
qf = quadrupole("QF", lq, kf)
qd = quadrupole("QD", lq, kd)
!b = rbend("B", larc, ang)
b = sbend("B", larc, ang)
cell = d1 + qd + d2 + b + d2 + qf + d1

PSR = 10 * cell
PSR = .ring.PSR

call survey(PSR)
call clean_up
end subroutine build_PSR_xTF
