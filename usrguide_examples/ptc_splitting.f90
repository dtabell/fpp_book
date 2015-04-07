program ptc_splitting
use run_madx
use pointer_lattice
implicit none

character*48 :: command_gino
type(layout), pointer :: r1, r2, r3, r4, r5, r6
type(layout), pointer :: psr1, psr2, fig8, col1, col2
type(fibre),pointer :: p, b, f, qf, qd, d1, d2
integer i,pos,j,mf,example
logical(lp) doneit
real(dp) a(3),d(3),x(6),b0,mis(6),thin
type(real_8) y1(6),y2(6)
real(dp) fix1(6),fix2(6)
type(normalform) n1,n2
type(damap) id
type(internal_state) state
type(taylor) eq(4)
type(gmap) g
integer method,limits(2)
logical exact
!-----------------------------------

lmax=100.d0
use_info=.true.

!== user stuff : one layout necessary before starting GUI
call ptc_ini_no_append

use_info=.true.

!!!!!!!!! producing the dna !!!!!!!!!!!

call append_empty_layout(m_u) ! number 1
call set_up(m_u%end)
r1 => m_u%end

exact = .false.
method = drift_kick_drift
call build_PSR_xTF(r1, exact, method)

call move_to(r1, qf, "qf", pos)
call move_to(r1, qd, "qd", pos)
call move_to(r1, b,  "b",  pos)

!!!! first respliting !!!!! example 1
thin = 0.01d0
limits(1:2) = 100000

call thin_lens_resplit(r1, thin, lim = limits)
write(6,*) qf%mag%name, qf%mag%p%method, qf%mag%p%nst
write(6,*) qd%mag%name, qd%mag%p%method, qd%mag%p%nst
write(6,*) b%mag%name,  b%mag%p%method,  b%mag%p%nst

pause 1
!!!! second respliting !!!!! !!!!! example 2
thin = 0.01d0
limits(1) = 8
limits(2) = 24

call thin_lens_resplit(r1, thin, lim = limits)
write(6,*) qf%mag%name, qf%mag%p%method, qf%mag%p%nst
write(6,*) qd%mag%name, qd%mag%p%method, qd%mag%p%nst
write(6,*) b%mag%name,  b%mag%p%method,  b%mag%p%nst

pause 2

!!!! Talman algorithm !!!!! !!!!! example 3
write(6,*) " "
write(6,*) "!!!! Talman algorithm !!!!! !!!!! example 3"
write(6,*) " "

thin = 0.001d0 ! cut like crazy
call thin_lens_resplit(r1, thin, lim = limits)
!!! ptc command file: could be a mad-x command or whatever
call read_ptc_command77("fill_beta0.txt") ! computing tune and beta around the ring

write(6,*) " "
write(6,*) " now reducing the number of steps and refitting "
write(6,*) " "
do i = 0, 2
  thin = 0.01d0 + i * 0.03
  call thin_lens_resplit(r1, thin, lim = limits) ! reducing number of cuts
  call read_ptc_command77("fit_to_beta0_results.txt") ! fitting to previous tunes
  call read_ptc_command77("fill_beta1.txt") ! computing dbeta/beta around the ring
end do

pause 3

write(6,*) " "
write(6,*) "!!!! sbend orbit small problem !!!!! !!!!! example 4"
write(6,*) " "

call append_empty_layout(m_u) ! number 2
call set_up(m_u%end)
r1 => m_u%end

exact = .true.
method = drift_kick_drift
call build_PSR_xTF(r1, exact, method)

write(6,*) " "
write(6,*) " now reducing the number of steps and refitting "
write(6,*) " "
do i = 0, 2
  thin = 0.01d0 + i * 0.03
  call thin_lens_resplit(r1, thin) ! reducing number of cuts
  call read_ptc_command77("fit_to_beta0_results_2.txt") ! fitting to previous tunes
  call read_ptc_command77("fill_beta1_2.txt") ! computing dbeta/beta around the ring
end do

pause 4
write(6,*) " "
write(6,*) "!!!! sbend orbit small problem !!!!! !!!!! example 5"
write(6,*) " "
write(6,*) " "
write(6,*) " now reducing the number of steps and refitting with xbend=1.d-4 "
write(6,*) " "
do i=0,2
  thin = 0.01d0+i*0.03
  call thin_lens_resplit(r1, thin, xbend = 1.d-4) ! reducing number of cuts
  call read_ptc_command77("fit_to_beta0_results_2.txt") ! fitting to previous tunes
  call read_ptc_command77("fill_beta1_2.txt") ! computing dbeta/beta around the ring
end do
pause 5

write(6,*) "!!!! even !!!!! !!!!! example 6"
call move_to(r1, d1, "d1", pos)
call move_to(r1, d2, "d2", pos)
call move_to(r1, qf, "qf", pos)
call move_to(r1, qd, "qd", pos)
call move_to(r1, b,  "b",  pos)

call thin_lens_restart(r1) ! puts back method =2 and nst=1 everywhere
thin = 0.01d0
call thin_lens_resplit(r1, thin, even = .true., xbend = 1.d-4) ! reducing number of cuts
call make_node_layout(r1)
write(6,*) qf%mag%name, qf%mag%p%method, qf%mag%p%nst, qf%t1%pos, qf%tm%pos, qf%t2%pos
write(6,*) qd%mag%name, qd%mag%p%method, qd%mag%p%nst, qd%t1%pos, qd%tm%pos, qd%t2%pos
write(6,*) b%mag%name,  b%mag%p%method,  b%mag%p%nst,  b%t1%pos,  b%tm%pos,  b%t2%pos

call thin_lens_restart(r1) ! puts back method =2 and nst=1 everywhere
thin = 0.01d0
call thin_lens_resplit(r1, thin, lim = limits, xbend = 1.d-4) ! reducing number of cuts
call make_node_layout(r1)
write(6,*) qf%mag%name, qf%mag%p%method, qf%mag%p%nst, qf%t1%pos, qf%tm%pos, qf%t2%pos
write(6,*) qd%mag%name, qd%mag%p%method, qd%mag%p%nst, qd%t1%pos, qd%tm%pos, qd%t2%pos
write(6,*) b%mag%name,  b%mag%p%method,  b%mag%p%nst,  b%t1%pos,  b%tm%pos,  b%t2%pos

write(6,*) "!!!! lmax0 keyword !!!!! !!!!! example 7"
resplit_cutting = 1
call thin_lens_restart(r1) ! puts back method =2 and nst=1 everywhere
thin = 0.01d0
call thin_lens_resplit(r1, thin, even = .true., lmax0 = 0.05d0, xbend = 1.d-4) ! reducing number of cuts
call make_node_layout(r1)
write(6,'(a8,2x,5(i4,8x))') d1%mag%name(1:8), d1%mag%p%method, d1%mag%p%nst, d1%t1%pos, d1%tm%pos, qf%t2%pos
write(6,'(a8,2x,5(i4,8x))') d2%mag%name(1:8), d2%mag%p%method, d2%mag%p%nst, d2%t1%pos, d2%tm%pos, d2%t2%pos
write(6,'(a8,2x,5(i4,8x))') qf%mag%name(1:8), qf%mag%p%method, qf%mag%p%nst, qf%t1%pos, qf%tm%pos, qf%t2%pos
write(6,'(a8,2x,5(i4,8x))') qd%mag%name(1:8), qd%mag%p%method, qd%mag%p%nst, qd%t1%pos, qd%tm%pos, qd%t2%pos
write(6,'(a8,2x,5(i4,8x))') b%mag%name(1:8),  b%mag%p%method,  b%mag%p%nst,  b%t1%pos,  b%tm%pos,  b%t2%pos
resplit_cutting = 2
call thin_lens_restart(r1) ! puts back method =2 and nst=1 everywhere
thin = 0.01d0
call thin_lens_resplit(r1, thin, even = .true., lmax0 = 0.05d0, xbend = 1.d-4) ! reducing number of cuts
call make_node_layout(r1)
write(6,'(a8,2x,5(i4,8x))') d1%mag%name(1:8), d1%mag%p%method, d1%mag%p%nst, d1%t1%pos, d1%tm%pos, qf%t2%pos
write(6,'(a8,2x,5(i4,8x))') d2%mag%name(1:8), d2%mag%p%method, d2%mag%p%nst, d2%t1%pos, d2%tm%pos, d2%t2%pos
write(6,'(a8,2x,5(i4,8x))') qf%mag%name(1:8), qf%mag%p%method, qf%mag%p%nst, qf%t1%pos, qf%tm%pos, qf%t2%pos
write(6,'(a8,2x,5(i4,8x))') qd%mag%name(1:8), qd%mag%p%method, qd%mag%p%nst, qd%t1%pos, qd%tm%pos, qd%t2%pos
write(6,'(a8,2x,5(i4,8x))') b%mag%name(1:8),  b%mag%p%method,  b%mag%p%nst,  b%t1%pos,  b%tm%pos,  b%t2%pos

999 command_gino = "opengino"
call context(command_gino)  ! context makes them capital
call call_gino(command_gino)
111 command_gino = "mini"
call context(command_gino)  ! context makes them capital
call call_gino(command_gino)

!!!!!!!!! vaguelynecessary baloney
command_gino = "closegino"
call call_gino(command_gino)

call ptc_end
end program ptc_splitting

