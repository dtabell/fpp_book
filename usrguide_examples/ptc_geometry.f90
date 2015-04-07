program ptc_geometry
use madx_ptc_module
use pointer_lattice
implicit none

character*48 :: command_gino
logical(lp) :: doit
integer :: i, j, mf, pos, example
real(dp) :: b0
real(dp), dimension(3) :: a, d
real(dp), dimension(6) :: fix1, fix2, mis, x
type(real_8), dimension(6) :: y1, y2
type(layout), pointer :: L1, L2, L3, L4, L5, L6
type(layout), pointer :: PSR1, PSR2, Fig8, Col1, Col2
type(fibre), pointer :: p1, p2, p3, pf, b, f
type(internal_state) :: state

type(pol_block) :: qf(2), qd(2)
type(normalform) :: n1, n2
type(damap) :: id
type(taylor) :: eq(4)
type(gmap) :: g
!-----------------------------------

Lmax = 100.d0
use_info = .true.

! one layout necessary before starting GUI
call ptc_ini_no_append

!thin=-1
!call thin_lens_resplit(m_u%end, thin, xbend = 1.d-10)

!==========================!
!== set up DNA sequences ==!
!==========================!
call append_empty_layout(m_u) ! DNA sequence 1
call set_up(m_u%end)
L1 => m_u%end
call build_PSR(L1)

call append_empty_layout(m_u) ! DNA sequence 2
call set_up(m_u%end)
L2 => m_u%end
call build_Quad_for_Bend(L2)

call append_empty_layout(m_u) ! DNA sequence 3
call set_up(m_u%end)
L3 => m_u%end
call build_PSR_minus(L3)

call append_empty_layout(m_u) ! DNA sequence 4
call set_up(m_u%end)
L4 => m_u%end
call build_PSR(L4)

call append_empty_layout(m_u) ! DNA sequence 5
call set_up(m_u%end)
L5 => m_u%end
call build_PSR_minus(L5)

call append_empty_layout(m_u) ! DNA sequence 6
call set_up(m_u%end)
L6 => m_u%end
call build_PSR(L6)


!================================!
!== create "trackable" layouts ==!
!================================!

!== PSR1 : forward ring  (layout 7)
call append_empty_layout(m_u)
PSR1 => m_u%end

p1 => L1%start
p2 => L2%start
do i = 1, L1%n
  if(p1%mag%name == "B") then
    ! read bends from L2
    call append_point(PSR1, p2)
    f => PSR1%end
    d = p1%chart%f%o - f%chart%f%o
    call translate(f, d)
    call compute_entrance_angle(f%chart%f%mid, p1%chart%f%mid, a)
    call rotate(f, f%chart%f%o, a, basis = f%chart%f%mid)
    p2 => p2%next
  else
    call append_point(PSR1, p1)
  end if
  p1 => p1%next
end do ! elements in PSR1 now in correct locations

f => PSR1%start
do i = 1, PSR1%n
  if(f%mag%name == "B_QUAD") then
    call find_patch(f%previous, f, next = .true.)
    call find_patch(f, f%next, next = .false.)
  end if
  f => f%next
end do ! PSR1 now patched

PSR1%name = "PSR 1"
PSR1%closed = .true.
call ring_L(PSR1, .true.) ! make it a ring topologically


!== PSR2 : backward ring (layout 8)
call append_empty_layout(m_u)
PSR2 => m_u%end

p1 => L1%end
p2 => L2%end
do i = 1, L1%n
  if(p1%mag%name == "B") then
    call append_point(PSR2, p2)
    p2 => p2%previous
  else
    call append_point(PSR2, p1)
  end if
  f => PSR2%end
  f%dir = -1
  f%charge = -1
  p1 => p1%previous
end do

f => PSR2%start
do i = 1, PSR2%n
  if(f%mag%name == "B_QUAD") then
    call find_patch(f%previous, f, next = .true.)
    call find_patch(f, f%next, next = .false.)
  end if
  f => f%next
end do

PSR2%name = "PSR 2"
PSR2%closed = .true.
call ring_l(PSR2, .true.) ! make it a ring topologically


!== Fig8 : figure-eight lattice (layout 9)
d = zero
d(3) = -40.d0
call translate(L4, d)
a = zero
a(2) = pi
call rotate(L3, L3%start%chart%f%a, a)
call move_to(L4, p1, "B", pos)
d = p1%chart%f%a - L3%end%chart%f%b
call translate(L3, d)

call append_empty_layout(m_u)
Fig8 => m_u%end
p1 => L4%start
do i = 1, L4%n
  call append_point(Fig8, p1)
  p1 => p1%next
end do

write(6,*) p1%mag%name
call append_point(Fig8, p1)
p1 => p1%next
write(6,*) p1%mag%name
call append_point(Fig8, p1)
p1 => p1%next
write(6,*) p1%mag%name
call append_point(Fig8, p1)

p1 => L3%end
do i = 1, L3%n
  call append_point(Fig8, p1)
  Fig8%end%dir = -1
  if(p1%mag%name == "B") p1%mag%bn(1) = -p1%mag%bn(1)
  p1 => p1%previous
end do

p1 => L4%end%previous%previous
write(6,*) p1%mag%name
call append_point(Fig8, p1)
p1 => p1%next
write(6,*) p1%mag%name
call append_point(Fig8, p1)
p1 => p1%next
write(6,*) p1%mag%name
call append_point(Fig8, p1)

write(6,*) "Fig8 has ", Fig8%n, " fibres"
Fig8%name = "Figure-Eight"
Fig8%closed = .true.
call ring_l(Fig8, .true.) ! make it topologically closed

p1 => Fig8%start
do i = 1, Fig8%n
  call check_need_patch(p1, p1%next, 1.d-10, pos)
  if(pos /= 0) call find_patch(p1, p1%next, next = .false.)
  p1 => p1%next
end do


!== Col1 : lower collider ring (layout 10)
!== Col2 : upper collider ring (layout 11)
d = zero
d(3) = 40.d0
call translate(L6, d)
a = zero
a(2) = pi
call rotate(L5, L5%start%chart%f%a, a)
call move_to(L6, p1, "B", pos)
d = p1%chart%f%a - L5%end%chart%f%b
call translate(L5, d)

call append_empty_layout(m_u)
Col1 => m_u%end
p1 => L6%start
do i = 1, L6%n
  call append_point(Col1, p1)
  p1 => p1%next
end do

write(6,*) "Collider 1 has ", Col1%n, " fibres"
Col1%name = "Collider 1"
Col1%closed = .true.
call ring_l(Col1, .true.) ! make it a ring topologically

call append_empty_layout(m_u)
Col2 => m_u%end
p1 => L6%start%next%next
do i = 1, 6
  write(6,*) p1%mag%name
  call append_point(Col2, p1)
  Col2%end%dir = -1
  p1 => p1%previous
end do
p1 => L5%start
do i = 1, L3%n
  call append_point(Col2, p1)
  p1 => p1%next
end do

write(6,*) "Collider 2 has ", Col2%n, " fibres"
Col2%name = "Collider 2"
Col2%closed = .true.
call ring_l(Col2, .true.) ! make it a ring topologically

p1 => Col2%start
do i = 1, Col2%n
  call check_need_patch(p1, p1%next, 1.d-10, pos)
  if(pos /= 0) call find_patch(p1, p1%next, next = .false.)
  p1 => p1%next
end do


!=======================!
!== set up DNA arrays ==!
!=======================!

allocate(PSR1%DNA(2))
PSR1%DNA(1)%L => L1
do i = 2, 2
  PSR1%DNA(i)%L => PSR1%DNA(i-1)%L%next ! L2
end do

allocate(PSR2%DNA(2))
PSR2%DNA(1)%L => L1
do i = 2, 2
  PSR2%DNA(i)%L => PSR2%DNA(i-1)%L%next ! L2
end do

allocate(Fig8%DNA(2))
Fig8%DNA(1)%L => L3
do i = 2, 2
  Fig8%DNA(i)%L => Fig8%DNA(i-1)%L%next ! L4
end do

allocate(Col1%DNA(2))
Col1%DNA(1)%L => L5
do i = 2, 2
  Col1%DNA(i)%L => Col1%DNA(i-1)%L%next ! L6
end do

allocate(Col2%DNA(2))
Col2%DNA(1)%L => L5
do i = 2, 2
  Col2%DNA(i)%L => Col2%DNA(i-1)%L%next ! L6
end do


!== maps with polymorphs
!== simple fit

qf(1) = 0
qf(1)%name = "qf"
qf(1)%ibn(2) = 1
qf(2) = 0
qf(2)%name = "qf"
qf(2)%ibn(2) = 3
qd(1) = 0
qd(1)%name = "qd"
qd(1)%ibn(2) = 2
qd(2) = 0
qd(2)%name = "qd"
qd(2)%ibn(2) = 4
Col1%dna(1)%L = qf(1)
Col1%dna(1)%L = qd(1)
CoL1%dna(2)%L = qf(2)
CoL1%dna(2)%L = qd(2)

101 continue
state = default0 + only_4d0

fix1 = 0.d0
fix2 = 0.d0;
call init(state, 2, c_%np_pol) ! c_%np_pol is automatically computed
call find_orbit(CoL1, fix1, 1, state, 1.d-6)
call find_orbit(Col2, fix2, 1, state, 1.d-6)
call alloc(y1)
call alloc(y2)
call alloc(id)
call alloc(n1)
call alloc(n2)
call alloc(eq);
id=1 ! identity damap
y1 = id + fix1 ! this is permitted in ptc only (not fpp)
y2 = id + fix2 ! closed orbit added to map
call track(Col1, y1, 1, +state) ! unary + activates knobs
call track(Col2, y2, 1, +state)
n1 = y1 ! normal forms: abused of language permitted by ptc
n2 = y2 ! normally one should do => damap=y; normalform=damap
write(6,*) " tunes 1 "
write(6,*) n1%tune(1:2)
write(6,*) " tunes 2 "
write(6,*) n2%tune(1:2)
eq(1) = n1%dhdj%v(1) - 0.254d0
eq(2) = n1%dhdj%v(2) - 0.255d0
eq(3) = n2%dhdj%v(1) - 0.130d0
eq(4) = n2%dhdj%v(2) - 0.360d0
do i = 1, 4
  eq(i) = eq(i) <= c_%npara
end do

call kanalnummer(mf,"eq.txt")
do i=1,4
  call daprint(eq(i), mf)
end do
close(mf)

call kill(y1)
call kill(y2)
call kill(id)
call kill(n1)
call kill(n2)
call kill(eq)
call init(1,4)
call alloc(g,4)
call kanalnummer(mf,"eq.txt")
do i = 1, 4
  call read(g%v(i), mf)
end do
close(mf)

g = g.oo.(-1)
tpsafit(1:4) = g
set_tpsafit = .true.
set_element = .true.
Col1%dna(1)%L = qf(1)
Col1%dna(1)%L = qd(1)
Col1%dna(2)%L = qf(2)
Col1%dna(2)%L = qd(2)
set_tpsafit = .false.
set_element = .false.
call kill(g)
write(6,*) " more "
read(5,*) i
if(i == 1) goto 101
call kill_para(Col1%dna(1)%l)
call kill_para(Col1%dna(2)%l)


!===============================!
!== set up Siamese and Girder ==!
!===============================!

call move_to(Col1, p1, 67)
call move_to(Col2, p2, 7)
p1%mag%siamese => p2%mag
p2%mag%siamese => p1%mag
call move_to(Col1, p1, 4)
call move_to(Col2, p2, 70)
p1%mag%siamese => p2%mag
p2%mag%siamese => p1%mag

call move_to(Col1, p1, 68)
pf => p1  ! remember start of girder linked-list
do i = 2, 7
  p2 => p1%next
  p1%mag%girders => p2%mag
  p1 => p1%next
end do
call move_to(Col2, p2,  7)
p1%mag%girders => p1%mag%siamese
p1%mag%siamese%girders => p2%mag
p2%mag%girders => p2%mag%siamese
call move_to(Col1, p1, 67)
call move_to(Col2, p2, 14)
p1%mag%girders => p2%mag
p2%mag%girders => pf%mag

call move_to(Col1, p1, 1)
call alloc_af(p1%mag%girder_frame, girder = .true.)
p1%mag%girder_frame%ent = p1%mag%parent_fibre%chart%f%ent
p1%mag%girder_frame%a   = p1%mag%parent_fibre%chart%f%a
p1%mag%girder_frame%exi = p1%mag%parent_fibre%chart%f%ent
p1%mag%girder_frame%b   = p1%mag%parent_fibre%chart%f%a

a = p1%mag%girder_frame%a
a(3) = a(3) - 5.d0
call move_to(Col1, p3, 67)
call alloc_af(p3%mag%siamese_frame)
call find_patch(p3%mag%p%f%a, p3%mag%p%f%ent, &
                a, p1%mag%girder_frame%ent, &
                p3%mag%siamese_frame%d, p3%mag%siamese_frame%angle)
a = p1%mag%girder_frame%a
a(3) = a(3) + 5.d0
call move_to(Col1, p3, 4)
call alloc_af(p3%mag%siamese_frame)
call find_patch(p3%mag%p%f%a, p3%mag%p%f%ent, &
                a, p1%mag%girder_frame%ent, &
                p3%mag%siamese_frame%d, p3%mag%siamese_frame%angle)


!===========================!
!== example misalignments ==!
!===========================!

write(6,*) "Example # (from the manual) 1--11 ?"
read(5,*) example

call move_to(Col2, p2, 7)
if(example == 1) then
  mis = 0.d0
  mis(5) = pi / 8.d0
  call misalign_girder(p2, mis)
elseif(example == 2) then
  mis = 0.d0
  mis(1) = 2.0d0
  call misalign_girder(p2, mis)
elseif(example == 3) then
  mis = 0.d0
  mis(5) = pi / 8.d0
  call misalign_girder(p2, mis)
  mis = 0.d0
  mis(1) = 2.d0
  call misalign_girder(p2, mis, add = .false.)
elseif(example == 4) then
  mis = 0.d0
  mis(5) = pi / 8.d0
  call misalign_girder(p2, mis)
  mis = 0.d0
  mis(1) = 2.d0
  call misalign_girder(p2, mis, add = .true.)
elseif(example == 5) then
  mis = 0.d0
  mis(1) = 2.d0
  mis(5) = pi / 8.d0
  call misalign_girder(p2, mis)
elseif(example == 6) then
  mis = 0.d0
  mis(1) = 2.d0
  call misalign_siamese(p2, mis)
elseif(example == 7) then
  mis = 0.d0
  mis(1) = 2.d0
  call misalign_siamese(p2, mis)
  mis = 0.d0
  mis(1) = 2.d0
  mis(5) = pi / 8.d0
  call misalign_girder(p2, mis, add = .false.)
elseif(example == 8) then
  mis = 0.d0
  mis(1) = 2.d0
  call misalign_siamese(p2, mis)
  mis = 0.d0
  mis(1) = 2.d0
  mis(5) = pi / 8.d0
  call misalign_girder(p2, mis, add = .true.)
elseif(example == 9) then
  mis = 0.d0
  mis(1) = 2.d0
  mis(5) = pi / 8.d0
  call misalign_girder(p2, mis)
  mis = 0.d0
  mis(1) = 2.d0
  call misalign_siamese(p2, mis, add = .false.)
elseif(example == 10) then
  mis = 0.d0
  mis(1) = 2.d0
  mis(5) = pi / 8.d0
  call misalign_girder(p2, mis)
  mis = 0.d0
  mis(1) = 2.d0
  call misalign_siamese(p2, mis, add = .true.)
elseif(example == 11) then
  mis = 0.d0
  mis(1) = 2.d0
  mis(5) = pi / 8.d0
  call misalign_girder(p2, mis)
  mis = 0.d0
  mis(1) = 2.d0
  call misalign_siamese(p2, mis, add = .false., &
                        preserve_girder = .true.)
end if

999 command_gino = "opengino"
call context(command_gino)   ! context makes them capital
call call_gino(command_gino)
111 command_gino = "mini"
call context(command_gino)   ! context makes them capital
call call_gino(command_gino)

!== vaguely necessary baloney ==
command_gino = "closegino"
call call_gino(command_gino)
call ptc_end
end program ptc_geometry


