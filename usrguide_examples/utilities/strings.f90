!!
!! some simple functions for inspecting and manipulating strings
!!

!!! convert all letters in a string to lower case
!!function to_lower(in_str) result(out_str)
!!  implicit none
!!  character(*), intent(in) :: in_str
!!  character(len(in_str)) :: out_str
!!  character(*), parameter :: alpha_lc = 'abcdefghijklmnopqrstuvwxyz'
!!  character(*), parameter :: alpha_UC = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
!!  integer :: i, n
!!
!!  out_str = in_str
!!  do i = 1, len(out_str)
!!    n = index(alpha_UC, out_str(i:i))
!!    if (n /= 0) out_str(i:i) = alpha_lc(n:n)
!!  end do
!!end function to_lower
!!
!!
!!! convert all letters in a string to upper case
!!function to_upper(in_str) result(out_str)
!!  implicit none
!!  character(*), intent(in) :: in_str
!!  character(len(in_str)) :: out_str
!!  character(*), parameter :: alpha_lc = 'abcdefghijklmnopqrstuvwxyz'
!!  character(*), parameter :: alpha_UC = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
!!  integer :: i, n
!!
!!  out_str = in_str
!!  do i = 1, len(out_str)
!!    n = index(alpha_lc, out_str(i:i))
!!    if (n /= 0) out_str(i:i) = alpha_UC(n:n)
!!  end do
!!end function to_upper


!!! return true if the 'answer' may reasonably be interpreted as a "no"
!!function is_no(answer) result(noQ)
!!  implicit none
!!  character(*), intent(in) :: answer
!!  character(len(answer)) :: lca
!!  logical :: noQ
!!  character(len(answer)), external :: to_lower
!!
!!  lca = to_lower(answer)
!!  noQ = .false.
!!  if (lca .eq. "n"     .or. lca .eq. "no" .or. &
!!      lca .eq. "0"     .or. lca .eq. "f"  .or. &
!!      lca .eq. "false" .or. lca .eq. ".false.") then
!!    noQ = .true.
!!  endif
!!end function is_no
!!
!!
!!! return true if the 'answer' may reasonably be interpreted as a "yes"
!!function is_yes(answer) result(yesQ)
!!  implicit none
!!  character(*), intent(in) :: answer
!!  character(len(answer)) :: lca
!!  logical :: yesQ
!!  character(len(answer)), external :: to_lower
!!
!!  lca = to_lower(answer)
!!  yesQ = .false.
!!  if (lca .eq. "y"    .or. lca .eq. "yes" .or. &
!!      lca .eq. "1"    .or. lca .eq. "t"  .or.  &
!!      lca .eq. "true" .or. lca .eq. ".true.") then
!!    yesQ = .true.
!!  endif
!!end function is_yes



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! convert all letters in a string to lower case
subroutine to_lower(str)
  character(*), intent(inout) :: str
  character(26), parameter :: alpha_lc = 'abcdefghijklmnopqrstuvwxyz'
  character(26), parameter :: alpha_UC = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  integer :: i, n

  do i = 1, len(str)
    n = index(alpha_UC, str(i:i))
    if (n /= 0) str(i:i) = alpha_lc(n:n)
  end do
end subroutine to_lower


! convert all letters in a string to upper case
subroutine to_upper(str)
  character(*), intent(inout) :: str
  character(26), parameter :: alpha_lc = 'abcdefghijklmnopqrstuvwxyz'
  character(26), parameter :: alpha_UC = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  integer :: i, n

  do i = 1, len(str)
    n = index(alpha_lc, str(i:i))
    if (n /= 0) str(i:i) = alpha_UC(n:n)
  end do
end subroutine to_upper


! return true if 'answer' may may reasonably be interpreted as a "no"
logical function is_no(answer)
  character(*), intent(in) :: answer
  character(len(answer)) :: ans

  ans = answer
  call to_lower(ans)
  is_no = .false.
  if (ans .eq. "n"     .or. ans .eq. "no" .or. &
      ans .eq. "0"     .or. ans .eq. "f"  .or. &
      ans .eq. "false" .or. ans .eq. ".false.") then
    is_no = .true.
  endif
end function is_no


! return true if 'answer' may reasonably be interpreted as a "yes"
logical function is_yes(answer)
  character(*), intent(in) :: answer
  character(len(answer)) :: ans

  ans = answer
  call to_lower(ans)
  is_yes = .false.
  if (ans .eq. "y"    .or. ans .eq. "yes" .or. &
      ans .eq. "1"    .or. ans .eq. "t"  .or.  &
      ans .eq. "true" .or. ans .eq. ".true.") then
    is_yes = .true.
  endif
end function is_yes

