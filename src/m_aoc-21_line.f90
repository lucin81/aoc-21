module m_aoc21_line
  use iso_fortran_env, only: i4=>int32
  implicit none

  private

  type :: point
    integer(kind=i4) :: x, y
  end type point 

  type :: line 
    type(point) :: p1, p2
    contains
      procedure, pass(self) :: parse => parse_line
      procedure, pass(self) :: print => print_line
      procedure, pass(self) :: h_or_v
      procedure, pass(self) :: h_or_v_or_d
  end type line

  public :: line
contains
  
  subroutine parse_line(self, string, separator)
    character(len=*), intent(in) :: string, separator
    class(line), intent(out) :: self
    integer(kind=i4) :: i, length_separator

    length_separator = len(separator)
    do i = 1, len_trim(string)-length_separator
      if (string(i:i+length_separator) == separator) exit
    enddo

    read(string(1:i-1), *) self%p1%x, self%p1%y
    read(string(i+length_separator+1:), *) self%p2%x, self%p2%y
  end subroutine parse_line

  subroutine print_line(self)
    class(line), intent(in) :: self
    character(len=200) :: string

    print*, self%p1%x,",",self%p1%y," -> ",self%p2%x,",",self%p2%y
    ! write(string, '(a)') self%p1%x,",",self%p1%y," -> ",self%p2%x,",",self%p2%y
    ! print*, trim(string)
  end subroutine print_line

  logical function h_or_v(self)
    class(line), intent(in) :: self

    h_or_v = self%p1%x == self%p2%x .or. self%p1%y == self%p2%y
  end function h_or_v

  logical function h_or_v_or_d(self)
    class(line), intent(in) :: self

    h_or_v_or_d = self%h_or_v() .or. &
      self%p1%x == self%p2%y .and. self%p1%y == self%p2%x .or. &
      self%p1%x == self%p2%x .and. self%p1%y == self%p2%y
  end function h_or_v_or_d
end module m_aoc21_line