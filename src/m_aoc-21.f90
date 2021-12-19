module m_aoc21
  use iso_fortran_env, only: i4 => int32
  implicit none
  private

  public :: i4
  public :: nrows, diff, smooth_fw
contains

  integer function nrows(fname)
    character(len=*), intent(in) :: fname
    integer(kind=i4) :: iu
    
    nrows=0
    open(newunit=iu, file=trim(fname), action='read')
      do
        read(iu, *, END=200)
        nrows=nrows+1 
      enddo
200 close(iu)
  end function nrows

  function diff(v) result(d)
    integer(kind=i4), allocatable :: d(:)
    integer(kind=i4) :: n, v(:)

    n=size(v)
    allocate(d(n-1))
    d=v(2:n)-v(1:(n-1))
  end function diff

  function smooth_fw(v, type, w) result(s)
    integer(kind=i4), intent(in) :: w, v(:)
    character(len=*), intent(in) :: type
    integer(kind=i4), allocatable :: s(:)
    integer(kind=i4) :: i, n

    n=size(v)
    allocate(s(n-w+1))
    if (type == 'sum') then 
      do i=1, n-w+1 
        s(i) = sum(v(i:(i+w-1)))
      enddo
    else 
      error stop 'not implemented'
    endif 

  end function smooth_fw
end module m_aoc21
