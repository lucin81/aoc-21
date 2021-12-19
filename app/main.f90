program main
  use m_aoc21, only: i4, nrows, diff, smooth_fw
  implicit none

  integer(kind=i4) :: iu, n, i
  integer(kind=i4), allocatable :: v(:), d(:), v2(:)
  character(len=:), allocatable :: fname

  fname='/mnt/data/aoc-21/day1/input.txt'

  n=nrows(fname)
  allocate(v(n))
  open(newunit=iu, file=trim(fname), action='read')
    do i=1, n
      read(iu, *) v(i)  
    enddo
  close(iu)

  d=diff(v)

  print*, "Answer to part 1"
  print*, "Number of measurements larger than previous: ", count(d>0)

  v2=smooth_fw(v, 'sum', 3)

  d=diff(v2)
  print*, "Answer to part 2"
  print*, "Number of smoothed measurements larger than previous: ", count(d>0)

end program main
