program aoc_d2p2
  use m_aoc21, only: i4, nrows
  implicit none

  integer(kind=i4) :: iu, n, i, value
  integer(kind=i4) :: x, y, aim
  character(len=:), allocatable :: direction, fname
  character(len=20) :: ctmp

  fname='/mnt/data/aoc-21/day2/input.txt'

  n = nrows(fname)
  print*, n
  x = 0
  y = 0
  aim = 0
  open(newunit=iu, file=trim(fname), action='read')
    do i=1, n
      read(iu, *) ctmp, value  
      direction = trim(adjustl(ctmp))

      if (direction == 'forward') then 
        x = x + value
        y = y + value * aim
      else if (direction == 'up') then
        aim = aim - value
      else if (direction == 'down') then 
        aim = aim + value 
      else 
        error stop "Invalid direction"
      endif 
    enddo
  close(iu)

  print*, "Horizontal coordinate: ", x
  print*, "Vertical coordinate: ", y
  print*, "Final aim: ", aim
  print*, "Answer to day 2 - part 2"
  print*, "Product of x and y coordinates: ", x * y

end program aoc_d2p2