program aoc_d3p1
  use m_aoc21, only: i4, nrows, ncols
  implicit none

  integer(kind=i4) :: iu, nr, nc, i, j
  integer(kind=i4) :: gamma_rate, epsilon_rate
  character(len=:), allocatable :: fname
  character(len=200) :: string
  character(len=1), allocatable :: bits(:,:)

  fname='/mnt/data/aoc-21/day3/input.txt'

  nr = nrows(fname)
  nc = ncols(fname)
  allocate(bits(nc, nr))
  open(newunit=iu, file=trim(fname), action='read', form='formatted', access='sequential')
    do j=1, nr 
      read(iu, '(a)') string
      do i=1, nc
        bits(i, j) = string(i:i)
      enddo 
    enddo  
  close(iu)

  gamma_rate = 0
  epsilon_rate = 0 
  do i=1, nc
    if (count(bits(i,:)=='1') > nr/2) then 
      gamma_rate = gamma_rate + 2**(nc-i)
    else 
      epsilon_rate = epsilon_rate + 2**(nc-i)
    endif
  enddo

  print*, "Gamma rate: ", gamma_rate
  print*, "Epsilon rate: ", epsilon_rate
  print*, "Answer to day3 - part 1"
  print*, "The product of gamma rate times epsilon rate is: ", gamma_rate * epsilon_rate
end program aoc_d3p1