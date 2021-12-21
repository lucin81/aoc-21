program aoc_d3p2
  use m_aoc21, only: i4, nrows, ncols, apply_filter, bits_array_to_int_4
  implicit none

  integer(kind=i4) :: iu, nr, nc, i, j
  integer(kind=i4) :: oxygen_generator_rating, co2_scrubber_rating
  character(len=:), allocatable :: fname
  character(len=200) :: string
  character(len=1), allocatable :: bits(:,:), res(:)

  fname='/mnt/data/aoc-21/day3/input.txt'

  nr = nrows(fname)
  nc = ncols(fname, ' ')
  allocate(bits(nc, nr))
  open(newunit=iu, file=trim(fname), action='read', form='formatted', access='sequential')
    do j=1, nr 
      read(iu, '(a)') string
      do i=1, nc
        bits(i, j) = string(i:i)
      enddo 
    enddo  
  close(iu)

  res = apply_filter(bits, "oxygen")
  oxygen_generator_rating = bits_array_to_int_4(res)

  res = apply_filter(bits, "co2")
  co2_scrubber_rating = bits_array_to_int_4(res)

  print*, "Oxygen generator rating: ", oxygen_generator_rating
  print*, "CO2 scrubber rating: ", co2_scrubber_rating
  print*, "Answer to day3 - part 2"
  print*, "The product of the two ratings is: ", oxygen_generator_rating * co2_scrubber_rating
end program aoc_d3p2