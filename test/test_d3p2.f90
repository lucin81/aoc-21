program test_d3p2
  use m_aoc21, only: i4, nrows, ncols, apply_filter, bits_array_to_int_4, filter_bits
  implicit none

  integer(kind=i4) :: iu, nr, nc, i, j
  integer(kind=i4) :: oxygen_generator_rating, co2_scrubber_rating
  integer(kind=i4), allocatable :: ind(:)
  character(len=:), allocatable :: fname
  character(len=200) :: string
  character(len=1), allocatable :: bits(:,:), res(:), bits2(:,:)

  fname='/mnt/data/aoc-21/day3/test/input.txt'

  nr = nrows(fname)
  nc = ncols(fname, ' ')
  print*, nr, nc
  allocate(bits(nc, nr))
  open(newunit=iu, file=trim(fname), action='read', form='formatted', access='sequential')
    do j=1, nr 
      read(iu, '(a)') string
      do i=1, nc
        bits(i, j) = string(i:i)
      enddo 
    enddo  
  close(iu)

  ind = filter_bits(bits(1,:),'oxygen')
  do i = 1, size(ind)
    print*, "ind=",ind(i),", bits(i,:)=",bits(:,ind(i))
  enddo
  print*, "expected=11110, 10110, 10111, 10101, 11100, 10000, and 11001"

  bits2=bits(:, ind)
  ind = filter_bits(bits2(2,:),'oxygen')
  do i = 1, size(ind)
    print*, "ind=",ind(i),", bits(i,:)=",bits2(:,ind(i))
  enddo

  res = apply_filter(bits, "oxygen")
  print*, "res=",res 
  print*, "expected=10111"
  
  oxygen_generator_rating = bits_array_to_int_4(res)
  print*, oxygen_generator_rating
  if (oxygen_generator_rating /= 23) error stop "result does not match"

end program test_d3p2
