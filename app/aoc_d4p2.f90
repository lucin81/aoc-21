program aoc_d4p2
  use m_aoc21, only: i4, nrows, ncols, split_string, number_of_boards, mark_board, &
    print_board, find_loser, board_marker, check_winner
  implicit none

  integer(kind=i4) :: iu, nr, nc, i, j, k, n, draw, loser, winner
  integer(kind=i4), allocatable :: numbers(:)

  integer(kind=i4) :: ios, nboards
  integer(kind=i4), allocatable :: boards(:,:,:)
  integer(kind=i4), parameter :: board_size = 5
  character(len=:), allocatable :: fname
  character(len=500) :: string

  fname='/mnt/data/aoc-21/day4/input.txt'

  ! One row for the draws and 1 blank line to separate boards
  nboards = (nrows(fname)-1) / (board_size + 1)
  allocate(boards(board_size, board_size, nboards))
  open(newunit=iu, file=trim(fname), action='read', form='formatted', access='sequential')
    read(iu, '(a)', iostat=ios) string
    numbers = split_string(string, ',')

    do k=1, nboards 
      read(iu, *)
      do i=1, board_size
        read(iu, *) (boards(j, i, k), j=1, board_size)
      enddo
    enddo
  close(iu)

  loser = -1
  do n=1, size(numbers)
    draw = numbers(n)
    do k=1, nboards
      call mark_board(boards(:, :, k), draw)
    enddo

    if (n>=board_size) then 
      print*, draw, loser
      if (loser == -1 ) then   
        call find_loser(boards, loser)
      else
        call check_winner(boards(:,:,loser), winner)
        if (winner /= -1) then 
          print*, "After ",n," draws the loser board (",loser,") finally wins!"
          call print_board(boards(:,:,loser))
          exit
        endif
      endif
    endif
  enddo

  print*, "Sum of the unmarked numbers: ", sum(boards(:,:,loser), &
    mask=boards(:,:,loser) /= board_marker)
  print*, "Last number drawn: ", draw
  print*, "Answer to day4 - part 1"
  print*, "The score of the winning board is: ", sum(boards(:,:,loser), &
  mask=boards(:,:,loser) /= board_marker) * draw
end program aoc_d4p2