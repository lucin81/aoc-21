module m_aoc21
  use iso_fortran_env, only: i4 => int32, i8 => int64
  implicit none
  private

  public :: i4, i8
  public :: nrows, ncols, diff, smooth_fw, apply_filter, bits_array_to_int_4, &
    filter_bits, split_string, number_of_boards, mark_board, print_board, check_winners, &
    board_marker, find_loser, check_winner

  integer(kind=i4), parameter :: board_marker = -10 
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

  integer function ncols(fname, separator)
    character(len=*), intent(in) :: fname
    character(len=1), intent(in) :: separator
    character(len=300) :: string
    integer(kind=i4) :: iu
    
    open(newunit=iu, file=trim(fname), action='read')
      read(iu, '(a)') string
    close(iu)
    ncols=1
    do while (string(ncols:ncols) /= separator)
      ncols = ncols + 1
    enddo
    ncols = ncols - 1
  end function ncols

  function split_string(string, separator) result(res)
    character(len=*), intent(in) :: string
    character(len=1), intent(in) :: separator

    integer(kind=i4) :: n, ios, i

    integer(kind=i4), allocatable :: res(:)

    i=1 ! need to be set to one due to a but in gfortran 
    n = count( [ (string(i:i), i=1, len_trim(string)) ] == separator)
    allocate(res(n+1))
    read(string, *) res
  end function split_string

  function number_of_boards(fname)
    character(len=*), intent(in) :: fname

    character(len=500) :: string
    integer(kind=i4) :: iu, ios 

    integer(kind=i4) :: number_of_boards

    open(newunit=iu, file=trim(fname), action='read', access='sequential')
      read(iu,*) ! random numbers 
      number_of_boards = 0
      do
        read(iu, '(a)', iostat=ios) string 
        if (ios/=0) exit
        if (len_trim(string) == 0) number_of_boards = number_of_boards + 1
      enddo
    close(iu)
  end function number_of_boards

  subroutine mark_board(board, draw)
    integer(kind=i4), intent(inout) :: board(:,:)
    integer(kind=i4), intent(in) :: draw
    integer(kind=i4) :: loc(size(board,2))
    
    where (board == draw)
      board = board_marker
    endwhere
  end subroutine mark_board

  subroutine print_board(board)
    integer(kind=i4), intent(in) :: board(:,:)
    integer(kind=i4) :: i, j, board_size

    board_size = size(board, 1)
    do i=1, board_size
      print*, (board(j,i), j=1, board_size)
    enddo
  end subroutine print_board

  subroutine check_winners(boards, winner)
    integer(kind=i4), intent(in) :: boards(:,:,:)

    integer(kind=i4) :: s1(size(boards, 2)), s2(size(boards, 1)), board_size, k

    integer(kind=i4), intent(out) :: winner

    board_size = size(boards, 1)
    if (size(boards, 2) /= board_size) error stop "boards are not squared!"
    do k=1, size(boards, 3)
      call check_winner(boards(:,:,k), winner)
      if (winner /= -1) then 
        winner = k
        exit
      endif
    enddo
  end subroutine check_winners

  subroutine check_winner(boards, winner)
    integer(kind=i4), intent(in) :: boards(:,:)

    integer(kind=i4) :: s1(size(boards, 2)), s2(size(boards, 1)), board_size, k

    integer(kind=i4), intent(out) :: winner

    board_size = size(boards, 1)
    winner = -1
    if (size(boards, 2) /= board_size) error stop "boards are not squared!"
    s1 = sum(boards, dim=1)
    s2 = sum(boards, dim=2)

    if (any(s1 .eq. board_marker * board_size .or. s2 .eq. board_marker * board_size)) then 
      winner = 1
    endif
  end subroutine check_winner

  subroutine find_loser(boards, loser)
    integer(kind=i4), intent(in) :: boards(:,:,:)

    integer(kind=i4) :: s1(size(boards, 2)), s2(size(boards, 1)), board_size, k
    integer(kind=i4) :: nwinners

    integer(kind=i4), intent(out) :: loser

    board_size = size(boards, 1)
    loser = -1
    nwinners = 0
    if (size(boards, 2) /= board_size) error stop "boards are not squared!"
    do k=1, size(boards, 3)
      s1 = sum(boards(:,:,k), dim=1)
      s2 = sum(boards(:,:,k), dim=2)

      if (any(s1 .eq. board_marker * board_size .or. s2 .eq. board_marker * board_size)) then 
        nwinners = nwinners + 1
      else 
        loser = k
      endif
    enddo

    if (nwinners /= size(boards, 3) - 1) loser = -1
  end subroutine find_loser

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

  function filter_bits(a, type) result(b)
    character(len=1), intent(in) :: a(:)
    integer(kind=i4), allocatable :: b(:)
    character(len=*), intent(in) :: type
    character(len=1) :: val
    integer(kind=i4) :: i, ix

    if (type=='oxygen') then 
      if (count(a=='1') >= count(a=='0')) then 
        val = '1'
      else 
        val = '0'
      endif           
    else if (type == 'co2') then 
      if (count(a=='1') >= count(a=='0')) then 
        val = '0'
      else 
        val = '1'
      endif 
    else 
      error stop "unrecognized type"
    endif

    b=pack([(ix, ix=1,size(a))], a==val)
  end function filter_bits 

  function apply_filter(a, type) result(b)
    character(len=1), intent(in) :: a(:,:)
    character(len=*), intent(in) :: type
    character(len=1) :: b(size(a,1))
    character(len=1), allocatable :: ax(:,:)
    integer(kind=i4), allocatable :: ind(:)
    integer(kind=i4) :: i

    ax = a
    do i=1, size(a,1)
      ind = filter_bits(ax(i,:),type)
      ax = ax(:, ind)
      ! print*, size(ax,2)
      ! if (size(ax,2)==2) then 
      !   print*, i, ind(1), ax(:,1)
      !   print*, i, ind(2), ax(:,2)
      ! endif
      if (size(ax,2) == 1) exit
    enddo

    if (size(ax, 2) > 1) then 
      error stop "Didn't find a single entry"
    else 
      b=ax(:,1)
    endif
  end function apply_filter

  function bits_array_to_int_4(a) result(res)
    character(len=1), intent(in) :: a(:)
    integer(kind=i4) :: res, i, nbits

    nbits = size(a)
    res = 0
    do i=1,nbits
      if (a(i)=='1') res = res + 2**(nbits-i)
    enddo
  end function bits_array_to_int_4
end module m_aoc21
