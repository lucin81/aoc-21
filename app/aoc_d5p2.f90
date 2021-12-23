program aoc_d5p1
  use m_aoc21, only: i4, nrows, ncols
  use m_aoc21_line, only: line
  implicit none

  integer(kind=i4) :: iu, nsafe, k, minx, maxx, miny, maxy, j, i
  integer(kind=i4), allocatable :: diagram(:,:), x_indexes(:), y_indexes(:)

  integer(kind=i4) :: nlines
  character(len=:), allocatable :: fname
  character(len=500) :: string

  type(line), allocatable :: lines(:)

  fname='inputs/day5/input.txt'

  ! One row for the draws and 1 blank line to separate boards
  nlines = nrows(fname)
  allocate(lines(nlines))

  open(newunit=iu, file=trim(fname), action='read', form='formatted', access='sequential')
    do k=1, nlines 
      read(iu, '(a)') string
      call lines(k)%parse(string, '->')
    enddo
  close(iu)

  maxx=maxval([lines(:)%p1%x, lines(:)%p2%x])
  minx=minval([lines(:)%p1%x, lines(:)%p2%x])
  maxy=maxval([lines(:)%p1%y, lines(:)%p2%y])
  miny=minval([lines(:)%p1%y, lines(:)%p2%y])
  ! print*, minx, maxx, miny, maxy
  allocate(diagram(miny:maxy, minx:maxx))
  diagram=0
  do k=1, nlines 
    associate (p1 => lines(k)%p1, p2 => lines(k)%p2)
      ! print*, p1%x, p1%y, '--', p2%x, p2%y
      x_indexes = [(i, i=p1%x, p2%x, sign(1, p2%x-p1%x))]
      y_indexes = [(i, i=p1%y, p2%y, sign(1, p2%y-p1%y))]
      if (size(x_indexes) == 1 .or. size(y_indexes) == 1) then 
        diagram(y_indexes, x_indexes) = diagram(y_indexes, x_indexes) + 1
      else if (size(x_indexes) == size(y_indexes)) then 
        do i = 1, size(x_indexes)
          diagram(y_indexes(i), x_indexes(i)) = diagram(y_indexes(i), x_indexes(i)) + 1
        enddo
      else 
        print*, p1%x, p1%y, '--', p2%x, p2%y
        ! print*, x_indexes
        ! print*, y_indexes
        error stop "The line is not h or v or d at 45deg!"
      endif
    end associate
    ! do i=miny, maxy
    !   print*, (diagram(i, j), j=minx,maxx) 
    ! enddo  
    ! print*, "------------------------------"
  enddo

  nsafe = count(diagram >= 2)
  
  print*, "Answer to day5 - part 1"
  print*, "The number of points where at lease two lines overlap is: ", nsafe
end program aoc_d5p1