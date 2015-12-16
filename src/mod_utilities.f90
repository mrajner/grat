module mod_utilities
  use, intrinsic :: iso_fortran_env
  use mod_constants, only: dp, pi

  implicit none

contains

! ==============================================================================
!> For given vectors x1, y1 and x2, y2 it gives x2 interpolated for x1
!!
!! uses \c ispline and \c spline subroutines
! ==============================================================================
subroutine spline_interpolation(x,y,n, x_interpolated, y_interpolated, n2, method)
  integer,intent(in) :: n, n2
  real(dp), intent(in) :: x(n), y(n), x_interpolated(n2)
  real(dp), intent(out) :: y_interpolated(n2)
  real(dp) :: b(n), c(n), d(n)
  integer :: i
  character(*), optional :: method

  call spline (x, y, b, c, d, size(x))

  do i=1, size(x_interpolated)
    y_interpolated(i) = ispline (                 &
      x_interpolated(i), x, y, b, c, d, size (x), &
      method = method                             &
      )
  enddo

end subroutine

! ==============================================================================
!> Compute coefficients for spline interpolation.
!!
!! From web sources \todo find url
!!
!! Original description below:
!! ==============================================================================
!!  Calculate the coefficients b(i), c(i), and d(i), i=1,2,...,n
!!  for cubic spline interpolation
!!  s(x) = y(i) + b(i)*(x-x(i)) + c(i)*(x-x(i))**2 + d(i)*(x-x(i))**3
!!  for  x(i) <= x <= x(i+1)
!!  Alex G: January 2010
!!
!!  input..
!!  x = the arrays of data abscissas (in strictly increasing order)
!!  y = the arrays of data ordinates
!!  n = size of the arrays xi() and yi() (n>=2)
!!  output..
!!  b, c, d  = arrays of spline coefficients
!!  comments ...
!!  spline.f90 program is based on fortran version of program spline.f
!!  the accompanying function fspline can be used for interpolation
!! ==============================================================================
subroutine spline (x, y, b, c, d, n)
  integer n
  real(dp) :: x(n), y(n), b(n), c(n), d(n)
  integer i, j, gap
  real(dp) ::  h

  gap = n-1
  ! check input
  if ( n < 2 ) return
  if ( n < 3 ) then
    b(1) = (y(2)-y(1))/(x(2)-x(1))   ! linear interpolation
    c(1) = 0.
    d(1) = 0.
    b(2) = b(1)
    c(2) = 0.
    d(2) = 0.
    return
  end if
  !
  ! step 1: preparation
  !
  d(1) = x(2) - x(1)
  c(2) = (y(2) - y(1))/d(1)
  do i = 2, gap
    d(i) = x(i+1) - x(i)
    b(i) = 2.0*(d(i-1) + d(i))
    c(i+1) = (y(i+1) - y(i))/d(i)
    c(i) = c(i+1) - c(i)
  end do
  !
  ! step 2: end conditions
  !
  b(1) = -d(1)
  b(n) = -d(n-1)
  c(1) = 0.0
  c(n) = 0.0
  if(n /= 3) then
    c(1) = c(3)/(x(4)-x(2)) - c(2)/(x(3)-x(1))
    c(n) = c(n-1)/(x(n)-x(n-2)) - c(n-2)/(x(n-1)-x(n-3))
    c(1) = c(1)*d(1)**2/(x(4)-x(1))
    c(n) = -c(n)*d(n-1)**2/(x(n)-x(n-3))
  end if
  !
  ! step 3: forward elimination
  !
  do i = 2, n
    h = d(i-1)/b(i-1)
    b(i) = b(i) - h*d(i-1)
    c(i) = c(i) - h*c(i-1)
  end do
  !
  ! step 4: back substitution
  !
  c(n) = c(n)/b(n)
  do j = 1, gap
    i = n-j
    c(i) = (c(i) - d(i)*c(i+1))/b(i)
  end do
  !
  ! step 5: compute spline coefficients
  !
  b(n) = (y(n) - y(gap))/d(gap) + d(gap)*(c(gap) + 2.0*c(n))
  do i = 1, gap
    b(i) = (y(i+1) - y(i))/d(i) - d(i)*(c(i+1) + 2.0*c(i))
    d(i) = (c(i+1) - c(i))/d(i)
    c(i) = 3.*c(i)
  end do
  c(n) = 3.0*c(n)
  d(n) = d(n-1)
end subroutine spline

! ==============================================================================
!> Evaluates the cubic spline interpolatione.
!!
! ==============================================================================
!> Function ispline evaluates the cubic spline interpolation at point z
!! ispline = y(i)+b(i)*(u-x(i))+c(i)*(u-x(i))**2+d(i)*(u-x(i))**3
!! where  x(i) <= u <= x(i+1)
!!----------------------------------------------------------------------
!! input..
!! u       = the abscissa at which the spline is to be evaluated
!! x, y    = the arrays of given data points
!! b, c, d = arrays of spline coefficients computed by spline
!! n       = the number of data points
!! output:
!! ispline = interpolated value at point u
!!
!! \date 2013-03-10
!! \author M. Rajner
!! added optional parameter method
!!=======================================================================
real (dp) function ispline(u, x, y, b, c, d, n, method)
  integer n
  real(dp)::  u, x(n), y(n), b(n), c(n), d(n)
  integer ::  i, j, k
  real(dp) :: dx
  character(*), optional :: method

  ! if u is ouside the x() interval take a boundary value (left or right)
  if(u <= x(1)) then
    ispline = y(1)
    return
  end if
  if(u >= x(n)) then
    ispline = y(n)
    return
  end if

  !*
  !  binary search for for i, such that x(i) <= u <= x(i+1)
  !*
  i = 1
  j = n+1
  do while (j > i+1)
    k = (i+j)/2
    if(u < x(k)) then
      j=k
    else
      i=k
    end if
  end do
  !*
  !  evaluate spline interpolation
  !*
  dx = u - x(i)

  if (present (method)) then
    if (method == "nearest") then
      if ((x(i+1)-u) < dx) then
        ispline = y(i+1)
        return
      else
        ispline = y(i)
        return
      endif
    elseif (method == "linear") then
      ispline = y(i) + (y(i+1)-y(i))/(x(i+1)-x(i)) * dx
      return
    endif
  endif
  ispline = y(i) + dx*(b(i) + dx*(c(i) + dx*d(i)))
end function ispline

! ==============================================================================
!> This function counts the word in line separated with space or multispaces
!!
!! taken from ArkM http://www.tek-tips.com/viewthread.cfm?qid=1688013
!!
!! or other optional separator
!! added Marcin Rajner 2013.10.08
! ==============================================================================
pure function ntokens(line, separator)
  character, intent(in) :: line(*)
  character(1), intent(in), optional :: separator
  integer:: i, n
  character(1) :: separator_
  integer  :: ntokens

  if (present(separator)) then
    separator_ = separator
  else
    separator_ = ' '
  endif

  i = 1
  n = len_trim(line)
  ntokens = 0

  do while(i <= n)

    do while(line(i:i) == separator_)
      i = i + 1
      if (n < i) return
    enddo

    ntokens = ntokens + 1

    do
      i = i + 1
      if (n < i) return
      if (line(i:i) == separator_) exit
    enddo

  enddo
end function

! ==============================================================================
!> This routine skips the lines with comment chars (default '#')
!! from opened files (unit) to read
! ==============================================================================
subroutine skip_header (unit, comment_char)
  integer, intent (in) :: unit
  character (len = 1), optional :: comment_char
  character (len = 60 ) :: dummy
  character (len = 1)  :: comment_char_
  integer :: io_stat

  if (present ( comment_char) ) then
    comment_char_ = comment_char
  else
    comment_char_ = '#'
  endif

  read (unit, '(a)', iostat = io_stat) dummy

  if(io_stat == iostat_end) then
    backspace(unit)
    return
  endif

  do while (dummy(1:1).eq.comment_char_)

    read (unit, *, iostat = io_stat) dummy

    if (io_stat == iostat_end) then
      backspace(unit)
      return
    endif

  enddo

  backspace(unit)
end subroutine

! =============================================================================
!> Check if argument is numeric
!!
!! \author Taken from www
!! \date 2013-03-19
!!
!! \date 2013.07.16 added exception e.g /home/...
!! \date 2014.08.27 added exception e.g comma ,
! =============================================================================
pure function is_numeric(string)
  logical :: is_numeric
  character(len=*), intent(in) :: string
  real :: x
  integer :: e

  if (string(1:1).eq."/") then
    is_numeric=.false.
  else if (index(string,",").gt.0) then
    is_numeric=.false.

    ! minus sign not on the first postion but allow 1e-5
  else if (                                              &
    index(string,"-").gt.1                               &
    .and.(.not.index(string,"e").eq.index(string,"-")-1) &
    ) then
    is_numeric=.false.

  else
    read(string, *, iostat=e) x
    is_numeric = e == 0
  endif
end function

! =============================================================================
!> Check if file exists.
!!
!! Logical function checking if given file exists.
!! \author M. Rajner (based on www)
!! \date 2013-03-04
! =============================================================================
function file_exists(string, double_check, verbose)
  character(len=*), intent(in) :: string
  logical, intent(in), optional :: double_check, verbose
  real :: randomnumber
  logical :: file_exists

  if (string =="") then
    file_exists=.false.
    return
  endif
  inquire(file=string, exist=file_exists)


  if (present(verbose).and.verbose) then
    if (file_exists) then
      write (error_unit, '(a,a)') trim(string), " exists"
    else
      write (error_unit, '(a,a)'), trim(string), " not exists"
    endif
  endif

  if (present(double_check).and.double_check.and..not.file_exists) then
    call random_number(randomnumber)

    if (present(verbose).and.verbose) then
      print '(a,a,i3,"s...")', &
        trim(string), " not exists, slepping", int(randomnumber*5+1)
    endif

    call sleep(int(randomnumber*5+1))
    inquire(file=string, exist=file_exists)

    if (present(verbose).and.verbose) then
      if (file_exists) then
        print '(a,a)', trim(string), " exists (indeed)"
      else
        print '(a,a)', trim(string), " not exists (still)"
      endif
    endif
  endif
end function


! =============================================================================
!> degree -> radian
!!
!! This function convert values given in decimal degrees to radians.
!! \author M. Rajner
!! \date 2013-03-04
! =============================================================================
elemental function d2r (degree)
  real(dp), intent (in) :: degree
  real(dp) :: d2r

  d2r= pi / 180. * degree
end function

! =============================================================================
!> radian -> degree
!!
!! This function convert values given in radians to decimal degrees.
!! \author Marcin Rajner
!! \date 2013-03-04
! =============================================================================
elemental function r2d ( radian )
  real(dp) :: r2d
  real(dp), intent (in) :: radian

  r2d= 180. / pi * radian
end function

! =============================================================================
!> Count rows and (or) columns of file.
!!
!! You can also specify the comment sign to ignore in data file.
!! The number of columns is set to maximum of number of columns in consecutive
!! rows.
!! \date 2013-03-10
!! \author M. Rajner
! =============================================================================
subroutine count_records_to_read (file_name, rows, columns, comment_char)
  integer, optional, intent (out) :: rows, columns
  character(*) :: file_name
  character(255) :: line
  integer :: file_unit, n_rows, n_columns, io_stat
  character(len=1), optional, intent(in):: comment_char
  character(len=1) :: comment_char_

  n_rows    = 0
  n_columns = 0

  if (present(comment_char)) then
    comment_char_ = comment_char
  else
    ! default comment_char
    comment_char_ = '#'
  endif

  open (newunit = file_unit,  file = file_name, status = "old", action ="read")
  do
    call skip_header (file_unit, comment_char_)
    read (file_unit, '(a)', iostat = io_stat) line

    if (io_stat == iostat_end) exit

    n_columns = max (n_columns, ntokens(line))
    n_rows    = n_rows + 1
  enddo

  if (present(rows))    rows    = n_rows
  if (present(columns)) columns = n_columns

  close (file_unit)
end subroutine

! ==============================================================================
!> \brief returns numbers of arguments for n times denser size
!!
!! i.e. * * * *  -->  * . . * . . * . . * (3 times denser)
! ==============================================================================
function size_ntimes_denser (size_original, ndenser)
  integer :: size_ntimes_denser
  integer, intent(in) :: size_original, ndenser

  size_ntimes_denser= (size_original - 1) * (ndenser) + 1
end function

! =============================================================================
!> Counts occurence of character (separator, default comma) in string
!!
!! added Marcin Rajner 2014.07.14
! =============================================================================
integer function count_separator (dummy, separator, consecutive_as_one)
  character(*), intent(in) :: dummy
  character(1), intent(in), optional :: separator
  logical, intent(in), optional :: consecutive_as_one
  character(1) :: sep
  character(:), allocatable :: dummy2
  integer :: i

  if (present(separator)) then
    sep = separator
  else
    sep = ","
  endif

  dummy2 = dummy

  count_separator = 0

  do
    i = index (dummy2, sep)
    if (i.eq.0) exit
    dummy2 = trim(adjustl(dummy2(i+1:)))
    count_separator = count_separator + 1

    if (present(consecutive_as_one).and.consecutive_as_one) then
      if (i == 1) then
        count_separator = count_separator -1
      endif
    endif

  enddo
end function

!TODO REMOVE
integer function count_separator2 (dumb, separator)
  character(*), intent(in) :: dumb
  character(1), intent(in), optional :: separator
  character(1) :: sep
  character(:), allocatable :: dumb2, pp
  integer :: i

  dumb2 = dumb

  if (present(separator)) then
    sep = separator
  else
    sep = ","
  endif

  count_separator2 = 0

  do
    i = index (dumb2, sep)
    call tmpwritei("i  ",i)

    if (i.eq.0) exit

    pp = dumb2(i+1:len(dumb2))
    dumb2 = pp

    call tmpwrite("du", dumb2)

    count_separator2 = count_separator2 + 1
  enddo

  print*, ''
  print*, count_separator2
  print*, ''

end function

subroutine tmpwrite(k,kk)
  character(*) :: k,kk
  print*, k,kk
end subroutine
subroutine tmpwritei(k,kk)
  character(*) :: k
  integer:: kk
  print*, k,kk
end subroutine

! ==============================================================================
! ==============================================================================
function datanameunit (dataname, datanames, count)
  integer:: datanameunit
  character(*), intent(in):: dataname
  integer, intent (in):: count
  character(*), intent(in)  :: datanames(count)
  integer :: i

  datanameunit = 0
  do i = 1, count
    if(datanames(i).eq.dataname) datanameunit = i
  enddo
end function

! ==============================================================================
!>  p = rho h g
! converts mm of EWT to Pascal
! inverted: converts Pascal to mm EWT
! ==============================================================================
function mmwater2pascal(mmwater, inverted)
  use mod_constants, only: density, earth

  real(dp) :: mmwater2pascal
  real(dp), intent(in) :: mmwater
  logical, optional, intent(in) :: inverted

  if (present(inverted).and.inverted) then
    mmwater2pascal= mmwater * 1000 / (earth%gravity%mean * density%water)
  else
    mmwater2pascal=density%water * mmwater /1000 * earth%gravity%mean
  endif
end function

! ==============================================================================
! ==============================================================================
function linspace(xmin, xmax, n)
  real(dp), intent(in) :: xmin, xmax
  real(dp), dimension(:), allocatable :: linspace
  integer, intent(in), optional :: n
  integer :: i

  if (present(n)) then
    allocate(linspace(n))
  else
    allocate(linspace(10))
  endif
  do i = 1, size(linspace)
    linspace(i) = xmin + (xmax-xmin) * real(i-1,dp) / real(size(linspace)-1,dp)
  end do
end function

! ==============================================================================
! ==============================================================================
function logspace(xmin, xmax, n)
  real(dp), intent(in) :: xmin, xmax
  real(dp), dimension(:), allocatable :: logspace
  integer, intent(in), optional :: n

  if (present(n)) then
    allocate(logspace(n))
  else
    allocate(logspace(10))
  endif

  logspace = 10._dp** linspace(log10(xmin), log10(xmax), n = n)
end function

! ==============================================================================
! This subroutine open new file with optional prefix name (default = tmp), and
! consecutive number with optional digits (default=3), start is optional
! start number
! ==============================================================================
subroutine uniq_name_unit (prefix, suffix, digits, start, unit, filename, ifcreate)
  character(*), intent(in),  optional :: prefix, suffix
  character(*), intent(out), optional :: filename
  integer, intent (in), optional :: digits, start
  logical, intent (in), optional :: ifcreate 
  integer :: counter
  integer, intent(out) :: unit
  character(200) :: name
  character(6) :: output_format
  
  name=''

  if (present(start)) then
    counter = start
  else
    counter = 1
  endif

  if (present(digits)) then
    write(output_format,'("i",i0,".",i0)') digits, digits
  else
    output_format="i3.3"
  endif

  do while (file_exists(name) .or. name == '' )
    write (name, '("tmp",'//output_format//',a)')  counter
    if (present(prefix)) name = prefix//name(4:)
    if (present(suffix)) name = trim(name)//suffix
    counter = counter + 1
  enddo

  if(.not.present(ifcreate).or.ifcreate) then
    open (newunit = unit, file=name, action="write")
  endif

  if (present(filename)) filename = name
end subroutine

! ==============================================================================
! ==============================================================================
real function mean (vec, i, nan)

  integer :: i
  real(dp)  :: vec(i)
  logical, intent(in), optional :: nan

  if (present(nan).and.nan) then
    mean = sum(vec, mask = .not.(isnan(vec))) / real(count(.not.isnan(vec)))
  else
    mean = sum(vec) / real(i)
  endif
end function

! ==============================================================================
! ==============================================================================
real function stdev (vec,i, nan)
  integer :: i
  real(dp)  :: vec(i)
  logical, intent(in), optional :: nan

  if (present(nan).and.nan) then
    stdev = sqrt(sum((vec - mean(vec,i,nan=nan))**2,mask=.not.isnan(vec))/real(size(vec)))
  else
    stdev = sqrt(sum((vec - mean(vec,i))**2)/real(size(vec)))
  endif
end function


! ==============================================================================
! http://rosettacode.org/wiki/Count_occurrences_of_a_substring#Fortran
! ==============================================================================
function countsubstring(s1, s2) result(c)
  character(*), intent(in) :: s1, s2
  integer :: c, p, posn

  c = 0
  if(len(s2) == 0) return
  p = 1
  do
    posn = index(s1(p:), s2)
    if(posn == 0) return
    c = c + 1
    p = p + posn + len(s2)
  end do
end function

! ==============================================================================
! sort integers in reverse order
! http://rosettacode.org/wiki/Sorting_algorithms/Bubble_sort#Fortran
! ==============================================================================
SUBROUTINE Bubble_Sort(a)
  integer, intent(in out), dimension(:) :: a
  integer :: temp
  integer :: i, j
  logical :: swapped = .true.

  do j = size(a)-1, 1, -1
    swapped = .false.
    do i = 1, j
      if (a(i) < a(i+1)) then
        temp = a(i)
        a(i) = a(i+1)
        a(i+1) = temp
        swapped = .true.
      end if
    end do
    if (.not. swapped) exit
  end do
END SUBROUTINE Bubble_Sort

! =============================================================================
!> Performs bilinear interpolation
!! \author Marcin Rajner
!! \date 2013-05-07
! =============================================================================
function bilinear (x, y, aux)
  real(dp) :: bilinear
  real(dp) :: x, y, aux(4,3)
  real(dp) :: a, b, c

  a  = ( x - aux(1,1) ) / (aux(4,1)-aux(1,1))
  b = a * (aux(3,3) - aux(1,3)) + aux(1,3)
  c = a * (aux(4,3) - aux(2,3)) + aux(2,3)
  bilinear = (y-aux(1,2))/(aux(4,2)-aux(1,2)) * (c-b) + b
end function

! =============================================================================
!> Convert celcius degree to kelvin
!! and kelvin to celcius (if optional inverted parameter is set to true)
!! \author Marcin Rajner
!! \date 2014-06-07
! =============================================================================
function celcius_to_kelvin (celcius, inverted)
  real(dp) :: celcius_to_kelvin
  real(dp), intent(in) :: celcius
  logical, intent(in), optional :: inverted

  if (present(inverted).and.inverted) then
    celcius_to_kelvin = celcius - 273.15
  else
    celcius_to_kelvin = celcius + 273.15
  end if

end function

! =============================================================================
! parse version number
! version  major minor  default
! v1.2-abc     1     2 v1.2-abc 
!
! =============================================================================
function version_split(version, which) 
  character(10) :: version_split
  character(*), intent(in) :: version
  character(*), intent(in), optional :: which
  integer :: i, j

  select case (which)

  case("major","MAJOR")
    outer: do i = 1, len(version)

      if(version(i:i)==".") then
        version_split=version(1:i-1)
        return
      endif

      if (is_numeric(version(i:i))) then
        do j = i+1, len(version)
          if (.not.is_numeric(version(j:j))) exit outer
        enddo
        exit outer
      endif

    enddo outer

    if(i.gt.len(version)) then
      version_split=version
      return
    endif

    version_split = version(i:j-1)
    return

  case("minor","MINOR")

    i=index(version,".")

    if (i.gt.0) then
      do j=i+1, len(version)
        if (.not.is_numeric(version(j:j))) exit 
      enddo
      version_split = version (i+1:j-1)
      return
    endif

  case default
    version_split = version
  end select
end function

function length_pl(string)
  character(*), intent(in) :: string
  character(2) :: polish_letters(18)= &
    ["Ą","ą","Ć","ć","Ł","ł", "Ś","ś", "Ż","ż","Ź","ź", "Ó","ó", "Ę","ę", "Ń","ń"]
  integer :: length_pl, i

  length_pl = 0

  do i = 1, size(polish_letters)
    length_pl = length_pl - countsubstring(string,polish_letters(i))
  enddo

  length_pl = length_pl + len_trim(string)
end function

end module
