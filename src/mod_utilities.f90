module mod_utilities

  implicit none
  private

  public:: &
    ntokens           , jd                   , mjd        , &
    invmjd            , spline_interpolation , spline     , &
    ispline           , file_exists          , skip_header, &
    d2r               , r2d                  , is_numeric , &
    spher_trig_inverse, count_records_to_read, spher_trig , &
    spher_area


contains

! ==============================================================================
!> For given vectors x1, y1 and x2, y2 it gives x2 interpolated for x1
!!
!! uses \c ispline and \c spline subroutines
! ==============================================================================
subroutine spline_interpolation(x,y,n, x_interpolated, y_interpolated, n2, method)
  use mod_constants, only:dp
  integer,intent(in) :: n, n2
  real(dp) , intent(in) :: x(n), y(n) , x_interpolated(n2)
  real(dp) , intent(out) :: y_interpolated(n2)
  real(dp) , dimension (:) , allocatable :: b, c, d
  integer :: i
  character(*), optional :: method

  allocate (b (size(x)))
  allocate (c (size(x)))
  allocate (d (size(x)))

  call spline ( x , y, b , c, d, size(x))
  
  do i=1, size(x_interpolated)
     y_interpolated(i) = ispline (x_interpolated(i) , x , y , b , c , d , size (x) , method = method )
  enddo

end subroutine

!the procedure below will be soon removed (just for backward compability)
subroutine spline_interpolation2 (x, y, x_interpolated, y_interpolated, method)
  use mod_constants, only:dp
  real(dp) , allocatable , dimension (:), intent(in)  :: x, y, x_interpolated
  real(dp) , allocatable , dimension (:), intent(out) :: y_interpolated
  real(dp) , dimension (:) , allocatable :: b, c, d
  integer :: i
  character(*), optional :: method

  allocate (b (size(x)))
  allocate (c (size(x)))
  allocate (d (size(x)))
  allocate (y_interpolated (size(x_interpolated)))

  call spline ( x , y, b , c, d, size(x))
  
  do i=1, size(x_interpolated)
     y_interpolated(i) = ispline (x_interpolated(i) , x , y , b , c , d , size (x) , method = method )
  enddo

end subroutine

! ==============================================================================
!> Compute coefficients for spline interpolation.
!!
!! From web sources \todo find url
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
  use mod_constants, only: dp
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
  use mod_constants, only : dp
  integer n
  real(dp)::  u, x(n), y(n), b(n), c(n), d(n)
  integer ::  i, j, k
  real(dp) :: dx
  character(*) , optional :: method

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
! ==============================================================================
integer function ntokens(line)
    character,intent(in) :: line*(*)
    integer:: i, n, toks

  i = 1;
  n = len_trim(line)
  toks = 0
  ntokens = 0
  do while(i <= n)
    do while(line(i:i) == ' ') 
      i = i + 1
      if (n < i) return
    enddo
    toks = toks + 1
    ntokens = toks
    do
      i = i + 1
      if (n < i) return
      if (line(i:i) == ' ') exit
    enddo
  enddo
end function ntokens 

! ==============================================================================
!> This routine skips the lines with comment chars (default '#')
!! from opened files (unit) to read
! ==============================================================================
subroutine skip_header ( unit , comment_char_optional )
  use, intrinsic :: iso_fortran_env, only: iostat_end
  integer , intent (in) :: unit 
  character (len = 1) , optional :: comment_char_optional
  character (len = 60 ) :: dummy
  character (len = 1)  :: comment_char
  integer :: io_stat

  if (present ( comment_char_optional ) ) then
    comment_char = comment_char_optional
  else
    comment_char = '#'
  endif

  read ( unit, * , iostat = io_stat) dummy
  if(io_stat == iostat_end) return

  do while ( dummy(1:1) .eq. comment_char ) 
    read ( unit, * , iostat = io_stat ) dummy
    if(io_stat == iostat_end) return
  enddo
  backspace(unit)
end subroutine

! ==============================================================================
!> Compute Julian date for given date.
!! 
!! Compute Julian Day (not MJD!). Seconds as integer!
!! \author  http://aa.usno.navy.mil/faq/docs/jd_formula.php
!! \todo mjd!
!! \date 2013-03-04
! ==============================================================================
function jd (year,month,day, hh,mm,ss)
  use mod_constants, only: dp
  integer, intent(in) ::  year,month,day
  integer, intent(in) :: hh,mm, ss
  integer :: i , j , k
  real(dp) :: jd 

  i= year
  j= month
  k= day
  jd= k-32075+1461*(i+4800+(j-14)/12)/4+367*(j-2-(j-14)/12*12)/12-3*((i+4900+(j-14)/12)/100)/4  + (hh/24.) &
  + mm/(24.*60.) +ss/(24.*60.*60.) ! - 2400000.5
  return
end function

! ==============================================================================
!> MJD from date.
!! 
!! Compute Modified Julian date for given date. Iput is six element array of 
!! !integers. Seconds also as integers!
!! \date 2013-03-04
! ==============================================================================
function mjd  (date)
  use mod_constants, only: dp
  integer ,intent(in) :: date (6)
  integer :: aux (6)
  integer :: i , k
  real(dp) :: dayfrac , mjd

  aux=date
  if ( aux(2) .le.  2) then
      aux(1)  = date(1) -  1
      aux(2) = date(2) +  12
  endif
  i = aux(1)/100
  k = 2 - i + int(i/4);
  mjd = int(365.25 * aux(1) ) - 679006
  dayfrac =  aux (4) / 24. + date(5)/(24. * 60. ) + date (6)/(24. * 3600. ) 
  mjd = mjd + int(30.6001*( aux(2) + 1)) + date(3) + k + dayfrac
end function

! ==============================================================================
!> Compute date from given Julian Day
!!
!! This subroutine computes date (as an six elements integer array) from 
!! Modified Julian Day
!! \date 2013-03-04
! ==============================================================================
subroutine invmjd (mjd , date)
  use mod_constants, only: dp
  real(dp), intent (in) :: mjd
  integer , intent (out):: date (6)
  integer :: t1 ,t4 , h , t2 , t3 , ih1 , ih2
  real(dp) :: dayfrac

  date =0

  t1 = 1+ int(mjd) + 2400000
  t4 = mjd - int (mjd);
   h = int ((t1 - 1867216.25)/36524.25);
  t2 = t1 + 1 + h - int (h/4)
  t3 = t2 - 1720995
  ih1 = int ((t3 -122.1)/365.25)
  t1 = int (365.25 * ih1)
  ih2 = int ((t3 - t1)/30.6001);
  date(3) = (t3 - t1 - int (30.6001 * ih2)) + t4;
  date(2) = ih2 - 1;
  if (ih2 .gt. 13) date(2) = ih2 - 13
   date(1) = ih1
  if (date(2).le. 2) date(1) = date(1) + 1

  dayfrac = mjd - int(mjd) + 1./ (60*60*1000)
  date(4) = int (dayfrac * 24. )
  date(5) = ( dayfrac - date (4) / 24. ) * 60 * 24  
  date(6) = ( dayfrac - date (4) / 24. - date(5)/(24.*60.)  ) * 60 * 24 *60
  if (date (6) .eq. 60 ) then
    date (6)=0
    date (5)=date(5) + 1
  endif
end subroutine

! =============================================================================
!> Check if argument is numeric
!!
!! \author Taken from www
!! \date 2013-03-19
! =============================================================================
function is_numeric(string)
  logical :: is_numeric
  character(len=*), intent(in) :: string
  real :: x
  integer :: e
  read(string,*,iostat=e) x
  is_numeric = e == 0
end function 


! =============================================================================
!> Check if file exists.
!!
!! Logical function checking if given file exists.
!! \author M. Rajner (based on www)
!! \date 2013-03-04
! =============================================================================
logical function file_exists(string)
  character(len=*), intent(in) :: string
  logical :: exists
  real :: x
  integer :: e
  if (string =="") then
    file_exists=.false.
    return
  endif
  inquire(file=string, exist=exists)
  file_exists=exists
end function 


! =============================================================================
!> degree -> radian
!!
!! This function convert values given in decimal degrees to radians.
!! \author M. Rajner
!! \date 2013-03-04
! =============================================================================
function d2r (degree)
  use mod_constants, only: pi, dp
  real(dp) , intent (in) :: degree
  real(dp) :: d2r
  d2r= pi / 180.0 * degree
end function

! =============================================================================
!> radian -> degree
!!
!! This function convert values given in radians to decimal degrees.
!! \author Marcin Rajner
!! \date 2013-03-04
! =============================================================================
function r2d ( radian )
  use mod_constants, only: pi, dp
  real(dp) :: r2d 
  real(dp), intent (in) :: radian
  r2d= 180. / pi * radian
end function

! =============================================================================
!> Calculate area of spherical segment 
!!
!! Computes spherical area on unit (default if optional argument \c radius is
!! not given) sphere given by:
!!   - method 1 (\c alternative_method not given or \c alternative_method .false.)
!!    - distance from station, segment size in spher distance and angle
!!   - method 2 (\c alternative_method .true.)
!!    - distance from station, segment size in spher distance and angle
!!
!! 
!! The ilustration explain optional \c method argument
!! \latexonly
!! \begin{center}
!!  \tikzsetfigurename{spher_area} 
!!  \input{/home/mrajner/src/grat/doc/rysunki/spher_area}
!! \end{center}
!! \endlatexonly
!! \image html /home/mrajner/src/grat/doc/rysunki/spher_area.svg
!!
!! \warning All input angles in radiana output area on unit sphere or 
!! in square units of given (optionally) \c radius.
! =============================================================================
subroutine spher_area (distance ,ddistance, azstp,  area, radius, alternative_method )
  use mod_constants, only: dp, sp
  real(dp), intent(out) :: area
  real(dp), intent(in)  :: distance,ddistance 
  real(dp), intent(in)  :: azstp
  logical, intent(in), optional :: alternative_method
  real(dp), intent(in), optional :: radius

  if (present(alternative_method).and.alternative_method) then
    area =  (-cos (ddistance) + cos (distance))*(azstp)
  else
    area =  (-cos (distance+ddistance/2.)+cos(distance-ddistance/2.))*(azstp)
  endif
  if(present(radius)) area = area * radius**2

end subroutine

! =============================================================================
!> This soubroutine gives the latitude and longitude of the point at the 
!! specified distance and azimuth from site latitude and longitude.
!!
!! all parameters in decimal degree
!! \author D.C. Agnew \cite Agnew96
!! \date 2012
!! \author M. Rajner - modification
!! \date 2013-03-06
! =============================================================================
subroutine spher_trig ( latin , lonin , distance , azimuth , latout , lonout)
  use mod_constants, only : dp 
  real(dp) , intent(in)  :: distance 
  real(dp) , intent(in)  :: latin , lonin , azimuth
  real(dp) , intent(out) :: latout, lonout 
  real(dp):: sg, cg , saz ,caz , st ,ct , cd ,sd  , cb , sb

  ct  = cos (d2r(90.-latin))
  st  = sin (d2r(90.-latin))
  cd  = cos (d2r(distance))
  sd  = sin (d2r(distance))
  saz = sin (d2r(azimuth))
  caz = cos (d2r(azimuth))
  cb = cd*ct + sd*st*caz
  !  todo !if(abs(cb).gt.1) cb = cb/abs(cb)
  sb = sqrt(1.-cb**2)
  latout = 90 - r2d(acos(cb))
  lonout = lonin + r2d(atan2(sd*saz/sb,(st*cd - sd*ct*caz)/sb))
end subroutine

! =============================================================================
!> For given coordinates for two points on sphere calculate distance and
!! azimuth in radians
!!
!! Input coordinates ub
!! \author M. Rajner
!! \date 2013-03-04
!! for small spherical distances you should always use havesine=.true.
!!
!! All arguments in radians
! =============================================================================
subroutine spher_trig_inverse (lat1, lon1, lat2 , lon2 , distance , azimuth, haversine)
  use mod_constants, only : dp , pi

  real(dp) , intent (in)         :: lat1 , lon1 , lat2 , lon2
  real(dp) , intent (out)        :: distance , azimuth
  real(dp)                       :: dlat , dlon ,  distancetmp , a
  logical, intent(in) , optional :: haversine

  dlon = lon2 - lon1
  dlat = lat2 - lat1

  if (dlon > pi) dlon=dlon-pi  !todo check if this is necessary

  if (present(haversine).and.haversine.eq..true.) then
    ! the formula below from Robert Chamberlain
    ! http://www.usenet-replayer.com/faq/comp.infosystems.gis.html
    a = (sin(dlat/2))**2 + cos(lat1) * cos(lat2) * (sin(dlon/2))**2
    distance = 2 * atan2( sqrt(a), sqrt(1-a) )
  else 
    distance = acos ( sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(dlon))
  endif

  azimuth = atan2( (sin(dlon)*cos(lat2)/sin(distance)) ,  ((sin(lat2)*cos(lat1) - cos(lat2)*sin(lat1)*cos(dlon))/sin(distance))  )
  if (azimuth.lt.0) azimuth = azimuth + 2 * pi
end subroutine

! =============================================================================
!> Count rows and (or) columns of file.
!!
!! You can also specify the comment sign to ignore in data file.
!! The number of columns is set to maximum of number of columns in consecutive
!! rows.
!! \date 2013-03-10
!! \author M. Rajner
! =============================================================================
subroutine count_records_to_read (file_name, rows , columns , comment_char )
  use, intrinsic:: iso_fortran_env
  integer , optional , intent (out) :: rows, columns
  character(*) :: file_name
  character(255) :: line
  integer :: file_unit, n_rows , n_columns , io_stat
  character(len=1), optional, intent(in):: comment_char

  n_rows    = 0
  n_columns = 0

  open (newunit = file_unit,  file=file_name, status = "old" , action ="read")
  do 
    call skip_header (file_unit, '#')
    read (file_unit, '(a)' , iostat=io_stat) line
    if (io_stat == iostat_end) exit
    n_columns = max (n_columns, ntokens(line))
    n_rows = n_rows + 1
  enddo

  if (present(rows))    rows    = n_rows
  if (present(columns)) columns = n_columns
end subroutine


end module
