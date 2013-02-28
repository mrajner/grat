! ==============================================================================
!> \file
!! This module define some constant values used
! ==============================================================================
module mod_constants
  implicit none
  integer , parameter :: dp = 8 !< real (kind_real) => real (kind = 8 )
  integer , parameter :: sp = 4 !< real (kind_real) => real (kind = 4 )
  real(dp) , parameter :: &
    T0          = 288.15,     &  !< surface temperature for standard atmosphere [K] (15 degC)
    g0          = 9.80665,    &  !< mean gravity on the Earth [m/s2]
    r0          = 6356.766,   &  !< Earth radius (US Std. atm. 1976)  [km]
    p0          = 1013.25,    &  !< surface pressure for standard Earth [hPa]
    G           = 6.672e-11,  &  !< Cavendish constant \f$[m^3/kg/s^2]\f$
    R_air       = 287.05,     &  !< dry air constant  [J/kg/K]
    pi          = 4*atan(1.), &  !< pi = 3.141592... [ ]
    rho_crust   = 2670.  ,    &  !< mean density of crust [kg/m3]
    rho_earth   = 5500.          !< mean density of Earth [kg/m3]

  real(dp) :: &
    Earth_mass = 5.97219e24, & ! mass of the Earth
    geocentric_constant = 398600.4419

contains

! ==============================================================================
!> For given vectors x1, y1 and x2, y2 it gives x2interpolated for x1
!!
!! uses \c ispline and \c spline subroutines
! ==============================================================================
subroutine spline_interpolation(x,y, x_interpolated, y_interpolated)
  implicit none
  real(dp) , allocatable , dimension (:) ,intent(in) :: x, y, x_interpolated
  real(dp) , allocatable , dimension (:) , intent(out) :: y_interpolated
  real(dp) , dimension (:) , allocatable :: b, c, d
  integer :: i

  allocate (b (size(x)))
  allocate (c (size(x)))
  allocate (d (size(x)))
  allocate (y_interpolated (size(x_interpolated)))

  call spline ( x , y, b , c, d, size(x))
  
  do i=1, size(x_interpolated)
     y_interpolated(i) = ispline (x_interpolated(i) , x , y , b , c , d , size (x) )
  enddo

end subroutine

! ==============================================================================
!> This subroutine was taken from
!! \todo give source
! ==============================================================================
!  Calculate the coefficients b(i), c(i), and d(i), i=1,2,...,n
!  for cubic spline interpolation
!  s(x) = y(i) + b(i)*(x-x(i)) + c(i)*(x-x(i))**2 + d(i)*(x-x(i))**3
!  for  x(i) <= x <= x(i+1)
!  Alex G: January 2010
!----------------------------------------------------------------------
!  input..
!  x = the arrays of data abscissas (in strictly increasing order)
!  y = the arrays of data ordinates
!  n = size of the arrays xi() and yi() (n>=2)
!  output..
!  b, c, d  = arrays of spline coefficients
!  comments ...
!  spline.f90 program is based on fortran version of program spline.f
!  the accompanying function fspline can be used for interpolation
! ==============================================================================
subroutine spline (x, y, b, c, d, n)
  implicit none
  integer n
  real(dp) :: x(n), y(n), b(n), c(n), d(n)
  integer i, j, gap
  real ::  h

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
!> This subroutine was taken from
!! \todo give source
! ==============================================================================
!======================================================================
! function ispline evaluates the cubic spline interpolation at point z
! ispline = y(i)+b(i)*(u-x(i))+c(i)*(u-x(i))**2+d(i)*(u-x(i))**3
! where  x(i) <= u <= x(i+1)
!----------------------------------------------------------------------
! input..
! u       = the abscissa at which the spline is to be evaluated
! x, y    = the arrays of given data points
! b, c, d = arrays of spline coefficients computed by spline
! n       = the number of data points
! output:
! ispline = interpolated value at point u
!=======================================================================
function ispline(u, x, y, b, c, d, n)
implicit none
real ispline
integer n
real(dp)::  u, x(n), y(n), b(n), c(n), d(n)
integer ::  i, j, k
real :: dx

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
ispline = y(i) + dx*(b(i) + dx*(c(i) + dx*d(i)))
end function ispline

! ==============================================================================
!> taken from ArkM http://www.tek-tips.com/viewthread.cfm?qid=1688013
! ==============================================================================
integer function ntokens(line)
character,intent(in):: line*(*)
integer i, n, toks

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
  use iso_fortran_env
  implicit none
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

!> downloaded from  http://aa.usno.navy.mil/faq/docs/jd_formula.php
!! \todo mjd!
real function jd (year,month,day, hh,mm,ss)
  implicit none
  integer, intent(in) ::  year,month,day
  integer, intent(in) :: hh,mm, ss
  integer :: i , j , k
  i= year
  j= month
  k= day
  jd= k-32075+1461*(i+4800+(j-14)/12)/4+367*(j-2-(j-14)/12*12)/12-3*((i+4900+(j-14)/12)/100)/4  + (hh/24.) &
  + mm/(24.*60.) +ss/(24.*60.*60.) ! - 2400000.5
  return
end function

real(dp) function mjd  (date)
  implicit none
  integer ,intent(in) :: date (6)
  integer :: aux (6)
  integer :: i , k
  real(dp) :: dayfrac

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

subroutine invmjd (mjd , date)
  implicit none
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

end module mod_constants
