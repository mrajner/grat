module mod_atmosphere
  implicit none

contains

! ==============================================================================
!> Compute air density for given altitude for standard atmosphere
!!
!! using formulae 12 in \cite Huang05
!! \date 2013-03-18
!! \author M. Rajner
!! height in meter
! ==============================================================================
function standard_density ( height , t_zero ,fels_type , method)
  use mod_constants, only: dp , R_air
  real(dp) , intent(in)  ::  height 
  real(dp) , intent(in), optional  :: t_zero 
  character(len = 22) , optional :: fels_type
  character(len = *) , optional :: method
  real(dp) :: standard_density 
  real(dp) :: p ,t

  t = standard_temperature (height, t_zero = t_zero, fels_type=fels_type)
  p = standard_pressure    (height, t_zero = t_zero, method=method)
  standard_density = p  / ( R_air * t )
end function
! =============================================================================
!> \brief Compute gravity acceleration of the Earth
!! for the specific height using formula
!! 
!! see \cite US1976 
!! height in meters
! =============================================================================
function standard_gravity (height)
  use mod_constants, only: dp, earth
  real(dp), intent(in)  :: height
  real(dp) :: standard_gravity

  standard_gravity = earth%gravity%mean * (earth%radius/(earth%radius + height))**2
end function


! =============================================================================
!> \brief Computes pressure [Pa] for specific height
!!
!! See \cite US1976 or  \cite Huang05 for details.
!! Uses formulae 5 from \cite Huang05.
!!
!! \warning pressure in Pa, height in meters
! =============================================================================
function standard_pressure (height,  &
    p_zero, t_zero, h_zero, method, dz)

  use mod_constants, only: dp, earth, atmosphere, R_air
  implicit none
  real(dp) , intent(in)            :: height
  real(dp) , intent(in) , optional :: t_zero , p_zero , h_zero
  character(*), intent(in) , optional :: method
  real(dp), intent(in) , optional :: dz
  real(dp) :: standard_pressure
  real(dp) :: lambda , sfc_height , sfc_temperature , sfc_gravity , alpha , sfc_pressure
  real(dp) :: z_, dz_

  sfc_temperature = atmosphere%temperature%standard
  sfc_pressure    = atmosphere%pressure%standard
  sfc_height      = 0.
  sfc_gravity     = earth%gravity%mean

  if (present(h_zero)) then
    sfc_height      = h_zero
    sfc_temperature = standard_temperature (sfc_height)
    sfc_gravity     = standard_gravity (sfc_height)
  endif

  if (present(p_zero)) sfc_pressure = p_zero
  if (present(t_zero)) sfc_temperature = t_zero


  alpha = -6.5e-3
  if (present (method)) then
    select case (method)
    case("berg")
      standard_pressure = sfc_pressure *(1-0.0000226 * (height -sfc_height))**(5.225)
    case ("simple")
      standard_pressure = sfc_pressure * exp (- (height -sfc_height) *standard_gravity(height) /standard_temperature(height) / R_air   ) 
    case ("full")
      standard_pressure=0.
      if (present(dz)) then
        dz_ = dz
      else
        dz_ = 0.1
      endif
      do z_=0, height, dz_
        standard_pressure = standard_pressure &
          + standard_gravity(z_)/(R_air * standard_temperature(z_)) *dz_
      enddo
      standard_pressure=atmosphere%pressure%standard * exp(-standard_pressure)
    case default
      stop "Method not known"
    endselect
  else
    !http://en.wikipedia.org/wiki/Barometric_formula
    standard_pressure= sfc_pressure &
      * (sfc_temperature /(sfc_temperature+alpha*(height-sfc_height))) &
      **(earth%gravity%mean /R_air/alpha)
  endif 

  !todo incorporate this
  !  Zdunkowski and Bott
  !  p(z) = p0 (T0-gamm z )/T0
  if (isnan(standard_pressure)) standard_pressure=0

end function

! =============================================================================
!> \brief Compute standard temperature [K] for specific height [km]
!!
!! if t_zero is specified use this as surface temperature
!! otherwise use T0.
!! A set of predifined temperature profiles ca be set using
!! optional argument \argument fels_type 
!! \cite Fels86
!! \li US standard atmosphere (default)
!! \li tropical 
!! \li subtropical_summer
!! \li subtropical_winter
!! \li subarctic_summer
!! \li subarctic_winter
! ==============================================================================
function standard_temperature ( height, t_zero , fels_type )
  use mod_constants, only: dp , earth, atmosphere
  use mod_printing, only : log

  real(dp) , intent(in)  :: height
  real(dp)  :: standard_temperature
  real(dp) , intent(in), optional  :: t_zero
  character (len=*) , intent(in), optional  :: fels_type 
  real(dp) :: aux , cn , t
  integer :: i,indeks
  real(dp) , dimension (10) :: z,c,d

  ! Read into memory the parameters of temparature height profiles
  ! for standard atmosphere 
  z = (/11.0 , 20.1 , 32.1 , 47.4  , 51.4 , 71.7  , 85.7  , 100.0 , 200.0 , 300.0 /)
  c = (/-6.5 ,  0.0 ,  1.0 ,  2.75 ,  0.0 , -2.75 , -1.97 ,   0.0 ,   0.0 ,   0.0 /)
  d = (/ 0.3 ,  1.0 ,  1.0 ,  1.0  ,  1.0 ,  1.0  ,  1.0  ,   1.0 ,   1.0 ,   1.0 /)
  t = atmosphere%temperature%standard

  if ( present (fels_type)) then
    if (fels_type .eq. "US1976" ) then
      elseif (fels_type .eq. "tropical" ) then
      z=(/ 2.0 ,  3.0 , 16.5 , 21.5 , 45.0 , 51.0 , 70.0 , 100.0  , 200.0 , 300.0 /)
      c=(/-6.0 , -4.0 , -6.7 ,  4.0 ,  2.2 ,  1.0 , -2.8 ,  -0.27 ,   0.0 ,   0.0 /)
      d=(/ 0.5 ,  0.5 ,  0.3 ,  0.5 ,  1.0 ,  1.0 ,  1.0 ,   1.0  ,   1.0 ,   1.0 /)
      t=300.0
      elseif (fels_type .eq. "subtropical_summer" ) then
      z = (/ 1.5 ,  6.5 , 13.0 , 18.0 , 26.0 , 36.0 , 48.0 , 50.0 , 70.0 , 100.0   /)
      c = (/-4.0 , -6.0 , -6.5 ,  0.0 ,  1.2 ,  2.2 ,  2.5 ,  0.0 , -3.0 ,  -0.025 /)
      d = (/ 0.5 ,  1.0 ,  0.5 ,  0.5 ,  1.0 ,  1.0 ,  2.5 ,  0.5 ,  1.0 ,   1.0   /)
      t = 294.0
      elseif (fels_type .eq. "subtropical_winter" ) then
      z = (/ 3.0 , 10.0 , 19.0 , 25.0 , 32.0 , 44.5 , 50.0 , 71.0 , 98.0 , 200.0 /)
      c = (/-3.5 , -6.0 , -0.5 ,  0.0 ,  0.4 ,  3.2 ,  1.6 , -1.8 ,  0.7 ,   0.0 /)
      d = (/ 0.5 ,  0.5 ,  1.0 ,  1.0 ,  1.0 ,  1.0 ,  1.0 ,  1.0 ,  1.0 ,   1.0 /)
      t = 272.2
      elseif (fels_type .eq. "subarctic_summer" ) then
      z = (/ 4.7 , 10.0 , 23.0 , 31.8 , 44.0 , 50.2 , 69.2 , 100.0 , 200.0 , 300.0 /)
      c = (/-5.3 , -7.0 ,  0.0 ,  1.4 ,  3.0 ,  0.7 , -3.3 ,  -0.2 ,   0.0 ,   0.0 /)
      d = (/ 0.5 ,  0.3 ,  1.0 ,  1.0 ,  2.0 ,  1.0 ,  1.5 ,   1.0 ,   1.0 ,   1.0 /)
      t = 287.0
      elseif (fels_type .eq. "subarctic_winter" ) then
      z = (/  1.0 ,  3.2 ,  8.5 , 15.5 , 25.0 , 30.0 , 35.0 , 50.0 , 70.0 , 100.0 /)
      c = (/  3.0 , -3.2 , -6.8 ,  0.0 , -0.6 ,  1.0 ,  1.2 ,  2.5 , -0.7 ,  -1.2 /)
      d = (/  0.4 ,  1.5 ,  0.3 ,  0.5 ,  1.0 ,  1.0 ,  1.0 ,  1.0 ,  1.0 ,   1.0 /)
      t = 257.1
    else
      write(log%unit, *) , "unknown fels_type argument: &
        using US standard atmosphere 1976 instead"
    endif
  endif

  if (present (t_zero) ) then
    t=t_zero
  endif

  do i=1,10
    if (height/1000..le.z(i)) then
      indeks=i
      exit
    endif
  enddo

  aux = 0.
  do i = 1 , indeks
    if (i.eq.indeks) then
      cn = 0.
    else
      cn = c(i+1)
    endif
    aux = aux + d(i) * ( cn - c(i) )  * dlog ( dcosh ( (height/1000. - z(i)) / d(i) ) / dcosh (z(i)/d(i)) ) 
  enddo
  standard_temperature = t + c(1) * (height/1000.)/2. + aux/2.
end function

! =============================================================================
!> Compute geometric height from geopotential heights
!!
!! \author M. Rajner
!! \date 2013-03-19
! =============================================================================
real(dp) function geop2geom (geopotential_height, inverse)
  use mod_constants, only: dp, earth
  real (dp) :: geopotential_height
  logical, intent(in), optional:: inverse

  if (present(inverse) .and. inverse) then
    geop2geom = geopotential_height * &
      (earth%radius / (earth%radius + geopotential_height))**2
  else
    geop2geom = geopotential_height / &
      (earth%radius / (earth%radius + geopotential_height))**2
  endif
end function

end module
