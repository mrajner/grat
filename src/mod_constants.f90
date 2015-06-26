! ==============================================================================
!> This module define some constant values oftenly used.
!! \author M. Rajner
!! \date 2013-03-04
! ==============================================================================
module mod_constants

  implicit none

  integer, parameter :: dp = selected_real_kind(15)
  integer, parameter :: sp = selected_real_kind(6)

  real(dp), parameter ::       & 
    R_air  = 287.05_dp,        & ! dry air constant  [J/kg/K]
    pi     = 4._dp*atan(1._dp)   ! 3.1415...

  real(dp), parameter :: speed_of_light = 299792458._dp ! [m/s]
  

  !---------------------------------------
  ! gravity
  !---------------------------------------
  type gravity_data
    real(dp) :: constant
  end type

  type(gravity_data) , parameter :: &
    gravity  = gravity_data(        &
    constant = 6.674e-11_dp         & ! m3 kg-1 s-2
    )

  !---------------------------------------
  ! Atmosphere
  !---------------------------------------
  type pressure_data
    real(dp) :: standard
  end type

  type temperature_data
    real(dp) :: standard
  end type

  type atmosphere_data
    type(pressure_data)    :: pressure
    type(temperature_data) :: temperature
  end type

  type(atmosphere_data) , parameter :: &
    atmosphere  = atmosphere_data (    &
    pressure    = pressure_data (      &
    standard    = 101325._dp           & ! Pa (not hectopascal!)
    ),                                 &
    temperature = temperature_data (   &
    standard    = 288.15_dp            & ! K (15°C)
    )                                  &
    )

  !---------------------------------------
  ! Earth
  !---------------------------------------
  type earth_gravity
    real(dp) :: mean
  end type

  type earth_density
    real(dp) :: crust
    real(dp) :: mean
  end type

  type earth_data
    real(dp) :: mass, radius, gm, angular_velocity
    type(earth_gravity) :: gravity
    type(earth_density) :: density
  end type

  type(earth_data), parameter ::           & 
    earth            = earth_data (        & 
    mass             = 5.97219e24_dp,      & !kg
    radius           = 6371000._dp,        & !m
    gm               = 3.986004419e14_dp,  & ! ^3/s^2
    angular_velocity = 7.2921151467e-5_dp, & ! rad/s
    gravity          = earth_gravity(      & 
    mean             = 9.80665_dp          & !m s-2
    ),                                     & 
    density          = earth_density(      & 
    crust            = 2670._dp,           & !kg m-3
    mean             = 5500._dp            & !kg m-3
    )                                      & 
    )

  !---------------------------------------
  ! celestial bodies
  !---------------------------------------
  type celestial_object_data
    real(dp)      :: mass
    real(dp)      :: distance
  end type

  type(celestial_object_data), parameter :: &
    moon        = celestial_object_data (   &
    distance    = 384.e6_dp,                & !m
    mass        = 7.35e22_dp                & !kg
    ),                                      &
    sun         = celestial_object_data (   &
    distance    = 149.6e9_dp,               & !m
    mass        = 1.99e30_dp                & !kg
    )

  !---------------------------------------
  ! densities
  !---------------------------------------
  type density_info
    real(dp) :: water
  end type

  type(density_info), parameter :: & 
  density = density_info (         & 
  water   = 1000._dp               & ! kg m-3
    )


contains

! ==============================================================================
!> naive method to force NaN for compilers
! ==============================================================================
real(dp) function setnan()
  real(dp) :: minusone=-1._dp

  setnan = sqrt(minusone)
end function

end module mod_constants
