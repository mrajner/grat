! ==============================================================================
!> Define constant values.
!!
!! This module define some constant values oftenly used.
!! \author M. Rajner
!! \date 2013-03-04
! ==============================================================================
module mod_constants
  implicit none

  integer, parameter :: dp = selected_real_kind(15)
  integer, parameter :: sp = selected_real_kind(6)

  real(dp), parameter ::        &
    R_air  = 287.05,            & ! dry air constant  [J/kg/K]
    pi     = 4.*atan(dble(1.)), &
    T_zero = -273.15

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
    standard    = 101325._dp           & ! Pa (not hectoPascal!)
    ),                                 &
    temperature = temperature_data (   &
    standard    = 288.15_dp            & ! K (15 degC)
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
    real(dp) :: mass
    real(dp) :: radius
    real(dp) :: gm
    type(earth_gravity) :: gravity
    type(earth_density) :: density
  end type

  type(earth_data), parameter ::            &
    earth       = earth_data (              &
    mass        = 5.97219e24_dp,            & ! kg
    radius      = 6371000.,                 & ! m
    gm          = 398600.4419,              & ! m3 s-2
    gravity     = earth_gravity(            &
    mean        = 9.80665                   & ! m s-2
    ),                                      &
    density     = earth_density(            &
    crust       = 2670.,                    & ! kg m-3
    mean        = 5500.                     & ! kg m-3
    )                                       &
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
    distance    = 384000000._dp,            & ! m
    mass        = 7.35e22_dp                & ! kg
    ),                                      &
    sun         = celestial_object_data (   &
    distance    = 149600000000._dp ,        & ! m
    mass        = 1.99e30_dp                & ! kg
    )

  !---------------------------------------
  ! densities
  !---------------------------------------
  type density_info
    real(dp)      :: water
  end type
  type(density_info), parameter :: &
    density = density_info ( &
    water = 1000._dp & ! kg m-3
    )

end module mod_constants
