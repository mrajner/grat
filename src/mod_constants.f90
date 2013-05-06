! ==============================================================================
!> Define constant values.
!! 
!! This module define some constant values oftenly used.
!! \author M. Rajner
!! \date 2013-03-04
! ==============================================================================
module mod_constants
  use , intrinsic :: ieee_arithmetic
  implicit none

  integer , parameter :: dp = 8 !< real (kind_real) => real (kind = 8 )
  integer , parameter :: sp = 4 !< real (kind_real) => real (kind = 4 )

  real(dp) , parameter :: &
    T0          = 288.15,     &  !< surface temperature for standard atmosphere [K] (15 degC)
    g0          = 9.80665,    &  !< mean gravity on the Earth [m/s2]
    r0          = 6356.766,   &  !< Earth radius (US Std. atm. 1976)  [km]
    p0          = 1013.25,    &  !< surface pressure for standard Earth [hPa]
    R_air       = 287.05,     &  !< dry air constant  [J/kg/K]
    pi          = 4*atan(dble(1.))     !< pi = 3.141592... []


  !---------------------------------------
  ! gravity
  !---------------------------------------
  type gravity_data
    real(dp) :: constant 
  end type
  type(gravity_data) , parameter :: & 
    gravity  = gravity_data(          & 
    constant = 6.674e-11                     & ! m3 kg-1 s-2 TODO find new value ~6.674e-11?
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


  type(earth_data), parameter :: & 
    earth   = earth_data (        &
    mass    = 5.97219e24,                    & ! kg
    radius  = 6371000.,                     & ! m
    gm      = 398600.4419,                   & ! m3 s-2
    gravity = earth_gravity( &
    mean    = 9.80665 & ! m s-2
    ), &
    density = earth_density( &
    crust    = 2670., & ! kg m-3
    mean     = 5500. & ! kg m-3
    ) &
    )

  !---------------------------------------
  ! celestial bodies
  !---------------------------------------
  type celestial_object_data
    real(dp)      :: mass
    real(dp)      :: distance
  end type
  type(celestial_object_data), parameter :: & 
    moon     = celestial_object_data (        & 
    distance = 384000000,                     & ! m
    mass     = 7.35e22                        & ! kg
    ),                                        & 
    sun      = celestial_object_data (        & 
    distance = 149600000000 ,                 & ! m
    mass     = 1.99e30                        & ! kg
    )

end module mod_constants
