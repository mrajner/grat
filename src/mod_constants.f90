! ==============================================================================
!> Define constant values.
!! 
!! This module define some constant values oftenly used.
!! \author M. Rajner
!! \date 2013-03-04
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


  type gravity_description
    real(dp) :: constant
  end type
  type(gravity_description) , parameter :: & 
    gravity  = gravity_description(          & 
    constant = 6.672e-11                     & ! m3 kg-1 s-2 TODO find new value ~6.674e-11?
    )

  !---------------------------------------
  ! celestial bodies
  !---------------------------------------
  type celestial_object_data
    real(dp)      :: mass
    real(dp)      :: distance
    real(dp)      :: radius
    real(dp)      :: gm 
  end type

  type(celestial_object_data), parameter :: & 
    earth    = celestial_object_data (        & 
    distance = 0 ,                            & 
    mass     = 5.97219e24,                    & ! kg
    gm       = 398600.4419 ,                  & ! m3 s-2
    radius   = 6371000                        & ! m
    ) ,                                       & 
    moon     = celestial_object_data (        & 
    distance = 384000000,                     & ! m
    mass     = 7.35e22,                       & ! kg
    gm       = 0 ,                            & ! m3 s-2
    radius   = 0                              & 
    ),                                        & 
    sun      = celestial_object_data (        & 
    distance = 149600000000 ,                 & ! m
    mass     = 1.99e30,                       & ! kg
    gm       = 0,                             & ! m3 s-2
    radius   = 0                              & 
    )

end module mod_constants
