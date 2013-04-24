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

  real(dp) :: &
    Earth_mass =               5.97219e24, &   ! mass of the Earth
    geocentric_constant = 398600.4419          ! GM_Earth

  type celestial_object_data
    real(dp)      :: mass
    real(dp)      :: distance
  end type

  type(celestial_object_data) :: moon
!  moon%distance=2.
  

end module mod_constants
