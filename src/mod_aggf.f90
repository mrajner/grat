! ==============================================================================
!> \file
!! \brief This module contains utitlities for computing
!! Atmospheric Gravity Green Functions
!!
!! In this module there are several subroutines for computing
!! AGGF and standard atmosphere parameters
! ==============================================================================
module mod_aggf
  implicit none

contains

! ==============================================================================
!> Compute first derivative of AGGF with respect to temperature
!! for specific angular distance (psi)
!!
!! optional argument define (-dt;-dt) range
!! See equation 19 in \cite Huang05
!! \author M. Rajner
!! \date 2013-03-19
!! \warning psi in radians
! ==============================================================================
function aggfdt (psi, deltat, dz , method)
  use mod_constants, only: atmosphere, dp
  real(dp), intent (in) :: psi
  real(dp), intent (in), optional :: deltat
  real(dp), intent (in), optional :: dz
  real(dp) :: aggfdt
  real(dp) :: deltat_ 
  character (len=*) , intent(in), optional  :: method

  deltat_ = 10. ! Default value
  if (present(deltat))  deltat_ = deltat
  aggfdt = ( &
    + aggf (psi, t_zero = atmosphere%temperature%standard+deltat_, dz=dz, method = method )  &
    - aggf (psi, t_zero = atmosphere%temperature%standard-deltat_, dz=dz, method = method)) &
    / ( 2. * deltat_)
end function

! ==============================================================================
!> This subroutine computes the value of atmospheric gravity green functions
!! (AGGF) on the basis of spherical distance (psi)
!! \author Marcin Rajner
!! \date 2013.07.15
!! \warning psi in radians h in meter
! ==============================================================================
function aggf ( &
    psi, &
    zmin, zmax, dz, &
    t_zero, &
    h, first_derivative_h, first_derivative_z, fels_type, &
    method)

  use mod_constants, only: dp, pi, earth, gravity, atmosphere, R_air
  use mod_utilities, only: d2r
  use mod_atmosphere 
  use mod_green, only : green_normalization

  real(dp), intent(in)          :: psi       ! spherical distance from site   [degree]
  real(dp), intent(in),optional :: & 
    zmin ,  & ! minimum height, starting point [m]     (default = 0)
    zmax ,  & ! maximum height, ending point   [m]     (default = 60000)
    dz ,    & ! integration step               [m]     (default = 0.1 -> 10 cm)
    t_zero, & ! temperature at the surface     [K]     (default = 288.15=t0)
    h         ! station height                 [m]     (default = 0)
  logical, intent(in), optional ::  first_derivative_h , first_derivative_z
  character (len=*) , intent(in), optional  :: fels_type , method
  real(dp) :: aggf
  real(dp) :: zmin_, zmax_, dz_ , h_
  real(dp) :: J_aux
  real(dp) :: dA, z_ , rho , l , z

  real(dp), dimension(:), allocatable,save :: heights, pressures
  integer :: i

  zmin_ = 0.
  zmax_ = 60000.
  dz_   = 0.1
  h_    = 0.

  if (present(zmin)) zmin_ = zmin
  if (present(zmax)) zmax_ = zmax
  if (present(  dz))   dz_ = dz
  if (present(   h))    h_ = h

  if(allocated(heights)) then
    if ( &
      ((zmin_ +dz_/2).ne.heights(1)) &
      .or.(zmax_-dz_/2).ne.heights(size(heights)) &
      .or.int((zmax_-zmin_)/dz_).ne.size(heights) &
      .or.present(t_zero) &
      ) then
      deallocate(heights)
      deallocate(pressures)
    endif
  endif

  if (.not.allocated(heights))  then
    allocate(heights(int((zmax_-zmin_)/dz_)))
    allocate(pressures(int((zmax_-zmin_)/dz_)))
    do i = 1, size(heights)
      heights(i) = zmin_ &
        + dz_/2  &
        + (i-1) * dz_
    enddo
    pressures(1) = standard_pressure(heights(1),method=method, t_zero=t_zero)
    do i = 2 , size(heights)
      pressures(i) = standard_pressure(heights(i),p_zero=pressures(i-1),h_zero = heights(i-1),method=method, t_zero=t_zero)
    enddo
  endif

  aggf = 0.
  do i = 1 , size(heights)
    l = ((earth%radius + heights(i))**2 + (earth%radius + h_)**2 & 
      - 2.*(earth%radius + h_)*(earth%radius+heights(i))*cos(psi))**(0.5)
    rho = pressures(i)/ R_air / standard_temperature(heights(i) , t_zero=t_zero)  
    if ( present ( first_derivative_h) .and. first_derivative_h ) then
      ! first derivative (respective to station height)
      ! micro Gal height / m
      ! see equation 22, 23 in \cite Huang05
      J_aux =  ((earth%radius + heights(i) )**2)*(1.-3.*((cos(psi))**2)) -2.*(earth%radius + h_)**2  &
        + 4.*(earth%radius+h_)*(earth%radius+heights(i))*cos(psi)
      aggf =  aggf + rho * (  J_aux  /  l**5  ) * dz_

      ! direct derivative of equation 20 \cite Huang05      
      !          J_aux = (2.* (earth%radius ) - 2 * (earth%radius +z )*cos(psir)) / (2. * r)
      !          J_aux =  -r - 3 * J_aux * ((earth%radius+z)*cos(psir) - earth%radius)
      !          aggf =  aggf +  rho * (  J_aux  /  r**4  ) * dz 
      !        else
      !      ! first derivative (respective to column height)
      !      ! according to equation 26 in \cite Huang05
      !      ! micro Gal / hPa / km
      !      if (present (first_derivative_z) .and. first_derivative_z) then
      !        if (z.eq.h_min) then
      !          aggf = aggf  &
      !            + rho*( ((earth%radius + z)*cos(psir) - ( earth%radius + h_station ) ) / ( r**3 ) ) 
      !        endif
    else
      ! GN microGal/hPa
      aggf = aggf -  &
        rho * ((earth%radius +heights(i))*cos(psi) - (earth%radius + h_)) / (l**3.)  * dz_ 
    endif
  enddo
  aggf = aggf /atmosphere%pressure%standard *gravity%constant * green_normalization("m", psi = psi) 
end function

! ==============================================================================
!> Compute AGGF GN for thin layer
!!
!! Simple function added to provide complete module
!! but this should not be used for atmosphere layer
!! See eq p. 491 in \cite Merriam92
!! \author M. Rajner
!! \date 2013-03-19
!! \warning psi in radian
!! \todo explanaition ?? 
! ==============================================================================
function GN_thin_layer (psi)
  use mod_constants, only: dp
  real(dp), intent(in) :: psi
  real(dp) :: GN_thin_layer

  GN_thin_layer = 1.627 * psi / sin ( psi / 2. )
end function


! ==============================================================================
!> \brief Bouger plate computation
!!
! ==============================================================================
real(dp) function bouger (h, R )
  use mod_constants, only: dp, gravity, pi
  real(dp), intent(in), optional :: R !< height of point above the cylinder
  real(dp), intent(in) ::  h 

  if (present( R ) ) then
    bouger = h + R - sqrt(R**2+H**2)
  else
    bouger = h
  endif
  bouger = 2 * pi * gravity%constant * bouger
  return
end function

! ==============================================================================
!> Bouger plate computation
!!
!! see eq. page 288 \cite Warburton77
!! \date 2013-03-18
!! \author M. Rajner 
! ==============================================================================
function simple_def (R)
  use mod_constants, only: dp, earth
  real(dp) :: R ,delta
  real(dp) :: simple_def

  delta = 0.22e-11 * R 
  simple_def = earth%gravity%mean / earth%radius *1000 * &
    delta * ( 2. - 3./2. * earth%density%crust / earth%density%mean &
    -3./4. * earth%density%crust / earth%density%mean * sqrt (2* (1. )) &
    ) * 1000
end function

end module
