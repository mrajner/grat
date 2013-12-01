module mod_3d
    use mod_constants, only: dp
    implicit none

    ! Gitlein <0.5° 0.05x0.05
    !         <10° 0.1x0.1
contains

! =============================================================================
!> all values in radians 
! =============================================================================
real(dp) function geometry (psi, h, z, method)
  use mod_constants, only: earth
  real(dp), intent(in) :: psi, h, z
  character(*), optional :: method
  real(dp) :: l, gamma

  if(present(method)) then
    if (method.eq."klugel") then
      gamma=atan(((z-h)*cos(psi/2))/((2*earth%radius+z)*sin(psi/2)))
      geometry=sin(gamma-psi/2)/(2*(earth%radius+h) * sin(psi/2)*cos(gamma)+(z-h)*sin(psi/2+gamma))**2
    else
      stop "method not known"
    endif
  else
    l = ((earth%radius + h)**2 + (earth%radius + z)**2 & 
        - 2.*(earth%radius+h)*(earth%radius+z)*cos(psi))**(0.5)
    geometry = ((earth%radius +z)*cos(psi) - (earth%radius + h))/l**3.
  endif
end function

! =============================================================================
!> all values in radians 
! =============================================================================
! real(dp) function potential (psi, h, z, method)
  ! use mod_constants, only: earth
  ! real(dp) &
      ! intent(in)
  ! :: psi, h, z
  ! character(*), optional :: method
  ! real(dp) :: l, gamma

  ! if(present(method)) then
    ! if (method.eq."klugel") then
      ! gamma=atan(((z-h)*cos(psi/2))/((2*earth%radius+z)*sin(psi/2)))
      ! geometry=sin(gamma-psi/2)/(2*(earth%radius+h) * sin(psi/2)*cos(gamma)+(z-h)*sin(psi/2+gamma))**2
    ! else
      ! stop "method not known"
    ! endif
  ! else
    ! l = ((earth%radius + h)**2 + (earth%radius + z)**2 & 
        ! - 2.*(earth%radius+h)*(earth%radius+z)*cos(psi))**(0.5)
    ! geometry = ((earth%radius +z)*cos(psi) - (earth%radius + h))/l**3.
  ! endif
! end function

! =============================================================================
!> all values in radians 
!! see formula Neumeyer et al., 2004 p. 442-443
!! this formula is identical as geometry in this module but is uses the
!! geographical coordinates
!! =============================================================================
real(dp) function point_mass_a (theta_s, lambda_s, height_s, theta, lambda, height)
  use mod_constants, only: earth, pi
  real (dp) :: theta_s, lambda_s, height_s ! site
  real (dp) :: theta, lambda, height       ! atmosphere cell
  real(dp) :: r_s, r, aux

  aux=sin(pi/2.-theta_s)*sin(pi/2.-theta) &
      * (cos(pi/2.-lambda_s)*cos(pi/2.-lambda) + sin(pi/2.-lambda_s)*sin(pi/2.-lambda)) &
      + cos(pi/2.-theta_s)*cos(pi/2.-theta)

  r_s=earth%radius+height_s
  r=earth%radius+height

  point_mass_a= &
      (r_s - r*aux) &
      / (r_s**2 + r**2 -2*(r_s)*r*aux)**(3./2.) 

end function
end module mod_3d
