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
      gamma    = atan(((z-h)*cos(psi/2))/((2*earth%radius+z)*sin(psi/2)))
      geometry = sin(gamma-psi/2)/(2*(earth%radius+h) * sin(psi/2)*cos(gamma)+(z-h)*sin(psi/2+gamma))**2
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
real(dp) function potential (psi1, psi2, dazimuth, h, z1, z2)
  use mod_constants, only: earth, pi

  real(dp), intent(in) :: psi1, psi2, h, z1, z2, dazimuth
  real(dp) :: r1, r2, r , s1, s2, s3, s4, n1, n2, n3, n4, l1, l2, l3, l4

  r  = earth%radius+h
  r1 = earth%radius+z1
  r2 = earth%radius+z2

  s1 = sqrt(r1**2+r**2-2*r1*r*cos(psi1))
  s2 = sqrt(r2**2+r**2-2*r2*r*cos(psi1))
  s3 = sqrt(r1**2+r**2-2*r1*r*cos(psi2))
  s4 = sqrt(r2**2+r**2-2*r2*r*cos(psi2))

  n1 = 2*r1**2-r**2+2*r*r1*cos(psi1)+3*r**2*cos(2*psi1)
  n2 = 2*r2**2-r**2+2*r*r2*cos(psi1)+3*r**2*cos(2*psi1)
  n3 = 2*r1**2-r**2+2*r*r1*cos(psi2)+3*r**2*cos(2*psi2)
  n4 = 2*r2**2-r**2+2*r*r2*cos(psi2)+3*r**2*cos(2*psi2)

  l1 = log(r1-r*cos(psi1)+s1)*6*r**3*cos(psi1)*(sin(psi1))**2
  l2 = log(r2-r*cos(psi1)+s2)*6*r**3*cos(psi1)*(sin(psi1))**2
  l3 = log(r1-r*cos(psi1)+s3)*6*r**3*cos(psi2)*(sin(psi2))**2
  l4 = log(r2-r*cos(psi1)+s4)*6*r**3*cos(psi2)*(sin(psi2))**2

  potential= &
    s1*n1-s2*n2-s3*n3+s4*n4 &
    -l1+l2+l3-l4

  potential = -potential* dazimuth / (6.*r**2)
end function

! =============================================================================
!> all values in radians
!! second improved version of cylinder, includes curvature of the earth
! =============================================================================
real(dp) function cylinder (psi1,psi2, dazimuth, h, z1, z2)
  use mod_constants, only: earth, pi
  real(dp), intent(in) :: psi1, psi2, dazimuth, h, z1, z2
  real(dp) :: psi, hh, zz1, zz2
  real(dp) :: r1, r2

  r1=(earth%radius+z1)*sin(psi1)
  r2=(earth%radius+z2)*sin(psi2)

  psi=psi1/2.+psi2/2.
  zz1=(earth%radius+z1)*cos(psi)
  zz2=(earth%radius+z2)*cos(psi)
  hh=(earth%radius+h)

  cylinder = -(sqrt((zz1-hh)**2+r1**2) &
    -sqrt((zz1-hh)**2+r2**2)) &
    +(sqrt((zz2-hh)**2+r1**2) &
    -sqrt((zz2-hh)**2+r2**2))
  cylinder = dazimuth * cylinder
end function

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
