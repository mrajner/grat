module mod_3d
    use mod_constants, only: dp
    implicit none

    ! Gitlein <0.5° 0.05x0.05
    !         <10° 0.1x0.1
contains

! =============================================================================
!> all values in radians 
! see formula Neumeyer et al., 2004 p. 442-443
! =============================================================================
subroutine point_mass (site, date)
  use mod_site, only : site_info
  use mod_date, only : dateandmjd
  use mod_utilities, only: d2r, logspace
  use mod_atmosphere
  use mod_constants, only: R_air, gravity, earth
  use mod_spherical, only: spher_trig, spher_area

  type (site_info) :: site
  type(dateandmjd),intent(in), optional :: date

  real(dp) :: val, volume
  integer  :: idistance, ndistance
  real(dp) :: distance, ddistance
  integer  :: iazimuth, nazimuth
  real(dp) :: azimuth, dazimuth, maxdistance, mindistance
  integer  :: iheight, nheight
  real(dp) :: height, dheight, maxheight, minheight


  print *
  maxdistance=0.5
  mindistance=0.0

  maxheight=39002
  minheight=10


  ddistance=0.05
  dazimuth=360
  dheight=0.1


  ndistance=ceiling((maxdistance-mindistance)/ddistance)
  nazimuth=ceiling(360/dazimuth)
  nheight=ceiling((maxheight-minheight)/dheight)

  ddistance=(maxdistance-mindistance)/ndistance
  dheight=(maxheight-minheight)/nheight
  dazimuth=360./nazimuth


  val=0
  do idistance = 1, ndistance
    distance=mindistance+ idistance*ddistance-ddistance/2

    volume = &
        spher_area(                        &
        d2r(distance-ddistance/2), &
        d2r(distance+ddistance/2), &
        d2r(dazimuth),                          &
        radius=earth%radius,                    &
        alternative_method=.true.) &
        * dheight

    do iazimuth = 1, nazimuth
      azimuth = (iazimuth-1)*dazimuth
      do iheight=1,nheight
        height=minheight+(iheight -1)*dheight


        !todo top bottom press
        val=val &
            + geometry(psi=d2r(distance), h=site%height, z=height) &
            *( &
            ( &
            standard_pressure(height,p_zero=101425._dp, method="standard", use_standard_temperature=.true.) &
            +standard_pressure(height+dheight,p_zero=101425._dp, method="standard", use_standard_temperature=.true.) &
            )/2 &
            - &
            ( &
            ( &
            standard_pressure(height, method="standard", use_standard_temperature=.true.) &
            +standard_pressure(height+dheight, method="standard", use_standard_temperature=.true.) &
            )/2 &
            ) &
            ) &
            /(R_air*standard_temperature(height))  &
            * volume

      enddo
    enddo
  enddo
  val=val*gravity%constant*1e8
  print*, ddistance,dheight,val

end subroutine


! =============================================================================
!> all values in radians 
! =============================================================================
real(dp) function geometry (psi, h, z, method)
  use mod_constants, only: earth
  real(dp) &
      ! intent(in)
  :: psi, h, z
  character(*), optional :: method
  real(dp) :: l, gamma

  h=100
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
