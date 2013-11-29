module mod_3d
    use mod_constants, only: dp
    implicit none

contains

! =============================================================================
!> all values in radians 
! see formula Neumeyer et al., 2004 p. 442-443
! =============================================================================
subroutine point_mass (site, date)
  use mod_site, only : site_info
  use mod_date, only : dateandmjd
  use mod_utilities, only: d2r
  use mod_atmosphere
  use mod_constants, only: R_air, gravity, earth
  type (site_info) :: site
  type(dateandmjd),intent(in), optional :: date

  real(dp) :: lat, lon, height
  real(dp) :: dhor, sizehor, dheight, sizeheight
  integer :: ilat, ilon, iheight, nhor, nheight
  real(dp) :: val

  dhor=1
  sizehor=0.5
  nhor=sizehor/dhor

  dheight=100
  sizeheight=60000
  !delete
  sizeheight=9000
  nheight=sizeheight/dheight

  val=0
  do ilat=-nhor,nhor
    lat=d2r(site%lon)+d2r(ilat*dhor)
    do ilon=-nhor, nhor
      lon=d2r(site%lon)+d2r(ilon*dhor)
      do iheight=1,nheight
        height=iheight*dheight
        
 print *, point_mass_a (d2r(site%lat), d2r(site%lon), site%height, lat, lon, height) 

        ! return
        ! val = val &
            ! + standard_pressure(height, method="standard", nan_as_zero=.true.) &
            ! /(R_air* standard_temperature(height)) &
            ! * point_mass_a (d2r(site%lat), d2r(site%lon), site%height, lat, lon, height) &
            ! * (earth%radius+height)**2 * dhor**2 * dheight * 1e8
      enddo
    enddo
  enddo
  return
  val=val*gravity%constant

  print *,val
end subroutine

! =============================================================================
!> all values in radians 
! =============================================================================
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

! =============================================================================
!> all values in radians 
! =============================================================================
real(dp) function geometry (psi, height)
    use mod_constants, only: earth
    real(dp), intent(in) :: psi, height
    real(dp) :: l
    ! l = ((earth%radius + heights(i))**2 + (earth%radius + h_)**2 & 
      ! - 2.*(earth%radius + h_)*(earth%radius+heights(i))*cos(psi))**(0.5)
    ! rho = pressures(i)/ R_air / (deltat+standard_temperature(heights(i), fels_type=fels_type))
        ! -rho*((earth%radius +heights(i))*cos(psi) - (earth%radius + h_)) / (l**3.) 
    end function
end module mod_3d
