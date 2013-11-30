module mod_3d
    use mod_constants, only: dp
    implicit none
    real(dp) :: result3d=0.

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
  use mod_utilities, only: d2r, r2d
  use mod_atmosphere
  use mod_constants, only: R_air, gravity, earth
  use mod_spherical, only: spher_trig, spher_area
  use mod_green, only: green, green_common
  use mod_cmdline
  use mod_printing, only: output
  use mod_data

  type (site_info) :: site
  type(dateandmjd),intent(in), optional :: date

  real(dp) :: val(size(model)), volume, result
  real(dp), dimension(:), allocatable :: azimuths
  integer :: igreen, idist, nazimuth, iazimuth, nheight, iheight
  real(dp) :: dazimuth, azimuth
  integer :: i
  real (dp) :: dheight, height, lat, lon

  result=0
  do igreen = 1, size(green_common)
    if (ind%moreverbose%v.ne.0) write(moreverbose(ind%moreverbose%v)%unit,*)

    do idist = 1, size(green_common(igreen)%distance)

      if (allocated(azimuths)) deallocate (azimuths)
      if (info(igreen)%azimuth%step.eq.0) then
        nazimuth = &
            (info(igreen)%azimuth%stop-info(igreen)%azimuth%start)/360 * &
            max(int(360*sin(d2r(green_common(igreen)%distance(idist)))), 100) * &
            info(igreen)%azimuth%denser
        if (nazimuth.eq.0) nazimuth=1
        dazimuth= (info(igreen)%azimuth%stop-info(igreen)%azimuth%start)/nazimuth
      else
        dazimuth = info(igreen)%azimuth%step
        nazimuth= (info(igreen)%azimuth%stop-info(igreen)%azimuth%start)/dazimuth
      endif

      nheight=ceiling((info(igreen)%height%stop-info(igreen)%height%start)/info(igreen)%height%step)

      ! calculate area using spherical formulae
      volume = spher_area(                    & 
          d2r(green_common(igreen)%start(idist)), & 
          d2r(green_common(igreen)%stop(idist)),  & 
          d2r(dazimuth),                          & 
          radius=earth%radius,                    & 
          alternative_method=.true.)              & 
          * info(igreen)%height%step

      allocate(azimuths(nazimuth))
      azimuths = [(info(igreen)%azimuth%start + (i-1) * dazimuth, i= 1, nazimuth)] 

      do iazimuth  = 1, nazimuth
        azimuth = azimuths(iazimuth)

        call spher_trig &
            (d2r(site%lat), d2r(site%lon), &
            d2r(green_common(igreen)%distance(idist)), d2r(azimuth), lat, lon, domain=.true.)

        call get_value (                                              & 
            model(ind%model%sp), r2d(lat), r2d(lon), val(ind%model%sp), & 
            level=1, method = info(igreen)%interpolation, date=date%date)
        call get_value (                                              & 
            model(ind%model%rsp), r2d(lat), r2d(lon), val(ind%model%rsp), & 
            level=1, method = info(igreen)%interpolation, date=date%date)

        do iheight=1, nheight
          height=info(igreen)%height%start+(iheight-0.5)*info(igreen)%height%step

          if (iheight.eq.1) then
            val(3)= standard_pressure(height, p_zero=val(ind%model%sp), method="standard", use_standard_temperature=.true.)
            val(4)= standard_pressure(height, p_zero=val(ind%model%rsp), method="standard", use_standard_temperature=.true.)
          else
            val(3)= standard_pressure(height, p_zero=val(3),h_zero=height-info(igreen)%height%step, method="standard", use_standard_temperature=.true.)
            val(4)= standard_pressure(height, p_zero=val(4),h_zero=height-info(igreen)%height%step, method="standard", use_standard_temperature=.true.)
          endif
          result=result &
              + geometry(psi=d2r(green_common(igreen)%distance(idist)), h=site%height, z=height) &
              *(val(3) - val(4)) &
              /(R_air*standard_temperature(height))  &
              * volume

          if (ind%moreverbose%v.ne.0) then
            print '(4f10.3,4e14.3)', azimuth, &
                green_common(igreen)%start(idist), &
                green_common(igreen)%stop(idist), &
                green_common(igreen)%distance(idist),height, &
                height-1./2. * (info(igreen)%height%step), &
                height--1./2. * (info(igreen)%height%step), result
          endif
        enddo
      enddo
    enddo
  enddo

  result=-result*gravity%constant*1e8
  write(output%unit,"("//output%form//"$)"), result

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
