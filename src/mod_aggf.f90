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
function aggfd( &
    psi,         &
    delta,       &
    dz,          &
    method,      &
    aggfdh,      &
    aggfdz,      &
    aggfdt,      &
    predefined,  &
    fels_type,   &
    rough        &
    )

  use mr_constants, only: atmosphere, dp

  real(dp), intent (in) :: psi
  real(dp), intent (in), optional :: delta
  real(dp), intent (in), optional :: dz
  logical,  intent (in), optional :: aggfdh, aggfdz, aggfdt, predefined, rough
  real(dp) :: aggfd
  real(dp) :: delta_
  character (len=*), intent(in), optional :: method, fels_type

  delta_ = 10._dp ! Default value

  if (present(delta)) delta_ = delta

  if(present(aggfdh).and.aggfdh) then
    aggfd = (                  &
      + aggf (                 &
      psi,                     &
      h          = +delta_,    &
      dz         = dz,         &
      method     = method,     &
      predefined = predefined, &
      fels_type  = fels_type,  &
      rough      = rough)      &
      - aggf (                 &
      psi,                     &
      h          = -delta_,    &
      dz         = dz,         &
      method     = method,     &
      predefined = predefined, &
      fels_type  = fels_type,  &
      rough      = rough))     &
      / ( 2._dp * delta_)

  elseif(present(aggfdz).and.aggfdz) then
    aggfd = (                  &
      + aggf (                 &
      psi,                     &
      zmin       = +delta_,    &
      dz         = dz,         &
      method     = method,     &
      predefined = predefined, &
      fels_type  = fels_type,  &
      rough      = rough)      &
      - aggf (                 &
    psi,                       &
      zmin       = -delta_,    &
      dz         = dz,         &
      method     = method,     &
      predefined = predefined, &
      fels_type  = fels_type,  &
      rough      = rough))     &
      / ( 2._dp * delta_)

  else if(present(aggfdt).and.aggfdt) then
    aggfd = (                  &
      + aggf (                 &
      psi,                     &
      t_zero     = +delta_,    &
      dz         = dz,         &
      method     = method,     &
      predefined = predefined, &
      fels_type  = fels_type,  &
      rough      = rough)      &
      - aggf (                 &
      psi,                     &
      t_zero     = -delta_,    &
      dz         = dz,         &
      method     = method,     &
      predefined = predefined, &
      fels_type  = fels_type,  &
      rough      = rough))     &
      / ( 2._dp * delta_)
  endif
end function

! ==============================================================================
!> This function computes the value of atmospheric gravity green functions
!! (AGGF) on the basis of spherical distance (psi)
!! \author Marcin Rajner
!! \date 2013.07.15
!! \warning psi in radians h in meter
!! t_zero is actually delta_t so if t_zero=10 (t_zero=288.15+10)
! ==============================================================================
function aggf(          &
    psi,                &
    zmin,               &
    zmax,               &
    dz,                 &
    t_zero,             &
    h,                  &
    first_derivative_h, &
    first_derivative_z, &
    fels_type,          &
    method,             &
    predefined,         &
    rough)

  use mr_constants,      only: dp, pi, earth, gravity, atmosphere, R_air
  use mr_utilities,      only: d2r
  use mr_atmosphere,     only: standard_temperature, standard_pressure
  use mod_normalization, only: green_normalization
  use mod_printing,      only: print_warning

  real(dp), intent(in)           :: psi ! spherical distance from site [rad]
  real(dp), intent(in), optional :: &
    zmin,                           &   ! minimum height, starting point [m]     (default = 0)
    zmax,                           &   ! maximum height, ending point   [m]     (default = 60000)
    dz,                             &   ! integration step               [m]     (default = 0.1 -> 10 cm)
    t_zero,                         &   ! temperature at the surface     [K]     (default = 15°C i.e., 288.15 = t0)
    h                                   ! station height                 [m]     (default = 0)

  logical, intent(in), optional :: &
    first_derivative_h, first_derivative_z, predefined, rough
  character (len=*), intent(in), optional  :: fels_type, method
  character (len=20) :: old_method
  real(dp) :: aggf
  real(dp) :: zmin_, zmax_, dz_, h_
  real(dp) :: J_aux
  real(dp) :: rho, l, deltat

  real(dp), dimension(:), allocatable, save :: heights, pressures
  integer :: i

  zmin_  = 0._dp
  zmax_  = 60000._dp
  dz_    = 0.1_dp
  h_     = 0._dp
  deltat = 0._dp

  aggf   = 0._dp

  if (present(zmin))   zmin_  = zmin
  if (present(zmax))   zmax_  = zmax
  if (present(  dz))     dz_  = dz
  if (present(   h))      h_  = h
  if (present(t_zero)) deltat = t_zero

  if(allocated(heights)) then

    if (                                                            &
      ((zmin_+dz_/2).ne.heights(1))                                 &
      .or.abs((zmax_-dz_/2)-heights(size(heights))).gt.zmax_/1e6_dp &
      .or.nint((zmax_-zmin_)/dz_).ne.size(heights)                  &
      .or. (present(predefined))                                    &
      .or. method.ne.old_method                                     &
      .or. present(t_zero)                                          &
      ) then

      deallocate(heights)
      deallocate(pressures)

    endif

  endif

  if (.not.allocated(heights))  then

    allocate(heights(nint((zmax_-zmin_)/dz_)))
    allocate(pressures(size(heights)))

    do i = 1, size(heights)
      heights(i) = zmin_ + dz_/2._dp + (i-1) * dz_
    enddo

    if (present(rough).and.rough) then
      ! do not use rough! it is only for testing
      call print_warning("obsolete rough", error = .true.)
      do i = 1, size(heights)
        pressures(i) = standard_pressure (   &
          heights(i),                        &
          method                   = method, &
          dz                       = dz,     &
          use_standard_temperature = .true.  &
          )
      enddo

    else
      pressures(1) = standard_pressure(                  &
        heights(1),                                      &
        method                   = method,               &
        h_zero                   = zmin_,                &
        dz                       = dz,                   &
        fels_type                = fels_type,            &
        use_standard_temperature = .true.,               &
        temperature              = standard_temperature( &
        zmin_, fels_type = fels_type) + deltat           &
        )

      do i = 2, size(heights)
        pressures(i) = standard_pressure(                               &
          heights(i),                                                   &
          p_zero                   = pressures(i-1),                    &
          h_zero                   = heights(i-1),                      &
          method                   = method,                            &
          dz                       = dz,                                &
          fels_type                = fels_type,                         &
          use_standard_temperature = .true.,                            &
          temperature              = standard_temperature(heights(i-1), &
          fels_type = fels_type)+deltat                                 &
          )

      enddo

    endif
  endif

  old_method = method

  do i = 1, size(heights)

    if(psi.lt.1e-5_dp) then
      l = sqrt(                                      &
        (                                            &
        2._dp *( earth%radius+h_) * sin(psi/2._dp)   &
        + (heights(i)-h_)*sin(psi/2._dp) ) **2._dp   &
        + ( (heights(i)-h_) *cos(psi/2._dp)) **2._dp &
        ) 
    else
      l = (                                                            &
        (earth%radius + heights(i))**2._dp                             &
        + (earth%radius + h_)**2._dp                                   &
        - 2._dp*(earth%radius + h_)*(earth%radius+heights(i))*cos(psi) &
        )**(0.5_dp)
    endif

    rho = pressures(i)/ R_air / (deltat+standard_temperature(heights(i), fels_type=fels_type))

    if (present(first_derivative_h) .and. first_derivative_h) then
      ! first derivative (respective to station height)
      ! micro Gal height / m
      ! see equation 22, 23 in \cite Huang05
      J_aux =                                                                &
        ((earth%radius + heights(i) )**2._dp)                                &
        *(1._dp-3._dp*((cos(psi))**2._dp)) -2._dp*(earth%radius + h_)**2._dp &
        + 4._dp*(earth%radius+h_)*(earth%radius+heights(i))*cos(psi)

      aggf = aggf + rho * (J_aux/l**5._dp) * dz_

    else if (present (first_derivative_z) .and. first_derivative_z) then
      ! first derivative (respective to column height)
      ! according to equation 26 in \cite Huang05
      ! micro Gal / hPa / m
      if (i.gt.1) exit
      aggf = rho *( ((earth%radius + heights(i))*cos(psi)-(earth%radius + h_)) / (l**3))
    else
      ! GN microGal/hPa
      aggf = aggf &
        -rho*((earth%radius +heights(i))*cos(psi) - (earth%radius + h_)) / (l**3.)  * dz_
    endif
  enddo

  aggf = aggf/atmosphere%pressure%standard &
    *gravity%constant                      &
    *green_normalization("m", psi = psi)
  
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
! ==============================================================================
pure function GN_thin_layer(psi)
  use mr_constants, only: dp
  real(dp), intent(in) :: psi
  real(dp) :: GN_thin_layer

  GN_thin_layer = 1.627_dp * psi / sin(psi/2._dp)
end function


! ==============================================================================
!> \brief Bouger plate computation
!!
! ==============================================================================
function bouger(h, R)
  use mr_constants, only: dp, gravity, pi

  real(dp) :: bouger
  real(dp), intent(in), optional :: R !< height of point above the cylinder
  real(dp), intent(in) ::  h

  if (present(R)) then
    bouger = h + R - sqrt(R**2+H**2)
  else
    bouger = h
  endif
  bouger = 2*pi*gravity%constant * bouger
  return
end function

! ==============================================================================
!> Bouger plate computation
!!
!! see eq. page 288 \cite Warburton77
!! \date 2013-03-18
!! \author M. Rajner
! ==============================================================================
function simple_def(R)
  use mr_constants, only: dp, earth
  real(dp) :: R, delta
  real(dp) :: simple_def

  delta = 0.22e-11 * R

  simple_def =                                                          &
    earth%gravity%mean / earth%radius * 1000._dp                        &
    * delta                                                             &
    * (                                                                 &
    2.-3./2. * earth%density%crust / earth%density%mean                 &
    -3./4. * earth%density%crust / earth%density%mean * sqrt (2*(1.)) &
    )                                                                   &
    * 1000._dp
end function

end module
