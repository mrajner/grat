module mod_atmosphere
  implicit none

contains

! =============================================================================
!> \brief Compute gravity acceleration of the Earth
!! for the specific height using formula
!!
!! see \cite US1976
!! \warning all units in SI
! =============================================================================
function standard_gravity (height)
  use mod_constants, only: dp, earth

  real(dp) :: standard_gravity
  real(dp), intent(in) :: height

  standard_gravity = &
    earth%gravity%mean * (earth%radius/(earth%radius + height))**2._dp
end function

! =============================================================================
!> \brief Computes pressure [Pa] for specific height
!!
!! See \cite US1976 or  \cite Huang05 for details.
!! Uses formulae 5 from \cite Huang05.
!!
!! \warning pressure in Pa, height in meters
! =============================================================================
function standard_pressure (  &
    height,                   &
    p_zero,                   &
    temperature,              &
    h_zero,                   &
    method,                   &
    dz,                       &
    fels_type,                &
    use_standard_temperature, &
    nan_as_zero               &
    )

  use mod_constants, only: dp, earth, atmosphere, R_air
  use iso_fortran_env
  use mod_printing, only: print_warning

  real(dp) :: standard_pressure
  real(dp),     intent(in) :: height
  real(dp),     intent(in), optional :: temperature, p_zero, h_zero
  character(*), intent(in), optional :: method, fels_type
  real(dp),     intent(in), optional :: dz
  real(dp) :: sfc_height, sfc_temperature, sfc_gravity, alpha, sfc_pressure
  real(dp) :: z_, dz_, t_
  logical, intent(in), optional :: use_standard_temperature, nan_as_zero
  integer :: i


  sfc_temperature = atmosphere%temperature%standard
  sfc_pressure    = atmosphere%pressure%standard
  sfc_height      = 0._dp
  sfc_gravity     = earth%gravity%mean

  if (present(h_zero)) then
    sfc_height      = h_zero
    sfc_temperature = standard_temperature (sfc_height, fels_type=fels_type)
    sfc_gravity     = standard_gravity (sfc_height)
  endif

  if (present(p_zero)) sfc_pressure = p_zero

  if (                                              &
    present(temperature).and.temperature.gt.100._dp &
    ) then
    sfc_temperature = temperature
  else
    sfc_temperature = standard_temperature(sfc_height)
  endif

  alpha = -6.5e-3_dp

  if (present (method)) then

    select case (method)

    case("berg")
      standard_pressure = sfc_pressure *(1._dp-2.26e-5_dp*(height-sfc_height))**(5.225_dp)

    case ("simple")
      standard_pressure = sfc_pressure &
        * exp(-(height-sfc_height)*standard_gravity(height)/sfc_temperature/R_air)

    case ("full")

      if (.not.present(use_standard_temperature)) then
        call print_warning(                     &
          "error: you have to specify "         &
          //"use_standard_temperature with "    &
          //"full method in standard_pressure", &
          error=.true.)
      endif

      standard_pressure = 0.

      if (present(dz)) then
        dz_ = dz
      else
        dz_ = 0.1_dp
      endif

      if (sfc_height.gt.height) dz_=-dz_

      z_=sfc_height-dz_/2._dp

      do i = 1, int((-sfc_height+dz_/2._dp + height)/ dz_)

        z_ = z_+ dz_

        if (present(use_standard_temperature) .and. use_standard_temperature) then
          if (present(temperature).and.(abs(z_-sfc_height).lt.5000._dp).and.temperature.gt.100._dp) then
            t_ = sfc_temperature+alpha*(z_-sfc_height)
          else
            t_ = standard_temperature(z_,fels_type=fels_type)
          endif
        else
          t_ = sfc_temperature+alpha*(z_-sfc_height)
        endif

        standard_pressure = standard_pressure + standard_gravity(sfc_height)/(R_air*t_)*dz_
      enddo

      standard_pressure = sfc_pressure*exp(-standard_pressure)

    case ("standard")
      !http://en.wikipedia.org/wiki/Barometric_formula
      standard_pressure= sfc_pressure                                      &
        * (sfc_temperature /(sfc_temperature + alpha*(height-sfc_height))) &
        **(earth%gravity%mean /R_air/alpha)

    case default
      call print_warning ("standard pressure: method not known", error=.true.)

    endselect

  else
    call print_warning("standard_pressure: set method explicitly",error=.true.)
  endif

  if (present(nan_as_zero).and.nan_as_zero) then
    if (isnan(standard_pressure)) standard_pressure = 0._dp
  endif

end function

! =============================================================================
!> \brief Compute standard temperature [K] for specific height [km]
!!
!! if t_zero is specified use this as surface temperature
!! otherwise use T0.
!! A set of predifined temperature profiles ca be set using
!! optional argument \argument fels_type
!! \cite Fels86
!! \li US standard atmosphere (default)
!! \li tropical
!! \li subtropical_summer
!! \li subtropical_winter
!! \li subarctic_summer
!! \li subarctic_winter
! ==============================================================================
function standard_temperature (height, fels_type, t_zero)
  use mod_constants, only: dp, earth, atmosphere

  real(dp), intent(in) :: height
  real(dp) :: standard_temperature
  character (len=*), intent(in), optional :: fels_type
  real(dp), intent(in), optional :: t_zero
  real(dp) :: aux, cn, t
  integer :: i,indeks
  real(dp), dimension (10) :: z, c, d

  ! Read into memory the parameters of temparature height profiles
  ! for standard atmosphere

  z = (/11.0 , 20.1 , 32.1 , 47.4  , 51.4 , 71.7  , 85.7  , 100.0 , 200.0 , 300.0 /)
  c = (/-6.5 ,  0.0 ,  1.0 ,  2.75 ,  0.0 , -2.75 , -1.97 ,   0.0 ,   0.0 ,   0.0 /)
  d = (/ 0.3 ,  1.0 ,  1.0 ,  1.0  ,  1.0 ,  1.0  ,  1.0  ,   1.0 ,   1.0 ,   1.0 /)
  t = atmosphere%temperature%standard

  if ( present (fels_type)) then
    if (fels_type .eq. "US1976" ) then
    elseif (fels_type .eq. "tropical" ) then
      z=(/ 2.0 ,  3.0 , 16.5 , 21.5 , 45.0 , 51.0 , 70.0 , 100.0  , 200.0 , 300.0 /)
      c=(/-6.0 , -4.0 , -6.7 ,  4.0 ,  2.2 ,  1.0 , -2.8 ,  -0.27 ,   0.0 ,   0.0 /)
      d=(/ 0.5 ,  0.5 ,  0.3 ,  0.5 ,  1.0 ,  1.0 ,  1.0 ,   1.0  ,   1.0 ,   1.0 /)
      t=300.0
    elseif (fels_type .eq. "subtropical_summer" ) then
      z = (/ 1.5 ,  6.5 , 13.0 , 18.0 , 26.0 , 36.0 , 48.0 , 50.0 , 70.0 , 100.0   /)
      c = (/-4.0 , -6.0 , -6.5 ,  0.0 ,  1.2 ,  2.2 ,  2.5 ,  0.0 , -3.0 ,  -0.025 /)
      d = (/ 0.5 ,  1.0 ,  0.5 ,  0.5 ,  1.0 ,  1.0 ,  2.5 ,  0.5 ,  1.0 ,   1.0   /)
      t = 294.0
    elseif (fels_type .eq. "subtropical_winter" ) then
      z = (/ 3.0 , 10.0 , 19.0 , 25.0 , 32.0 , 44.5 , 50.0 , 71.0 , 98.0 , 200.0 /)
      c = (/-3.5 , -6.0 , -0.5 ,  0.0 ,  0.4 ,  3.2 ,  1.6 , -1.8 ,  0.7 ,   0.0 /)
      d = (/ 0.5 ,  0.5 ,  1.0 ,  1.0 ,  1.0 ,  1.0 ,  1.0 ,  1.0 ,  1.0 ,   1.0 /)
      t = 272.2
    elseif (fels_type .eq. "subarctic_summer" ) then
      z = (/ 4.7 , 10.0 , 23.0 , 31.8 , 44.0 , 50.2 , 69.2 , 100.0 , 200.0 , 300.0 /)
      c = (/-5.3 , -7.0 ,  0.0 ,  1.4 ,  3.0 ,  0.7 , -3.3 ,  -0.2 ,   0.0 ,   0.0 /)
      d = (/ 0.5 ,  0.3 ,  1.0 ,  1.0 ,  2.0 ,  1.0 ,  1.5 ,   1.0 ,   1.0 ,   1.0 /)
      t = 287.0
    elseif (fels_type .eq. "subarctic_winter" ) then
      z = (/  1.0 ,  3.2 ,  8.5 , 15.5 , 25.0 , 30.0 , 35.0 , 50.0 , 70.0 , 100.0 /)
      c = (/  3.0 , -3.2 , -6.8 ,  0.0 , -0.6 ,  1.0 ,  1.2 ,  2.5 , -0.7 ,  -1.2 /)
      d = (/  0.4 ,  1.5 ,  0.3 ,  0.5 ,  1.0 ,  1.0 ,  1.0 ,  1.0 ,  1.0 ,   1.0 /)
      t = 257.1
    else
      stop "unknown fels_type argument"
    endif
  endif

  do i=1,10
    if (height/1000._dp.le.z(i)) then
      indeks=i
      exit
    endif
  enddo

  aux = 0._dp
  do i = 1, indeks
    if (i.eq.indeks) then
      cn = 0._dp
    else
      cn = c(i+1)
    endif
    aux = aux                                                        &
      + d(i) * (cn - c(i))                                           &
      * log (cosh ((height/1000._dp - z(i)) / d(i)) / cosh (z(i)/d(i)))
  enddo

  standard_temperature = t + c(1) * (height/1000._dp)/2._dp + aux/2._dp

  if(present(t_zero)) then
    standard_temperature = t_zero + c(1) * (height/1000._dp)/2._dp + aux/2._dp
  endif
end function

! =============================================================================
!> Compute geometric height from geopotential heights
!!
!! \author M. Rajner
!! \date 2013-03-19
! =============================================================================
function geop2geom (geopotential_height, inverse)
  use mod_constants, only: dp, earth

  real(dp) :: geopotential_height
  logical, intent(in), optional:: inverse
  real(dp) :: geop2geom

  if (present(inverse).and.inverse) then
    !conversion from geometric to geopotential height
    geop2geom = geopotential_height                        &
      *(earth%radius/(earth%radius + geopotential_height))
  else
    !conversion from  geopotential to geometric height
    geop2geom = geopotential_height          &
      * (earth%radius + geopotential_height) &
      /(earth%radius)
  endif
end function

! =============================================================================
!> Compute virtual temperature using temperature and specific humidity
! =============================================================================
function virtual_temperature(t, sh)
  use mod_constants, only: dp
  real(dp) :: virtual_temperature
  real(dp), intent(in) :: t, sh

  virtual_temperature=t*(1._dp+0.608_dp*sh)
end function

end module
