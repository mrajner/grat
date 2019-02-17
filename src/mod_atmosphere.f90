module mod_atmosphere
  implicit none

contains

! =============================================================================
!> \brief Computes pressure [Pa] for specific height
!!
!! See \cite US1976 or  \cite Huang05 for details.
!! Uses formulae 5 from \cite Huang05.
!!
!! \warning pressure in Pa, height in meters
! =============================================================================
function standard_pressure(   &
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

  use mr_constants, only: dp, earth, atmosphere, R_air
  use iso_fortran_env
  use mod_printing, only: print_warning
  use mr_atmosphere, only: standard_temperature, standard_gravity

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
    sfc_temperature = standard_temperature(sfc_height, fels_type=fels_type)
    sfc_gravity     = standard_gravity(sfc_height)
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
      call print_warning("standard pressure: method not known", error=.true.)

    endselect

  else
    call print_warning("standard_pressure: set method explicitly", error=.true.)

  endif

  if (present(nan_as_zero).and.nan_as_zero) then
    if (isnan(standard_pressure)) standard_pressure = 0._dp
  endif

end function
end module
