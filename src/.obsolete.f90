! =============================================================================
! > This will transfer pressure beetween different height using barometric
! formulae
! =============================================================================
!> \warning OBSOLETE ROUTINE -- use \c standard_pressure() instead with optional args
subroutine transfer_pressure (height1 , height2 , pressure1 , pressure2 , &
  temperature , polish_meteo )
  real (dp) , intent (in) :: height1 , height2 , pressure1
  real (dp) , intent (in), optional :: temperature
  real (dp) :: sfc_temp , sfc_pres
  logical , intent (in), optional :: polish_meteo
  real(dp) , intent(out) :: pressure2
  
  sfc_temp = t0

  ! formulae used to reduce press to sfc in polish meteo service
  if (present(polish_meteo) .and. polish_meteo) then
    sfc_pres = exp (log (pressure1) + 2.30259 * height1*1000. &
      /(18400.*(1+0.00366*((temperature-273.15) + 0.0025*height1*1000.)))  )
  else
  ! different approach
    if(present(temperature) ) then
      call surface_temperature( height1 , temperature , sfc_temp )
    endif
    call standard_pressure (height1 , sfc_pres , t_zero=sfc_temp , &
      inverted=.true. , p_zero = pressure1 )
  endif

  ! move from sfc to height2
  call standard_pressure (height2 , pressure2 , t_zero=sfc_temp , &
    p_zero = sfc_pres )
end subroutine

