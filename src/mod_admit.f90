!> \file
module mod_admit
  use mod_constants, only: dp

  implicit none

contains

! =============================================================================
! =============================================================================
real(dp) function admit(lat,lon, height)
  use mod_cmdline, only: ind, info, admitance
  use mod_data, only: get_value, model
  use mod_utilities, only: r2d
  use mod_atmosphere, only: standard_pressure
  real(dp) :: val, rsp, hp
  real(dp), intent(in) :: lat,lon, height

  if (ind%model%sp.ne.0) then
    call get_value (                   & 
        model=model(ind%model%sp),     & 
        lat=lat,                       & 
        lon=lon,                       & 
        val=val,                       & 
        level=1,                       & 
        method = info(1)%interpolation & 
        )
  endif
  if(admitance%level.eq."site" &
      .or.admitance%level.eq."model") then
    if (ind%model%hp.ne.0) then
      call get_value (                   & 
          model=model(ind%model%hp),     & 
          lat=lat,                       & 
          lon=lon,                       & 
          val=hp,                       & 
          level=1,                       & 
          method = info(1)%interpolation & 
          )
    else
      stop "you want transfer pressure but no @HP model given"
    endif
    if (admitance%level.eq."model") then
      if(ind%model%h.ne.0) then
        call get_value (                   & 
            model=model(ind%model%hp),     & 
            lat=lat,                       & 
            lon=lon,                       & 
            val=hp,                       & 
            level=1,                       & 
            method = info(1)%interpolation & 
            )
      else
        stop "you want transfer pressure but no @H model given"
      endif
    endif
    val = standard_pressure(             & 
        height=height,                   & 
        h_zero=hp,                       & 
        p_zero=val,                      & 
        method="full",                   & 
        use_standard_temperature=.true. & 
        )
  endif
  if (ind%model%rsp.ne.0) then
    call get_value (                   & 
        model=model(ind%model%rsp),    & 
        lat=lat,                       & 
        lon=lon,                       & 
        val=rsp,                       & 
        level=1,                       & 
        method = info(1)%interpolation & 
        )
      val=val-rsp
  endif
    admit = admitance%value*1e-2 * val
end function

subroutine parse_admit(cmd_line_entry)
  use mod_cmdline
  use mod_printing
  type (cmd_line_arg) :: cmd_line_entry
  if (cmd_line_entry%field(1)%subfield(1)%name.ne."") then
    read(cmd_line_entry%field(1)%subfield(1)%name, *) admitance%value
  endif
  if (cmd_line_entry%field(1)%subfield(2)%name.ne."") then
    admitance%level = cmd_line_entry%field(1)%subfield(2)%name
  endif
  write(log%unit, '('//form%t2//',a,x,f6.2,x,a)')  "admitance:" , admitance%value,  "uGal/hPa"
end subroutine
end module
