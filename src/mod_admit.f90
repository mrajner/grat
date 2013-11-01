!> \file
module mod_admit
  use mod_constants, only: dp

  implicit none

contains
! =============================================================================
! =============================================================================
real(dp) function admit(site_, date)
  use mod_cmdline, only: ind, info, admitance
  use mod_data, only: get_value, model
  use mod_utilities, only: r2d
  use mod_atmosphere, only: standard_pressure
  use mod_site
  use mod_cmdline, only: transfer_sp

  real(dp) :: val, rsp, t
  type(site_info) :: site_
  integer, optional :: date(6)

  if (ind%model%sp.ne.0) then
    call get_value (                 & 
      model=model(ind%model%sp),     & 
      lat=site_%lat,                 & 
      lon=site_%lon,                 & 
      val=val,                       & 
      level=1,                       & 
      method = info(1)%interpolation, & 
      date=date &
      )
  endif
 
  if (site_%hp%if .and. transfer_sp ) then
    if (site_%h%if) then
      site_%height = site_%h%val
    endif

    if (ind%model%t.ne.0) then
      call get_value (                 & 
        model=model(ind%model%t),      & 
        lat=site_%lat,                 & 
        lon=site_%lon,                 & 
        val=t,                         & 
        level=1,                       & 
        method = info(1)%interpolation, & 
        date=date &
        )
    
      val = standard_pressure(           & 
        height=site_%height,             & 
        h_zero=site_%hp%val,             & 
        p_zero=val,                      & 
        method="full",                   & 
        temperature = t,                 & 
        use_standard_temperature=.false. & 
        )
    else
      val = standard_pressure(           & 
        height=site_%height,             & 
        h_zero=site_%hp%val,             & 
        p_zero=val,                      & 
        method="full",                   & 
        use_standard_temperature=.true. & 
        )
    endif
  endif

  if (ind%model%rsp.ne.0) then
    call get_value (                 & 
      model=model(ind%model%rsp),    & 
      lat=site_%lat,                 & 
      lon=site_%lon,                 & 
      val=rsp,                       & 
      level=1,                       & 
      method = info(1)%interpolation & 
      )
    val=val-rsp
  endif

  admit = admitance%value*1e-2 * val
end function

! =============================================================================
!> \date 2013.10.15
!! \author Marcin Rajner
! =============================================================================
subroutine parse_admit(cmd_line_entry)
  use mod_cmdline
  use mod_printing
  type (cmd_line_arg) :: cmd_line_entry
  if (cmd_line_entry%field(1)%subfield(1)%name.ne."") then
    read(cmd_line_entry%field(1)%subfield(1)%name, *) admitance%value
  endif
  write(log%unit, '('//form%t2//',a,x,f6.2,x,a)') "admitance:", admitance%value, "uGal/hPa"
  if (size(cmd_line_entry%field(1)%subfield).gt.1 &
    .and.cmd_line_entry%field(1)%subfield(2)%name.ne." ") then
    admitance%level=cmd_line_entry%field(1)%subfield(2)%name
  else
    admitance%level="none"
  endif
  write(log%unit, form%i2) "level:", admitance%level
end subroutine
end module
