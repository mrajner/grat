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

  real(dp) :: val, rsp, t !, hrsp
  type(site_info) :: site_
  integer, optional :: date(6)
  integer :: i
  logical, save :: first_warning=.true.


  if (site_%lp%if) then
    val=0
    do i=1,size(site_%lp%date)
      if(all(site_%lp%date(i,1:6).eq.date(1:6))) then
        val=site_%lp%data(i)
        exit
      endif
      if(i.eq.size(site_%lp%date)) then
        if(first_warning) call print_warning("date not found in @LP")
        val=sqrt(-1.)
      endif

    enddo
  else
    ! get SP
    if (ind%model%sp.ne.0                           &
      .and.(model(ind%model%sp)%if                &
      .or. model(ind%model%sp)%if_constant_value) &
      ) then
      call get_value (                    &
        model=model(ind%model%sp),      &
        lat=site_%lat,                  &
        lon=site_%lon,                  &
        val=val,                        &
        level=1,                        &
        method = info(1)%interpolation, &
        date=date                       &
        )
    else
      call print_warning("@SP is required with -M1D", error=.true.)
    endif
  endif


  ! get RSP
  if (ind%model%rsp.ne.0) then
    call get_value (                   &
      model=model(ind%model%rsp),    &
      lat=site_%lat,                 &
      lon=site_%lon,                 &
      val=rsp,                       &
      level=1,                       &
      method = info(1)%interpolation &
      )
  endif

  if (transfer_sp%if) then
    if (ind%model%h.eq.0 ) then
      if (first_warning) call print_warning("transfer on topo but no @H")
    endif

    ! get T
    if (ind%model%t.ne.0) then
      call get_value (                  &
        model=model(ind%model%t),     &
        lat=site_%lat,                &
        lon=site_%lon,                &
        val=t,                        &
        level=1,                      &
        method=info(1)%interpolation, &
        date=date                     &
        )
    endif

    ! transfer SP
    if (site_%hp%if.and..not.isnan(val)) then
      val = standard_pressure(                &
        height=site_%height,                &
        h_zero=site_%hp%val,                &
        p_zero=val,                         &
        method=transfer_sp%method,          &
        temperature=t,                      &
        use_standard_temperature            &
        = ind%model%t.eq.0,                 &
        nan_as_zero=.false.)
    endif

    ! if (ind%model%hrsp.ne.0 .and.ind%model%rsp.ne.0)  then
    ! call get_value (                 &
    ! model=model(ind%model%hrsp),   &
    ! lat=site_%lat,                 &
    ! lon=site_%lon,                 &
    ! val=hrsp,                      &
    ! level=1,                       &
    ! method = info(1)%interpolation &
    ! )

    ! rsp = standard_pressure(     &
    ! height=site_%height,       &
    ! h_zero=hrsp,               &
    ! p_zero=rsp,                &
    ! method=transfer_sp%method, &
    ! temperature=t,             &
    ! use_standard_temperature   &
    ! = ind%model%t.eq.0,        &
    ! nan_as_zero=.false.)

    ! elseif(ind%model%hrsp.ne.0) then
    ! if (first_warning) call print_warning("@RSP not found but @HRSP and -U given")
    ! elseif(ind%model%rsp.ne.0) then
    ! if (first_warning) call print_warning("@HRSP not found but @RSP and -U given")
    ! end if
  endif

  if (ind%model%rsp.ne.0) val = val-rsp
  admit = admitance%value*1.e-2 * val

  if (first_warning) first_warning=.false.
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
  if (.not.log%sparse) &
    write(log%unit, '('//form%t2//',a,x,f6.2,x,a)') "admitance:", admitance%value, "uGal/hPa"

  ! not sure what trying to achive
  ! if (size(cmd_line_entry%field(1)%subfield).gt.1 &
  ! .and.cmd_line_entry%field(1)%subfield(2)%name.ne." ") then
  ! admitance%level=cmd_line_entry%field(1)%subfield(2)%name
  ! else
  ! admitance%level="none"
  ! endif
  ! write(log%unit, form%i2) "level:", admitance%level
end subroutine
end module
