module mod_parser
  use, intrinsic :: iso_fortran_env, only: iostat_end
  use mr_constants, only: dp
  use mod_printing

  implicit none

contains

! =============================================================================
!> This subroutine counts the command line arguments and parse appropriately
! =============================================================================
subroutine parse_option(cmd_line_entry, accepted_switches, version, cdate, gdate, program_calling)
  use mod_cmdline
  use mod_site,     only: parse_site
  use mod_date,     only: parse_date
  use mod_polygon,  only: parse_polygon
  use mod_data,     only: parse_model, parse_level, all_huge, model
  use mod_green,    only: parse_green, green
  use mr_utilities, only: file_exists, is_numeric,  basename
  use mod_admit,    only: parse_admit

  type(cmd_line_arg), intent(in) :: cmd_line_entry
  character(len=*), optional     :: accepted_switches
  character(len=*), optional     :: version, cdate, gdate, program_calling
  integer :: i
  logical :: file_already_opened

  write(log%unit, form%i1) cmd_line_entry%switch, "{", trim(cmd_line_entry%full), "}"
  if(.not.if_accepted_switch(cmd_line_entry%switch, accepted_switches=accepted_switches)) &
    then
    call print_warning ("not accepted switch "//cmd_line_entry%switch)
    return
  endif

  select case (cmd_line_entry%switch)

  case ('-V')
    if (.not.log%sparse) then
      write(log%unit, form%i2) 'verbose mode'
    endif

    if (len(trim(cmd_line_entry%field(1)%subfield(1)%name)).gt.0) then
      if (.not.log%sparse) write(log%unit, form_62) 'the log file was set', trim(basename(trim(log%name)))
    endif

  case ('-r')
    do i = 1, size(cmd_line_entry%field)
      if (any(cmd_line_entry%field(i)%subfield(:)%name.eq."t" )) result_total     = .true.
      if (any(cmd_line_entry%field(i)%subfield(:)%name.eq."T" )) then
        result_total_all = .true.
        result_total     = .true.
      endif
      if (any(cmd_line_entry%field(i)%subfield(:)%name.eq."nc")) result_component = .false.
    enddo

  case ('-S', '-R')
    call parse_site(cmd_line_entry)

  case ("-I")
    call parse_info(cmd_line_entry)

  case ("-Q")
    optimize=.true.
    write(log%unit, form%i3) "optimize"

  case ("-L")
    call parse_moreverbose(cmd_line_entry)

  case ("-B")

    inverted_barometer=.false.
    do i = 1, size(cmd_line_entry%field)
      select case(cmd_line_entry%field(i)%subfield(1)%name)
      case ("N","n","nib","NIB")
        non_inverted_barometer = .true.
      case default
        inverted_barometer=.true.
      end select
    enddo

    if (.not.log%sparse) then
      write(log%unit, form%i3) "    inverted barometer assumption [T/F]:", &
        inverted_barometer
      write(log%unit, form%i3) "non inverted barometer assumption [T/F]:", &
        non_inverted_barometer
    endif

  case ("-O")
    if (any(cmd_line_entry%field(1)%subfield(1:size(cmd_line_entry%field(1)%subfield))%name.eq."C")) then
      ocean_conserve_mass = .true.
      if (.not.log%sparse) write(log%unit, form%i3) "conservation ocean mass"
    endif
    if (any(cmd_line_entry%field(1)%subfield(1:size(cmd_line_entry%field(1)%subfield))%name.eq."I")) then
      inverted_landsea_mask = .true.
      if (.not.log%sparse) write(log%unit, form%i3) "inverted landsea mask"
    endif

  case ('-D')
    call parse_date(cmd_line_entry)

  case ('-A')
    admitance%if=.true.
    call parse_admit(cmd_line_entry)

  case ('-F')
    call parse_model(cmd_line_entry)

  case ('-C')
    center_data=.true.

  case ('-!')
    all_huge=.true.
    write(log%unit, form%i2) 'all model as huge'
    if (ubound(model,1).ge.1) call print_warning("put -! before -F")

  case ("-G")
    call parse_green(cmd_line_entry)

  case ("-H")
    if (.not.log%sparse) write(log%unit, form%i3) 'header'
    output%header=.true.

  case ('-M')
    method=.false.
    do i =1, size(cmd_line_entry%field)
      select case (cmd_line_entry%field(i)%subfield(1)%name)
      case ("n")
        dryrun = .true.
      case ("1D", "1")
        method(1) =.true.
      case ("2D", "2")
        method(2) =.true.
      case ("3D", "3")
        method(3) =.true.
        if (allocated(green)) then
          call print_warning("with 3D selection -G* should be AFTER -M*3*",error=.true.)
        endif

         method3d=cmd_line_entry%field(i)%subfield(2)%name==method3dnames

        if (.not.any(method3d)) then
          call print_warning (                                        &
            "no explicit method3d given"//                            &
            " - falling into point mass (for backward compability,"// &
            " not recomended, use -M3 : potential|cylinder)"          &
            )
          method3d(1)=.true.
        endif

        select case (cmd_line_entry%field(i)%subfield(3)%name)
        case ("reference", "ref")
          method3d_compute_reference=.true.

        case default
          if (warnings%all) then
            call print_warning (                            &
              "no explicit ref for 3d given" //             &
              " - using @GN[...] if any or put -M3 : : ref" &
              )
          endif
        endselect

        if(is_numeric(cmd_line_entry%field(i)%subfield(2)%dataname)) then
          read(cmd_line_entry%field(i)%subfield(2)%dataname, *) method3d_refinment_distance
        endif

      case default
        call print_warning("method not known " // trim(cmd_line_entry%field(i)%subfield(1)%name))
      end select
    enddo

    if (.not.log%sparse) then
      write(log%unit, form_62, advance="no") 'method was set:'

      do i=1,size(method)
        if (method(i)) write(log%unit, '(i1,"D ")' , advance = "no") i
      enddo
      write(log%unit, '(a)')
    endif

    if (method(3)) then
      write(log%unit, form_62, advance="no") "method refinment for near area using 3D"
      write(log%unit,'(a$)') pack(method3dnames,method3d)
      write(log%unit, '(g10.3)') method3d_refinment_distance
    endif

    if (.not.any(method).and..not.dryrun) then
      call print_warning("no correct method found", error=.true.)
    endif

  case ('-o')
    output%if = .true.
    output%name = cmd_line_entry%field(1)%subfield(1)%name

    if(cmd_line_entry%field(1)%subfield(1)%dataname.ne."") then
      output%name=trim(cmd_line_entry%field(1)%subfield(1)%name) &
        // "@" // trim(cmd_line_entry%field(1)%subfield(1)%dataname)
    endif

    if (any(cmd_line_entry%field(1)%subfield(2:size(cmd_line_entry%field(1)%subfield))%name.eq."tee")) then
      output%tee = .true.
    endif
    if (any(cmd_line_entry%field(1)%subfield(2:size(cmd_line_entry%field(1)%subfield))%name.eq."nc")) then
      output%noclobber = .true.
    endif
    if (any(cmd_line_entry%field(1)%subfield(2:size(cmd_line_entry%field(1)%subfield))%name.eq."c")) then
      output%noclobber = .false.
    endif

    if (any(cmd_line_entry%field(1)%subfield(2:size(cmd_line_entry%field(1)%subfield))%name.eq."free")) then
      output%form = "f13.3"
    endif
    if (any(cmd_line_entry%field(1)%subfield(2:size(cmd_line_entry%field(1)%subfield))%name.eq."gp2h")) then
      output%gp2h = .true.
    endif
    if (any(cmd_line_entry%field(1)%subfield(2:size(cmd_line_entry%field(1)%subfield))%name.eq."height")) then
      output%height=.true.
    endif
    if (any(cmd_line_entry%field(1)%subfield(2:size(cmd_line_entry%field(1)%subfield))%name.eq."nan")) then
      output%nan=.true.
    endif

    if (any(cmd_line_entry%field(1)%subfield(2:size(cmd_line_entry%field(1)%subfield))%name.eq."prune")) then
      output%prune=.true.
    endif

    if (any(cmd_line_entry%field(1)%subfield(2:size(cmd_line_entry%field(1)%subfield))%name.eq."level")) then
      output%level=.true.
    endif
    if (any(cmd_line_entry%field(1)%subfield(2:size(cmd_line_entry%field(1)%subfield))%name.eq."rho")) then
      output%rho=.true.
    endif

    if (.not.log%sparse) write(log%unit, form_62) 'output file was set:', trim(basename(trim(output%name)))

    if (file_exists(output%name).and.output%noclobber) then
      call print_warning ("nc", more=trim(output%name), error=.true.)
    endif

    if (len(output%name).gt.0.and. output%name.ne."") then

      inquire(file = output%name, opened = file_already_opened, number = i)

      if (file_already_opened) then
        output%unit = i
      else
        open (newunit = output%unit, file = output%name, action = "write" )
      endif
    endif

  case ('-P')
    call parse_polygon(cmd_line_entry)

  case ('-w')
    if (.not.log%sparse) write(log%unit, form%i2) "warnings"

  case ('-q')

    if (cmd_line_entry%field(1)%full.ne."") then
      read (cmd_line_entry%field(1)%full,*) quiet_step
    else
      quiet=.true.
      quiet_step=0
    endif

    write(log%unit,form%i3) "quiet step", quiet_step

  case ('-U')
    select case (cmd_line_entry%field(1)%subfield(1)%name)
    case ("n","N")
      transfer_sp%if = .false.
    case default
      transfer_sp%if = .true.
    end select

    if (cmd_line_entry%field(1)%subfield(1)%name.ne."") then
      transfer_sp%method=cmd_line_entry%field(1)%subfield(1)%name
    endif

    if (.not. log%sparse) then
      write(log%unit,form%i2) &
        "force transfer SP from @HP to @H and RSP from @HRSP to @H [T/F]: ", transfer_sp%if
      write(log%unit, "(" // form%t2 //  "a)") &
        "force transfer SP on"
    endif

  case ("-J")
    call parse_level(cmd_line_entry)

  case ("-n")
    write(log%unit, form%i2) "dryrun"
    dryrun=.true.

  case ("--")
    call parse_long_option(             &
      cmd_line_entry,                   &
      version         = version,        &
      cdate           = cdate,          &
      gdate           = gdate,          &
      program_calling = program_calling &
      )

  case default
    if (.not.log%sparse) call print_warning("unknown argument "// cmd_line_entry%switch)
  endselect
end subroutine

! =============================================================================
!> This subroutine counts the command line arguments
!!
!! Depending on command line options set all initial parameters and reports it
!!
!! optional accepted_switches:
!!  if given check if cmdlineargs are accepted, if not ignore them
!! optional cmdlineargs:
!!  if .false. [default] run program anyway.
!!  if .true. stop program if no cmdline argumenst was given.
!!
!! \date 2012-12-20
!! \author M. Rajner
!! \date 2013-03-19 parsing negative numbers after space fixed
!!    (-S -11... was previously treated as two cmmand line entries, now only -?
!!    non-numeric terminates input argument)
! =============================================================================
subroutine intro(      &
    program_calling,   &
    accepted_switches, &
    cmdlineargs,       &
    version,           &
    cdate,             &
    gdate,             &
    fflags,            &
    compiler,          &
    host               &
    )
  use mod_cmdline
  use mr_utilities, only: file_exists
  use mr_parser, only: get_command_cleaned

  character(len=*), intent(in) :: program_calling
  character(len=*), intent (in), optional :: accepted_switches
  logical, intent (in), optional :: cmdlineargs
  character(*), intent (in), optional :: version, cdate, gdate, fflags, compiler, host
  integer :: i, j
  character(len = 855) :: dummy
  integer,dimension(8):: execution_date

  logical :: file_already_opened
  

  if(present(cmdlineargs).and.command_argument_count().eq.0) then
    if (cmdlineargs) call print_warning("args", program_calling=program_calling, error=.true.)
  endif

  call get_command_cleaned(dummy)
  call collect_args(dummy)

  if (any(cmd_line%switch.eq.'-h')                   &
    .and.if_accepted_switch("-h",accepted_switches)) &
    then
    call print_help(                         &
      program_calling   = program_calling,   &
      accepted_switches = accepted_switches)
    call exit
  endif

  if (                                               &
    any(cmd_line%switch.eq.'-v')                     &
    .and.if_accepted_switch("-v",accepted_switches)) then

    if (size(cmd_line).eq.1 .and. cmd_line(1)%full == "-vv") then
      write(output_unit,'(a)') __GRAT_VERSION__
      call exit
    endif

    if (size(cmd_line).eq.1 .and. cmd_line(1)%full == "-vg") then
      write(output_unit,'(a)') __GDATE__
      call exit
    endif

    if (size(cmd_line).eq.1 .and. cmd_line(1)%full == "-vd") then
      write(output_unit,'(a)') __CDATE__
      call exit
    endif

    call print_version (                 &
      program_calling = program_calling, &
      version         = version,         &
      cdate           = cdate,           &
      gdate           = gdate,           &
      fflags          = fflags ,         &
      compiler        = compiler,        &
      host            = host             &
      )
    call exit

  endif

  if (any(cmd_line%switch.eq.'-w')                   &
    .and.if_accepted_switch("-w",accepted_switches)) &
    then

    do i=1,size(cmd_line)
      if (cmd_line(i)%switch.eq."-w") then
        if ( &
          any(cmd_line(i)%field(1)%subfield(1:)%name.eq."n") &
          ) then
          warnings%if=.false.
        endif

        if ( &
          any(cmd_line(i)%field(1)%subfield(1:)%name.eq."t") &
          ) then
          warnings%time=.true.
        endif

        if ( &
          any(cmd_line(i)%field(1)%subfield(1:)%name.eq."a") &
          ) then
          warnings%all=.true.
        endif

        ! -ws -- strict warning
        if (                                                 &
          any(cmd_line(i)%field(1)%subfield(1:)%name.eq."s") &
          ) then
          warnings%strict  = .true.
          output%noclobber = .true.
          log%noclobber    = .true.
        endif

        if ( &
          any(cmd_line(i)%field(1)%subfield(1:)%name.eq."f") &
          ) then
          warnings%file_exist = .true.
        endif
      endif
    enddo
  endif

  if (.not.any(cmd_line%switch.eq.'-I')) then
    call parse_info()
  endif

  if (any(cmd_line%switch.eq.'-V')) then
    do i=1,size(cmd_line)
      if (cmd_line(i)%switch.eq."-V") then
        if ( &
          any(cmd_line(i)%field(1)%subfield(2:)%name.eq."s") &
          .or. &
          any(cmd_line(i)%field(1)%subfield(2:)%name.eq."sparse") &
          ) then
          log%sparse = .true.
        endif
        if (any(cmd_line(i)%field(1)%subfield(2:)%name.eq."nc")) then
          log%noclobber = .true.
        endif
        if (any(cmd_line(i)%field(1)%subfield(2:)%name.eq."c")) then
          log%noclobber = .false.
        endif

        if (any(cmd_line(i)%field(1)%subfield(2:)%name.eq."full")) then
          log%full = .true.
        endif

        if (len(trim(cmd_line(i)%field(1)%subfield(1)%name)).gt.0) then
          log%if = .true.
          log%name=cmd_line(i)%field(1)%subfield(1)%name

          if(cmd_line(i)%field(1)%subfield(1)%dataname.ne."") then
            log%name=trim(cmd_line(i)%field(1)%subfield(1)%name)     &
              //"@"//trim(cmd_line(i)%field(1)%subfield(1)%dataname)
          endif

          if (file_exists(log%name).and.log%noclobber) then
            if (.not.quiet) &
              call print_warning ("nc", more=trim(log%name), error=.true.)
          endif

          inquire(file = log%name, opened = file_already_opened, number = j)

          if (file_already_opened) then
            log%unit = j
          else
            open (newunit = log%unit, file = log%name, action = "write" )
          endif

        else
          log%unit=output_unit
        endif
      endif
    enddo

  else
    ! if you don't specify log file, or not switch on verbose mode
    ! all additional information will go to trash
    ! Change /dev/null accordingly if your file system does not
    ! support this name
    ! inquire(file = "/dev/null", opened = file_already_opened, number = j)
    open (newunit = log%unit, file = "/dev/null", action = "write" )

  endif

  if (.not. log%sparse) then
    call print_version (                 &
      program_calling = program_calling, &
      version         = version,         &
      cdate           = cdate,           &
      fflags          = fflags ,         &
      compiler        = compiler         &
      )
  endif

  call date_and_time (values = execution_date)

  write(log%unit, &
    '("Program started:", &
    1x,i4,2("-",i2.2), 1x,i2.2,2(":",i2.2),1x,"(",dp,SP,i3.2,"h UTC)")') &
    execution_date (1:3),execution_date(5:7),execution_date(4)/60
  write(log%unit, form%separator)

  write (log%unit, form%i0) "Command invoked:"
  call get_command(dummy)

  do i = 1, int(len(trim(dummy))/72)+1
    write (log%unit, '(a72)') trim(dummy(72*(i-1)+1:))
  enddo

  write(log%unit, form%separator)

  write (log%unit, form%i0) "Command parsing:"

  ! do i = 1, size(cmd_line)
  !   call parse_option(                  &
  !     cmd_line(i),                      &
  !     accepted_switches,                &
  !     version = version,                &
  !     cdate   = cdate,                  &
  !     gdate   = gdate,                  &
  !     program_calling = program_calling &
  !     )
  ! enddo

  call get_index()
  call check_arguments(program_calling = program_calling)
end subroutine

! =============================================================================
! =============================================================================
subroutine check_arguments(program_calling)
  use mod_date, only: date
  use mod_data, only: model, parse_level
  use mod_cmdline, only: cmd_line, method, quiet, ind, transfer_sp, &
    inverted_barometer
  use mod_site, only: gather_site_model_info
  use mod_green, only: parse_green

  character(len=*), intent(in) :: program_calling
  integer :: i
  logical :: ls_in_model

  if (program_calling.eq."grat") then

    ! assume method using for computation if not given explicitly
    if (.not.any(cmd_line%switch.eq.'-M')) then
      if (any(cmd_line%switch.eq.'-G')) then
        method(2)=.true.
        if (.not.quiet) call print_warning("method", more= "assuming 2D")
      else
        method(1)=.true.
        if (.not.quiet) call print_warning("method", more= "assuming 1D")
      endif
    endif

    if (method(2) .and. .not.any(cmd_line%switch.eq.'-G')) then
      call print_warning("green_missing", error=.true.)
    endif

    if (method(3)) then
      if(.not.any(cmd_line%switch.eq.'-G')) then
        call print_warning("no method 2D, so 3D result will be shifted")
        call parse_green()
      endif

      if (.not.any(cmd_line%switch.eq.'-J')) then
        call parse_level()
      endif
    endif

    if (ind%model%ls.eq.0) then
      ls_in_model = .false.
    elseif (.not.model(ind%model%ls)%if                 &
      .and..not.model(ind%model%ls)%if_constant_value) then
      ls_in_model = .false.
    else
      ls_in_model = .true.
    endif

    if (((method(2).or.method(3))                  &
      .and. inverted_barometer)                    &
      .and. .not.ls_in_model                       &
      .and. ind%green%ge.ne.0) then
      call print_warning(                          &
        "inverted barometer, but no landsea mask", &
        error=.true.                               &
        )
    endif

  endif

  do i=1, ubound(model,1)
    if (model(i)%autoload) then
      if (.not. allocated(date)) then
        call print_warning("alias_without_date", error=.true.)
      endif
    endif
  enddo

  call gather_site_model_info()

  if (ind%model%hp.ne.0.and. .not.transfer_sp%if) then
    call print_warning ("maybe use -U")
  endif

  if (transfer_sp%if .and. ind%model%hp.eq.0) then
    call print_warning ("-U but no @HP found")
  endif

  ! just to avoid error when no levels are found
  if (method(3)) then
    if (any([ind%model%gp].eq.0)) then
      call print_warning ("@GP is missing", error=.true.)
    endif
  endif

  if (.not.any(cmd_line%switch.eq.'-S')) then
    call print_warning("-S not given")
  endif

  if ((ind%green%ghn.ne.0 .or. ind%green%ghe.ne.0) .and. ind%green%gr.eq.0) then
    call print_warning("using @GHN or @GHE without @GR in green specification can cause seqfault")
  endif

end subroutine

! =============================================================================
!> This function is true if switch is used by calling program or false if it
!! is not
! =============================================================================
pure logical function if_accepted_switch(switch, accepted_switches)
  character(len= *), intent (in) :: switch
  character(len= *), intent (in), optional :: accepted_switches
  integer :: i

  if (.not.present(accepted_switches)) then
    if_accepted_switch=.true.
    return
  endif

  ! default
  if_accepted_switch=.false.

  ! loop trough accepted switches
  do i = 1, len(accepted_switches)
    if (switch(2:2).eq.accepted_switches(i:i)) then
      if_accepted_switch=.true.
      return
    endif
  enddo
end function

! =============================================================================
!> This subroutine parse -L option.
!!
!! \author M. Rajner
!! \date 2013.05.24
! =============================================================================
subroutine parse_moreverbose(cmd_line_entry)
  use mod_cmdline
  use mr_utilities, only: file_exists

  type (cmd_line_arg) :: cmd_line_entry
  integer :: i, j
  logical :: file_already_opened

  if(allocated(moreverbose)) then
    call print_warning ("repeated")
    return
  endif

  allocate(moreverbose(size(cmd_line_entry%field)))

  do i = 1, size(cmd_line_entry%field)

    moreverbose(i)%name     = trim(cmd_line_entry%field(i)%subfield(1)%name)
    moreverbose(i)%dataname = trim(cmd_line_entry%field(i)%subfield(1)%dataname)

    if (dataname(moreverbose(i)%dataname).ne."unknown") then

      if (moreverbose(i)%name.ne."") then

        if (any(cmd_line_entry%field(i)%subfield(2:)%name.eq."nc")) then
          moreverbose(i)%noclobber=.true.
          if (file_exists(moreverbose(i)%name)) then
            call print_warning ("nc", more=trim(moreverbose(i)%name), error=.true.)
          endif
        endif

        inquire(file = moreverbose(i)%name, opened = file_already_opened, number = j)

        if (file_already_opened) then
          moreverbose(i)%unit = j
        else
          open(                            &
            newunit = moreverbose(i)%unit, &
            file    = moreverbose(i)%name, &
            action  = 'write'              &
            )
        endif

      else
        moreverbose(i)%unit = output_unit
      endif
    endif

    if (                                                             &
      any(cmd_line_entry%field(i)%subfield(2:)%name.eq."s")          &
      .or.any(cmd_line_entry%field(i)%subfield(2:)%name.eq."sparse") &
      ) then
      moreverbose(i)%sparse = .true.
    endif

    write (log%unit, form_62)                        &
      trim(moreverbose(i)%name),                     &
      "<-", trim(dataname(moreverbose(i)%dataname)), &
      "|sparse: ", moreverbose(i)%sparse

  enddo
end subroutine

! =============================================================================
!> This subroutine parse -I option.
!!
!! \author M. Rajner
!! \date 2013-05-17
! =============================================================================
subroutine parse_info(cmd_line_entry)
  use mr_utilities, only:is_numeric
  use mod_cmdline
  type (cmd_line_arg), intent(in),optional :: cmd_line_entry
  integer :: i,j

  if(allocated(info)) then
    call print_warning ("repeated")
    return
  endif

  if (present(cmd_line_entry)) then
    allocate (info(size(cmd_line_entry%field)))
    do i = 1, size(cmd_line_entry%field)
      write(log%unit, form%i2) "Range:", i
      call info_defaults(info(i))

      do j = 1, size(cmd_line_entry%field(i)%subfield)
        if (is_numeric(cmd_line_entry%field(i)%subfield(j)%name)) then
          select case (cmd_line_entry%field(i)%subfield(j)%dataname)

          case ("DB")
            read (cmd_line_entry%field(i)%subfield(j)%name,*) info(i)%distance%start
            if (info(i)%distance%start.lt.0) then
              call print_warning("changing -I@DB to 0")
              info(i)%distance%start = 0
            endif

          case ("DE")
            read (cmd_line_entry%field(i)%subfield(j)%name,*) info(i)%distance%stop
            if (info(i)%distance%stop.gt.180) then
              call print_warning("changing -I@DE to 180")
              info(i)%distance%stop = 180
            endif

          case ("DS")
            read (cmd_line_entry%field(i)%subfield(j)%name,*) info(i)%distance%step

          case ("DD")
            read (cmd_line_entry%field(i)%subfield(j)%name,*) info(i)%distance%denser

          case ("AB")
            read (cmd_line_entry%field(i)%subfield(j)%name,*) info(i)%azimuth%start

          case ("AE")
            read (cmd_line_entry%field(i)%subfield(j)%name,*) info(i)%azimuth%stop

          case ("AD")
            read (cmd_line_entry%field(i)%subfield(j)%name,*) info(i)%azimuth%denser

          case ("AS")
            read (cmd_line_entry%field(i)%subfield(j)%name,*) info(i)%azimuth%step

          case ("HB")
            read (cmd_line_entry%field(i)%subfield(j)%name,*) info(i)%height%start

          case ("HE")
            read (cmd_line_entry%field(i)%subfield(j)%name,*) info(i)%height%stop

          case ("HD")
            read (cmd_line_entry%field(i)%subfield(j)%name,*) info(i)%height%denser

          case ("HS")
            read (cmd_line_entry%field(i)%subfield(j)%name,*) info(i)%height%step

          case ("HSP")
            info(i)%height_progressive=.true.
            read (cmd_line_entry%field(i)%subfield(j)%name,*) info(i)%height%step

          case ("3D")
            read (cmd_line_entry%field(i)%subfield(j)%name,*) info(i)%distance%stop_3d
          endselect

        else
          select case (cmd_line_entry%field(i)%subfield(j)%dataname)
          case ("I")
            read (cmd_line_entry%field(i)%subfield(j)%name,*) info(i)%interpolation
          endselect
        end if
      enddo

      if (info(i)%distance%denser.eq.0) info(i)%distance%denser = 1

      write(log%unit,                  &
        "("//form%t3//"                &
        'DB:',     f7.2,               &
        '|DE:',    f8.3,               &
        '|I:',     a,                  &
        '|DD:',    i2,                 &
        '|DS:',    f7.2,               &
        '|HB:',    f8.1,               &
        '|HE:',    f8.1,               &
        '|HS:',    f7.2,               &
        '|'                            &
        )")                            &
        info(i)%distance%start,        &
        info(i)%distance%stop,         &
        info(i)%interpolation,         &
        info(i)%distance%denser,       &
        info(i)%distance%step,         &
        info(i)%height%start,          &
        info(i)%height%stop,           &
        info(i)%height%step


      if (method(3)) then
        write(log%unit,                  &
          "("//form%t3//"                &
          'HSP:',   l,                   &
          '|3D:',    f7.2,               &
          '|3D:',    a,                  &
          '|3:@DE',  f7.2,               &
          '|3::ref', l,                  &
          '|'                            &
          )")                            &
          info(i)%height_progressive,    &
          info(i)%distance%stop_3d,      &
          pack(method3dnames,method3d) , &
          method3d_refinment_distance,   &
          method3d_compute_reference
      endif

      if (info(i)%distance%stop_3d.lt.info(i)%distance%stop.and.method(3)) then
        call print_warning (                                                       &
          "stop_3d distance is less " //                                           &
          "then stop distance - distant area filled with 2D result GN[d..] if any" &
          )
      endif

    enddo
  else
    allocate(info(1))
    call info_defaults(info(1))
  endif
end subroutine

! =============================================================================
! =============================================================================
subroutine info_defaults(info)
  use mod_cmdline, only: info_info
  type(info_info),intent(inout) :: info

  info%interpolation   = "n"

  info%distance%start  = 0.
  info%distance%stop   = 180.
  info%distance%denser = 1
  info%distance%step   = 0

  info%azimuth%start   = 0.
  info%azimuth%stop    = 360.
  info%azimuth%step    = 0
  info%azimuth%denser  = 1

  info%height%start    = 0.
  info%height%stop     = 60000.
  info%height%step     = 25.
  info%height%denser   = 1

  info%distance%stop_3d = 10.

end subroutine

!! =============================================================================
!! =============================================================================
subroutine print_help(program_calling, accepted_switches)
  character(*), intent(in) :: program_calling
  character(*), intent(in),optional :: accepted_switches
  integer :: help_unit, io_stat
  character(500)::line
  character(255)::syntax
  logical:: if_print_line = .false., if_optional=.true.

  if_print_line = .false.

  ! TODO make appropriate man documentation using
  ! include for common switches

  ! change this path according to your settings
  open(                                         &
    newunit = help_unit,                          &
    file="/home/mrajner/src/grat/dat/help.hlp", &
    action="read",                              &
    status="old"                                &
    )

  write (log%unit, "(a)", advance="no" ) program_calling
  ! first loop - print only syntax with squre brackets if parameter is optional
  do
    read (help_unit, '(a)', iostat=io_stat) line

    if ((io_stat==iostat_end .or. line(1:1) == "-") .and. if_print_line ) then
      if (if_optional) write(log%unit, '(a)', advance="no") " ["
      if (if_optional) write(log%unit, '(a)', advance="no") trim(syntax)
      if (if_optional) write(log%unit, '(a)', advance="no") "]"
    endif

    if (io_stat==iostat_end) then
      write(log%unit, '(a)') " "
      if_print_line = .false.
      exit
    endif
    if(line(1:1)=="-") then
      if(if_accepted_switch (line(1:2),accepted_switches )) then
        if_print_line = .true.
      else
        if(line(1:1)=="-") if_print_line=.false.
      endif
    endif

    if (line(5:13) == "optional " &
      .and. (line(2:2) == program_calling(1:1) .or. line(2:2)=="")) &
      then
      if_optional=.true.
    else if (line(5:13) == "mandatory") then
      if_optional=.false.
    endif
    if (line(2:2)=="s") then
      syntax = trim(adjustl(line(3:)))
    endif
  enddo
  rewind(help_unit)

  write(log%unit, form_60) 'Summary of available options for program '//program_calling
  ! second loop - print informations
  do
    read (help_unit, '(a)', iostat=io_stat) line
    if (io_stat==iostat_end) exit

    if(line(1:1)=="-") then

      !todo
      if(if_accepted_switch (line(1:2), accepted_switches)) then
        if_print_line = .true.
        write (log%unit, form_61 ) trim(line)

      else
        if(line(1:1)=="-") if_print_line=.false.

      endif

    else if (line(2:2)==program_calling(1:1) .or. line(2:2)=="s") then
      if (if_print_line) then
        write (log%unit, form_61 ) "  "//trim(line(3:))
      endif

    else if (line(2:2)=="") then
      if (if_print_line) write (log%unit, form_61 ) trim(line)

    endif

  enddo
  close(help_unit)
end subroutine

! =============================================================================
!> Attach full dataname by abbreviation
!!
!! \date 2013-03-21
!! \author M. Rajner
! =============================================================================
! todo split to appropriate modules and call
function dataname(abbreviation)
  character(len=2), intent(in) :: abbreviation
  character(len=40) :: dataname

  select case(abbreviation)
  case("n")
    dataname = "nearest"
  case("l")
    dataname = "bilinear"
  case("g")
    dataname = "green function used"
  case("p")
    dataname = "points"
  case("r")
    dataname = "results"
  case("a")
    dataname = "auxiliary"
  case("d")
    dataname = "dates"
  case("s")
    dataname = "summary"
  case("o")
    dataname = "ocean conserve mass"
  case("t")
    dataname = "total mass"
  case("b")
    dataname = "progress bar"
  case("j")
    dataname = "level"
  case default
    dataname="unknown"
  end select
end function


! =============================================================================
!> This soubroutine stores indexes of specific dataname for data, green
!! functions, polygon etc.
! =============================================================================
subroutine get_index()
  use mod_polygon, only: polygon
  use mod_data,    only: model
  use mod_green,   only: green
  use mod_cmdline, only: ind, moreverbose

  integer(2) :: i

  do i = 1, ubound(model,1)
    select case (model(i)%dataname)
    case ("SP")
      ind%model%sp = i
    case ("EWT")
      ind%model%ewt = i
    case ("T")
      ind%model%t = i
    case ("RSP")
      ind%model%rsp = i
    case ("HRSP")
      ind%model%hrsp = i
    case ("LS")
      ind%model%ls = i
    case ("H")
      ind%model%h = i
    case ("HP")
      ind%model%hp = i
    case ("GP")
      ind%model%gp = i
    case ("VT")
      ind%model%vt = i
    case ("VSH")
      ind%model%vsh = i
    endselect
  enddo

  do i = 1, ubound(moreverbose,1)
    select case (moreverbose(i)%dataname)
    case ("p")
      ind%moreverbose%p = i
    case ("g")
      ind%moreverbose%g = i
    case ("a")
      ind%moreverbose%a = i
    case ("d")
      ind%moreverbose%d = i
    case ("r")
      ind%moreverbose%r = i
    case ("s")
      ind%moreverbose%s = i
    case ("o")
      ind%moreverbose%o = i
    case ("t")
      ind%moreverbose%t = i
    case ("b")
      ind%moreverbose%b = i
    case ("n")
      ind%moreverbose%n = i
    case ("l")
      ind%moreverbose%l = i
    case ("j")
      ind%moreverbose%j = i
    case ("v")
      ind%moreverbose%v = i
    end select
  enddo

  do i = 1, ubound(green,1)
    select case (green(i)%dataname)
    case ("GE")
      ind%green%ge    = i
    case ("GEGdt")
      ind%green%gegdt = i
    case ("GN")
      ind%green%gn    = i
    case ("GNc")
      ind%green%gnc   = i
    case ("GR")
      ind%green%gr    = i
    case ("GHN")
      ind%green%ghn   = i
    case ("GHE")
      ind%green%ghe   = i
    case ("GG")
      ind%green%gg    = i
    case ("GNdt")
      ind%green%gndt  = i
    case ("GNdh")
      ind%green%gndh  = i
    case ("GNdz")
      ind%green%gndz  = i
    case ("GNdz2")
      ind%green%gndz2 = i
    case default
      call print_warning("not recoginzed green type: "//green(i)%dataname)
    endselect
  enddo

  do i = 1, ubound(polygon,1)
    select case (polygon(i)%dataname)
    case ("E","")
      ! assume polygon is for elastic part
      ind%polygon%e = i
    case ("N")
      ind%polygon%n = i
    endselect
  enddo

end subroutine

! =============================================================================
! git more information about executable
! or use long version
! =============================================================================
subroutine parse_long_option(cmd_line_entry, version, cdate, gdate, program_calling)
  use mod_cmdline,   only: cmd_line_arg

  type(cmd_line_arg) :: cmd_line_entry
  character(len=*), optional :: version, cdate, gdate, program_calling

  select case (trim(cmd_line_entry%full))

  case ("--date", "--gdate", "--version")
    if (present(cdate)) then
      write(output%unit, '(a)') gdate(1:4)//gdate(6:7)//gdate(9:10)
    endif

  case ("--cdate")
    if (present(cdate)) then
      write(output%unit, '(a)') cdate(1:4)//cdate(6:7)//cdate(9:10)
    endif

  case ("--help")
    call print_warning("please refer to `"// program_calling // "` man pages")

  case default
    call print_warning("not recoginzed long option")

  end select

  call exit(0)
end subroutine
end module
