module mod_parser
  use mod_constants, only : dp
  use iso_fortran_env
  use mod_printing
  !----------------------------------------------------
  ! various
  !----------------------------------------------------
  !  character (len = 2) :: method = "2D"  !< computation method


  ! todo --- make @ like for models
  !  character(len=5) :: green_names(5) = [ "GN   ", "GN/dt", "GN/dh","GN/dz","GE   "]

contains
! =============================================================================
!> This subroutine counts the command line arguments and parse appropriately
! =============================================================================
subroutine parse_option (cmd_line_entry , program_calling ,accepted_switches)
  use mod_site,    only: parse_site
  use mod_date,    only: parse_date
  use mod_polygon, only: parse_polygon
  use mod_data,    only: parse_model
  use mod_green,   only: parse_green
  use mod_cmdline

  type(cmd_line_arg),intent(in):: cmd_line_entry
  character(len=*), optional :: program_calling, accepted_switches
  integer :: i

  write(log%unit, form_61) cmd_line_entry%switch , "{", trim(cmd_line_entry%full) ,"}"
  if(.not.if_accepted_switch(cmd_line_entry%switch, accepted_switches= accepted_switches)) &
    then
    write(log%unit, form_62) 'this switch is not accepted'
    return
  endif

  select case (cmd_line_entry%switch)
  case ('-V')
    write(log%unit, form_62) 'verbose mode' ,trim(log%name)
    if (len(trim(cmd_line_entry%field(1)%subfield(1)%name)).gt.0) then
      write(log%unit, form_62) 'the log file was set:' ,log%name
    endif
  case ('-S','-R')
    call parse_site(cmd_line_entry)
  case ("-I")
    call parse_info(cmd_line_entry)
  case ("-L")
    call parse_moreverbose(cmd_line_entry)
  case ("-B")
    if (cmd_line_entry%field(1)%subfield(1)%name.eq."N" ) &
      inverted_barometer = .false.
  case ('-D')
    call parse_date(cmd_line_entry)
  case ('-F')
    call parse_model(cmd_line_entry)
  case ("-G")
    call parse_green(cmd_line_entry)
  case ('-M')
    !    !> \todo rozbudować
    !    method = cmd_line_entry%field(1)%subfield(1)%name
    !    write(log%unit, form_62), 'method was set: ' , method
  case ('-o')
    output%if=.true.
    output%name=cmd_line_entry%field(1)%subfield(1)%name
    write(log%unit, form_62), 'output file was set: ' , trim(output%name)
    if (len(output%name).gt.0.and. output%name.ne."") then
      open (newunit = output%unit , file = output%name , action = "write" )
    endif
  case ('-P')
    call parse_polygon(cmd_line_entry)
  case default
    write(log%unit,form_62), "unknown argument: IGNORING"
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
subroutine intro (program_calling, accepted_switches , cmdlineargs , version)
  use mod_cmdline
  character(len=*), intent(in) :: program_calling
  character(len=*) , intent (in), optional :: accepted_switches
  logical , intent (in), optional :: cmdlineargs
  character(*) , intent (in), optional :: version
  integer :: i
  character(len=355) :: dummy,dummy_cleaned
  integer,dimension(8):: execution_date 

  if(present(cmdlineargs).and.cmdlineargs.and.iargc().eq.0) then
    write(output_unit , '(a)' ) , &
      'No cmd line args! Try: ./'//program_calling//' -h' 
    call exit
  endif

  call collect_args()

  if (any(cmd_line%switch.eq.'-h') &
    .and.if_accepted_switch("-h",accepted_switches)) &
    then
    call print_help(program_calling=program_calling, &
      accepted_switches = accepted_switches)
    call exit
  endif
  if (any(cmd_line%switch.eq.'-v') &
    .and.if_accepted_switch("-v",accepted_switches)) &
    then
    call print_version &
      (program_calling=program_calling, version=version)
    call exit
  endif
  if (any(cmd_line%switch.eq.'-V')) then
    if_verbose = .true.
    do i=1,size(cmd_line)
      if (cmd_line(i).switch.eq."-V") then
        if (len(trim(cmd_line(i)%field(1)%subfield(1)%name)).gt.0) then
          log%if = .true.
          log%name = trim(cmd_line(i)%field(1)%subfield(1)%name)
          open (newunit=log%unit , file = log%name , action='write')
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
    open (newunit=log%unit, file = "/dev/null", action = "write" )
  endif

  call print_version(program_calling=program_calling, version=version)
  call date_and_time (values = execution_date)
  write(log%unit, & 
    '("Program started:", & 
    1x,i4,2("-",i2.2), 1x,i2.2,2(":",i2.2),1x,"(",dp,SP,i3.2,"h UTC)")'),&
    execution_date (1:3),execution_date(5:7),execution_date(4)/60
  write(log%unit, form%separator)
  write (log%unit, form%i0) "Command invoked:"
  call get_command(dummy)
  do i = 1, int(len(trim(dummy))/72)+1
    write (log%unit, '(a72)' ) trim(dummy(72*(i-1)+1:))
  enddo

  write(log%unit, form%separator)
  write (log%unit, form%i0) "Command parsing:"
  do i =1 , size(cmd_line)
    call parse_option(cmd_line(i))
  enddo
end subroutine

! =============================================================================
!> This function is true if switch is used by calling program or false if it
!! is not
! =============================================================================
logical function if_accepted_switch (switch , accepted_switches)
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
  do i =1, len(accepted_switches)
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
subroutine parse_moreverbose (cmd_line_entry)
  use mod_cmdline
  type (cmd_line_arg)  :: cmd_line_entry
  integer :: i,j

  if(allocated(moreverbose)) then
    call print_warning ("repeated")
    return
  endif
  allocate(moreverbose(size(cmd_line_entry%field)))
  do i = 1, size(cmd_line_entry%field)
    moreverbose(i)%name = trim(cmd_line_entry%field(i)%subfield(1)%name)
    moreverbose(i)%dataname = trim(cmd_line_entry%field(i)%subfield(1)%dataname)
    if (dataname(moreverbose(i)%dataname).ne."unknown") then 
      if (moreverbose(i)%name.ne."") then
        open( & 
          newunit=moreverbose(i)%unit, &
          file =moreverbose(i)%name , action = 'write' & 
          )
      else
        moreverbose(i)%unit = output_unit
      endif
    endif
    write (log%unit , form_62), trim(moreverbose(i)%name) , &
      "<-", dataname(moreverbose(i)%dataname)
  enddo
end subroutine

! =============================================================================
!> This subroutine parse -I option. 
!!
!! \author M. Rajner
!! \date 2013-05-17
! =============================================================================
subroutine parse_info (cmd_line_entry)
  use mod_utilities, only:is_numeric
  use mod_cmdline
  type (cmd_line_arg)  :: cmd_line_entry
  integer :: i,j

  if(allocated(info)) then
    call print_warning ("repeated")
    return
  endif

  allocate (info(size(cmd_line_entry%field)))
  do i = 1 , size(cmd_line_entry%field)
    write(log%unit, form%i2) , "Range:" , i
    info(i)%distance%start=0.
    info(i)%distance%stop=180.
    do j = 1 , size(cmd_line_entry%field(i)%subfield)
      if (is_numeric(cmd_line_entry%field(i)%subfield(j)%name)) then
        select case (cmd_line_entry%field(i)%subfield(j)%dataname)
        case ("DB")
          read (cmd_line_entry%field(i)%subfield(j)%name,*) info(i)%distance%start
        case ("DE")
          read (cmd_line_entry%field(i)%subfield(j)%name,*) info(i)%distance%stop
        case ("AB")
          read (cmd_line_entry%field(i)%subfield(j)%name,*) info(i)%azimuth%start
        case ("AE")
          read (cmd_line_entry%field(i)%subfield(j)%name,*) info(i)%azimuth%stop
        case ("DS")
          read (cmd_line_entry%field(i)%subfield(j)%name,*) info(i)%distance%step
        case ("DD")
          read (cmd_line_entry%field(i)%subfield(j)%name,*) info(i)%distance%denser
        case ("AD")
          read (cmd_line_entry%field(i)%subfield(j)%name,*) info(i)%azimuth%denser
        endselect
      else 
        select case (cmd_line_entry%field(i)%subfield(j)%dataname)
        case ("I")
          read (cmd_line_entry%field(i)%subfield(j)%name,*) info(i)%interpolation
          write(log%unit, form_63) , "interpolation:", info(i)%interpolation
        endselect
      end if
    enddo
    if (i.gt.1) then
      if (info(i)%distance%start.lt.info(i-1)%distance%stop) then
        info(i-1)%distance%stop = info(i)%distance%start
      endif
      if (info(i)%distance%stop.lt.info(i)%distance%start) then
        info(i)%distance%stop = info(i)%distance%start
      endif
    endif
  enddo
end subroutine

! =============================================================================
!> Print version of program depending on program calling
!! 
!! \author M. Rajner
!! \date 2013-03-06
! =============================================================================
subroutine print_version (program_calling,version)
  character(*) :: program_calling 
  integer :: version_unit , io_stat
  character(*) , optional :: version

  write(log%unit , form_header ) 
  write(log%unit,form_inheader ) , trim(program_calling)
  write(log%unit,form_inheader ) , version
  write(log%unit , form_header ) 
  write(log%unit,form_inheader ) , 'Copyright 2013 by Marcin Rajner'
  write(log%unit,form_inheader ) , 'Warsaw University of Technology'
  write(log%unit,form_inheader ) , 'License: GPL v3 or later'
  write(log%unit , form_header ) 
end subroutine

!! =============================================================================
!! =============================================================================
subroutine print_help (program_calling, accepted_switches)
  character(*) , intent(in) :: program_calling
  character(*) , intent(in),optional :: accepted_switches
  integer :: help_unit , io_stat
  character(500)::line
  character(255)::syntax
  logical:: if_print_line = .false., if_optional=.true.

  if_print_line=.false.

  ! change this path according to your settings
  open(newunit=help_unit, file="~/src/grat/dat/help.hlp", action="read",status="old")

  write (log%unit ,"(a)" , advance="no" ) program_calling
  ! first loop - print only syntax with squre brackets if parameter is optional
  do 
    read (help_unit , '(a)', iostat=io_stat) line
    if ((io_stat==iostat_end .or. line(1:1) == "-") .and. if_print_line ) then
      if (if_optional) write(log%unit, '(a)' , advance="no") " ["
      if (if_optional) write(log%unit, '(a)' , advance="no") trim(syntax)
      if (if_optional) write(log%unit, '(a)' , advance="no") "]"
    endif
    if (io_stat==iostat_end) then
      write(log%unit, *) " " 
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

    if (line(5:13) == "optional " .and. (line(2:2) == program_calling(1:1) .or. line(2:2)=="")) then
      if_optional=.true.
    else if (line(5:13) == "mandatory") then
      if_optional=.false.
    endif
    if (line(2:2)=="s") then
      syntax = trim(adjustl(line(3:)))
    endif
  enddo
  rewind(help_unit)

  write(log%unit , form_60) , 'Summary of available options for program '//program_calling
  ! second loop - print informations
  do 
    read (help_unit , '(a)', iostat=io_stat) line
    if (io_stat==iostat_end) exit

    if(line(1:1)=="-") then
      !todo
      if(if_accepted_switch (line(1:2),accepted_switches )) then
        if_print_line = .true.
        write (log%unit , form_61 ) trim(line)
      else
        if(line(1:1)=="-") if_print_line=.false.
      endif
    else if (line(2:2)==program_calling(1:1) .or. line(2:2)=="s") then
      if (if_print_line) then
        write (log%unit , form_61 ) "  "//trim(line(3:))
      endif
    else if (line(2:2)=="") then
      if (if_print_line) write (log%unit , form_61 ) trim(line)
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
  character(len=40) :: dataname
  character(len=2) :: abbreviation

  dataname="unknown"
  !  if (abbreviation.eq."LS") dataname = "Land-sea mask"
  !  if (abbreviation.eq."SP") dataname = "Surface pressure"
  !  if (abbreviation.eq."RS") dataname = "Reference surface pressure"
  if (abbreviation.eq."n")  dataname = "nearest"
  if (abbreviation.eq."l")  dataname = "bilinear"
  !  if (abbreviation.eq."GN") dataname = "Green newtonian"
end function
end module
