! =============================================================================
!> \file
!! \brief This module sets the initial values for parameters
!! reads from command line and gives help
!! it allows to specify commands with or without spaces therefore it is 
!! convienient to use with auto completion of names
! =============================================================================
module mod_cmdline

  use mod_constants, only: dp 
  use, intrinsic :: iso_fortran_env

  implicit none

  !----------------------------------------------------
  ! Greens function
  !----------------------------------------------------
  type green_functions
    character (len=255) :: name
    character (len=25) :: dataname
    real(dp),allocatable,dimension(:) :: distance
    real(dp),allocatable,dimension(:) :: data
  end type
  type(green_functions), allocatable , dimension(:) :: green

  !----------------------------------------------------
  ! polygons
  !----------------------------------------------------
  type polygon_data
    logical :: use
    real(dp), allocatable , dimension (:,:) :: coords
  end type

  type polygon_info
    integer :: unit
    character(:), allocatable  :: name
    type(polygon_data) , dimension (:) , allocatable :: polygons
    logical :: if
    ! global setting (+|-) which override this in polygon file
    character(1):: pm
  end type

  type(polygon_info) , dimension (2) :: polygons

  !----------------------------------------------------
  ! info
  !----------------------------------------------------
  type range
    real(dp):: start
    real(dp):: stop
    integer :: denser
  end type
  type info_info
    type (range):: distance,azimuth
    real (dp) :: interpolation
  end type
  type(info_info), dimension(:), allocatable:: info

  !----------------------------------------------------
  ! dates
  !----------------------------------------------------
  type dateandmjd
    real(dp) :: mjd
    integer,dimension (6) :: date
  end type

  real(dp) :: cpu_start , cpu_finish  
  type(dateandmjd) , allocatable,dimension (:) :: dates

  !----------------------------------------------------
  ! command line entry
  !----------------------------------------------------
  type subfield_info
    character (len=255) :: name
    character (len=25) :: dataname
  end type
  type field_info
    character (len=255) :: full
    type(subfield_info), allocatable, &
      dimension(:)  :: subfield
  end type
  type cmd_line_arg
    character(2) :: switch
    type (field_info), allocatable, &
      dimension(:) :: field
    character (len=255) :: full
    logical :: accepted=.true.
  end type
  type(cmd_line_arg) , allocatable , dimension(:) :: cmd_line

  !---------------------------------------------------
  ! site information
  !---------------------------------------------------
  type site_data 
    character(:), allocatable :: name
    real(dp)                  :: lat,lon,height
  end type 

  type(site_data) , allocatable , dimension(:) :: sites

  !----------------------------------------------------
  ! various
  !----------------------------------------------------
  ! unit of scratch file
  integer,dimension(8):: execution_date !< To give time stamp of execution
  character (len = 2) :: method = "2D"  !< computation method

  !----------------------------------------------------
  ! Site names file
  !----------------------------------------------------
  character(:), allocatable &
    :: filename_site 
  integer :: fileunit_site

  type file
    !    character(:), allocatable :: name 
    character(60) :: name 

    ! varname , lonname,latname,levelname , timename
    character(len=50) :: names(5) = [ "z", "lon", "lat","level","time"]

    character(len=15) :: dataname

    integer :: unit = output_unit

    ! if file was determined
    logical :: if =.false.

    ! to read into only once
    logical :: first_call =.true.

    ! boundary of model e , w ,s ,n
    real(dp):: limits(4)

    real(dp), allocatable, dimension(:) :: lat , lon , time ,level
    integer , allocatable, dimension(:,: ) :: date

    real (dp), dimension(2) :: latrange , lonrange

    logical :: if_constant_value
    real(dp):: constant_value

    ! 4 dimension - lat , lon , level , mjd
    real(dp) , allocatable , dimension (:,:,:) :: data

    ! netcdf identifiers
    integer :: ncid
    integer :: interpolation = 1
  end type

  ! External files
  type(file) ::  log  , output 
  type(file) , allocatable, dimension (:) :: model , moreverbose

  ! todo --- make @ like for models
  !  character(len=5) :: green_names(5) = [ "GN   ", "GN/dt", "GN/dh","GN/dz","GE   "]

  logical :: if_verbose  = .false.  
  logical :: inverted_barometer  = .true.  

  character (50) :: interpolation_names (2) &
    = [ "nearest" , "bilinear" ]

  !----------------------------------------------------
  ! For preety printing
  !----------------------------------------------------
  character(len=255), parameter ::  &
    form_header    = '(60("#"))' , &
    form_separator = '("#",59("-"))' , &
    form_inheader  = '(("#"),1x,a56,1x,("#"))' , &
    form_60        = "(a,100(1x,g0))",          &
    form_61        = "(2x,a,100(1x,g0))",    &
    form_62        = "(4x,a,100(1x,g0))",       &
    form_63        = "(6x,100(x,g0))",       &
    form_64        = "(4x,4x,a,4x,a)"

contains
! =============================================================================
! =============================================================================
subroutine collect_args (accepted_switches)
  use mod_utilities, only: ntokens
  character(255) :: dummy , dummy_aux ,dummy_aux2
  character(len=*) , intent (in), optional :: accepted_switches
  integer :: i, j, n,  indeks_space,indeks_comma, indeks_at , indeks_colon

  call get_command_cleaned(dummy)

  allocate(cmd_line(ntokens(dummy)))
  do i=1, ntokens(dummy)
    indeks_space = index(dummy," ")
    cmd_line(i)%full= dummy(1:indeks_space-1)
    cmd_line(i)%switch=cmd_line(i)%full(1:2)
    if(present(accepted_switches).and. &
      .not.if_accepted_switch(cmd_line(i)%switch, accepted_switches)) &
      cmd_line(i)%accepted=.false.
    allocate(cmd_line(i)%field (count_separator (cmd_line(i)%full,",") + 1))

    dummy_aux = cmd_line(i)%full(3:)
    do j=1,size(cmd_line(i)%field)
      indeks_comma=index(dummy_aux,",")
      if (indeks_comma.gt.0) then
        cmd_line(i)%field(j)%full=dummy_aux(1:indeks_comma-1)
      else
        cmd_line(i)%field(j)%full=dummy_aux
      endif

      allocate(cmd_line(i)%field(j)%subfield &
        (count_separator (cmd_line(i)%field(j)%full,":") + 1))
      dummy_aux2 = cmd_line(i)%field(j)%full
      do n = 1 , count_separator(cmd_line(i)%field(j)%full,":")+1
        indeks_colon=index(dummy_aux2,":")
        if (indeks_colon.gt.0) then
          cmd_line(i)%field(j)%subfield(n)%name=dummy_aux2(1:indeks_colon-1)
        else
          cmd_line(i)%field(j)%subfield(n)%name=dummy_aux2
        endif
        dummy_aux2=dummy_aux2(indeks_colon+1:)
        indeks_at=index(cmd_line(i)%field(j)%subfield(n)%name,"@")
        if (indeks_at.gt.0) then
          cmd_line(i)%field(j)%subfield(n)%dataname = &
            cmd_line(i)%field(j)%subfield(n)%name(indeks_at+1:) 
          cmd_line(i)%field(j)%subfield(n)%name = &
            cmd_line(i)%field(j)%subfield(n)%name(1:indeks_at-1) 
        else
          cmd_line(i)%field(j)%subfield(n)%dataname = " "
        endif
      enddo

      dummy_aux=dummy_aux(indeks_comma+1:)
    enddo
    dummy= dummy(indeks_space+1:)
  enddo
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
subroutine intro (program_calling, accepted_switches , cmdlineargs )
  integer :: i
  character(len=255) :: dummy,dummy_cleaned
  character(len=*), intent(in) :: program_calling
  character(len=*) , intent (in), optional :: accepted_switches
  logical , intent (in), optional :: cmdlineargs
  character(2) :: delete

  if(present(cmdlineargs).and.cmdlineargs.and.iargc().eq.0) then
    write(output_unit , '(a)' ) , &
      'No cmd line args! Try: ./'//program_calling//' -h' 
    call exit
  endif

  call collect_args(accepted_switches=accepted_switches)

  if (any(cmd_line%switch.eq.'-h') &
    .and.if_accepted_switch("-h",accepted_switches)) &
    then
    log%unit=error_unit
    call print_help(program_calling=program_calling, &
      accepted_switches = accepted_switches)
    call exit
  endif
  if (any(cmd_line%switch.eq.'-v') &
    .and.if_accepted_switch("-v",accepted_switches)) &
    then
    log%unit=error_unit
    call print_version(program_calling=program_calling)
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

  call print_version(program_calling=program_calling)
  write (log%unit, form_61) "Command invoked:"
  call get_command(dummy)
  write (log%unit, form_62) trim(dummy)

  do i =1 , size(cmd_line)
    call parse_option(cmd_line(i))
  enddo
end subroutine

! ==============================================================================
!! This subroutine removes unnecesary blank spaces from cmdline entry
!!
!! Marcin Rajner
!! \date 2013-05-13
!! allows specification like '-F file' and '-Ffile'
!! but  if -[0,9] it is treated as number belonging to switch (-S -2)
!! but  if -[\s,:] do not start next command line option
! ==============================================================================
subroutine get_command_cleaned(dummy)
  character(*) , intent(out) :: dummy
  character(255) :: a , b , arg
  integer :: i
  dummy=" " 
  do i = 1 , iargc()
    call get_command_argument(i,a)
    call get_command_argument(i+1,b)
    if (check_if_switch_or_minus(a)) then
      arg = trim(a)
    else
      arg=trim(arg)//trim(a)
    endif
    if(check_if_switch_or_minus(b).or.i.eq.iargc()) &
      then
      if(trim(dummy).eq."") then
        dummy=trim(arg)
      else
        dummy=trim(dummy)//" "//trim(arg)
      endif
    endif
  enddo
end subroutine

! ==============================================================================
!> Check if - starts new option in command line or is just a minus in command
!! line entry
!!
!! if after '-' is space or number or ',' or ':' (field separators) do not start
!! next option for command line
!! If switch return .true. otherwise return .false
!!
!! \author M. Rajner
!! \date 2013-03-19
! ==============================================================================
function check_if_switch_or_minus(dummy)
  use mod_utilities, only: is_numeric
  logical:: check_if_switch_or_minus
  character(*) :: dummy

  check_if_switch_or_minus = .false.
  if (dummy(1:1).eq."-") check_if_switch_or_minus = .true.
  if (dummy(2:2).eq." ") check_if_switch_or_minus = .false.
  if (dummy(2:2).eq.",") check_if_switch_or_minus = .false.
  if (dummy(2:2).eq.":") check_if_switch_or_minus = .false.
  if (is_numeric(dummy(2:2))) check_if_switch_or_minus = .false.
end function

! =============================================================================
!> This function is true if switch is used by calling program or false if it
!! is not
! =============================================================================
logical function if_accepted_switch (switch , accepted_switches)
  character(len= *), intent (in) :: switch ,accepted_switches
  integer :: i

  ! default
  if_accepted_switch=.false.
  ! loop trough accepted switches
  do i =1, len(accepted_switches)
    if (switch(2:2).eq.accepted_switches(i:i)) if_accepted_switch=.true.
  enddo
end function

! =============================================================================
!> This subroutine counts the command line arguments and parse appropriately
! =============================================================================
subroutine parse_option (cmd_line_entry , program_calling ,accepted_switches)
  use mod_utilities, only : file_exists, is_numeric
  type(cmd_line_arg),intent(in):: cmd_line_entry
  character(len=*), optional :: program_calling,accepted_switches
  integer :: i

  write(log%unit, form_61) cmd_line_entry%switch , "{", trim(cmd_line_entry%full) ,"}"
  if(.not.cmd_line_entry%accepted) then
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
    ! check if format is proper for site
    ! i,e. -Sname,B,L[,H]
    if (.not. allocated(sites)) then
      if(index(cmd_line_entry%field(1)%subfield(1)%name, "/" ).ne.0 &
        .or.(cmd_line_entry%field(1)%subfield(1)%name.eq. "g" ) ) &
        then
        call parse_GMT_like_boundaries ( cmd_line_entry )
        return
      endif
      if (size(cmd_line_entry%field).ge.3) then
        if (is_numeric(cmd_line_entry%field(2)%subfield(1)%name) &
          .and.is_numeric(cmd_line_entry%field(3)%subfield(1)%name) &
          .and.index(cmd_line_entry%field(1)%subfield(1)%name, "/" ).eq.0 &
          .and.(.not.cmd_line_entry%field(1)%subfield(1)%name.eq. "Rg" ) &
          ) then
          allocate (sites(1))
          sites(1)%name = trim(cmd_line_entry%field(1)%subfield(1)%name)
          read ( cmd_line_entry%field(2)%subfield(1)%name , * ) sites(1)%lat
          if (abs(sites(1)%lat).gt.90.) sites(1)%lat = sign(90.,sites(1)%lat) 
          read ( cmd_line_entry%field(3)%subfield(1)%name , * ) sites(1)%lon
          if (sites(1)%lon.ge.360.) sites(1)%lon = mod(sites(1)%lon,360.)
          if (size(cmd_line_entry%field).ge.4) then
            if (is_numeric (cmd_line_entry%field(4)%subfield(1)%name ) ) then
              read (cmd_line_entry%field(4)%subfield(1)%name, * ) sites(1)%height
            endif
          endif
          write(log%unit, form_62) 'the site was set (BLH):' , &
            sites(1)%name, real(sites(1)%lat) , &
            real(sites(1)%lon) , real(sites(1)%height) 
          return
        endif
      endif
      !or read sites from file
      if (file_exists (cmd_line_entry%field(1)%subfield(1)%name))  then
        write(log%unit, form_62) 'the site file was set:' , &
          cmd_line_entry%field(1)%subfield(1)%name
        call read_site_file (cmd_line_entry%field(1)%subfield(1)%name)
        return
      endif
      call print_warning ( "site" )
    else
      call print_warning ( "repeated" , error_unit)
    endif
  case ("-I")
    call parse_info(cmd_line_entry)
  case ("-L")
    write (log%unit , form_62) "printing additional information:"
    allocate(moreverbose(size(cmd_line_entry%field)))
    do i = 1, size(cmd_line_entry%field)
      moreverbose(i)%name = trim(cmd_line_entry%field(i)%subfield(1)%name)
      moreverbose(i)%dataname = trim(cmd_line_entry%field(i)%subfield(2)%name)
      if (dataname(moreverbose(i)%dataname).ne."unknown") then 
        if (moreverbose(i)%name.ne."") then
          open(newunit=moreverbose(i)%unit, &
            file =moreverbose(i)%name , action = 'write')
        else
          moreverbose(i)%unit = output_unit
        endif
      endif
      write (log%unit , form_62),  moreverbose(i)%name , &
        "<-", dataname(moreverbose(i)%dataname)
    enddo
  case ("-B")
    if (cmd_line_entry%field(1)%subfield(1)%name.eq."N" ) inverted_barometer = .false.
  case ('-D')
    call parse_dates (cmd_line_entry)
  case ('-F')
    if (allocated(model)) then
      call print_warning ('repeated')
    else
      call get_model_info (cmd_line_entry)
    endif
  case ("-G")
    call parse_green(cmd_line_entry)
  case ('-M')
    !> \todo rozbudować
    method = cmd_line_entry%field(1)%subfield(1)%name
    write(log%unit, form_62), 'method was set: ' , method
  case ('-o')
    output%if=.true.
    output%name=cmd_line_entry%field(1)%subfield(1)%name
    write(log%unit, form_62), 'output file was set: ' , output%name 
    if (len(output%name).gt.0.and. output%name.ne."") then
      open (newunit = output%unit , file = output%name , action = "write" )
    endif
  case ('-P')
    do i = 1, size(cmd_line_entry%field)
      ! prevent from multiple -P
      if (polygons(i)%if) then
        call print_warning ("repeated")
        return
      endif
      polygons(i)%name=cmd_line_entry%field(i)%subfield(1)%name
      if (file_exists((polygons(i)%name))) then
        write(log%unit, form_62), 'polygon file was set: ' , polygons(i)%name
        polygons(i)%if=.true.
        polygons(i)%pm = trim(cmd_line_entry%field(i)%subfield(2)%name)
      else
        write(log%unit, form_62), 'file do not exist. Polygon file was IGNORED'
      endif
    enddo
  case default
    write(log%unit,form_62), "unknown argument: IGNORING"
  end select
end subroutine 


! =============================================================================
!> This subroutine parse -I option. 
!!
!! \author M. Rajner
!! \date 2013-05-17
! =============================================================================
subroutine parse_info (cmd_line_entry)
  use mod_utilities, only:is_numeric
  type (cmd_line_arg)  :: cmd_line_entry
  integer :: i,j

  allocate (info(size(cmd_line_entry%field)))
  do i = 1 , size(cmd_line_entry%field)
    do j = 1 , size(cmd_line_entry%field(i)%subfield)
      print * , j
      if (is_numeric(cmd_line_entry%field(i)%subfield(j)%name)) then
        select case (cmd_line_entry%field(i)%subfield(j)%dataname)
        case ("B")
          read (cmd_line_entry%field(i)%subfield(j)%name,*) info(i)%distance%start
        case ("E")
          read (cmd_line_entry%field(i)%subfield(j)%name,*) info(i)%distance%stop
        case ("b")
          read (cmd_line_entry%field(i)%subfield(j)%name,*) info(i)%azimuth%start
        case ("e")
          read (cmd_line_entry%field(i)%subfield(j)%name,*) info(i)%azimuth%stop
        case ("I")
          read (cmd_line_entry%field(i)%subfield(j)%name,*) info(i)%interpolation
        end select
        end if
      enddo
      if (i.gt.1) then
        if (info(i)%distance%start.lt.info(i-1)%distance%stop) then
          info(i)%distance%start = info(i-1)%distance%stop
        endif
        endif

      enddo
      !      write( log%unit , form_62 , advance="no" ) "interpolation method was set:"
      !      do i = 1 , size(cmd_line_entry%field)
      !        if (is_numeric(cmd_line_entry%field(i)%subfield(1)%name)) then
      !          read ( cmd_line_entry%field(i)%subfield(1)%name , * ) model(i)%interpolation
      !          write(log%unit , '(a10,x,$)' ) interpolation_names (model(i)%interpolation)
      !          if (model(i)%interpolation.gt.size(interpolation_names)) then
      !            model(i)%interpolation=1
      !          endif
      !        endif
      !      enddo
      !      write(log%unit , *)

      !    if (cmd_line_entry%field(i)%subfield(1)%dataname.eq."A") then
      !      if (is_numeric(cmd_line_entry%field(i)%subfield(1)%name)) then
      !        read(cmd_line_entry%field(i)%subfield(1)%name,*) denser%azimuth
      !        write(log%unit, form_63) "Denser azimuth: " , denser%azimuth 
      !      endif
      !      elseif (cmd_line_entry%field(i)%subfield(1)%dataname.eq."D") then
      !      if (is_numeric(cmd_line_entry%field(i)%subfield(1)%name)) then
      !        read(cmd_line_entry%field(i)%subfield(1)%name,*) denser%distance
      !        write(log%unit, form_63) "Denser distance:" , denser%distance
      !      endif
    end subroutine

    ! =============================================================================
    !> This subroutine parse -G option -- Greens function.
    !!
    !! This subroutines takes the -G argument specified as follows:
    !!   -G 
    !!
    !! \author M. Rajner
    !! \date 2013-03-06
    ! =============================================================================
    subroutine parse_green ( cmd_line_entry)
      use mod_utilities, only: file_exists, is_numeric, skip_header
      type (cmd_line_arg)  :: cmd_line_entry
      character (60) :: filename
      integer :: i , iunit , io_status , lines ,  ii
      integer :: fields(2)= [1,2]
      real (dp) , allocatable , dimension(:) :: tmp

      write(log%unit , form_62) "Green function file was set:"
      allocate (green (size(cmd_line_entry%field)))

      do i = 1 , size(cmd_line_entry%field)
!                  if (cmd_line_entry%field(i)%subfield(1)%dataname.eq."GN") then
!                  green
!                endif

!         shortcuts for common files
              if (.not.file_exists(cmd_line_entry%field(i)%subfield(1)%name) &
                .and. (.not. cmd_line_entry%field(i)%subfield(1)%name.eq."merriam" &
                .and.  .not. cmd_line_entry%field(i)%subfield(1)%name.eq."huang" &
                .and.  .not. cmd_line_entry%field(i)%subfield(1)%name.eq."rajner" )) then
                  cmd_line_entry%field(i)%subfield(1)%name="merriam"
                endif

          ! change the paths accordingly
                if (cmd_line_entry%field(i)%subfield(1)%name.eq."merriam") then
                  green(1)%name="/home/mrajner/src/grat/dat/merriam_green.dat"
                  if (i.eq.1) fields = [1,2]
                  if (i.eq.2) fields = [1,3]
                  if (i.eq.3) fields = [1,4]
                  if (i.eq.4) fields = [1,4]
                  if (i.eq.5) fields = [1,6]
                else if (cmd_line_entry%field(i)%subfield(1)%name.eq."huang") then
                  filename="/home/mrajner/src/grat/dat/huang_green.dat"
                  if (i.eq.1) fields = [1,2]
                  if (i.eq.2) fields = [1,3]
                  if (i.eq.3) fields = [1,4]
                  if (i.eq.4) fields = [1,5]
                  if (i.eq.5) fields = [1,6]
                else if (cmd_line_entry%field(i)%subfield(1)%name.eq."rajner") then
                  filename="/home/mrajner/src/grat/dat/rajner_green.dat"
                  if (i.eq.1) fields = [1,2]
                  if (i.eq.2) fields = [1,3]
                  if (i.eq.3) fields = [1,4]
                  if (i.eq.4) fields = [1,5]
                  if (i.eq.5) fields = [1,6]
                else if (file_exists(cmd_line_entry%field(i)%subfield(1)%name)) then
                  filename = cmd_line_entry%field(i)%subfield(1)%name
                  do ii=1, 2
                    if(is_numeric (cmd_line_entry%field(i)%subfield(ii+1)%name ) ) then
                      read( cmd_line_entry%field(i)%subfield(ii+1)%name, *) fields(ii)
                    endif
                  enddo
                endif

          ! todo move it to green module : read_green!!!
                allocate(tmp(max(fields(1),fields(2))))
                lines = 0
                open ( newunit =iunit,file=filename,action="read")
                do 
                  call skip_header (iunit)
                  read (iunit , * , iostat = io_status)
                  if (io_status == iostat_end) exit
                  lines = lines + 1
                enddo
                allocate (green(i)%distance(lines))
                allocate (green(i)%data(lines))
                rewind(iunit)
                lines = 0
                do 
                  call skip_header (iunit)
                  lines = lines + 1
                  read (iunit , * , iostat = io_status) tmp
                  if (io_status == iostat_end) exit
                  green(i)%distance(lines) = tmp (fields(1))
                  green(i)%data(lines)     = tmp (fields(2))
                enddo
                deallocate(tmp)
                close(iunit)

                ! file specific 
                if (cmd_line_entry%field(i)%subfield(1)%name.eq."merriam" .and. i.eq.4) then
                  green(i)%data = green(i)%data * (-1.)
                endif
                !    if (cmd_line_entry%field(i)%subfield(1)%name.eq."huang" .and. (i.eq.3.or.i.eq.4)) then
                !      green(i)%data = green(i)%data * 1000.
                !    endif
                write(log%unit , form_63) &
                  !                    trim(green_names(i))
                trim(cmd_line_entry%field(i)%subfield(1)%name),":", fields, &
                  dataname(cmd_line_entry%field(i)%subfield(1)%dataname)

        enddo
      end subroutine

      ! =============================================================================
      !> Counts occurence of character (separator, default comma) in string
      ! =============================================================================
      integer function count_separator (dummy , separator)
        character(*) , intent(in) ::dummy
        character(1), intent(in), optional  :: separator
        character(1)  :: sep
        character(:), allocatable :: dummy2
        integer :: i

        dummy2=dummy
        sep = ","
        if (present(separator)) sep = separator
        count_separator=0
        do 
          i = index (dummy2, sep)
          if (i.eq.0) exit
          dummy2 = dummy2(i+1:)
          count_separator=count_separator+1
        enddo
      end function

      !
      !! =============================================================================
      !!> This subroutine fills the fields of command line entry for every input arg
      !!!
      !!! \author M. Rajner
      !!! \date 2013-03-21
      !!! DELETE TODO
      !! =============================================================================
      !subroutine mod_cmdline_entry (&
      !    dummy , cmd_line_entry , program_calling , accepted_switches &
      !    )
      !  character(*) :: dummy 
      !  character(:), allocatable :: dummy2
      !  type (cmd_line_arg),intent(out) :: cmd_line_entry
      !  character(1) :: separator=","
      !  character(len=*) , intent(in) , optional :: program_calling, accepted_switches
      !  integer :: i , j , ii , jj 
      !
      !  cmd_line_entry%switch = dummy(1:2)
      !  write(log%unit, form_61) , dummy
      !  if (present(accepted_switches).and. &
      !    (.not.if_accepted_switch(cmd_line_entry%switch,accepted_switches))) &
      !    then
        !    write ( log%unit , form_62 ) "this switch is IGNORED by program "//program_calling
        !    return
        !  endif
        !
        !  dummy=dummy(3:)
        !  cmd_line_entry%fields = count_separator (dummy) + 1
        !  allocate(cmd_line_entry%field (cmd_line_entry%fields) )
        !
        !  ! if ":" separator is present in command line allocate
        !  ! additional array for fieldnames
        !  if (count_separator(dummy, ":" ).ge.1) then
        !    allocate(cmd_line_entry%fieldnames (cmd_line_entry%fields) )
        !  endif
        !  do i = 1 , cmd_line_entry%fields 
        !    j = index(dummy, separator) 
        !    cmd_line_entry%field(i) = dummy(1:j-1)
        !    if (i.eq.cmd_line_entry%fields) cmd_line_entry%field(i)=dummy
        !    dummy=dummy(j+1:)
        !
        !    ! separate field and fieldnames
        !    if ( index(cmd_line_entry%field(i),":").ne.0 ) then
        !      dummy2 = trim (cmd_line_entry%field(i))//":"
        !      allocate ( cmd_line_entry%fieldnames(i)%names(count_separator (dummy2,":") - 1 ))
        !      do ii = 1, size(cmd_line_entry%fieldnames(i)%names)+1
        !        jj = index(dummy2, ":")
        !        if (ii.eq.1) then
        !          cmd_line_entry%field(i) = dummy2 (1:jj-1)
        !        else
        !          cmd_line_entry%fieldnames(i)%names(ii-1) = dummy2 (1:jj-1)
        !        endif
        !        dummy2 = dummy2 (jj+1:)
        !      enddo
        !    endif
        !  enddo
        !  call parse_option (cmd_line_entry , program_calling = program_calling , accepted_switches=accepted_switches)
        !end subroutine
        !
        ! =============================================================================
        !> This subroutine fills the model info
        ! =============================================================================
        subroutine get_model_info ( cmd_line_entry )
          use mod_utilities, only : file_exists, is_numeric
          type(cmd_line_arg),intent(in):: cmd_line_entry
          integer ::  i , j , indeks

          allocate(model(size(cmd_line_entry%field)))

          do i = 1 , size(model)
            model(i)%name = trim(cmd_line_entry%field(i)%subfield(1)%name)
            model(i)%dataname = trim(cmd_line_entry%field(i)%subfield(1)%dataname)
            if (model(i)%dataname.eq." ") model(i)%dataname="NN"
            write(log%unit, form_62), trim(cmd_line_entry%field(i)%full)
            if (model(i)%name.eq."") then
              call print_warning ("model")
            endif
            write (log%unit , form_63,advance='no') , trim (dataname(model(i)%dataname)), &
              "("//trim(model(i)%dataname)//")"
            if ( file_exists (model(i)%name) ) then
              do j =2 , size (cmd_line_entry%field(i)%subfield)
                if (cmd_line_entry%field(i)%subfield(j)%name.ne."") then
                  model(i)%names(j-1)=cmd_line_entry%field(i)%subfield(j)%name
                endif
              enddo
              write(log%unit, '(5(a,x))', advance="no") , (trim(model(i)%names(j)), j =1,5)
              model(i)%if=.true.
              write(log%unit, *) 
            else if (is_numeric(model(i)%name)) then
              model(i)%if_constant_value=.true.
              read (model(i)%name , * ) model(i)%constant_value
              write(log%unit, *), 'constant value was set: ' , model(i)%constant_value
            else
              call print_warning ("model")
            endif
          enddo
        end subroutine
        !
        !
        ! =============================================================================
        !> 
        ! =============================================================================
        subroutine parse_GMT_like_boundaries ( cmd_line_entry )
          use mod_constants, only : dp ,dp 
          use mod_utilities, only : is_numeric
          real(dp) :: limits (4) , resolution (2) =[1,1]
          real(dp) :: range_lon , range_lat , lat , lon
          character(10) :: dummy
          integer :: i , ii , indeks_slash
          type (cmd_line_arg) , intent (in) :: cmd_line_entry
          character(:) ,allocatable :: text
          integer :: n_lon , n_lat 

          text = cmd_line_entry%field(1)%subfield(1)%name

          do i=1,4
            indeks_slash=index(text,"/")
            if (indeks_slash.eq.0) indeks_slash=len(text)

            if ( is_numeric (text(1:indeks_slash-1)) ) then
              read ( text(1:indeks_slash-1) , * )  limits(i)
            else
              if (text.eq."g" ) then
                limits=[0. , 359.9999 , -90 , 90. ]
                exit
              endif
            endif
            text=text(index(text,"/")+1:)
          enddo

          do i = 1 ,2 
            if (limits(i).lt. -180. .or. limits(i).gt.360. ) then
              call print_warning ("boundaries")
              return
            else
              ! noramlize longitude to <0,360 deg>
              if (limits(i).lt.0.) limits(i)=limits(i)+360.
            endif
          enddo
          do i =3,4
            if (limits(i).lt. -90. .or. limits(i).gt.90. ) then
              call print_warning ("boundaries")
              return
            endif
          enddo
          if (limits(3).gt.limits(4)) then
            call print_warning ("boundaries")
            return
          endif

          if (size(cmd_line_entry%field).ge.2) then
            if (is_numeric (cmd_line_entry%field(2)%subfield(1)%name ) ) then
              read (cmd_line_entry%field(2)%subfield(1)%name , * ) resolution(1)
              resolution(2) = resolution(1)
            endif
            if (size(cmd_line_entry%field).ge.3) then
              if (is_numeric (cmd_line_entry%field(3)%subfield(1)%name ) ) then
                read (cmd_line_entry%field(3)%subfield(1)%name , * ) resolution(2)
              endif
            endif
          endif

          range_lon=limits(2) - limits(1)
          if (range_lon.lt.0) range_lon = range_lon + 360.
          range_lat=limits(4) - limits(3)
          n_lon = floor ( range_lon / resolution(1)) + 1
          n_lat = floor ( range_lat / resolution(2)) + 1  
          allocate (sites ( n_lon * n_lat ) )

          do i = 1 , n_lon
            lon = limits (1) + (i-1) * resolution(1)
            if (lon.ge.360.) lon = lon - 360. 
            do ii = 1 , n_lat
              lat = limits (3) + (ii-1) * resolution (2)
              sites( (i-1) * n_lat + ii  )%lon = lon
              sites( (i-1) * n_lat + ii  )%lat = lat
            enddo
          enddo
        end subroutine

        ! =============================================================================
        !> Read site list from file
        !!
        !! checks for arguments and put it into array \c sites
        ! =============================================================================
        subroutine read_site_file (file_name)
          use mod_utilities, only: is_numeric, ntokens
          character(len=*) , intent(in) ::  file_name
          integer :: io_status , i , good_lines = 0 , number_of_lines = 0 , nloop
          character(len=255) ,dimension(4)  :: dummy
          character(len=255) :: line_of_file
          type(site_data) :: aux

          open ( newunit = fileunit_site , file = file_name, &
            iostat = io_status ,status = "old" , action="read" )

          ! two loops, first count good lines and print rejected
          ! second allocate array of sites and read coordinates into it
          do nloop = 1, 2
            if (nloop.eq.2) allocate(sites(good_lines))
            if (number_of_lines.ne.good_lines) then
              call print_warning ("site_file_format")
            endif
            good_lines=0
            do 
              read ( fileunit_site , '(a)' , iostat = io_status ) line_of_file 
              if ( io_status == iostat_end)  exit
              number_of_lines = number_of_lines + 1
              ! we need at least 3 parameter for site (name , B , L )
              if (ntokens(line_of_file).ge.3) then
                ! but no more than 4 parameters (name , B , L, H)
                if (ntokens(line_of_file).gt.4) then
                  read ( line_of_file , * ) dummy(1:4)
                else
                  read ( line_of_file , * ) dummy(1:3)
                  ! if site height was not given we set it to zero
                  dummy(4)="0."
                endif
              endif
              ! check the values given
              if(    is_numeric(trim(dummy(2)))   &
                .and.is_numeric(trim(dummy(3)))   &
                .and.is_numeric(trim(dummy(4)))   &
                .and.ntokens(line_of_file).ge.3 ) then
                aux%name= trim(dummy(1))
                read( dummy(2),*) aux%lat
                read(dummy(3),*) aux%lon 
                read(dummy(4),*) aux%height 

                ! todo
                if (aux%lat.ge.-90 .and. aux%lat.le.90) then
                  if (aux%lon.ge.-180 .and. aux%lon.le.360) then
                    good_lines=good_lines+1
                    if (nloop.eq.2) then
                      sites(good_lines)%name= trim(dummy(1))
                      read(dummy(2),*) sites(good_lines)%lat 
                      read(dummy(3),*) sites(good_lines)%lon 
                      read(dummy(4),*) sites(good_lines)%height 
                    endif
                  else
                    if (nloop.eq.2) then 
                      write ( log%unit, form_63) "rejecting (lon limits):" , line_of_file
                    endif
                  endif
                else 
                  if (nloop.eq.2) then
                    write ( log%unit, form_63) "rejecting (lat limits):" , line_of_file
                  endif
                endif
              else
                ! print it only once
                if (nloop.eq.2) then
                  write ( log%unit, form_63) "rejecting (args):      " , line_of_file
                endif
              endif
            enddo
            if (nloop.eq.1) rewind(fileunit_site)
          enddo

          ! if longitude <-180, 180> change to <0,360) domain
          do i =1, size (sites)
            if (sites(i)%lon.lt.0) sites(i)%lon= sites(i)%lon + 360.
            if (sites(i)%lon.eq.360) sites(i)%lon= 0.
          enddo
        end subroutine

        !! =============================================================================
        !!> Parse date given as 20110503020103  to yy mm dd hh mm ss and mjd
        !!! 
        !!! \warning decimal seconds are not allowed
        !! =============================================================================
        subroutine parse_dates (cmd_line_entry ) 
          use mod_utilities, only: is_numeric,mjd,invmjd
          type(cmd_line_arg) cmd_line_entry
          integer , dimension(6) :: start , stop , swap 
          real (dp) :: step =6. ! step in hours
          integer :: i
          character(1) :: interval_unit="h"

          call string2date(cmd_line_entry%field(1)%subfield(1)%name, start)
          stop = start

          if (size(cmd_line_entry%field).eq.3) then
            if(len(trim(cmd_line_entry%field(3)%subfield(1)%dataname)).ne.0) then
              read(cmd_line_entry%field(3)%subfield(1)%dataname,*) interval_unit
            endif
            if (len(trim(cmd_line_entry%field(3)%subfield(1)%name)).ne.0) &
              then
              read(cmd_line_entry%field(3)%subfield(1)%name,*) step
            endif
          endif
          if (size(cmd_line_entry%field).ge.2) then
            if(len(trim(cmd_line_entry%field(2)%subfield(1)%name)).ne.0) then
              call string2date(cmd_line_entry%field(2)%subfield(1)%name, stop)
              if(len(trim(cmd_line_entry%field(2)%subfield(1)%dataname)).ne.0) then
                if(cmd_line_entry%field(2)%subfield(1)%dataname.eq.'Y') then
                  stop(1)=start(1)+stop(1)
                  stop(2:)=start(2:)
                else if(cmd_line_entry%field(2)%subfield(1)%dataname.eq.'M') then
                  stop(2)=start(2)+stop(1)
                  stop(1)=start(1)
                  stop(3:)=start(3:)
                  if (stop(2).gt.12) then
                    stop(1) =stop(1)+int(stop(2)/12)
                    stop(2) =modulo(stop(2),12)
                  else if (stop(2).lt.1) then
                    stop(1) =stop(1)-int(-stop(2)/12+1)
                    stop(2) =stop(2)+12*(1+int(-stop(2)/12))
                  endif
                endif
              endif
            endif
          endif

          write (log%unit , "(T6, a,x,i4.4,x,5(i2.2,x))") "start date:" , start
          write (log%unit , "(T6, a,x,i4.4,x,5(i2.2,x))") "stop  date:" , stop
          write (log%unit , "(T6, a,x,f8.0,a)") "interval:" , step, interval_unit

          ! allow that stop is previous than start and list in reverse order
          ! chage the sign of step in dates if necessery
          if(mjd(stop).lt.mjd(start).and. step.gt.0) step = -step
          ! or if step is negative
          if(mjd(stop).gt.mjd(start).and. step.lt.0) then
            swap=start
            start=stop
            stop=swap
          endif

          if (interval_unit.eq."M".or.interval_unit.eq."Y") then
            if (interval_unit.eq."Y") then
              step=step*12
              interval_unit="M"
            endif
            if (interval_unit.eq."M") then
              allocate (dates( int((12*(stop(1) - start(1))+stop(2)-start(2))/(step)) +1 ))
              dates(1)%date=start
              dates(1)%mjd=mjd(dates(1)%date)
              do i= 2 ,size(dates)
                dates(i)%date=dates(i-1)%date
                dates(i)%date(2)=dates(i-1)%date(2)+step
                if (dates(i)%date(2).gt.12) then
                  dates(i)%date(1) =dates(i)%date(1)+int(dates(i)%date(2)/12)
                  dates(i)%date(2) =modulo(dates(i)%date(2),12)
                else if (dates(i)%date(2).lt.1) then
                  dates(i)%date(1) =dates(i)%date(1)-int(-dates(i)%date(2)/12+1)
                  dates(i)%date(2) =dates(i)%date(2)+12*(1+int(-dates(i)%date(2)/12))
                endif
                dates(i)%mjd=mjd(dates(i)%date)
              enddo
            endif
          else
            if (interval_unit.eq."D") step = 24. * step
            if (interval_unit.eq."m") step = step /60.
            if (interval_unit.eq."s") step = step /60./60.

            allocate (dates (int((mjd(stop)-mjd(start)) / step * 24. + 1 ) ))
            do i = 1 , size(dates)
              dates(i)%mjd = mjd(start) + ( i -1 ) * step / 24.
              call invmjd ( dates(i)%mjd , dates(i)%date)
            enddo
          endif
          !      do i = 1 , size(dates)
          !        print * , ":" ,dates(i)%mjd ,dates(i)%date(1:4)
          !      enddo
        end subroutine
        !
        !
        !! =============================================================================
        !!> Convert dates given as string to integer (6 elements)
        !!! 
        !!! 20110612060302 --> [2011 , 6 , 12 , 6 , 3 , 2
        !!! you can omit
        !!! \warning decimal seconds are not allowed
        !! =============================================================================
        subroutine string2date ( string , date )
          use mod_utilities, only: is_numeric
          integer , dimension(6) ,intent(out):: date 
          character (*) , intent(in) :: string
          integer :: start_char , end_char , j

          ! this allow to specify !st Jan of year simple as -Dyyyy
          date = [2000 , 1 , 1 , 0 ,0 ,0]

          start_char = 1
          do j = 1 , 6 
            if (j.eq.1) then
              end_char=start_char+3
            else
              end_char=start_char+1
            endif
            if (is_numeric(string(start_char : end_char) )) then
              read(string(start_char : end_char),*) date(j)
            endif
            start_char=end_char+1
          enddo 

        end subroutine


        !! =============================================================================
        !! =============================================================================
        !subroutine sprawdzdate(mjd)
        !  use mod_utilities 
        !  real(dp):: mjd
        !  !    if (mjd.gt.jd(data_uruchomienia(1),data_uruchomienia(2),data_uruchomienia(3),data_uruchomienia(4),data_uruchomienia(5),data_uruchomienia(6))) then
        !  write (*,'(4x,a)') "Data późniejsza niż dzisiaj. KOŃCZĘ!"
        !  !      call exit
        !  !    else if (mjd.lt.jd(1980,1,1,0,0,0)) then
        !  !      write (*,'(4x,a)') "Data wcześniejsza niż 1980-01-01. KOŃCZĘ!"
        !  !      call exit
        !  !    endif
        !  !    if (.not.log_E) then
        !  !      data_koniec=data_poczatek
        !  !      mjd_koniec=mjd_poczatek
        !  !    endif
        !  !    if (mjd_koniec.lt.mjd_poczatek) then
        !  !      write (*,*) "Data końcowa większa od początkowej. KOŃCZĘ!"
        !  !      write (*,form_64) "Data końcowa większa od początkowej. KOŃCZĘ!"
        !  !    endif
        !end subroutine

        ! =============================================================================
        !> Print version of program depending on program calling
        !! 
        !! \author M. Rajner
        !! \date 2013-03-06
        ! =============================================================================
        subroutine print_version (program_calling)
          character(*) :: program_calling 
          integer :: version_unit , io_stat
          character(40) :: version

          ! from the file storing version number
          open(newunit=version_unit, file = '/home/mrajner/src/grat/dat/version.txt', &
            action = 'read' , status = 'old')
          do 
            read (version_unit , '(a)' , iostat = io_stat ) version
            if (io_stat == iostat_end) exit
            if (version(1:2) == ' '//program_calling(1:1)) exit
          enddo
          write(log%unit , form_header ) 
          write(log%unit,form_inheader ) , trim(program_calling)
          write(log%unit,form_inheader ) , trim(version(3:))
          write(log%unit , form_header ) 
          write(log%unit,form_inheader ) , 'Copyright 2013 by Marcin Rajner'
          write(log%unit,form_inheader ) , 'Warsaw University of Technology'
          write(log%unit,form_inheader ) , 'License: GPL v3 or later'
          write(log%unit , form_header ) 
        end subroutine

        ! =============================================================================
        !> Print settings 
        ! =============================================================================
        subroutine print_settings (program_calling)
          logical :: exists
          character (len=255):: dummy
          integer :: io_status , j
          character(*), intent(in), optional :: program_calling

          call date_and_time (values = execution_date)
          write(log%unit,'("Program started:",1x,i4,2("-",i2.2), &
            1x,i2.2,2(":",i2.2),1x,"(",dp,SP,i3.2,"h UTC)")'),          &
            execution_date (1:3),execution_date(5:7),execution_date(4)/60
          write(log%unit, form_separator)

          !----------------------------------------------------
          ! Site summary
          !----------------------------------------------------
          if (size(sites).ge.1) then
            write(log%unit, form_separator)
            write(log%unit, form_60 ) "Processing:", size(sites), "site(s)"
            if (size(sites).le.15) then
              write(log%unit, '(2x,a,t16,3a15)') &
                "Name" , "lat [deg]" , "lon [deg]" ,"H [m]"
              do j = 1,size(sites)
                write(log%unit, '(2x,a,t16,3f15.4)') &
                  sites(j)%name, sites(j)%lat, sites(j)%lon , sites(j)%height
              enddo
            endif
          endif

          !----------------------------------------------------
          ! Computation method summary
          !----------------------------------------------------
          !if (program_calling.eq."grat" ) then
          !  write(log%unit, form_separator)
          !  write(log%unit, form_60 ) "Method used:", method
          !endif

          !write(log%unit, form_separator)
          !write(log%unit, form_60 ) "Interpolation data:", & 
          !  interpolation_names(model%interpolation)(1:7)
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

        subroutine print_warning (  warn , unit)
          character (len=*)  :: warn
          integer , optional :: unit
          integer :: def_unit

          def_unit=log%unit
          if (present (unit) ) def_unit=unit

          if (warn .eq. "site_file_format") then
            write(def_unit, form_63) "Some records were rejected"
            write(def_unit, form_63) "you should specify for each line at least 3[4] parameters in free format:"
            write(def_unit, form_63) "name lat lon [H=0] (skipped)"
          else if (warn .eq. "boundaries") then
            write(def_unit, form_62) "something wrong with boundaries. IGNORED"
          else if (warn .eq. "site") then
            write(def_unit, form_62) "something wrong with -S|-R specification. IGNORED"
          else if (warn .eq. "repeated") then
            write(def_unit, form_62) "reapeted specification. IGNORED"
          else if (warn .eq. "dates") then
            write(def_unit, form_62) "something wrong with date format -D. IGNORED"
          else if (warn .eq. "model") then
            write(def_unit, form_62) "something wrong with -F."
          endif
        end subroutine

        ! =============================================================================
        !> Counts number of properly specified models
        !!
        !! \date 2013-03-15
        !! \author M. Rajner
        ! =============================================================================
        integer function nmodels (model)
          type(file) , allocatable, dimension (:) :: model
          integer :: i

          nmodels = 0
          do i = 1 , size (model)
            if (model(i)%if) nmodels =nmodels + 1
            if (model(i)%if_constant_value) nmodels =nmodels + 1
          enddo
        end function

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
          if (abbreviation.eq."LS") dataname = "Land-sea mask"
          if (abbreviation.eq."SP") dataname = "Surface pressure"
          if (abbreviation.eq."RS") dataname = "Reference surface pressure"
          if (abbreviation.eq."n") dataname = "interpolation nearest"
          if (abbreviation.eq."b") dataname = "interpolation bilinear"
          if (abbreviation.eq."GN") dataname = "Green newtonian"
        end function
      end module mod_cmdline
