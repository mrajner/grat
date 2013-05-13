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
    real(dp),allocatable,dimension(:) :: distance
    real(dp),allocatable,dimension(:) :: data
    logical  :: if
  end type
  type(green_functions), allocatable , dimension(:) :: green
  integer :: denser(2) = [1,1] 

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
  type additional_info
    character (len=55) ,allocatable ,dimension(:) :: names
  end type
  type cmd_line
    character(2) :: switch
    integer ::      fields
    character (len=255) ,allocatable ,dimension(:) :: field
    type (additional_info), allocatable , dimension(:) :: fieldnames 
  end type

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
  integer :: fileunit_tmp
  integer,dimension(8):: execution_date !< To give time stamp of execution
  character (len = 2) :: method = "2D"  !< computation method

  !----------------------------------------------------
  ! Site names file
  !----------------------------------------------------
  character(:), allocatable &
    :: filename_site 
  integer :: fileunit_site

  type file
    character(:), allocatable :: name 

    ! varname , lonname,latname,levelname , timename
    character(len=50) :: names(5) = [ "z", "lon", "lat","level","time"]

    !choose with -F filename@XX:pres...
    character(len=40) :: dataname

    integer :: unit = output_unit

    ! if file was determined
    logical :: if =.false.

    ! to read into only once
    logical :: first_call =.true.

    ! boundary of model e , w ,s ,n
    real(dp):: limits(4)

    !     resolution of model in lon lat
    !    real(dp):: resolution(2)

    real(dp) , allocatable ,dimension(:) :: lat , lon , time ,level
    integer , allocatable , dimension(:,: ) :: date

    real (dp), dimension(2) :: latrange , lonrange

    ! todo
    logical :: if_constant_value
    real(dp):: constant_value

    ! data 
    !> 4 dimension - lat , lon , level , mjd
    ! todo
    real(dp) , allocatable , dimension (:,:,:) :: data

    ! netcdf identifiers
    integer :: ncid
    integer :: interpolation = 1
  end type

  ! External files
  type(file) ::  log  , output 
  type(file) , allocatable, dimension (:) :: model , moreverbose

  !  character (len =40) :: model_names (5) = ["pressure_surface" , &
  !    "temperature_surface" , "topography" , "landsea" , "pressure levels" ]

  ! todo --- make @ like for models
  character(len=5) :: green_names(5) = [ "GN   ", "GN/dt", "GN/dh","GN/dz","GE   "]

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

  !  private
  !  public :: nmodels

contains
! =============================================================================
!> This subroutine counts the command line arguments
!!
!! Depending on command line options set all initial parameters and reports it
!!
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
  integer :: i, j
  character(len=255) :: dummy, dummy2,arg
  character(len=*), intent(in) :: program_calling
  type(cmd_line) :: cmd_line_entry
  character(len=*) , intent (in), optional :: accepted_switches
  logical , intent (in), optional :: cmdlineargs

  if(present(cmdlineargs).and.cmdlineargs.and.iargc().eq.0) then
    write(output_unit , '(a)' ) , &
      'No cmd line args! Try: ./'//program_calling//' -h' 
    call exit
  endif

  ! tmp file to write in before all cmdline argument will be parsed
  open(newunit=fileunit_tmp,status='scratch')
  call get_command(dummy)
  write (fileunit_tmp,form_61) "command invoked"
  write (fileunit_tmp,form_62) trim(dummy)

  do i = 1 , iargc()
    call get_command_argument(i,dummy)

    ! allows specification like '-F file' and '-Ffile'
    ! but  if -[0,9] it is treated as number belonging to switch (-S -2)
    ! but  if -[\s,:] do not start next command line option
    call get_command_argument(i+1,dummy2)
    if (check_if_switch_or_minus(dummy)) then
      arg = trim(dummy)
    else
      arg=trim(arg)//trim(dummy)
    endif
    if(check_if_switch_or_minus(dummy2).or.i.eq.iargc()) &
      then
      call mod_cmdline_entry ( &
        arg, cmd_line_entry , program_calling = program_calling ,&
        accepted_switches = accepted_switches &
        )
    endif
  enddo

  ! Where and if to log the additional information
  if (log%if) then 
    ! if file name was given then automaticall switch verbose mode
    if_verbose = .true.
    open (newunit = log%unit, file = log%name , action = "write" )
  else
    ! if you don't specify log file, or not switch on verbose mode
    ! all additional information will go to trash
    ! Change  /dev/null accordingly if your file system does not
    ! support this name
    if (.not.if_verbose) then
      open (newunit=log%unit, file = "/dev/null", action = "write" )
    endif
  endif
  call print_settings(program_calling)
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
  type(cmd_line),intent(in):: cmd_line_entry
  character(len=*), optional :: program_calling,accepted_switches
  integer :: i

  ! all the command line option are stored in tmp file and later its decided
  ! if it is written to STDOUT , log_file or nowwhere
  select case (cmd_line_entry%switch)
  case ('-h')
    call print_help (program_calling ,accepted_switches)
    call exit
  case ('-v')
    call print_version (program_calling)
    call exit ()
  case ('-V')
    if_verbose = .true.
    write(fileunit_tmp, form_62) 'verbose mode' ,trim(log%name)
    if (len(trim(cmd_line_entry%field(1))).gt.0) then
      log%if = .true.
      log%name = trim(cmd_line_entry%field(1))
      write(fileunit_tmp, form_62) 'the log file was set:' ,log%name
    endif
  case ('-S','-R')
    ! check if format is proper for site
    ! i,e. -Sname,B,L[,H]
    if (.not. allocated(sites)) then
      if (is_numeric(cmd_line_entry%field(2)) &
        .and.is_numeric(cmd_line_entry%field(3)) &
        .and.index(cmd_line_entry%field(1), "/" ).eq.0 &
        .and.(.not.cmd_line_entry%field(1).eq. "Rg" ) &
        ) then
        allocate (sites(1))
        sites(1)%name = trim(cmd_line_entry%field(1))
        read ( cmd_line_entry%field(2) , * ) sites(1)%lat
        if (abs(sites(1)%lat).gt.90.) &
          sites(1)%lat = sign(90.,sites(1)%lat) 
        read ( cmd_line_entry%field(3) , * ) sites(1)%lon
        if (sites(1)%lon.ge.360.) sites(1)%lon = mod(sites(1)%lon,360.)
        if (is_numeric (cmd_line_entry%field(4) ) ) then
          read ( cmd_line_entry%field(4) , * ) sites(1)%height
        endif
        write(fileunit_tmp, form_62) 'the site was set (BLH):' , &
          sites(1)%name, real(sites(1)%lat) , &
          real(sites(1)%lon) , real(sites(1)%height) 
      else
        ! or read sites from file
        if (file_exists (cmd_line_entry%field(1) ))  then
          write(fileunit_tmp, form_62) 'the site file was set:' , &
            cmd_line_entry%field(1)
          call read_site_file (cmd_line_entry%field(1))
        else if (index(cmd_line_entry%field(1), "/" ).ne.0 &
          .or.cmd_line_entry%field(1).eq."Rg")  then
          call parse_GMT_like_boundaries ( cmd_line_entry )
        else
          call print_warning ( "site" , fileunit_tmp)
        endif
      endif
    else
      call print_warning ( "repeated" , fileunit_tmp)
    endif
  case ("-I")
    !> \todo add maximum minimum distances for integration
    write( fileunit_tmp , form_62 , advance="no" ) "interpolation method was set:"
    do i = 1 , cmd_line_entry%fields
      if (is_numeric(cmd_line_entry%field(i))) then
        read ( cmd_line_entry%field(i) , * ) model(i)%interpolation
        write(fileunit_tmp , '(a10,x,$)' ) interpolation_names (model(i)%interpolation)
        if (model(i)%interpolation.gt.size(interpolation_names)) then
          model(i)%interpolation=1
        endif
      endif
    enddo
    write(fileunit_tmp , *)
  case ("-L")
    write (fileunit_tmp , form_62) "printing additional information:"
    allocate(moreverbose(cmd_line_entry%fields))
    do i = 1, cmd_line_entry%fields
      print *,i 
      if (cmd_line_entry%field(i).ne."") then
        moreverbose(i)%name = trim(cmd_line_entry%field(i))
        if (allocated(cmd_line_entry%fieldnames(i)%names)) then
          moreverbose(i)%dataname = &
            trim(cmd_line_entry%fieldnames(i)%names(1))
          if (dataname(moreverbose(i)%dataname).ne."unknown") then 
            if (moreverbose(i)%name.ne."") then
              open(newunit=moreverbose(i)%unit, &
                file =moreverbose(i)%name , action = 'write')
            else
              moreverbose(i)%unit = output_unit
            endif
          endif
        endif
        write (fileunit_tmp , form_62),  moreverbose(i)%name , &
          "<-", dataname(moreverbose(i)%dataname)
      endif
    enddo
    call exit
  case ("-B")
    if (cmd_line_entry%field(1).eq."N" ) inverted_barometer = .false.
  case ('-D')
    call parse_dates (cmd_line_entry)
  case ('-F')
    allocate(model(cmd_line_entry%fields))
    do i = 1, cmd_line_entry%fields
      call get_model_info (model (i) , cmd_line_entry , i )
    enddo
  case ("-G")
    !> \todo when no given take defaults
    call parse_green(cmd_line_entry)
  case ('-M')
    !> \todo rozbudować
    method = cmd_line_entry%field(1)
    write(fileunit_tmp, form_62), 'method was set: ' , method
  case ('-o')
    output%if=.true.
    output%name=cmd_line_entry%field(1)
    write(fileunit_tmp, form_62), 'output file was set: ' , output%name 
    if (len(output%name).gt.0.and. output%name.ne."") then
      open (newunit = output%unit , file = output%name , action = "write" )
    endif
  case ('-P')
    do i = 1, cmd_line_entry%fields
      ! prevent from multiple -P
      if (polygons(i)%if) then
        call print_warning ("repeated", fileunit_tmp)
        return
      endif
      polygons(i)%name=cmd_line_entry%field(i)
      if (file_exists((polygons(i)%name))) then
        write(fileunit_tmp, form_62), 'polygon file was set: ' , polygons(i)%name
        polygons(i)%if=.true.
        if (allocated(cmd_line_entry%fieldnames)) then
          polygons(i)%pm = trim(cmd_line_entry%fieldnames(i)%names(1))
        endif
      else
        write(fileunit_tmp, form_62), 'file do not exist. Polygon file was IGNORED'
      endif
    enddo
  case default
    write(fileunit_tmp,form_62), "unknown argument: IGNORING"
  end select
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
  type (cmd_line)  :: cmd_line_entry
  character (60) :: filename
  integer :: i , iunit , io_status , lines ,  ii
  integer :: fields(2)= [1,2]
  real (dp) , allocatable , dimension(:) :: tmp

  write(fileunit_tmp , form_62) "Green function file was set:"
  allocate (green (cmd_line_entry%fields))

  do i = 1 , cmd_line_entry%fields
    if (i.eq.6) then
      if (is_numeric(cmd_line_entry%field(i))) then
        read( cmd_line_entry%field(i), *) denser(1)
        if (is_numeric(cmd_line_entry%fieldnames(i)%names(1))) then
          read( cmd_line_entry%fieldnames(i)%names(1), *) denser(2)
        endif
        return
      endif
    endif

    if (.not.file_exists(cmd_line_entry%field(i)) &
      .and. (.not. cmd_line_entry%field(i).eq."merriam" &
      .and. .not. cmd_line_entry%field(i).eq."huang" &
      .and. .not. cmd_line_entry%field(i).eq."rajner" )) then
      cmd_line_entry%field(i)="merriam"
    endif

    !> change the paths accordingly
    if (cmd_line_entry%field(i).eq."merriam") then
      filename="/home/mrajner/src/grat/dat/merriam_green.dat"
      if (i.eq.1) fields = [1,2]
      if (i.eq.2) fields = [1,3]
      if (i.eq.3) fields = [1,4]
      if (i.eq.4) fields = [1,4]
      if (i.eq.5) fields = [1,6]
    else if (cmd_line_entry%field(i).eq."huang") then
      filename="/home/mrajner/src/grat/dat/huang_green.dat"
      if (i.eq.1) fields = [1,2]
      if (i.eq.2) fields = [1,3]
      if (i.eq.3) fields = [1,4]
      if (i.eq.4) fields = [1,5]
      if (i.eq.5) fields = [1,6]
    else if (cmd_line_entry%field(i).eq."rajner") then
      filename="/home/mrajner/src/grat/dat/rajner_green.dat"
      if (i.eq.1) fields = [1,2]
      if (i.eq.2) fields = [1,3]
      if (i.eq.3) fields = [1,4]
      if (i.eq.4) fields = [1,5]
      if (i.eq.5) fields = [1,6]
    else if (file_exists(cmd_line_entry%field(i))) then
      filename = cmd_line_entry%field(i)
      if (size(cmd_line_entry%fieldnames).ne.0 .and. allocated(cmd_line_entry%fieldnames(i)%names)) then
        do ii=1, 2
          if(is_numeric (cmd_line_entry%fieldnames(i)%names(ii) ) ) then
            read( cmd_line_entry%fieldnames(i)%names(ii), *) fields(ii)
          endif
        enddo
      endif
    endif

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
    if (cmd_line_entry%field(i).eq."merriam" .and. i.eq.4) then
      green(i)%data = green(i)%data * (-1.)
    endif
    if (cmd_line_entry%field(i).eq."huang" .and. (i.eq.3.or.i.eq.4)) then
      green(i)%data = green(i)%data * 1000.
    endif
    write(fileunit_tmp , form_63) trim(green_names(i)), &
      trim(cmd_line_entry%field(i)),":", fields
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


! =============================================================================
!> This subroutine fills the fields of command line entry for every input arg
!!
!! \author M. Rajner
!! \date 2013-03-21
! =============================================================================
subroutine mod_cmdline_entry (dummy , cmd_line_entry , program_calling , &
    accepted_switches)
  character(*) :: dummy 
  character(:), allocatable :: dummy2
  type (cmd_line),intent(out) :: cmd_line_entry
  character(1) :: separator=","
  character(len=*) , intent(in) , optional :: program_calling, accepted_switches
  integer :: i , j , ii , jj 

  ! prevent to printing  messages if fileunit_tmp was not opened 
  ! previously with subroutine intro
  if (fileunit_tmp.eq.0) fileunit_tmp=1

  cmd_line_entry%switch = dummy(1:2)
  write(fileunit_tmp, form_61) , dummy
  if (present(accepted_switches).and. &
    (.not.if_accepted_switch(cmd_line_entry%switch,accepted_switches))) &
    then
    write ( fileunit_tmp , form_62 ) "this switch is IGNORED by program "//program_calling
    return
  endif

  dummy=dummy(3:)
  cmd_line_entry%fields = count_separator (dummy) + 1
  allocate(cmd_line_entry%field (cmd_line_entry%fields) )

  ! if ":" separator is present in command line allocate
  ! additional array for fieldnames
  if (count_separator(dummy, ":" ).ge.1) then
    allocate(cmd_line_entry%fieldnames (cmd_line_entry%fields) )
  endif
  do i = 1 , cmd_line_entry%fields 
    j = index(dummy, separator) 
    cmd_line_entry%field(i) = dummy(1:j-1)
    if (i.eq.cmd_line_entry%fields) cmd_line_entry%field(i)=dummy
    dummy=dummy(j+1:)

    ! separate field and fieldnames
    if ( index(cmd_line_entry%field(i),":").ne.0 ) then
      dummy2 = trim (cmd_line_entry%field(i))//":"
      allocate ( cmd_line_entry%fieldnames(i)%names(count_separator (dummy2,":") - 1 ))
      do ii = 1, size(cmd_line_entry%fieldnames(i)%names)+1
        jj = index(dummy2, ":")
        if (ii.eq.1) then
          cmd_line_entry%field(i) = dummy2 (1:jj-1)
        else
          cmd_line_entry%fieldnames(i)%names(ii-1) = dummy2 (1:jj-1)
        endif
        dummy2 = dummy2 (jj+1:)
      enddo
    endif
  enddo
  call parse_option (cmd_line_entry , program_calling = program_calling , accepted_switches=accepted_switches)
end subroutine

! =============================================================================
!> This subroutine fills the model info
! =============================================================================
subroutine get_model_info (model , cmd_line_entry , field)
  use mod_utilities, only : file_exists, is_numeric
  type(cmd_line),intent(in):: cmd_line_entry
  type(file),intent(inout):: model
  integer :: field , i , indeks

  ! split name and dataname (separated by @ - optional)
  model%name = trim(cmd_line_entry%field(field))
  model%dataname = "NN"
  indeks = index(cmd_line_entry%field(field),'@')
  if (indeks.gt.0) then
    model%name = trim(cmd_line_entry%field(field)(1:indeks-1))
    model%dataname = trim(cmd_line_entry%field(field)(indeks+1:))
  endif
  if (model%name.eq."") return
  write (fileunit_tmp , form_62) , trim (dataname(model%dataname)), &
    "("//trim(model%dataname)//")"
  if ( file_exists (model%name) ) then
    do i =1 , size (model%names)
      if (size(cmd_line_entry%fieldnames).gt.0) then
        if ( &
          i.le.size (cmd_line_entry%fieldnames(field)%names) &
          .and. cmd_line_entry%fieldnames(field)%names(i).ne."" &
          ) model%names(i) = cmd_line_entry%fieldnames(field)%names(i)
      endif
      write(fileunit_tmp, form_63, advance="no") , trim(model%names(i))
    enddo
    model%if=.true.
    write(fileunit_tmp, form_63)
  else if (is_numeric(model%name)) then
    model%if_constant_value=.true.
    read (model%name , * ) model%constant_value
    write(fileunit_tmp, form_63), 'constant value was set: ' , model%constant_value
    model%if_constant_value=.true.
  else
    write (fileunit_tmp , form_63 ) "no (correct) model in field: ", field
  endif
end subroutine


! =============================================================================
!> P
! =============================================================================
subroutine parse_GMT_like_boundaries ( cmd_line_entry )
  use mod_constants, only : dp ,dp 
  use mod_utilities, only : is_numeric
  real(dp) :: limits (4) , resolution (2) =[1,1]
  real(dp) :: range_lon , range_lat , lat , lon
  character(10) :: dummy
  integer :: i , ii
  type (cmd_line) , intent (in) :: cmd_line_entry
  character(:) ,allocatable :: text
  integer :: n_lon , n_lat 

  text = cmd_line_entry%field(1)

  do i=1,3
    if ( is_numeric (text(1:index(text, "/"))) ) then
      read ( text(1:index(text, "/")) , * )  limits(i)
    else
      if (text.eq."Rg" ) then
        limits=[0. , 360. , -90 , 90. ]
        exit
      endif
    endif
    text=text(index(text,"/")+1:)
  enddo

  if ( is_numeric (text(1:)) ) then
    read ( text(1:) , * )  limits(4)
  else
    call print_warning ("boundaries")
  endif

  do i = 1 ,2 
    if (limits(i).lt. -180. .or. limits(i).gt.360. ) then
      call print_warning ("boundaries")
    else
      if (limits(i).lt.0.) limits(i)=limits(i)+360.
    endif
  enddo
  do i =3,4
    if (limits(i).lt. -90. .or. limits(i).gt.90. ) then
      call print_warning ("boundaries")
    endif
  enddo
  if (limits(3).gt.limits(4)) then
    call print_warning ("boundaries")
  endif

  if (is_numeric (cmd_line_entry%field(2) ) ) then
    read (cmd_line_entry%field(2) , * ) resolution(1)
    resolution(2) = resolution(1)
  endif
  if (is_numeric (cmd_line_entry%field(3) ) ) then
    read (cmd_line_entry%field(3) , * ) resolution(2)
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
subroutine read_site_file ( file_name )
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
              write ( fileunit_tmp, form_63) "rejecting (lon limits):" , line_of_file
            endif
          endif
        else 
          if (nloop.eq.2) then
            write ( fileunit_tmp, form_63) "rejecting (lat limits):" , line_of_file
          endif
        endif
      else
        ! print it only once
        if (nloop.eq.2) then
          write ( fileunit_tmp, form_63) "rejecting (args):      " , line_of_file
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

! =============================================================================
!> Parse date given as 20110503020103  to yy mm dd hh mm ss and mjd
!! 
!! \warning decimal seconds are not allowed
! =============================================================================
subroutine parse_dates (cmd_line_entry ) 
  use mod_utilities, only: is_numeric,mjd,invmjd
  type(cmd_line) cmd_line_entry
  integer , dimension(6) :: start , stop , swap 
  real (dp) :: step =6. ! step in hours
  integer :: i

  call string2date(cmd_line_entry%field(1), start)
  write (fileunit_tmp , "(T6, a,x,i4.4,x,5(i2.2,x))") "start date:" , start
  if (cmd_line_entry%field(2).eq."".or.cmd_line_entry%fields.le.1) then
    stop = start
  else
    call string2date(cmd_line_entry%field(2), stop )
    write (fileunit_tmp , "(T6, a,x,i4.4,x,5(i2.2,x))") "stop date:" , stop
  endif
  if (is_numeric (cmd_line_entry%field(3)).and.cmd_line_entry%fields.ge.3) then
    read(cmd_line_entry%field(3),*) step
    write (fileunit_tmp , form_62) "interval [h]:" , step
  endif

  ! allow that stop is previous than start and list in reverse order
  ! chage the sign of step in dates if necessery
  if(mjd(stop).lt.mjd(start).and. step.gt.0) step = -step
  ! or if step is negative
  if(mjd(stop).gt.mjd(start).and. step.lt.0) then
    swap=start
    start=stop
    stop=swap
  endif

  allocate (dates ( int( ( mjd(stop) - mjd(start) ) / step * 24. + 1 ) ))
  do i = 1 , size(dates)
    dates(i)%mjd = mjd(start) + ( i -1 ) * step / 24.
    call invmjd ( dates(i)%mjd , dates(i)%date)
  enddo
end subroutine


! =============================================================================
!> Convert dates given as string to integer (6 elements)
!! 
!! 20110612060302 --> [2011 , 6 , 12 , 6 , 3 , 2
!! you can omit
!! \warning decimal seconds are not allowed
! =============================================================================
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


! =============================================================================
! =============================================================================
subroutine sprawdzdate(mjd)
  use mod_utilities 
  real(dp):: mjd
  !    if (mjd.gt.jd(data_uruchomienia(1),data_uruchomienia(2),data_uruchomienia(3),data_uruchomienia(4),data_uruchomienia(5),data_uruchomienia(6))) then
  write (*,'(4x,a)') "Data późniejsza niż dzisiaj. KOŃCZĘ!"
  !      call exit
  !    else if (mjd.lt.jd(1980,1,1,0,0,0)) then
  !      write (*,'(4x,a)') "Data wcześniejsza niż 1980-01-01. KOŃCZĘ!"
  !      call exit
  !    endif
  !    if (.not.log_E) then
  !      data_koniec=data_poczatek
  !      mjd_koniec=mjd_poczatek
  !    endif
  !    if (mjd_koniec.lt.mjd_poczatek) then
  !      write (*,*) "Data końcowa większa od początkowej. KOŃCZĘ!"
  !      write (*,form_64) "Data końcowa większa od początkowej. KOŃCZĘ!"
  !    endif
end subroutine

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
  write(log%unit,form_inheader ) , ''
  write(log%unit,form_inheader ) , 'Marcin Rajner'
  write(log%unit,form_inheader ) , 'Warsaw University of Technology'
  write(log%unit , form_header ) 
  write(log%unit,form_inheader ) , 'Copyright 2013 by Marcin Rajner'
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

  call print_version ( program_calling = program_calling)
  call date_and_time ( values = execution_date )
  write(log%unit,'("Program started:",1x,i4,2("-",i2.2), &
    1x,i2.2,2(":",i2.2),1x,"(",dp,i3.2,"h UTC)")'),          &
    execution_date (1:3),execution_date(5:7),execution_date(4)/60
  write(log%unit, form_separator)

  inquire(fileunit_tmp, exist=exists)
  if (exists) then
    write (log%unit, form_60 ) 'Summary of command line arguments'

    !----------------------------------------------------
    ! Cmd line summary (from scratch file)
    !----------------------------------------------------
    rewind(fileunit_tmp)
    do
      read(fileunit_tmp,'(a80)', iostat = io_status ) dummy
      if ( io_status == iostat_end)  exit 
      write (log%unit, '(a80)') dummy
    enddo 

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
    if (program_calling.eq."grat" ) then
      write(log%unit, form_separator)
      write(log%unit, form_60 ) "Method used:", method
    endif

    write(log%unit, form_separator)
    write(log%unit, form_60 ) "Interpolation data:", & 
      interpolation_names(model%interpolation)(1:7)
  endif
end subroutine

! =============================================================================
! =============================================================================
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

  def_unit=fileunit_tmp
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
function dataname(abbreviation)
  character(len=40) :: dataname
  character(len=2) :: abbreviation

  dataname="unknown"
  if (abbreviation.eq."LS") dataname = "Land-sea mask"
  if (abbreviation.eq."SP") dataname = "Surface pressure"
  if (abbreviation.eq."RS") dataname = "Reference surface pressure"
  if (abbreviation.eq."n") dataname = "interpolation nearest"
  if (abbreviation.eq."b") dataname = "interpolation bilinear"
end function
end module mod_cmdline
