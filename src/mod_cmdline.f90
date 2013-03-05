! =============================================================================
!> \file
!! \brief This module sets the initial values for parameters
!! reads from command line and gives help
!! it allows to specify commands with or without spaces therefore it is 
!! convienient to use with auto completion of names
! =============================================================================
module mod_cmdline

  use mod_constants, only: dp , sp
  use iso_fortran_env
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
    real(sp), allocatable , dimension (:,:) :: coords
  end type

  type polygon_info
    integer :: unit
    character(:), allocatable  :: name
    type(polygon_data) , dimension (:) , allocatable :: polygons
    logical :: if
  end type

  type(polygon_info) , dimension (2) :: polygons

  !----------------------------------------------------
  ! dates
  !----------------------------------------------------
  type dateandmjd
    real(dp) :: mjd
    integer,dimension (6) :: date
  end type

  real(sp) :: cpu_start , cpu_finish  
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
   real(sp)                  :: lat,lon,height
  end type 

  type(site_data) , allocatable , dimension(:) :: sites

  !----------------------------------------------------
  ! various
  !----------------------------------------------------
  integer :: fileunit_tmp               !< unit of scratch file
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

    integer :: unit = output_unit

    ! if file was determined
    logical :: if =.false.

    ! to read into only once
    logical :: first_call =.true.

    ! boundary of model e , w ,s ,n
    real(sp):: limits(4)

!     resolution of model in lon lat
!    real(sp):: resolution(2)

    real(sp) , allocatable ,dimension(:) :: lat , lon , time ,level
    integer , allocatable , dimension(:,: ) :: date
    
    real (sp), dimension(2) :: latrange , lonrange

    ! todo
    logical :: if_constant_value
    real(sp):: constant_value

    ! data 
    !> 4 dimension - lat , lon , level , mjd
    ! todo
    real(sp) , allocatable , dimension (:,:,:) :: data

    ! netcdf identifiers
    integer :: ncid
    integer :: interpolation = 1
  end type

  ! External files
  type(file) ::  log  , output , refpres
  type(file) , allocatable, dimension (:) :: model 
  type(file) :: moreverbose

  character (len =40) :: model_names (5) = ["pressure_surface" , &
    "temperature_surface" , "topography" , "landsea" , "pressure levels" ]


  character(len=5) :: green_names(5) = [ "GN   ", "GN/dt", "GN/dh","GN/dz","GE   "]


  ! Verbose information and the output for log_file
  logical :: if_verbose  = .false.  !< whether print all information
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
!> This subroutine counts the command line arguments
!!
!! Depending on command line options set all initial parameters and reports it
! =============================================================================
subroutine intro (program_calling)
  integer :: i, j
  character(len=255) :: dummy, dummy2,arg
  character(len=*), intent(in) :: program_calling
  type(cmd_line) :: cmd_line_entry

  if(iargc().eq.0) then
    write(output_unit , '(a)' ) , 'Short description: ./'//program_calling//' -h' 
    call exit
  else
    open(newunit=fileunit_tmp,status='scratch')
    write (fileunit_tmp,form_61) "command invoked"
    call get_command(dummy)
    write (fileunit_tmp,form_62) trim(dummy)
    do i = 1 , iargc()
      call get_command_argument(i,dummy)
      ! allow specification like '-F file' and '-Ffile'
      call get_command_argument(i+1,dummy2)
      if (dummy(1:1).eq."-") then
        arg = trim(dummy)
      else
        arg=trim(arg)//trim(dummy)
      endif
      if(dummy2(1:1).eq."-".or.i.eq.iargc()) then
         call mod_cmdline_entry (arg, cmd_line_entry , program_calling = program_calling)
      endif
    enddo

    call if_minimum_args ( program_calling = program_calling )

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
  endif
end subroutine

! =============================================================================
!> Check if at least all obligatory command line arguments were given
!! if not print warning
! =============================================================================
subroutine if_minimum_args (program_calling)
  character (*) , intent(in) :: program_calling

  if (program_calling.eq."grat" ) then
    
    if (size(sites) .eq. 0) then
      write(error_unit, * ) "ERROR:", program_calling
      write(error_unit, * ) "ERROR:", "no sites!"
      call exit
    endif
  elseif (program_calling.eq."polygon_check" ) then
  endif
end subroutine

! =============================================================================
!> This function is true if switch is used by calling program or false if it
!! is not
! =============================================================================
logical function if_switch_program (program_calling , switch )
  character(len=*), intent (in) :: program_calling
  character(len=*),  intent (in) :: switch
  character, dimension(:) , allocatable :: accepted_switch  
  integer :: i

  ! default
  if_switch_program=.false.

  ! depending on program calling decide if switch is permitted
  if (program_calling.eq."grat") then
    allocate( accepted_switch (15) )
    accepted_switch = [ "V" , "f" , "S", "B" , "L" , "G" , "P" , "p", &
      "o" , "F" , "I" , "D" , "L" , "v" , "h"   ]
  elseif (program_calling.eq."polygon_check") then
    allocate( accepted_switch (12) )
    accepted_switch = [ "V" , "f" , "A", "B" , "L" , "P" , "o", "S" , & 
      "h" , "v" , "I" , "i"]
  elseif (program_calling.eq."value_check") then
    allocate( accepted_switch (9) )
    accepted_switch = [ "V" , "F" , "o", "S" , "h" , "v" , "I" , "D" , "L"]
  else
    if_switch_program=.true.
    return
  endif

  ! loop trough accepted switches
  do i =1, size (accepted_switch)
    if (switch(2:2).eq.accepted_switch(i)) if_switch_program=.true.
  enddo
end function

! =============================================================================
!> This subroutine counts the command line arguments and parse appropriately
! =============================================================================
subroutine parse_option (cmd_line_entry , program_calling)
  use mod_utilities, only : file_exists, is_numeric
  type(cmd_line),intent(in):: cmd_line_entry
  character(len=*), optional :: program_calling
  integer :: i

  ! all the command line option are stored in tmp file and later its decide
  ! if it is written to STDOUT , log_file or nowwhere
    select case (cmd_line_entry%switch)
    case ('-h')
      call print_help (program_calling)
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
    case ('-S')
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
              sites(1)%name, sites(1)%lat , sites(1)%lon , sites(1)%height 
        else
          ! or read sites from file
          if (file_exists (cmd_line_entry%field(1) ) ) then
            write(fileunit_tmp, form_62) 'the site file was set:' , &
              cmd_line_entry%field(1)
            call read_site_file (cmd_line_entry%field(1))
          elseif (index(cmd_line_entry%field(1), "/" ).ne.0 &
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
      !> \todo make it mulitichoice: -Lfile:s,file2:b ...
      write (fileunit_tmp , form_62) "printing additional information"
!      allocate(moreverbose(cmd_line_entry%fields))
!      print *,size(moreverbose),"XXXX"
!      do i = 1, cmd_line_entry%fields
!        call get_model_info (moreverbose (i) , cmd_line_entry , i )
!        write (fileunit_tmp , form_62) "file: ", moreverbose(i)%name
!      enddo
!      write (fileunit_tmp , form_62) "what: ", moreverbose%names(1)
!      if (len(moreverbose%name).gt.0 .and. moreverbose%name.ne."") then
!        open (newunit = moreverbose%unit , file = moreverbose%name , action = "write" )
!      endif
    case ("-B")
      if (cmd_line_entry%field(1).eq."N" ) inverted_barometer=.false.
    case ("-R")
      if (cmd_line_entry%field(1).eq."+" ) refpres%if = .true.
    case ('-D')
      call parse_dates ( cmd_line_entry )
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
      do i = 1 , 2 !size(cmd_line_entry%field) 
        polygons(i)%name=cmd_line_entry%field(i)
        if (file_exists((polygons(i)%name))) then
          write(fileunit_tmp, form_62), 'polygon file was set: ' , polygons(i)%name
          polygons(i)%if=.true.
          !> todo
!          call read_polygon (polygons(i))
        else
          write(fileunit_tmp, form_62), 'file do not exist. Polygon file was IGNORED'
        endif
      enddo
    case default
      write(fileunit_tmp,form_62), "unknown argument: IGNORING"
    end select
    return
end subroutine 

! =============================================================================
!> This subroutine parse -G option i.e. reads Greens function
! =============================================================================
subroutine parse_green ( cmd_line_entry)
  use mod_utilities
  type (cmd_line)  :: cmd_line_entry
  character (60) :: filename
  integer :: i , iunit , io_status , lines ,  ii
  integer :: fields(2)= [1,2]
  real (sp) , allocatable , dimension(:) :: tmp

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
      elseif (cmd_line_entry%field(i).eq."huang") then
        filename="/home/mrajner/src/grat/dat/huang_green.dat"
        if (i.eq.1) fields = [1,2]
        if (i.eq.2) fields = [1,3]
        if (i.eq.3) fields = [1,4]
        if (i.eq.4) fields = [1,5]
        if (i.eq.5) fields = [1,6]
      elseif (cmd_line_entry%field(i).eq."rajner") then
        filename="/home/mrajner/src/grat/dat/rajner_green.dat"
        if (i.eq.1) fields = [1,2]
        if (i.eq.2) fields = [1,3]
        if (i.eq.3) fields = [1,4]
        if (i.eq.4) fields = [1,5]
        if (i.eq.5) fields = [1,6]
      elseif (file_exists(cmd_line_entry%field(i))) then
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
! =============================================================================
subroutine mod_cmdline_entry (dummy , cmd_line_entry , program_calling )
  character(*) :: dummy 
  character(:), allocatable :: dummy2
  type (cmd_line),intent(out) :: cmd_line_entry
  character(1) :: separator=","
  character(len=*) , intent(in) , optional :: program_calling
  integer :: i , j , ii , jj 

  cmd_line_entry%switch = dummy(1:2)
  write(fileunit_tmp, form_61) , dummy
  if (.not.if_switch_program(program_calling, cmd_line_entry%switch)) then
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
  call parse_option (cmd_line_entry , program_calling = program_calling)
end subroutine
!
! =============================================================================
!> This subroutine fills the model info
! =============================================================================
subroutine get_model_info ( model , cmd_line_entry , field)
  use mod_utilities, only : file_exists, is_numeric
  type(cmd_line),intent(in):: cmd_line_entry
  type(file),intent(inout):: model
  integer :: field , i 

  model%name = trim(cmd_line_entry%field(field))
  if (model%name.eq."") return
  if ( file_exists (model%name) ) then
    write (fileunit_tmp , form_62) , trim (model_names(field) )
    write(fileunit_tmp, form_63), trim(model%name)

    do i =1 , size (model%names)
      if (size(cmd_line_entry%fieldnames).gt.0) then
        if (i.le.size (cmd_line_entry%fieldnames(field)%names) &
          .and. cmd_line_entry%fieldnames(field)%names(i).ne."" &
          ) then
          model%names(i) = cmd_line_entry%fieldnames(field)%names(i)
        endif
      endif
      write(fileunit_tmp, form_63, advance="no") , trim( model%names(i))
    enddo
    model%if=.true.
    write(fileunit_tmp, form_63)
  elseif(is_numeric(model%name)) then
    model%if_constant_value=.true.
    read (model%name , * ) model%constant_value
    write (fileunit_tmp , form_62) , trim (model_names(field) )
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
  use mod_constants, only : dp ,sp 
  use mod_utilities, only : is_numeric
  real(sp) :: limits (4) , resolution (2) =[1,1]
  real(sp) :: range_lon , range_lat , lat , lon
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
      nloops: do nloop = 1, 2
      if (nloop.eq.2) allocate(sites(good_lines))
      if (number_of_lines.ne.good_lines) then
        call print_warning ("site_file_format")
      endif
      good_lines=0
      line_loop:do 
        read ( fileunit_site , '(a)' , iostat = io_status ) line_of_file 
        if ( io_status == iostat_end)  exit  line_loop
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

!          ! todo
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
              if (nloop.eq.2) write ( fileunit_tmp, form_63) "rejecting (lon limits):" , line_of_file
            endif
          else
            if (nloop.eq.2) write ( fileunit_tmp, form_63) "rejecting (lat limits):" , line_of_file
          endif
          
        else
          ! print it only once
          if (nloop.eq.2) then
              write ( fileunit_tmp, form_63) "rejecting (args):      " , line_of_file
          endif
        endif
      enddo line_loop
      if (nloop.eq.1) rewind(fileunit_site)
    enddo nloops

  ! if longitude <-180, 180> change to <0,360) domain
  do i =1 , size (sites)
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
  integer , dimension(6) :: start , stop 
  real (sp) :: step =6. ! step in hours
  integer :: i

  call string2date(cmd_line_entry%field(1), start)
  write (fileunit_tmp , form_62) "start date:" , start
  if (cmd_line_entry%field(2).eq."".or.cmd_line_entry%fields.le.1) then
    stop = start
  else
    call string2date(cmd_line_entry%field(2), stop )
    write (fileunit_tmp , form_62) "stop date: " , stop
  endif
  if (is_numeric (cmd_line_entry%field(3)).and.cmd_line_entry%fields.ge.3) then
    read(cmd_line_entry%field(3),*) step
    write (fileunit_tmp , form_62) "interval [h]:" , step
  endif

  allocate (dates ( int( ( mjd(stop) - mjd(start) ) / step * 24. + 1 ) ))
  do i = 1 , size(dates)
    dates(i)%mjd = mjd(start) + ( i -1 ) * step / 24.
    call invmjd ( dates(i)%mjd , dates(i)%date)
  enddo
end subroutine

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


!!subroutine sprawdzdate(mjd)
!!  real:: mjd
!!    if (mjd.gt.jd(data_uruchomienia(1),data_uruchomienia(2),data_uruchomienia(3),data_uruchomienia(4),data_uruchomienia(5),data_uruchomienia(6))) then
!!      write (*,'(4x,a)') "Data późniejsza niż dzisiaj. KOŃCZĘ!"
!!      call exit
!!    elseif (mjd.lt.jd(1980,1,1,0,0,0)) then
!!      write (*,'(4x,a)') "Data wcześniejsza niż 1980-01-01. KOŃCZĘ!"
!!      call exit
!!    endif
!!    if (.not.log_E) then
!!      data_koniec=data_poczatek
!!      mjd_koniec=mjd_poczatek
!!    endif
!!    if (mjd_koniec.lt.mjd_poczatek) then
!!      write (*,*) "Data końcowa większa od początkowej. KOŃCZĘ!"
!!      write (*,form_64) "Data końcowa większa od początkowej. KOŃCZĘ!"
!!    endif
!!end subroutine
!
! =============================================================================
!> Print version of program depending on program calling
! =============================================================================
subroutine print_version (program_calling)
  character(*) :: program_calling 
  integer :: version_unit , io_stat
  character(40) :: version

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
    1x,i2.2,2(":",i2.2),1x,"(",SP,i3.2,"h UTC)")'),          &
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
    write(log%unit, form_separator)
    write(log%unit, form_60 ) "Processing:", size(sites), "sites"
    write(log%unit, '(2x,a,t16,3a15)') "Name" , "lat [deg]" , "lon [deg]" ,"H [m]"
    do j = 1,size(sites)
      write(log%unit, '(2x,a,t16,3f15.4)') &
        sites(j)%name, sites(j)%lat, sites(j)%lon , sites(j)%height
      if (j.eq.10) exit
    enddo
    if (size(sites).gt.10) write(log%unit , form_62 ) &
      "and", size(sites)-10, "more"

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

subroutine print_help (program_calling)
  character(*) :: program_calling
  integer :: help_unit , io_stat
  character(500)::line
  character(255)::syntax
  logical:: if_print_line = .false., if_optional=.true.

  if_print_line=.false.

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
      write(log%unit, *) "" 
      if_print_line = .false.
      exit
    endif
    if(line(1:1)=="-") then
      if(if_switch_program (program_calling , line(1:2) )) then
        if_print_line = .true.
      else
        if(line(1:1)=="-") if_print_line=.false.
      endif
    endif

    if (line(5:13) == "optional " .and. (line(2:2) == program_calling(1:1) .or. line(2:2)=="")) then
      if_optional=.true.
    elseif (line(5:13) == "mandatory") then
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
      if(if_switch_program (program_calling , line(1:2) )) then
        if_print_line = .true.
        write (log%unit , form_61 ) trim(line)
      else
        if(line(1:1)=="-") if_print_line=.false.
      endif
    elseif (line(2:2)==program_calling(1:1) .or. line(2:2)=="s") then
      if (if_print_line) then
        write (log%unit , form_61 ) "  "//trim(line(3:))
      endif
    elseif (line(2:2)=="") then
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
  elseif (warn .eq. "boundaries") then
    write(def_unit, form_62) "something wrong with boundaries. IGNORED"
  elseif (warn .eq. "site") then
    write(def_unit, form_62) "something wrong with -S specification. IGNORED"
  elseif (warn .eq. "repeated") then
    write(def_unit, form_62) "reapeted specification. IGNORED"
  elseif (warn .eq. "dates") then
    write(def_unit, form_62) "something wrong with date format -D. IGNORED"
  endif
end subroutine


! =============================================================================
!> Counts number of properly specified models
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

end module mod_cmdline
