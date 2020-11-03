! =============================================================================
!> This module gives routines to read, and write data
!!
!! The netCDF format is widely used in geoscienses. Moreover it is
!! self-describing and machine independent. It also allows for reading and
!! writing small subset of data therefore very efficient for large datafiles
!! (this case)
!! \cite netcdf
!! \author M. Rajner
!! \date 2013-03-04
! =============================================================================
module mod_data
  use mr_constants, only: dp
  use mr_utilities, only: setnan

  implicit none

  type file
    character(90) :: name

    ! varname,lonname,latname,levelname,timename
    character(len=25) :: names(5) = &
      [character (len=25) :: "z", "lon", "lat", "level", "time"]

    character(len=100) :: datanames(5) = " "

    character(len=15) :: dataname

    ! if file was determined
    logical :: if =.false.

    real(dp), allocatable, dimension(:)   :: lat, lon, time
    integer , allocatable, dimension(:)   :: level
    integer , allocatable, dimension(:,:) :: date

    real (dp), dimension(2) :: latrange, lonrange, varrange

    logical :: if_constant_value = .false.
    real(dp):: constant_value

    real(dp), allocatable, dimension(:,:,:) :: data

    integer :: ncid = 0
    logical :: huge     = .false.
    logical :: autoload = .false.
    logical :: exist    = .false.
    character(10) :: autoloadname
  end type

  type(file), allocatable, dimension(:) :: model

  logical :: all_huge = .false.

  private :: dataname

  type level_info
    integer,  allocatable, dimension(:) :: level
    real(dp), allocatable, dimension(:) :: &
      height, temperature, humidity
    logical :: all = .false.
  end type

  type(level_info) :: level

contains

! =============================================================================
!> This subroutine parse model information from command line entry
!!
!! \author M. Rajner
!! \date 2013.05.20
! =============================================================================
subroutine parse_model(cmd_line_entry)
  use mod_cmdline
  use mod_printing
  use mr_utilities, only: file_exists, is_numeric
  type(cmd_line_arg)  :: cmd_line_entry
  integer :: i, j

  if (allocated(model)) then
    call print_warning ("repeated", more = "-F")
    return
  endif

  allocate(model(size(cmd_line_entry%field)))

  do i = 1, size(cmd_line_entry%field)

    model(i)%exist = .true.

    if (.not.log%sparse) then
      write(log%unit, form_62) trim(cmd_line_entry%field(i)%full)
    endif

    model(i)%name = trim(cmd_line_entry%field(i)%subfield(1)%name)
    model(i)%dataname = trim(cmd_line_entry%field(i)%subfield(1)%dataname)

    if (model(i)%dataname.eq."") then
      model(i)%dataname = "NN"

    elseif (index(model(i)%dataname,"!").ne.0) then

      model(i)%huge = .true.
      model(i)%dataname = model(i)%dataname (1: index(model(i)%dataname,"!")-1)

      if (.not.log%sparse) write(log%unit, form%i3) "!:huge"

    endif

    if (all_huge) model(i)%huge = .true.

    if (model(i)%name.eq."") then
      if (i.gt.1) then
        model(i)%name = model(i-1)%name
      else
        call print_warning ("model", error = .true.)
      endif
    endif

    do j = 2, size(cmd_line_entry%field(i)%subfield)

      if (cmd_line_entry%field(i)%subfield(j)%dataname.ne."") then
        model(i)%datanames(j-1) = cmd_line_entry%field(i)%subfield(j)%dataname
      endif

    enddo

    if (file_exists(model(i)%name)) then

      do j =2, size (cmd_line_entry%field(i)%subfield)
        if (cmd_line_entry%field(i)%subfield(j)%name.ne."") then
          model(i)%names(j-1) = cmd_line_entry%field(i)%subfield(j)%name
        endif
      enddo

      if (.not.log%sparse) then
        write (log%unit, form%i3,advance = 'no') &
          trim (dataname(model(i)%dataname)), &
          "("//trim(model(i)%dataname)//")"
        write(log%unit, '(5(a,1x))', advance="no") (trim(model(i)%names(j)), j=1,5)
        write(log%unit, *)
      endif

      model(i)%if=.true.
      if (model(i)%dataname.ne."ascii") then
        call read_netCDF(model(i), print=.not.log%sparse)
      endif

      ! listing in log
      model(i)%constant_value =      &
        variable_modifier(           &
        model(i)%constant_value,     &
        model(i)%datanames(1),       &
        verbose   = .not.log%sparse, &
        list_only = .true.           &
        )

    else if (is_numeric(model(i)%name)) then

      model(i)%if_constant_value=.true.
      write (log%unit, form%i3)            &
        trim (dataname(model(i)%dataname)), &
        "("//trim(model(i)%dataname)//")"

      read (model(i)%name, * ) model(i)%constant_value
      write(log%unit, '(' // form%t3 // "a," // output%form // ")") &
        'constant value was set:   ', model(i)%constant_value

      if (trim(model(i)%datanames(1)).ne."") then
        model(i)%constant_value =   &
          variable_modifier(        &
          model(i)%constant_value,  &
          model(i)%datanames(1),    &
          verbose = .not.log%sparse &
          )

        write(log%unit, '(' // form%t3 // "a," // output%form // ")") &
          'constant value was re-set:', model(i)%constant_value

      endif

      model(i)%lonrange = [  0,360]
      model(i)%latrange = [-90, 90]

    else
      !check autoload
      call model_aliases(model(i), dryrun=.true.)

      if (.not.model(i)%if) then
        call print_warning (                                   &
          "model",                                             &
          more  = trim(model(i)%name)//" : file do not exist", &
          error = .true.                                       &
          )
      endif

    endif
  enddo
end subroutine

! =============================================================================
! =============================================================================
subroutine model_aliases(model, dryrun, year, month)
  use mod_printing
  use mr_utilities, only: file_exists
  use mod_printing,   only: warnings

  type(file) :: model
  logical, intent(in), optional :: dryrun
  integer, intent(in), optional :: year, month
  character(150) :: prefix
  integer :: year_, month_
  logical :: if_dryrun

  if_dryrun = .false.
  if(present(dryrun)) if_dryrun = dryrun

  if (present(year)) then
    year_=year
    if (present(month)) then
      month_=month
    endif
  else
    year_  = 9999
    month_ = 99
  endif

  if (.not. model%autoload) model%autoloadname = model%name
  model%if=.true.

  select case (model%autoloadname)

  case ("NCEP", "NCEP2", "NCEP1")

    if (.not.model%autoload) then
      model%autoload=.true.
    endif

    if (model%autoloadname.eq."NCEP1") then
      prefix="/home/mrajner/dat/ncep_reanalysis/reanalysis1/"
    else
      prefix="/home/mrajner/dat/ncep_reanalysis/"
    endif

    select case (model%dataname)
    case ("SP")
      model%names(1)="pres"
      write(model%name,'(a,a,i4,a)') trim(prefix),"pres.sfc.",year_,".nc"

    case ("GP")
      model%names(1)="hgt"
      write(model%name,'(a,a,i4,a)') trim(prefix),"hgt.",year_,".nc"
      if (if_dryrun) then
        if (model%datanames(1).eq."") then
          model%datanames(1) = "gh2h"
        else
          model%datanames(1) = "gh2h@" // trim(model%datanames(1))
        endif
      endif

    case ("VT")
      model%names(1) = "air"
      write(model%name,'(a,a,i4,a)') trim(prefix),"air.",year_,".nc"

    case ("VRH")
      model%names(1) = "rhum"
      write(model%name,'(a,a,i4,a)') trim(prefix),"rhum.",year_,".nc"

    case ("VSH")
      if (model%autoloadname.eq."NCEP1") then
        model%names(1) = "shum"
        write(model%name, '(a,a,i4,a)') trim(prefix),"shum.",year_,".nc"

      else
        call print_warning ("not yet NCEP@VSH", error=.true.)

      endif

    case ("SRH")
      if (model%autoloadname.eq."NCEP1") then
        model%names(1) = "rhum"
        write(model%name,'(a,a,i4,a)') trim(prefix),"rhum.sig995.",year_,".nc"
      else
        call print_warning ("not yet NCEP@SRH", error=.true.)
      endif

    case ("T")
      if (model%autoloadname.eq."NCEP1") then
        model%names(1) = "air"
        write(model%name,'(a,a,i4,a)') trim(prefix),"air.sig995.",year_,".nc"
      else
        model%names(1) = "temp"
        call print_warning ("not yet NCEP@T", error = .true.)
      endif

    case ("HP","H")
      model%names(1) = "hgt"
      write(model%name,'(a,a,i4,a)') trim(prefix),"hgt.sfc.nc"
      model%autoload=.false.
      if (if_dryrun) then
        if (model%datanames(1).eq."") then
          model%datanames(1) = "gh2h"
        else
          model%datanames(1) = "gh2h@"// trim(model%datanames(1))
        endif
      endif

    case ("LS")
      model%names(1) = "land"
      model%name     = "/home/mrajner/dat/landsea/ncep_ls.nc"
      model%autoload = .false.

    case default
      model%autoload=.false.
      model%if=.false.
      return
    endselect

  case ("ERA", "ERAq")
    if (.not.model%autoload) model%autoload=.true.
    prefix="/home/mrajner/dat/erainterim/"

    select case (model%dataname)
    case ("SP")
      model%names(1)="sp"
      write(model%name,'(a,a,i4,a)') trim(prefix),"sp.",year_,".nc"
    case ("GP")
      write(model%name,'(a,a,i4,i2.2,a)') trim(prefix),"gp_l.",year_,month_,".nc"
      if (if_dryrun) then
        if (model%datanames(1).ne."") then
          model%datanames(1) = "gp2h@"// trim(model%datanames(1))
        else
          model%datanames(1) = "gp2h"
        endif
      endif

    case ("VT")
      model%names(1)="t"
      write(model%name,'(a,a,i4,i2.2,a)') trim(prefix),"t_l.",year_,month_,".nc"
    case ("VSH")
      model%names(1)="q"
      write(model%name,'(a,a,i4,i2.2,a)') trim(prefix),"sh_l.",year_,month_,".nc"
    case ("T")
      model%names(1)="v2t"
      model%names(1)="t2m"
      write(model%name,'(a,a,i4,a)') trim(prefix),"t.",year_,".nc"
    case ("HP","H")
      model%names(1)="z"
      write(model%name,'(a,a,i4,a)') trim(prefix),"gp.nc"
      model%autoload=.false.
      if (if_dryrun) then
        if (model%datanames(1).ne."") then
          model%datanames(1) = "gp2h@"// trim(model%datanames(1))
        else
          model%datanames(1) = "gp2h"
        endif
      endif
    case ("LS")
      model%names(1)="lsm"
      model%name="/home/mrajner/dat/landsea/era_ls.nc"
      model%autoload=.false.
    case default
      model%autoload=.false.
      model%if=.false.
    endselect

    if (model%autoloadname=="ERAq") then
      model%name(index(model%name,".nc"):)="_j.nc"
    endif

  case ("VIENNA")
    prefix="/home/mrajner/dat/refpres/"

    select case (model%dataname)

    case ("RSP", "SP")
      model%names(1)="rp"
      write(model%name,'(a,a)') trim(prefix),"refpres0p5.nc"

    case ("H", "HRSP", "HP")
      model%names(1)="height"
      write(model%name,'(a,a)') trim(prefix),"refpres0p5.nc"

    case default
      model%if=.false.

    endselect

  case ("ETOPO")
    prefix="/home/mrajner/dat/etopo/"
    model%huge=.true.

    select case (model%dataname)

    case ("LS")
      model%names(1)= "z"
      write(model%name,'(a,a)') trim(prefix),"ETOPO_ls.nc"

    case ("H")
      model%names(1)="z"
      write(model%name,'(a,a)') trim(prefix),"ETOPO_zero_ocean.grd"


    case default
      model%if=.false.
    endselect

  case default
    model%if       = .false.
    model%autoload = .false.
  endselect

  ! listing in log
  model%constant_value =         &
    variable_modifier(           &
    model%constant_value,        &
    model%datanames(1),          &
    verbose   = .not.log%sparse, &
    list_only = .true.           &
    )

  if (model%if.and..not.model%autoload) then
    call read_netCDF(model, print=.not.log%sparse)
  endif

  if(if_dryrun) return

  if (.not.file_exists(model%name)) then
    call print_warning (                                &
      "model",                                          &
      more  = trim(model%name)//" : file do not exist", &
      error = warnings%file_exist                       &
      )
    model%exist=.false.
    return
  else
    model%exist=.true.
  endif

  call read_netCDF(model, print=.not.log%sparse, force=.true.)
end subroutine

! =============================================================================
! TODO do not modify all matrices, only values used in output or computation
! or convert this routine as `elemental`
! =============================================================================
function variable_modifier(val, modifier, verbose, list_only)
  use mr_atmosphere, only: geop2geom
  use mr_constants,  only: earth
  use mr_utilities,  only: ntokens
  use mr_conversion, only: mmwater2pascal
  use mod_printing,  only: print_warning, form, log, output

  real(dp) :: variable_modifier
  real(dp), intent(in) :: val
  character(*), intent(in) :: modifier
  character(20) :: key, keyval
  character(100) :: modifier_
  real(dp) :: numerickeyval
  integer :: i
  logical, optional, intent(in) :: verbose, list_only
  logical :: if_verbose

  if_verbose = .false.
  if (present(verbose)) if_verbose = verbose

  variable_modifier = val
  modifier_         = modifier
  do i = 1, ntokens(modifier_,"@")
    keyval=''

    if (ntokens(modifier_,"@").eq.1) then
      key = modifier_
    else
      key = modifier_(1: index(modifier_, "@")-1)
    endif

    if (index(key,"=").gt.0)  then
      keyval = trim(key(index(key,"=")+1:))
      key    = trim(key(1:index(key,"=")-1))
    endif

    if (if_verbose) then
      write(log%unit, "("//form%t3//"3(a,x)$)" ) &
        "var modifier:", trim(key), trim(keyval)
    endif

    select case (key)

    case ("gh2h") ! g2h is obsolete
      variable_modifier=geop2geom(variable_modifier)
      ! case ("gp2gh")
      ! variable_modifier=variable_modifier/earth%gravity%mean
      call print_warning("gh2h is obsolete noop")

    case ("gp2h")
      variable_modifier=geop2geom(variable_modifier)/earth%gravity%mean

    case ("nan")
      read(keyval,*) numerickeyval
      if (isnan(variable_modifier)) variable_modifier = numerickeyval

    case ("scale")
      read(keyval,*) numerickeyval
      variable_modifier = numerickeyval*variable_modifier

    case ("invscale")
      read(keyval,*) numerickeyval
      variable_modifier=1./numerickeyval*variable_modifier

    case ("offset")
      read(keyval,*) numerickeyval
      variable_modifier=numerickeyval+variable_modifier

    case ("mmwater2pascal")
      variable_modifier= mmwater2pascal(variable_modifier)

    case ("pascal2mmwater")
      variable_modifier= mmwater2pascal(variable_modifier, &
        inverted = .true.)

    case default
      call print_warning ("variable modifier not found " // key, error=.true.)

    endselect

    if (.not.present(list_only)) then
      if(if_verbose) then
        write (log%unit, '('// output%form // ')') variable_modifier
      endif
    else
      if(if_verbose) write (log%unit, *)
    endif

    modifier_ = modifier_(index(modifier_, "@")+1:)
  enddo
end function

! =============================================================================
!> Read netCDF file into memory
! =============================================================================
subroutine read_netCDF(model, print, force)
  use netcdf
  use mod_printing
  use mod_cmdline,  only: ind
  use mr_utilities, only: file_exists, basename

  type (file) :: model
  logical, optional, intent(in) :: print, force
  integer :: i
  logical :: if_force, if_print

  if_force = .false.
  if_print = .false.
  if (present(force)) if_force = force
  if (present(print)) if_print = print

  if (if_force) then
    if(allocated(model%data))  deallocate(model%data)
    if(allocated(model%lat))   deallocate(model%lat)
    if(allocated(model%lon))   deallocate(model%lon)
    if(allocated(model%date))  deallocate(model%date)
    if(allocated(model%level)) deallocate(model%level)
    if(allocated(model%time))  deallocate(model%time)
  endif

  if (.not.file_exists(model%name)) &
    call print_warning("file not exist " // trim (model%name), &
    error=.true.)

  if (model%ncid.ne.0) then
    call nc_error  (nf90_close (model%ncid))
    write (log%unit, form%i4) &
      "closing file:", trim(basename(trim(model%name)))
  endif

  if (print) then
    write (log%unit, form%i3)           &
      "Opening file:",                  &
      trim(basename(trim(model%name))), &
      ", huge [T/F]:",                  &
      model%huge
  endif

  call nc_error (nf90_open(model%name, nf90_nowrite, model%ncid))

  do i = 2,5
    call get_dimension (model, i, print=.not.log%sparse)
  enddo

  if (size(model%time).ge.1) call nctime2date (model, print=print)
end subroutine

! =============================================================================
!> \brief Get dimension, allocate memory and fill with values
!! \author Marcin Rajner
!! \date 2013.05.24
! =============================================================================
subroutine get_dimension(model, i, print)
  use netcdf
  use mod_printing

  type(file) :: model
  integer :: dimid, varid
  integer, intent(in) :: i
  integer :: length, status
  logical :: print

  if (print)then
    write (log%unit, form%i4, advance='no') "Getting dim:",trim(model%names(i)), ".."
  endif

  status = nf90_inq_dimid(model%ncid,model%names(i), dimid)

  if (status /=nf90_noerr) then

    if(model%names(i).eq."lon") then

      model%names(i)="longitude"

      if (print) then
        write(log%unit, '(a)', advance='no') model%names(i)
      endif

      status = nf90_inq_dimid(model%ncid, model%names(i), dimid)

    else if(model%names(i).eq."lat") then

      model%names(i)="latitude"

      if (print)then
        write(log%unit, '(a)', advance='no') model%names(i)
      endif
    endif

    status = nf90_inq_dimid(model%ncid, model%names(i), dimid)
  endif

  if(status /= nf90_noerr.and.any(i.eq.[2,3])) then

    call print_warning("key variable not found: " &
      // trim(model%names(i)), error=.true.)

  elseif(status /= nf90_noerr) then

    if (print)then
      write (log%unit, '(a6,1x,a)') &
        trim(model%names(i)),"not found, allocating (1)..."
      call nc_info(model)
    endif

    length=1
  else

    if (print)then
      write (log%unit, '(a6,1x,a)') "ok"
    endif

    call nc_error (nf90_inquire_dimension(model%ncid, dimid, len=length))

    call nc_error (nf90_inq_varid(model%ncid, model%names(i), varid))

  endif

  if (i.eq.3 ) then
    allocate(model%lat(length))
    call nc_error (nf90_get_var (model%ncid, varid, model%lat))

    status = nf90_get_att(model%ncid, varid, &
      "actual_range", model%latrange)

    if (status /= nf90_noerr ) then
      model%latrange =[model%lat(1), model%lat(size(model%lat)) ]
    endif

  else if (i.eq.2) then
    allocate(model%lon(length))
    call nc_error (nf90_get_var (model%ncid,  varid, model%lon))

    status = nf90_get_att ( model%ncid, varid, &
      "actual_range", model%lonrange)

    if (status /= nf90_noerr ) &
      model%lonrange = [model%lon(1), model%lon(size(model%lon))]

    where (model%lonrange.ge.357.5)
      model%lonrange = 360
    end where

  else if (i.eq.4) then
    allocate(model%level(length))
    status = nf90_inq_dimid(model%ncid,model%names(i), dimid)

    if (status.ne.nf90_noerr) then
      model%level=0
      return
    else
      status = nf90_get_var (model%ncid, varid, model%level)
    endif

  elseif (i.eq.5) then
    allocate(model%time(length))
    status = nf90_get_var (model%ncid, varid, model%time)
  endif
end subroutine

! =============================================================================
!> Change time in netcdf to dates
!!
!! \author M. Rajner
!! \date 2013-03-04
! TODO make it more generic, too many repetition in code
! strip T in isodate (only made ones)
! =============================================================================
subroutine nctime2date(model, print)
  use netcdf
  use mod_printing
  use mr_mjd,      only: mjd, invmjd

  type (file)        :: model
  real(dp)           :: mjd_start, mjd_
  integer            :: varid, i, ind(3), date(6), status, length
  character(:), allocatable :: dummy
  logical, optional :: print

  date = 0

  status = nf90_inq_varid (model%ncid, model%names(5), varid)
  if (status /= nf90_noerr) return

  call nc_error  (nf90_inquire_attribute (model%ncid, varid, "units", len=length))

  ! not working with old gfortran
  allocate(character(len=length):: dummy)

  ! working 
  ! dummy=repeat(" ",length)

  call nc_error  (nf90_get_att (model%ncid, varid, "units", dummy))

  allocate (model%date(size(model%time), 6))

  if (.not. (present(print).and..not.print))then
    write(log%unit, form%i4) "Converting time: ", trim(dummy)
  endif

  if (dummy.eq. "hours since 1-1-1 00:00:0.0") then
    ! -2 is necessary to keep it with ncep convention
    ! this may need (?) change for other data fields
    ! be carefull
    mjd_start =  mjd([1,1,1,0,0,0]) - 2

  else if (index(dummy,"hours since").eq.1) then
    dummy = trim (dummy(len("hours since")+1:))

    do
      ind=[index(dummy,'-'), index(dummy,':'), index(dummy,'T')]
      do i=1,2
        if (ind(i).ne.0) dummy = trim(dummy(1:ind(i)-1))//" "//trim(dummy(ind(i)+i:))
      enddo
      if (index(dummy,'-').eq.0 .and. index(dummy,':').eq.0) exit
    enddo

    if (dummy(len(dummy)-1:) == ".0") then
      dummy = dummy(1:len(dummy)-2)//" 0"
    endif

    read(dummy,*) date

    mjd_start = mjd (date)

  else if (index(dummy,"Days since").eq.1 .or. index(dummy,"days since").eq.1) then
    ! this option for gldas from grace tellus

    do i=1,2
      dummy = dummy(index(dummy, ' ')+1:)
    enddo

    do
      ind=[index(dummy,'-'), index(dummy,':'), index(dummy,'T')]
      do i=1, 3
        if (ind(i).ne.0) dummy = dummy(1:ind(i)-1)//" "//dummy(ind(i)+i:)
      enddo
      if (index(dummy,'-').eq.0 .and. index(dummy,':').eq.0 .and. index(dummy,'T').eq.0) exit
    enddo

    !TODO "remove time zone"
    if ( dummy(len(dummy):len(dummy)).eq."Z") then
      dummy = dummy(1:len(dummy)-1)
    endif


    dummy = dummy//" 0 0 0"

    read(dummy,*) date

    mjd_start  = mjd (date)
    model%time = model%time*24

  else if (index(dummy,"seconds since").eq.1 .or. index(dummy,"Seconds since").eq.1) then

    do i=1,2
      dummy = dummy(index(dummy, ' ')+1:)
    enddo

    do
      ind=[index(dummy,'-'), index(dummy,':'), index(dummy,'T')]
      do i=1,2
        if (ind(i).ne.0) dummy = dummy(1:ind(i)-1)//" "//dummy(ind(i)+i:)
      enddo
      if (index(dummy,'-').eq.0 .and. index(dummy,':').eq.0) exit
    enddo

    dummy = dummy//" 0 0 0"
    read(dummy,*) date

    mjd_start  = mjd(date)
    model%time = model%time/3600

  else
    call print_warning("unknown time begining in ncfile")

  endif

  do i = 1, size(model%time)
    mjd_= model%time(i) / 24 + mjd_start
    
    model%date(i,:) = invmjd(mjd_)
  enddo
end subroutine

! =============================================================================
!> get time index
! =============================================================================
function get_time_index(model,date)
  integer :: get_time_index
  type (file), intent(in) :: model
  integer, intent(in), dimension(6), optional ::date
  integer :: i


  get_time_index=0
  if (.not.present(date) .or. (.not.allocated(model%date)) .or. size(model%date(:,1)).le.1) then
    get_time_index=1
    return
  endif

  do i = 1, size(model%date(:,1))
    if (all(model%date(i,1:6) .eq. date(1:6))) then
      get_time_index=i
    endif
  enddo
end function

! =============================================================================
!> get level index
! =============================================================================
function get_level_index(model, level, sucess)
  use mod_printing, only: print_warning
  use mr_utilities, only: basename
  use netcdf

  integer :: get_level_index
  type (file), intent(in) :: model
  integer, intent(in), optional :: level
  logical, intent(out), optional :: sucess
  integer :: i
  logical :: first_fail=.true.

  get_level_index = 1

  if (.not.present(level).or.ubound(model%level,1).le.1) then
    if (present(sucess)) sucess = .true.
    return
  endif

  if (.not.if_variable_use_dimension(model, ivarname=1, idimname=4)) then
    sucess = .true.
    return
  endif

  do i = 1, size(model%level)
    if (model%level(i).eq.level) then
      get_level_index=i
      if (present(sucess)) sucess=.true.
      return
    endif
  enddo

  if (present(sucess)) sucess = .false.

  if (first_fail) call print_warning("level not found (no warning again) " &
    //basename(model%name))
  first_fail=.false.
end function

! =============================================================================
! =============================================================================
subroutine nc_info(model)
  use netcdf
  use mod_printing
  type (file), intent(in) :: model
  integer :: ndims, nvars, i
  integer, allocatable, dimension(:) :: varids
  character(20), allocatable, dimension(:) :: name

  call nc_error  (nf90_inquire(model%ncid, ndims, nvars))
  allocate(varids(nvars))
  allocate(name(nvars))

  call nc_error  (nf90_inq_varids(model%ncid, nvars, varids))

  do i=1, nvars
    call nc_error (nf90_inquire_variable(model%ncid, varids(i), name(i)))
  enddo

  write(log%unit, form%i5 ) (trim(name(i))//",", i =1,nvars)
end subroutine

! =============================================================================
!> \brief Get variable from netCDF file for specified variables
! =============================================================================
subroutine get_variable(model, date, print, level)
  use netcdf
  use mod_printing, only: log, print_warning

  type (file), intent(inout) :: model
  integer, optional, intent(in), dimension(6) ::date
  integer :: varid, status
  integer :: start(4)
  integer :: index_time, i, j, k
  real (dp) :: scale_factor, add_offset
  logical, optional :: print
  integer, optional :: level
  character (20) :: aux
  logical :: first_warning=.true., ifprint

  ifprint = .true.
  if (present(print)) ifprint = print

  if (                          &
    model%huge                  &
    .or.model%if_constant_value &
    .or..not. model%if) return

  index_time = 0

  status = nf90_inq_varid(model%ncid, model%names(1), varid)

  if (status /= nf90_noerr) then
    call nc_info(model)
    call print_warning(                               &
      "variable not found: " // trim(model%names(1)), &
      error=.true.                                    &
      )
  endif

  if (allocated(model%data)) deallocate(model%data)

  if (if_variable_use_dimension(model, 1 , 4)) then
    allocate (          &
      model%data (      &
      size(model%lon),  &
      size(model%lat),  &
      size(model%level) &
      )                 &
      )

  else
    allocate (          &
      model%data (      &
      size(model%lon),  &
      size(model%lat),  &
      1 &
      )                 &
      )
  endif

  if (size(date).gt.0 .and. present(date)) then
    index_time = get_time_index(model, date)

    if (index_time.eq.0) then

      if (ifprint)then
        if (.not.log%sparse) then
          write(aux, '(i4.4,5i2.2)') date
          call print_warning("cannot find date: "// aux &
            // "var: " // trim(model%names(1)) // " in file: " // trim(model%name))
        endif
      endif

      model%data= setnan()
      return

    endif

  else
    index_time = 1
  endif

  if(present(level)) stop '!?! look into source -- strange (not probable) execution!'

  if (if_variable_use_dimension(model,1,4))  then
    start = [1,1,1,index_time]
    if (first_warning) then
      call print_warning('reading whole file with levels into memory could slow down computation')
      first_warning=.false.
    endif
  else
    start = [1,1,index_time,1]
  endif

  call nc_error(nf90_get_var( &
    ncid   = model%ncid,      &
    varid  = varid,           &
    values = model%data,      &
    start  = start            &
    )                         &
    )

  call get_scale_and_offset (model%ncid, model%names(1), scale_factor, add_offset, status)
  model%data = model%data * scale_factor + add_offset

  status = nf90_get_att(model%ncid, varid, "actual_range", model%varrange)

  if (status == nf90_noerr) then
    where(model%data.gt.model%varrange(2).or.model%data.lt.model%varrange(1))
      model%data=setnan()
    end where
  end if

  ! TODO make elemental function variable_modifier and use without loop
  ! test if any speed gain
  if (trim(model%datanames(1)).ne."") then
    do i = 1, size(model%data,1)
      do j = 1, size(model%data,2)
        do k = 1, size(model%data,3)
          model%data(i,j,k) = variable_modifier (model%data(i,j,k), model%datanames(1))
        enddo
      enddo
    enddo
  endif

  return
end subroutine

! =============================================================================
!> \brief Unpack variable
!!
!! from \cite netcdf
!! see http://www.unidata.ucar.edu/software/netcdf/docs/BestPractices.html
! =============================================================================
subroutine get_scale_and_offset(ncid, varname, scale_factor, add_offset, status)
  use netcdf
  integer, intent(in) :: ncid
  character(*), intent(in)::varname
  integer :: varid
  integer, intent(out) :: status
  real(dp), intent(out) :: scale_factor, add_offset

  call nc_error (nf90_inq_varid(ncid, varname, varid))
  status = nf90_get_att (ncid, varid, "scale_factor", scale_factor)
  if (status /=nf90_noerr) scale_factor=1

  status = nf90_get_att (ncid, varid, "add_offset", add_offset)
  if (status /=nf90_noerr) add_offset=0
end subroutine

! =============================================================================
!> nc_error check the return code from netCDF manipulation
!!
!! \author From netcdf website \cite netcdf
!! \date 2013-03-04
! =============================================================================
subroutine nc_error(status, success)
  use netcdf, only: nf90_noerr, nf90_strerror
  use mod_printing
  use iso_fortran_env

  integer, intent (in) :: status
  logical, intent(out), optional :: success


  if(status /= nf90_noerr) then
    call print_warning( &
      "check " // trim(nf90_strerror(status)))
    if (present(success)) then
      success=.false.
    endif
    return
  else
    if (present(success)) then
      success=.true.
    endif
    return
  end if
end subroutine

! =============================================================================
!> \brief Returns the value from model file
!!
!! The ilustration explain optional \c method argument
!! \latexonly
!! \begin{center}
!!  \tikzsetfigurename{interpolation_ilustration}
!!  \input{/home/mrajner/src/grat/doc/figures/interpolation_ilustration}\\
!! \end{center}
!! \endlatexonly
!! \image html /home/mrajner/src/grat/doc/figures/interpolation_ilustration.svg
!!
!! lat and lon in decimal degree
! =============================================================================
subroutine get_value(model, lat, lon, val, level, method, date)
  use mod_cmdline, only: moreverbose, ind
  use mr_utilities, only: r2d, bilinear, basename
  use netcdf
  use mod_printing, only: warnings, print_warning
  use iso_fortran_env, only: error_unit

  type(file), intent (in) :: model
  real(dp) :: lat, lon
  real(dp), intent(out) ::  val
  character(1), optional, intent(in) :: method
  integer, optional, intent(in) :: level
  integer :: j, ilevel = 1
  integer :: ilon, ilat, ilon2, ilat2, varid, status
  real(dp), dimension(4,3) :: array_aux
  integer, dimension(4,2) :: array_aux_ind
  real(dp) :: scale_factor, add_offset
  integer, intent(in), optional :: date(6)
  logical :: success, success2, first_warning=.false.

  if (model%ncid==0 .and. .not.model%if_constant_value) return

  val = setnan()

  if (model%if_constant_value) then
    val = model%constant_value
    return
  end if

  if (.not.model%exist.or..not.model%if) then
    return
  endif

  if (present(level)) ilevel=level

  if (model%autoloadname.eq."ETOPO".and.lat.lt.-89.9999) then
    lat=-89.99999
  endif

  ! check if inside model range
  if(lon.lt.min(model%lonrange(1), model%lonrange(2))) lon = lon + 360
  if(lon.gt.max(model%lonrange(1), model%lonrange(2))) lon = lon - 360

  if (                                                   &
    lat.lt.min(model%latrange(1), model%latrange(2))     &
    .or.lat.gt.max(model%latrange(1), model%latrange(2)) &
    .or.lon.lt.min(model%lonrange(1), model%lonrange(2)) &
    .or.lon.gt.max(model%lonrange(1), model%lonrange(2)) &
    ) then

    if (.not.first_warning .and. warnings%if) then

      write(error_unit,                                                         &
        '(/,"lon, lat", 2f10.3,/, "latrange", 2f10.3,/, "lonrange", 2f10.3,/)') &
        lon , lat, model%latrange, model%lonrange

      call print_warning(                                 &
        "outside lon|lat range "                          &
        // "maybe actual range not specified in nc file:" &
        // basename(model%name)                           &
        )

      first_warning = .true.

    endif

    val = setnan()

    if (.not.(size(model%lon) == 1 .and. size(model%lat) == 1)) then
      return
    endif

  endif

  ilat = minloc(abs(model%lat-lat),1)
  ilon = minloc(abs(model%lon-lon),1)

  ! if linear interpolation was selected find auxikary lat and lon
  if (present(method) .and. method .eq."l") then
    ilon2 = minloc(abs(model%lon-lon), 1, model%lon/=model%lon(ilon))
    ilat2 = minloc(abs(model%lat-lat), 1, model%lat/=model%lat(ilat))

    if (ilon2 == 0 .or. ilat2 == 0) then
      val = setnan()
      return
    endif

    if (lon.gt.model%lon(ilon2).and. lon.gt.model%lon(ilon)) then
      error stop "rare exception!"
    endif

  endif

  ! do not look into data array - get value directly
  if (model%huge) then

    if (present(method) .and. method .eq."l") then
      array_aux_ind(1, :) = [ ilon,  ilat  ]
      array_aux_ind(2, :) = [ ilon,  ilat2 ]
      array_aux_ind(3, :) = [ ilon2, ilat  ]
      array_aux_ind(4, :) = [ ilon2, ilat2 ]
      do j = 1,4
        array_aux(j, 1:2) = [ model%lon(array_aux_ind(j,1)), model%lat(array_aux_ind(j,2))]
        array_aux(j, 3)=  getrawsinglevaluebyindexfrommodel(model,array_aux_ind(j,2),array_aux_ind(j,1),ilevel=ilevel,date=date)
      enddo
      val = bilinear(lon, lat, array_aux)

    else
      val = getrawsinglevaluebyindexfrommodel(model,ilat,ilon,ilevel=ilevel,date=date)
    endif

    ! todo problem z sea sla global
    call get_scale_and_offset(model%ncid, model%names(1), scale_factor, add_offset, status)

    if (status==nf90_noerr) then
      val = val * scale_factor + add_offset
    endif

    if (trim(model%datanames(1)).ne."") then
      val = variable_modifier (val, model%datanames(1))
    endif

    return
  endif

  if (present(method) .and. method .eq."l") then
    array_aux (1, :) = [ model%lon(ilon),  model%lat(ilat),  model%data(ilon,  ilat,  ilevel) ]
    array_aux (2, :) = [ model%lon(ilon),  model%lat(ilat2), model%data(ilon,  ilat2, ilevel) ]
    array_aux (3, :) = [ model%lon(ilon2), model%lat(ilat),  model%data(ilon2, ilat,  ilevel) ]
    array_aux (4, :) = [ model%lon(ilon2), model%lat(ilat2), model%data(ilon2, ilat2, ilevel) ]

    if (ind%moreverbose%l.ne.0) then
      write(moreverbose(ind%moreverbose%l)%unit, '(3f15.4," l")') &
        (array_aux(j,2),array_aux(j,1),array_aux(j,3), j = 1, 4)
      write(moreverbose(ind%moreverbose%l)%unit, '(">")')
    endif

    val = bilinear(lon, lat, array_aux)
    return

  endif

  ! if the last element is the nearest then check if the first is not nearer
  ! i.e. search in 0-357.5E for 359E
  if (ilon .eq. size (model%lon) ) then
    if (abs(model%lon(ilon)-lon).gt.abs(model%lon(1)+360.-lon)) ilon = 1
  endif

  if (ind%moreverbose%n.ne.0) then
    write(moreverbose(ind%moreverbose%n)%unit,  '(3f15.4," n")') &
      model%lat(ilat), model%lon(ilon), model%data(ilon,ilat,ilevel)
    write(moreverbose(ind%moreverbose%n)%unit,  '(">")')
  endif

  val = model%data(ilon, ilat, get_level_index(model,ilevel,success2))

  if (.not.success2) val = setnan()
end subroutine

real(dp) function getrawsinglevaluebyindexfrommodel(model,ilat,ilon, ilevel, date)
  use netcdf, only: nf90_inq_varid, nf90_get_var
  use mod_printing, only: print_warning
  type(file), intent (in) :: model
  integer, intent(in) :: ilat, ilon
  integer, optional, intent(in) :: ilevel
  integer, intent(in), optional :: date(6)
  integer :: varid
  logical :: success, success2

  call nc_error(nf90_inq_varid(model%ncid, model%names(1), varid))

  if (if_variable_use_dimension(model, 1, 4)) then
    call nc_error (                             &
      nf90_get_var(                             &
      model%ncid,                               &
      varid,                                    &
      getrawsinglevaluebyindexfrommodel,        &
      start = [                                 &
      ilon,                                     &
      ilat,                                     &
      get_level_index(model, ilevel, success2), &
      get_time_index(model,date=date)           &
      ]),                                       &
      success = success)
    if(.not.success2) getrawsinglevaluebyindexfrommodel = setnan()

  else
    call nc_error (nf90_get_var(         &
      model%ncid,                        &
      varid,                             &
      getrawsinglevaluebyindexfrommodel, &
      start = [                          &
      ilon,                              &
      ilat,                              &
      get_time_index(model, date=date)   &
      ]),                                &
      success = success)
  endif

  if(.not. success) then
    call print_warning("skipping getrawsinglevaluebyindexfrommodel")
    getrawsinglevaluebyindexfrommodel = setnan()
    return
  endif
endfunction

! =============================================================================
!> Attach full dataname by abbreviation
!!
!! \date 2013-03-21
!! \author M. Rajner
! =============================================================================
function dataname(abbreviation)
  character(len=40) :: dataname
  character(*) :: abbreviation

  select case(abbreviation)
  case("LS")
    dataname = "Land-sea mask"
  case("SP")
    dataname = "Surface pressure"
  case("T")
    dataname = "Surface temperature"
  case("H")
    dataname = "Surface height"
  case("HP")
    dataname = "Model height"
  case("RSP")
    dataname = "Reference surface pressure"
  case("HRSP")
    dataname = "Height of reference surface pressure"
  case("EWT")
    dataname = "Equivalent water thickness"
  case("TP")
    dataname = "Theoretical pressure"
  case("GP")
    dataname = "geopotential or geop. height"
  case default
    dataname = "unknown"
  end select
end function


! =============================================================================
!> If inverted barometer is set then average all pressure above the oceans
!
! working only for regular grid!
! =============================================================================
subroutine conserve_mass(model, landseamask, date, inverted_landsea_mask)
  use mr_utilities, only: d2r
  use mod_cmdline,   only: ind, moreverbose
  use mod_printing
  use mod_polygon
  use mr_mjd

  type (file) :: model, landseamask
  logical, intent(in):: inverted_landsea_mask
  real(dp) ::  val, total_area, ocean_area, valoceanarea, valls
  integer :: ilat, ilon
  integer(2) :: iok
  integer, intent(in), optional :: date(6)
  integer(1) :: ivalls

  total_area   = 0
  ocean_area   = 0
  valoceanarea = 0

  do ilat = 1, size(model%lat)
    do ilon = 1, size(model%lon)
      total_area = total_area + cos(d2r(model%lat(ilat)))

      call get_value(landseamask, model%lat(ilat), model%lon(ilon), valls)
      ivalls=valls

      if (ind%polygon%e.ne.0) then
        call chkgon (model%lon(ilon), model%lat(ilat), polygon(ind%polygon%e), iok)
        if (iok.eq.0) cycle
      endif

      if ((ivalls.eq.0.and..not.inverted_landsea_mask) &
        .or.(ivalls.eq.1 .and. inverted_landsea_mask)) then

        call get_value(model, model%lat(ilat), model%lon(ilon), val)

        ocean_area   = ocean_area + cos(d2r(model%lat(ilat)))
        valoceanarea = valoceanarea + val * cos(d2r(model%lat(ilat)))
        model%data(ilon,ilat,1) = -9999

      endif
    enddo
  enddo

  where (model%data.eq.-9999)
    model%data=valoceanarea/ ocean_area
  end where

  if (ind%moreverbose%o.ne.0) then
    if (output%header)  then
      if (present(date)) then
        write (moreverbose(ind%moreverbose%o)%unit,'(a12,1x,a14)', advance='no') "mjd", "date"
      endif
      write (moreverbose(ind%moreverbose%o)%unit,'(2a12)') "ocean[%]", "mean_val"
    endif

    if (present(date)) then
      write (moreverbose(ind%moreverbose%o)%unit,'(f12.3,1x, i4.2,5i2.2)', advance='no') mjd(date), date
    endif

    write (moreverbose(ind%moreverbose%o)%unit,'(f12.3,f12.3)') &
      ocean_area/total_area*100.,                               &
      valoceanarea/ocean_area
  endif
end subroutine

! =============================================================================
!> Mean pressure all over the model area
!
! working only for regular grid!
! =============================================================================
subroutine total_mass(model, date)
  use mr_utilities, only: d2r
  use mod_cmdline,  only: ind, moreverbose
  use mod_printing
  use mr_mjd
  type (file) :: model
  real(dp) ::  val, valarea, totalarea
  integer :: ilat, ilon
  integer, intent(in),optional :: date(6)


  totalarea = 0
  valarea   = 0

  do ilat = 1, size(model%lat)
    do ilon =1,size(model%lon)
      totalarea = totalarea + cos(d2r(model%lat(ilat)))
      call get_value(model, model%lat(ilat), model%lon(ilon), val)
      valarea = valarea + val * cos(d2r(model%lat(ilat)))
    enddo
  enddo

  if (output%header)  then

    if (present(date)) then
      write (moreverbose(ind%moreverbose%t)%unit, '(a12,1x,a14)', advance='no') &
        "mjd",  "date"
    endif

    write (moreverbose(ind%moreverbose%t)%unit, '(a12)') "mean_val"

  endif

  if (present(date)) then
    write (moreverbose(ind%moreverbose%t)%unit, '(f12.3,1x,i4.2,5i2.2)', advance='no') &
      mjd(date), date
  endif

  write (moreverbose(ind%moreverbose%t)%unit, '(f12.3)') &
    valarea/totalarea
end subroutine

! =============================================================================
! =============================================================================
subroutine parse_level(cmd_line_entry)
  use mod_cmdline,  only: cmd_line_arg
  use mod_printing, only: print_warning, form, log

  type(cmd_line_arg), optional :: cmd_line_entry
  integer :: i

  if (present(cmd_line_entry)) then

    if (allocated(level%level)) then
      call print_warning ("repeated", more="-J")
      return
    endif

    if (cmd_line_entry%field(1)%subfield(1)%name.eq."m") then
      level%all=.true.
    else
      allocate (level%level(size(cmd_line_entry%field)))

      do i =1,  size(level%level)
        read(cmd_line_entry%field(i)%subfield(1)%name, *) level%level(i)
      enddo

    endif

    write(log%unit, form%i2, advance="no") "level pressure:"
    if (allocated(level%level)) then
      write (log%unit, '(*(i4))') level%level
    else if (level%all) then
      write (log%unit, '(a)') "all"
    endif
  else
    level%all=.true.
  endif

end subroutine

! =============================================================================
! =============================================================================
subroutine customfile_value(what, sp, t, hp, sh, gp, vsh, vt, level, val, rho)
  use mod_printing, only: print_warning
    use mr_atmosphere, only: standard_temperature, virtual_temperature, standard_pressure
  use mr_constants, only: R_air

  character(*), intent(in) :: what
  real(dp), intent(in), optional :: sp,t, hp, sh, gp, vsh, vt
  integer,  intent(in), optional :: level
  logical,  intent(in), optional :: rho
  real(dp), intent(out) :: val
  real(dp):: t_aux, vt_aux

  select case (what)

  case ("TPF+H")
    t_aux  = virtual_temperature(t,sh)
    vt_aux = virtual_temperature(vt,vsh)

  case default
    t_aux  = t
    vt_aux = vt

  end select

  select case (what)
  case ("TP","TP_TS")
    val=                               &
      standard_pressure (              &
      gp,                              &
      use_standard_temperature=.true., &
      method = "full"                  &
      )

  case ("TPF", "TPF+H")
    val=                               &
      standard_pressure (              &
      gp,                              &
      p_zero = sp,                     &
      temperature = t_aux,             &
      use_standard_temperature=.true., &
      h_zero = hp,                     &
      method = "full"                  &
      )

  case ("RHO")
    val= 100.*level/(R_air * vt)

  case default
    call print_warning(                                            &
      "nothing I know for @custom file specification"//trim(what), &
      error=.true.)
  endselect

  if (present(rho).and.what.ne."RHO") then
    if(rho) then
      select case(what)
      case("TP_TS")
        vt_aux=standard_temperature(gp, t_zero=t)
      end select

      val = val/(R_air*vt_aux)
    endif
  endif

end subroutine

! =============================================================================
! =============================================================================
function if_variable_use_dimension(model, ivarname, idimname)
  use netcdf

  logical :: if_variable_use_dimension

  type(file), intent(in) :: model
  integer, intent(in) :: ivarname, idimname
  integer :: i, dimids(4), status

  dimids = 0

  call nc_error(nf90_inq_varid(model%ncid, model%names(ivarname),i))
  call nc_error(nf90_inquire_variable(model%ncid,i,dimids=dimids))
  status = nf90_inq_varid(model%ncid, model%names(idimname), i)

  if(any(dimids == i) .and. status .eq. nf90_noerr) then
    if_variable_use_dimension = .true.
  else
    if_variable_use_dimension = .false.
  endif
end function

end module
