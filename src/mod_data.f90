! ===================================================
!> This modele gives routines to read, and write data
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
  use mod_constants, only: dp

  implicit none
  type file
    character(90) :: name 
    ! varname,lonname,latname,levelname,timename
    character(len=50) :: names(5) = ["z", "lon", "lat", "level", "time"]
    character(len=100) :: datanames(5)=" "

    character(len=15) :: dataname

    ! if file was determined
    logical :: if =.false.

    real(dp), allocatable, dimension(:) :: lat, lon, time
    integer, allocatable, dimension(:) :: level
    integer, allocatable, dimension(:,:) :: date

    real (dp), dimension(2) :: latrange, lonrange

    logical :: if_constant_value
    real(dp):: constant_value

    real(dp), allocatable, dimension(:,:,:) :: data

    integer :: ncid
    logical :: huge=.false.
    logical :: autoload=.false.
    logical :: exist=.false.
    character(10) :: autoloadname
  end type
  type(file), allocatable, dimension(:) :: model 

  private :: dataname

  type level_info
    integer, allocatable, dimension(:) :: level
    real(dp), allocatable, dimension(:) :: height, temperature
    logical :: all=.false.
  end type
  type(level_info) :: level

contains
! =============================================================================
!> This subroutine parse model information from command line entry
!!
!! \author M. Rajner
!! \date 2013.05.20
! =============================================================================
subroutine parse_model (cmd_line_entry)
  use mod_cmdline
  use mod_printing
  use mod_utilities, only: file_exists, is_numeric
  type(cmd_line_arg)  :: cmd_line_entry
  integer :: i, j

  if (allocated(model)) then
    call print_warning ("repeated", more="-F")
    return
  endif

  allocate(model(size(cmd_line_entry%field)))

  do i = 1, size(cmd_line_entry%field)
    model(i)%exist=.true.
    write(log%unit, form_62), trim(cmd_line_entry%field(i)%full)
    model(i)%name = trim(cmd_line_entry%field(i)%subfield(1)%name)
    model(i)%dataname = trim(cmd_line_entry%field(i)%subfield(1)%dataname)
    if (model(i)%dataname.eq."") then
      model(i)%dataname="NN"
    else
      if ( &
          index(model(i)%dataname,"!").ne.0 &
      ) then
        model(i)%huge=.true.
        model(i)%dataname = model(i)%dataname (1: index(model(i)%dataname,"!")-1)
        write(log%unit, form%i2) "!:huge"
      endif
    endif

    if (model(i)%name.eq."") then
      if (i.gt.1) then
        model(i)%name=model(i-1)%name
      else
        call print_warning ("model", error=.true.)
      endif
    endif

    do j = 2, size(cmd_line_entry%field(i)%subfield)
      if (cmd_line_entry%field(i)%subfield(j)%dataname.ne."") then
        model(i)%datanames(j-1)=cmd_line_entry%field(i)%subfield(j)%dataname
      endif
    enddo
    if (file_exists (model(i)%name) ) then
      do j =2, size (cmd_line_entry%field(i)%subfield)
        if (cmd_line_entry%field(i)%subfield(j)%name.ne."") then
          model(i)%names(j-1)=cmd_line_entry%field(i)%subfield(j)%name
        endif
      enddo

      write (log%unit, form%i3,advance='no'), &
          trim (dataname(model(i)%dataname)), &
          "("//trim(model(i)%dataname)//")"
      write(log%unit, '(5(a,x))', advance="no"), (trim(model(i)%names(j)), j=1,5)
      model(i)%if=.true.
      write(log%unit, *) 
      if (model(i)%dataname.ne."ascii") then
        call read_netCDF(model(i))
      endif

      ! listing in log
      model(i)%constant_value=     & 
          variable_modifier(        & 
          model(i)%constant_value, & 
          model(i)%datanames(1),   & 
          verbose=.true.,          & 
          list_only=.true.         & 
          )
    else if (is_numeric(model(i)%name)) then
      model(i)%if_constant_value=.true.
      write (log%unit, form%i3),              & 
          trim (dataname(model(i)%dataname)), & 
          "("//trim(model(i)%dataname)//")"
      read (model(i)%name, * ) model(i)%constant_value
      write(log%unit, '(' // form%t3 // "a," // output%form // ")") &
          'constant value was set:   ', model(i)%constant_value
      if (trim(model(i)%datanames(1)).ne."") then
        model(i)%constant_value= &
            variable_modifier( &
            model(i)%constant_value, model(i)%datanames(1),verbose=.true.)
      endif
      write(log%unit, '(' // form%t3 // "a," // output%form // ")") &
          'constant value was re-set:', model(i)%constant_value
      model(i)%lonrange=[  0,360]
      model(i)%latrange=[-90, 90]
    else
      !check autoload
      call model_aliases(model(i), dryrun=.true.)
      if (.not.model(i)%if.and. .not.any(["TP","TPF"].eq.model(i)%dataname)) then
        call print_warning ("model", more=trim(model(i)%name)//" : file do not exist", error=.false.)
      endif
    endif
  enddo
end subroutine

! =============================================================================
! =============================================================================
subroutine model_aliases(model, dryrun, year, month)
  use mod_printing
  use mod_utilities, only: file_exists
  type(file) :: model
  logical, intent(in), optional :: dryrun
  integer, intent(in), optional :: year, month
  character(150) :: prefix
  integer :: year_, month_

  if (present(year)) then
    year_=year
    if (present(month)) then
      month_=month
    endif
  else
    year_=9999
    month_=99
  endif

  if(.not. model%autoload) model%autoloadname=model%name
  model%if=.true.
  select case (model%autoloadname)
  case ("NCEP", "NCEP2", "NCEP1")
    if (.not.model%autoload) model%autoload=.true.
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
      if (present(dryrun) .and. dryrun) then
        if (model%datanames(1).eq."") then
          model%datanames(1) = "gh2h"
        else
          model%datanames(1) = "gh2h@"// trim(model%datanames(1))
        endif
      endif
    case ("VT")
      model%names(1)="air"
      write(model%name,'(a,a,i4,a)') trim(prefix),"air.",year_,".nc"
    case ("T")
      if (model%autoloadname.eq."NCEP1") then
        model%names(1)="air"
        write(model%name,'(a,a,i4,a)') trim(prefix),"air.sig995.",year_,".nc"
      else
        model%names(1)="temp"
        call print_warning ("not yet NCEP@T", error=.true.)
      endif
    case ("HP","H")
      model%names(1)="hgt"
      write(model%name,'(a,a,i4,a)') trim(prefix),"hgt.sfc.nc"
      model%autoload=.false.
      if (present(dryrun) .and. dryrun) then
        if (model%datanames(1).eq."") then
          model%datanames(1) = "gh2h"
        else
          model%datanames(1) = "gh2h@"// trim(model%datanames(1))
        endif
      endif
    case ("LS")
      model%names(1:3)=["z", "x", "y"]
      model%name="/home/mrajner/dat/landsea/ncep_closed_seas_caspian.nc"
      model%autoload=.false.
    case default
      model%autoload=.false.
      model%if=.false.
      return
    endselect
  case ("ERA")
    if (.not.model%autoload) model%autoload=.true.
    prefix="/home/mrajner/dat/erainterim/"
    select case (model%dataname)
    case ("SP")
      model%names(1)="sp"
      write(model%name,'(a,a,i4,a)') trim(prefix),"sp.",year_,".nc"
    case ("GP")
      write(model%name,'(a,a,i4,i2.2,a)') trim(prefix),"gp_l.",year_,month_,".nc"
      if (present(dryrun) .and. dryrun) then
        if (model%datanames(1).ne."") then
          model%datanames(1) = "gp2h@"// trim(model%datanames(1))
        else
          model%datanames(1) = "gp2h"
        endif
      endif
    case ("VT")
      model%names(1)="t"
      write(model%name,'(a,a,i4,i2.2,a)') trim(prefix),"t_l.",year_,month_,".nc"
    case ("T")
      model%names(1)="v2t"
      write(model%name,'(a,a,i4,a)') trim(prefix),"t.",year_,".nc"
    case ("HP","H")
      model%names(1)="z"
      write(model%name,'(a,a,i4,a)') trim(prefix),"gp.nc"
      model%autoload=.false.
      if (present(dryrun) .and. dryrun) then
        if (model%datanames(1).ne."") then
          model%datanames(1) = "gp2h@"// trim(model%datanames(1))
        else
          model%datanames(1) = "gp2h"
        endif
      endif
    case ("LS")
      model%names(1:3)=["z", "x", "y"]
      model%name="/home/mrajner/dat/landsea/era_closed_seas_caspian.nc"
      model%autoload=.false.
    case default
      model%autoload=.false.
      model%if=.false.
    endselect
  case ("VIENNA")
    prefix="/home/mrajner/dat/refpres/"
    select case (model%dataname)
    case ("RSP")
      model%names(1)="rp"
      write(model%name,'(a,a)') trim(prefix),"refpres0p5.nc"
    case ("H", "HRSP")
      model%names(1)="height"
      write(model%name,'(a,a)') trim(prefix),"refpres0p5.nc"
    case default
      model%if=.false.
    endselect
  case ("ETOPO")
    prefix="/home/mrajner/dat/etopo/"
    select case (model%dataname)
    case ("LS")
      model%names(1:3)=["z", "x", "y"]
      write(model%name,'(a,a)') trim(prefix),"ETOPO_LANDSEA.grd"
    case ("H")
      model%names(1)="z"
      write(model%name,'(a,a)') trim(prefix),"ETOPO_zero_ocean.grd"
    case default
      model%if=.false.
    endselect
  case ("LANDSEA")
    write(*, *) "use ERA or NCEP @LS!, or ETOPO@LS (not corrected for closed seas)"
    call exit(1)
    prefix="/home/mrajner/dat/landsea/"
    select case (model%dataname)
    case ("LS")
      model%names(1:3)=["z", "x", "y"]
      write(model%name,'(a,a)') trim(prefix), "LANDSEA_closed_seas.grd"
    case default
      model%if=.false.
    endselect
  case default
    model%if=.false.
    model%autoload=.false.
  endselect
  ! listing in log
  model%constant_value=     & 
      variable_modifier(        & 
      model%constant_value, & 
      model%datanames(1),   & 
      verbose=.true.,          & 
      list_only=.true.         & 
      )
  if (model%if.and..not.model%autoload) call read_netCDF(model)

  if(present(dryrun) .and. dryrun) return

  if (.not.file_exists(model%name)) then
    call print_warning ("model", more=trim(model%name)//" : file do not exist", error=.false.)
    model%exist=.false.
    return
  else
    model%exist=.true.
  endif
  call read_netCDF(model, force=.true., print=.not.log%sparse)
end subroutine

! =============================================================================
! =============================================================================
function variable_modifier (val, modifier, verbose, list_only)
    use mod_atmosphere, only: geop2geom
    use mod_constants,  only: earth
    use mod_utilities,  only: ntokens
    use mod_printing,   only: print_warning, form, log, output
    real(dp) :: variable_modifier
    real(dp), intent(in) :: val
    character(*), intent(in) :: modifier
    character(20) :: key, keyval
    character(100) :: modifier_
    real(dp) :: numerickeyval
    integer :: i
    logical, optional, intent(in) :: verbose, list_only


    variable_modifier = val
    modifier_=modifier
    do i = 1, ntokens(modifier_,"@")
      keyval=" "
      if (ntokens(modifier_,"@").eq.1) then
        key = modifier_
      else
        key = modifier_(1: index(modifier_, "@")-1)
      endif
      if (index(key,"=").gt.0)  then
        keyval = trim(key(index(key,"=")+1:))
        key    = trim(key(1:index(key,"=")-1))
      endif
      if (present(verbose).and.verbose) &
          write(log%unit, "("//form%t3//"a,a6,a7$)" ) &
          "var modifier: ", trim(key), trim(keyval)
      select case (key)
      case ("gh2h") ! g2h is obsolete
        variable_modifier=geop2geom(variable_modifier)
      ! case ("gp2gh")
        ! variable_modifier=variable_modifier/earth%gravity%mean
      case ("gp2h")
        variable_modifier=geop2geom(variable_modifier)/earth%gravity%mean
      case ("nan")
        read(keyval,*) numerickeyval
        if (isnan(variable_modifier)) variable_modifier=numerickeyval 
      case ("scale")
        read(keyval,*) numerickeyval
        variable_modifier=numerickeyval*variable_modifier
      case ("invscale")
        read(keyval,*) numerickeyval
        variable_modifier=1./numerickeyval*variable_modifier
      case ("offset")
        read(keyval,*) numerickeyval
        variable_modifier=numerickeyval+variable_modifier
      case default
        call print_warning ("variable modifier not found" // key, error=.true.)
      endselect
      if (.not.present(list_only)) then
        if(present(verbose).and.verbose) &
            write (log%unit, '('// output%form // ')') variable_modifier
      else
        if(present(verbose).and.verbose) &
            write (log%unit, *)
      endif
      modifier_ = modifier_(index(modifier_, "@")+1:)
    enddo
end function

! =============================================================================
!> Read netCDF file into memory
! =============================================================================
subroutine read_netCDF (model, print, force)
  use netcdf
  use mod_printing
  use mod_cmdline,   only: ind
  use mod_utilities, only: file_exists
  type (file) :: model
  logical, optional, intent(in) :: print, force
  integer :: i 

  if (present(force) .and. force) then
    if (allocated(model%data))   deallocate(model%data)
    if (allocated(model%lat))    deallocate(model%lat)
    if (allocated(model%lon))    deallocate(model%lon)
    if (allocated(model%date))   deallocate(model%date)
    if (allocated(model%level))  deallocate(model%level)
    if (allocated(model%time))   deallocate(model%time)
  endif
  if (.not.file_exists(model%name)) &
      call print_warning("file not exist " // trim (model%name), &
      error=.true.) 

  if (.not. (present(print).and. .not. print)) then
    write (log%unit, form%i3) "Opening file:", trim(basename(trim(model%name)))
  endif
  call check (nf90_open (model%name, nf90_nowrite, model%ncid))

  do i = 2,5
    call get_dimension (model, i, print=print)
  enddo
  if (size (model%time).ge.1) call nctime2date (model, print=print)
end subroutine

! =============================================================================
!> \brief Get dimension, allocate memory and fill with values
!! \author Marcin Rajner
!! \date 2013.05.24
! =============================================================================
subroutine get_dimension (model, i, print)
  use netcdf
  use mod_printing
  use mod_utilities, only: countsubstring
  type(file) :: model
  integer :: dimid, varid 
  integer, intent(in) :: i
  integer :: length, status
  logical, optional :: print

  if (.not. (present(print).and..not.print))then
    write (log%unit, form%i4, advance='no') "Getting dim:",trim(model%names(i)), ".."
  endif
  status = nf90_inq_dimid(model%ncid,model%names(i), dimid)
  if (status /=nf90_noerr) then
    if(model%names(i).eq."lon") then
      model%names(i)="longitude"
      if (.not.(present(print).and..not.print)) then
        write(log%unit, '(a)', advance='no') "longitude"
      endif
      status = nf90_inq_dimid(model%ncid,"longitude", dimid)
    else if(model%names(i).eq."lat") then
      model%names(i)="latitude"
      if (.not. (present(print).and..not.print))then
        write(log%unit, '(a)', advance='no') "latitude"
      endif
    endif
    status = nf90_inq_dimid(model%ncid,model%names(i), dimid)
  endif
  if(status /= nf90_noerr.and.any(i.eq.[2,3])) then 
    call print_warning("key variable not found: " &
        // trim(model%names(i)), error=.true.)
  elseif(status /= nf90_noerr) then 
    if (.not. (present(print).and..not.print))then
      write (log%unit, '(a6,1x,a)') &
          trim(model%names(i)),"not found, allocating (1)..." 
      call nc_info(model)
    endif
    length=1
  else
    if (.not. (present(print).and..not.print))then
      write (log%unit, '(a6,1x,a)') "ok"
    endif
    call check(nf90_inquire_dimension(model%ncid, dimid, len=length))
    call check(nf90_inq_varid(model%ncid, model%names(i), varid))
  endif
  if (i.eq.3 ) then
    allocate(model%lat(length))
    call check(nf90_get_var (model%ncid, varid, model%lat))
    status = nf90_get_att(model%ncid, varid, &
        "actual_range", model%latrange) 
    if (status /= nf90_noerr ) then
      model%latrange =[model%lat(1), model%lat(size(model%lat)) ]
    endif
  else if (i.eq.2 ) then
    allocate(model%lon(length))
    call check(nf90_get_var (model%ncid,  varid, model%lon))
    status = nf90_get_att ( model%ncid, varid, &
        "actual_range", model%lonrange) 
    if (status /= nf90_noerr ) model%lonrange &
        =[model%lon(1), model%lon(size(model%lon)) ]
    where (model%lonrange.ge.357.5) 
      model%lonrange=360
    end where
  else if (i.eq.4 ) then
    allocate(model%level(length))
    status = nf90_inq_dimid(model%ncid,model%names(i), dimid)
    if (status.ne.nf90_noerr) then 
      model%level=0
      return
    else
      status = nf90_get_var (model%ncid, varid, model%level)
    endif
  elseif (i.eq.5 ) then
    allocate(model%time (length) )
    status = nf90_get_var (model%ncid, varid, model%time)
  endif
end subroutine

! =============================================================================
!> Change time in netcdf to dates
!!
!! \author M. Rajner
!! \date 2013-03-04
! =============================================================================
subroutine nctime2date (model, print)
  use netcdf
  use mod_printing
  use mod_constants, only: dp
  use mod_mjd,      only: mjd, invmjd
  type (file)        :: model
  real(dp)           :: mjd_start, mjd_
  integer            :: varid,i, ind(2), date (6), status
  character (len=66) :: dummy
  logical, optional :: print

  status = nf90_inq_varid (model%ncid, model%names(5), varid)
  if (status /=nf90_noerr) return
  call check (nf90_get_att (model%ncid, varid, "units", dummy))

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
    do i=1,2
      dummy = dummy(index(dummy, ' ')+1:)
    enddo
    do 
      ind=[index(dummy,'-'), index(dummy,':')]
      do i=1,2
        if (ind(i).ne.0) dummy = dummy(1:ind(i)-1)//" "//dummy(ind(i)+i:)
      enddo
      if (index(dummy,'-').eq.0 .and. index(dummy,':').eq.0) exit
    enddo
    read(dummy,*) date
    mjd_start = mjd (date)
  else if (index(dummy,"Days since").eq.1) then
    ! this option for gldas from grace tellus
    do i=1,2
      dummy = dummy(index(dummy, ' ')+1:)
    enddo
    do 
      ind=[index(dummy,'-'), index(dummy,':')]
      do i=1,2
        if (ind(i).ne.0) dummy = dummy(1:ind(i)-1)//" "//dummy(ind(i)+i:)
      enddo
      if (index(dummy,'-').eq.0 .and. index(dummy,':').eq.0) exit
    enddo
    dummy = dummy//" 0 0 0"
    read(dummy,*) date(1:3)
    mjd_start = mjd (date)
    model%time = model%time *24
  else
    write (log%unit, form%i4 ) "unknown time begining"
  endif
  do i = 1, size(model%time)
    mjd_= model%time(i) / 24 + mjd_start 
    call invmjd(mjd_,date)
    model%date(i,:) = date
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
  if (.not.present(date).or. size(model%date(:,1)).le.1) then
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

  integer :: get_level_index
  type (file), intent(in) :: model
  integer, intent(in), optional :: level
  logical, intent(out), optional :: sucess
  integer :: i

  get_level_index=1
  if (.not.present(level).or.size(model%level).le.1) then
    get_level_index=1
    return
  endif
  do i = 1, size(model%level)
    if (model%level(i).eq.level) then
      get_level_index=i
      if (present(sucess)) sucess=.true.
      return
    endif
  enddo 
  if (present(sucess)) sucess=.false.
  call print_warning("level not found")
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

  call check (nf90_inquire(model%ncid, ndims, nvars))
  allocate(varids(nvars))
  allocate(name(nvars))

  call check (nf90_inq_varids(model%ncid, nvars, varids))
  do i=1, nvars
    call check(nf90_inquire_variable(model%ncid, varids(i), name(i)))
  enddo
  write(log%unit, form%i5 ) (trim(name(i))//",", i =1,nvars)
end subroutine

! =============================================================================
!> \brief Get variable from netCDF file for specified variables
! =============================================================================
subroutine get_variable(model, date, print, level)
  use netcdf
  use mod_printing
  type (file), intent(inout) :: model
  integer, optional, intent(in), dimension(6) ::date
  integer :: varid, status
  integer :: start(3)
  integer :: startv(4)
  integer :: index_time, i, j, k
  real (dp) :: scale_factor, add_offset
  logical, optional :: print
  integer, optional :: level
  character (20) :: aux

  if ( &
      model%huge &
      .or.model%if_constant_value &
      .or..not. model%if) return

  index_time = 0
  status = nf90_inq_varid (model%ncid, model%names(1), varid)
  if (status /= nf90_noerr) then
    call nc_info(model)
    call print_warning( &
        "variable not found: " // trim(model%names(1)), &
        error=.true.)
  endif

  if (allocated(model%data)) deallocate(model%data)
  allocate ( &
      model%data ( &
      size(model%lon), &
      size(model%lat), &
      size(model%level) &
      ) &
      )

  if (size(date).gt.0 .and. present(date)) then                       
    index_time = get_time_index(model, date)
    if (index_time.eq.0) then
      if (.not. (present(print).and..not.print))then
        if (.not.log%sparse) then
          write(aux, '(i4.4,5i2.2)') date
          call print_warning("cannot find date: "// aux & 
              // "var: " // trim(model%names(1)) // " in file: " // trim(model%name))
        endif
      endif
      model%data= sqrt(-1.)
      return
    endif
  else
    index_time = 1
  endif

  if(present(level)) stop "XXXXXXX"
  select case (model%dataname)
  case ("VT","GP")
    startv = [1,1,1,index_time]
    call check (nf90_get_var ( & 
        ncid=model%ncid,         & 
        varid=varid,             & 
        values=model%data,       & 
        start=startv)             & 
        )

  case default
    start = [1,1,index_time]
    call check (nf90_get_var ( & 
        ncid=model%ncid,         & 
        varid=varid,             & 
        values=model%data,       & 
        start=start)             & 
        )
  end select

  call get_scale_and_offset (model%ncid, model%names(1), scale_factor, add_offset, status)
  model%data = model%data*scale_factor + add_offset
  if (trim(model%datanames(1)).ne."") then
    do i =1, size(model%data,1)
      do j =1, size(model%data,2)
        do k =1, size(model%data,3)
          model%data(i,j,k) = variable_modifier (model%data(i,j,k), model%datanames(1))
        enddo
      enddo
    enddo
  endif
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

  call check(nf90_inq_varid(ncid, varname, varid))
  status = nf90_get_att (ncid, varid, "scale_factor", scale_factor) 
  if (status /=nf90_noerr) scale_factor=1

  status = nf90_get_att (ncid, varid, "add_offset", add_offset) 
  if (status /=nf90_noerr) add_offset=0
end subroutine

!
! =============================================================================
!> Check the return code from netCDF manipulation
!!
!! \author From netcdf website \cite netcdf
!! \date 2013-03-04
! =============================================================================
subroutine check(status, success)
  use netcdf
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
    stop "XXX"
    return
  else
    if (present(success)) then
      success=.true.
    endif
    return
  end if
end subroutine check  

! =============================================================================
!> \brief Returns the value from model file
!! 
!! The ilustration explain optional \c method argument
!! \latexonly
!! \begin{center}
!!  \tikzsetfigurename{interpolation_ilustration}
!!  \input{/home/mrajner/src/grat/doc/rysunki/interpolation_ilustration}\\
!! \end{center}
!! \endlatexonly
!! \image html /home/mrajner/src/grat/doc/rysunki/interpolation_ilustration.svg
!!
!! lat and lon in decimal degree
! =============================================================================
subroutine get_value(model, lat, lon, val, level, method, date)
  use mod_constants, only: dp 
  use mod_cmdline
  use mod_utilities, only: r2d
  use netcdf
  use :: mod_printing, only: print_warning

  type(file), intent (in) :: model
  real(dp)  &
      !, intent (in) &
  :: lat, lon
  real(dp), intent(out) ::  val 
  character(1), optional, intent(in) :: method
  integer, optional, intent(in) :: level
  integer :: j, ilevel = 1 
  integer  :: ilon, ilat, ilon2, ilat2, varid, status
  real(dp), dimension(4,3) :: array_aux 
  real(dp) :: scale_factor, add_offset
  integer, intent(in), optional::date(6)
  logical :: success, success2

  if (model%if_constant_value) then
    val = model%constant_value
    return
  end if

  if (.not.model%exist.or..not.model%if) then
    val = sqrt(-1.)
    return
  endif

  if (present(level)) ilevel=level

  if (model%autoloadname.eq."ETOPO") then
    if (lat.lt.-89.9999) lat=-89.99999
  endif

  ! check if inside model range
  if(lon.lt.min(model%lonrange(1), model%lonrange(2))) lon = lon + 360 
  if(lon.gt.max(model%lonrange(1), model%lonrange(2))) lon = lon - 360
  if (  lat.lt.min(model%latrange(1), model%latrange(2))  &
      .or.lat.gt.max(model%latrange(1), model%latrange(2)) &
      .or.lon.lt.min(model%lonrange(1), model%lonrange(2)) &
      .or.lon.gt.max(model%lonrange(1), model%lonrange(2)) &
      ) then
    val = sqrt(-1.)
    return
  endif

  ilat = minloc(abs(model%lat-lat),1)
  ilon = minloc(abs(model%lon-lon),1)

  ! do not look into data array - get value directly 
  if (model%huge) then
    status=nf90_inq_varid (model%ncid, model%names(4), varid)
    call check (nf90_inq_varid (model%ncid, model%names(1), varid))
    if (status.eq.nf90_noerr) then
      call check (& 
          nf90_get_var(              & 
          model%ncid, varid,                   & 
          val,                                 & 
          start = [                            & 
          ilon,                                & 
          ilat,                                & 
          get_level_index(model, ilevel, success2),& 
          get_time_index(model,date=date)           & 
          ]),                                  & 
          success=success)
      if(.not.success2) val =sqrt(-1.)
    else
      call check (nf90_get_var(         & 
          model%ncid, varid, val,       & 
          start = [ilon,ilat,           & 
          get_time_index(model,date=date)]), & 
          success=success)
    endif
    if(.not. success) then
      call print_warning ("skipping get_value")
      val = sqrt(-1.)
      return
    endif

    call get_scale_and_offset(model%ncid, model%names(1), scale_factor, add_offset,status)
    if (status==nf90_noerr) val = val *scale_factor + add_offset
    if (trim(model%datanames(1)).ne."") then
      val = variable_modifier (val, model%datanames(1))
    endif
    return
  endif

  if (present(method) .and. method .eq. "l" ) then
    ilon2 = minloc(abs(model%lon-lon),1, model%lon/=model%lon(ilon))
    ilat2 = minloc(abs(model%lat-lat),1, model%lat/=model%lat(ilat))

    if (lon.gt.model%lon(ilon2).and. lon.gt.model%lon(ilon)) then
    else
      array_aux (1, :) = [ model%lon(ilon), model%lat(ilat), model%data(ilon, ilat, ilevel) ]
      array_aux (2, :) = [ model%lon(ilon), model%lat(ilat2), model%data(ilon, ilat2, ilevel) ]
      array_aux (3, :) = [ model%lon(ilon2), model%lat(ilat), model%data(ilon2, ilat, ilevel) ]
      array_aux (4, :) = [ model%lon(ilon2), model%lat(ilat2), model%data(ilon2, ilat2, ilevel) ]

      if (ind%moreverbose%l.ne.0) then
        write(moreverbose(ind%moreverbose%l)%unit, '(3f15.4," l")'), &
            (array_aux(j,2),array_aux(j,1),array_aux(j,3), j = 1, 4)
        write(moreverbose(ind%moreverbose%l)%unit, '(">")')
      endif
      val = bilinear ( lon, lat, array_aux )
      return
    endif
  endif

  ! if the last element is the neares then check if the firt is not nearer
  ! i.e. search in 0-357.5E for 359E
  if (ilon .eq. size (model%lon) ) then
    if (abs(model%lon(ilon)-lon).gt.abs(model%lon(1)+360.-lon)) ilon = 1
  endif
  if (ind%moreverbose%n.ne.0) then
    write(moreverbose(ind%moreverbose%n)%unit,  '(3f15.4," n")'), &
        model%lat(ilat), model%lon(ilon), model%data(ilon,ilat,ilevel)
    write(moreverbose(ind%moreverbose%n)%unit,  '(">")')
  endif
  val = model%data(ilon, ilat, get_level_index(model,ilevel))

end subroutine 

! =============================================================================
!> Performs bilinear interpolation
!! \author Marcin Rajner
!! \date 2013-05-07
! =============================================================================
function bilinear (x, y, aux )
  use mod_constants, only: dp
  real(dp) :: bilinear
  real(dp) :: x, y, aux(4,3) 
  real(dp) :: a, b, c
  a  = ( x - aux(1,1) ) /(aux(4,1)-aux(1,1))
  b = a * (aux(3,3)  - aux(1,3)) + aux(1,3) 
  c = a * (aux(4,3)  - aux(2,3)) + aux(2,3)
  bilinear = (y-aux(1,2))/(aux(4,2) -aux(1,2)) * (c-b) + b
end function

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

!! =============================================================================
!!> Put netCDF COARDS compliant 
!!!
!!! for GMT drawing
!! =============================================================================
!subroutine put_grd (model, time, level, filename_opt )
!  use netcdf
!  use mod_cmdline, only : file, form_separator, log
!  type (file) :: model
!  integer  :: time, level, ncid
!  integer :: londimid, latdimid, dimids(2), varid, latvarid,lonvarid
!  character (*), intent (in), optional :: filename_opt
!  character(60) :: filename = "tmp_.grd"
!
!  if (present(filename_opt)) filename = filename_opt
!  write(log%unit, form_separator)
!  call check ( nf90_create ( filename, 0, ncid ))
!
!  ! Define the dimensions. NetCDF will hand back an ID for each. 
!  call check( nf90_def_dim(ncid, "lon", size(model%lon), londimid) )
!  call check( nf90_def_dim(ncid, "lat", size(model%lat), latdimid) )
!
!  dimids =  (/ londimid, latdimid /)
!
!  call check( nf90_def_var(ncid, "lat", NF90_float, latdimid, latvarid) )
!  call check( nf90_def_var(ncid, "lon", NF90_float, londimid, lonvarid) )
!  call check( nf90_def_var(ncid, "data", NF90_float, dimids, varid) )
!  call check( nf90_enddef(ncid) )
!
!  call check( nf90_put_var(ncid, latvarid, model%lat ))
!  call check( nf90_put_var(ncid, lonvarid, model%lon ))
!  call check( nf90_put_var(ncid, varid, model%data(:,:,1)) )
!  call check( nf90_close(ncid) )
!end subroutine
!

! =============================================================================
!> If inverted barometer is set then averaga all pressure above the oceans
!
! working only for regular grid!
! =============================================================================
subroutine conserve_mass (model, landseamask, date, inverted_landsea_mask)
  use mod_utilities, only: d2r
  use mod_cmdline,   only: ind, moreverbose
  use mod_printing
  use mod_polygon
  use mod_mjd
  type (file) :: model, landseamask
  logical, intent(in):: inverted_landsea_mask
  real(dp) ::  val, valls, total_area, ocean_area, valoceanarea
  integer :: ilat, ilon 
  integer(2) :: iok
  integer, intent(in), optional :: date(6)

  total_area = 0
  ocean_area = 0
  valoceanarea    = 0

  do ilat = 1, size(model%lat)
    do ilon =1,size(model%lon)
      total_area = total_area + cos(d2r(model%lat(ilat)))
      call get_value(landseamask, model%lat(ilat), model%lon(ilon), valls)
      if (ind%polygon%e.ne.0) then
        call chkgon ( model%lon(ilon), model%lat(ilat), polygon(ind%polygon%e), iok)
        if (iok.eq.0) cycle
      endif
      if ((valls.eq.0.and..not.inverted_landsea_mask) &
          .or.(valls.eq.1 .and. inverted_landsea_mask)) then
        call get_value(model, model%lat(ilat), model%lon(ilon), val)
        ocean_area = ocean_area + cos(d2r(model%lat(ilat)))
        valoceanarea    = valoceanarea + val * cos(d2r(model%lat(ilat)))
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
        write (moreverbose(ind%moreverbose%o)%unit,'(a12,x,a14)', advance='no'), "mjd", "date"
      endif
      write (moreverbose(ind%moreverbose%o)%unit,'(2a12)'), "ocean[%]", "mean_val"
    endif
    if (present(date)) then
      write (moreverbose(ind%moreverbose%o)%unit,'(f12.3,x, i4.2,5i2.2)', advance='no'), mjd(date), date
    endif
    write (moreverbose(ind%moreverbose%o)%unit,'(f12.3,f12.3)'), & 
        ocean_area/total_area*100.,                                & 
        valoceanarea/ocean_area
  endif
end subroutine

! =============================================================================
!> Mean pressure all over the model area
!
! working only for regular grid!
! =============================================================================
subroutine total_mass (model, date)
  use mod_utilities, only: d2r
  use mod_cmdline,   only: ind, moreverbose
  use mod_printing
  use mod_mjd
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
      valarea    = valarea + val * cos(d2r(model%lat(ilat)))
    enddo
  enddo

  if (output%header)  then
    if (present(date)) then
      write (moreverbose(ind%moreverbose%t)%unit,'(a12,x,a14)', advance='no'), "mjd",  "date"
    endif
    write (moreverbose(ind%moreverbose%t)%unit,'(a12)'), "mean_val"
  endif
  if (present(date)) then
    write (moreverbose(ind%moreverbose%t)%unit,'(f12.3,x, i4.2,5i2.2)', advance='no'), mjd(date), date
  endif
  write (moreverbose(ind%moreverbose%t)%unit,'(f12.3)'),  valarea/ totalarea
end subroutine

! =============================================================================
! =============================================================================
subroutine parse_level (cmd_line_entry)
  use mod_cmdline, only: cmd_line_arg
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
        read(cmd_line_entry%field(i)%subfield(1)%name, '(i)') level%level(i)
      enddo
    endif

    write(log%unit, form%i2, advance="no") "level pressure:"
    if (allocated(level%level)) then
      write (log%unit, '(<size(level%level)>i4)'), level%level
    else if (level%all) then
      write (log%unit, '(a)'), "all"
    endif
  else
    level%all=.true.
  endif

end subroutine
end module
