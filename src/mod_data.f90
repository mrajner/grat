! =============================================================================
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
    !    character(:), allocatable :: name 
    character(60) :: name 

    ! varname,lonname,latname,levelname,timename
    character(len=50) :: names(5) = [ "z", "lon", "lat","level","time"]

    character(len=15) :: dataname

    ! if file was determined
    logical :: if =.false.

    ! boundary of model e , w ,s ,n
    real(dp):: limits(4)

    real(dp), allocatable, dimension(:) :: lat , lon , time ,level
    integer , allocatable, dimension(:,:) :: date

    real (dp), dimension(2) :: latrange , lonrange

    logical :: if_constant_value
    real(dp):: constant_value

    ! 4 dimension - lat , lon , level , mjd
    real(dp) , allocatable , dimension (:,:,:) :: data

    ! netcdf identifiers
    integer :: ncid
  end type
  type(file) , allocatable, dimension (:) :: model 
  
  private :: dataname

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
  integer :: i , j

  if (allocated(model)) then
    call print_warning ("repeated")
    return
  endif

  allocate(model(size(cmd_line_entry%field)))

  do i = 1 , size(cmd_line_entry%field)
    write(log%unit, form_62), trim(cmd_line_entry%field(i)%full)
    model(i)%name = trim(cmd_line_entry%field(i)%subfield(1)%name)
    model(i)%dataname = trim(cmd_line_entry%field(i)%subfield(1)%dataname)
    if (model(i)%dataname.eq." ") model(i)%dataname="NN"

    if (model(i)%name.eq."") then
      call print_warning ("model")
    endif
    if ( file_exists (model(i)%name) ) then
      do j =2 , size (cmd_line_entry%field(i)%subfield)
        if (cmd_line_entry%field(i)%subfield(j)%name.ne."") then
          model(i)%names(j-1)=cmd_line_entry%field(i)%subfield(j)%name
        endif
      enddo
      write (log%unit , form%i3,advance='no') , &
        trim (dataname(model(i)%dataname)), &
        "("//trim(model(i)%dataname)//")"
      write(log%unit, '(5(a,x))', advance="no") , (trim(model(i)%names(j)), j =1,5)
      model(i)%if=.true.
      write(log%unit, *) 
      call read_netCDF(model(i))
    else if (is_numeric(model(i)%name)) then
      model(i)%if_constant_value=.true.
      read (model(i)%name , * ) model(i)%constant_value
      write(log%unit, *), 'constant value was set: ' , model(i)%constant_value
    else
      call print_warning ("model")
      stop
    endif
  enddo
end subroutine

! =============================================================================
!> Read netCDF file into memory
! =============================================================================
subroutine read_netCDF (model)
  use netcdf
  use mod_printing
  type (file) :: model
  integer :: i 

  write (log%unit , form%i3) "Opening file:" , trim(model%name)
  call check ( nf90_open ( model%name , nf90_nowrite , model%ncid ) )

  do i = 2,5
    call get_dimension (model, i)
  enddo
  if (size (model%time).gt.1) call nctime2date (model)
end subroutine

! =============================================================================
!> \brief Get dimension, allocate memory and fill with values
!! \author Marcin Rajner
!! \date 2013.05.24
! =============================================================================
subroutine get_dimension ( model , i )
  use netcdf
  use mod_printing
  type(file) :: model
  integer :: dimid , varid 
  integer , intent(in) :: i
  integer :: length , status

  write (log%unit , form%i4, advance='no') "Getting dim:",trim(model%names(i)), ".."

  status = nf90_inq_dimid(model%ncid,model%names(i) , dimid )
  if(status /= nf90_noerr) then 
    write (log%unit , form%i0) trim(model%names(i)),"not found, allocating size 1" 
    length=1
  else
    write (log%unit , form%i0) "ok"
    call check (nf90_inquire_dimension(model%ncid, dimid , len = length) )
    call check (nf90_inq_varid(model%ncid, model%names(i) , varid))
  endif
  if (i .eq. 3 ) then
    allocate(model%lat (length))
    call check(nf90_get_var  (model%ncid,  varid , model%lat))
    status = nf90_get_att ( model%ncid ,varid , &
      "actual_range" , model%latrange) 
    if (status /= nf90_noerr ) model%latrange &
      =[model%lat(1), model%lat(size(model%lat)) ]
    elseif (i.eq.2 ) then
    allocate(model%lon (length) )
    call check(nf90_get_var  (model%ncid,  varid , model%lon))
    status = nf90_get_att ( model%ncid ,varid , &
      "actual_range" , model%lonrange) 
    if (status /= nf90_noerr ) model%lonrange &
      =[model%lon(1) , model%lon(size(model%lon)) ]
    elseif (i.eq.4 ) then
    allocate(model%level (length) )
    status = nf90_get_var  (model%ncid,  varid , model%level)
    elseif (i.eq.5 ) then
    allocate(model%time (length) )
    status = nf90_get_var (model%ncid,  varid , model%time)
  endif
end subroutine

! =============================================================================
!> Change time in netcdf to dates
!!
!! \author M. Rajner
!! \date 2013-03-04
! =============================================================================
subroutine nctime2date (model)
  use netcdf
  use mod_printing
  use mod_constants, only: dp
  use mod_date, only: mjd, invmjd
  type (file)        :: model
  real(dp)           :: mjd_start , mjd_
  integer            :: varid ,i , date (6)
  character (len=66) :: dummy

  call check ( nf90_inq_varid (model%ncid, "time" , varid ) )
  call check (nf90_get_att ( model%ncid , varid , "units" , dummy) )

  allocate (model%date(size(model%time), 6))
  write(log%unit , form%i4) "Converting time: " , trim(dummy)
  if (dummy.eq. "hours since 1-1-1 00:00:0.0") then
    mjd_start =  mjd([1,1,1,0,0,0])
    do i = 1 , size(model%time)
      mjd_= model%time(i) / 24 + mjd_start  - 2
      call invmjd(mjd_,date)
      model%date(i,:) = date
    enddo
  else
    write (log%unit , form%i4 ) "unknown time begining"
  endif
end subroutine

! =============================================================================
!> \brief Get values from netCDF file for specified variables
! =============================================================================
subroutine get_variable(model , date)
  use netcdf
  use mod_printing
  type (file), intent(inout) :: model
  integer , optional , intent(in) ,dimension(6) ::date
  integer :: varid
  integer :: start(3)
  integer :: index_time, i , j

  index_time = 0
  ! write (log%unit , form_61) "Getting var id:" , trim(model%names(1))
  call check ( nf90_inq_varid ( model%ncid , model%names(1) ,  varid ) )
  if (allocated(model%data)) deallocate(model%data)
  allocate (model%data (size(model%lon), size(model%lat), &
    size (model%level)))

  if (size(date).gt.0 .and. present(date)) then                       
    outer: do i = 1 , size(model%date(:,1))
    do j = 1 , 6
      if (model%date(i,j) .eq. date(j)) then
        if (j.eq.6) then 
          index_time = i
          exit outer
        endif
      else
        exit
      endif
    enddo
  enddo outer
  if (index_time.eq.0 ) then
    write(log%unit,form_61) "Cannot find date:", date, &
      "var:", trim(model%names(1)), "file:" , model%name
    model%data= sqrt(-1.)
    return
  endif
else
  index_time = 1
endif
start = [ 1,1,index_time]
call check (nf90_get_var (model%ncid , varid , model%data , start = start ))
call unpack_netcdf(model)
end subroutine

! =============================================================================
!> \brief Unpack variable 
!!
!! from \cite netcdf
! =============================================================================
subroutine unpack_netcdf ( model )
  use netcdf
  use mod_constants, only : dp ,sp
  type(file) :: model
  integer :: varid , status
  real(dp):: scale_factor , add_offset

  call check(nf90_inq_varid(model%ncid, model%names(1) , varid))
  status = nf90_get_att ( model%ncid , varid , "scale_factor" , scale_factor) 
  if (status /=nf90_noerr) scale_factor = 1.

  status = nf90_get_att ( model%ncid , varid , "add_offset" , add_offset) 
  if (status /=nf90_noerr) add_offset = 0.

  model%data = model%data * scale_factor + add_offset
end subroutine
!
! =============================================================================
!> Check the return code from netCDF manipulation
!!
!! \author From netcdf website \cite netcdf
!! \date 2013-03-04
! =============================================================================
subroutine check(status)
  use netcdf
  use mod_printing
  use iso_fortran_env
  integer, intent (in) :: status

  if(status /= nf90_noerr) then 
    write(error_unit, form%i4), trim(nf90_strerror(status))
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
! =============================================================================
subroutine get_value( model, lat , lon , val ,level, method )
  use mod_constants , only: dp 
  use mod_cmdline

  type(file) , intent (in) :: model
  real(dp) , intent (in) :: lat,lon
  real(dp) , intent(out) ::  val 
  character(1), optional , intent(in) :: method
  integer , optional , intent(in) :: level
  integer :: i ,j , ilevel =1 
  integer  :: ilon, ilat , ilon2 , ilat2
  real(dp), dimension(4,3) :: array_aux 

  if (present(level)) ilevel=level
  ! check if inside model range
  if (   lat.lt.min(model%latrange(1), model%latrange(2))  &
    .or.lat.gt.max(model%latrange(1), model%latrange(2)) &
    .or.lon.lt.min(model%latrange(1), model%latrange(2)) &
    .or. (lon.gt.max(model%latrange(1), model%latrange(2)) &
    .and.   lon.gt.max(model%latrange(1), model%latrange(2))) &
    ) then
    val = sqrt(-1.)
  endif
  if (model%if_constant_value) then
    val = model%constant_value
    return
  end if

  ilat = minloc(abs(model%lat-lat),1)
  ilon = minloc(abs(model%lon-lon),1)

  if (present(method) .and. method .eq. "l" ) then
    ilon2 = minloc(abs(model%lon-lon),1, model%lon/=model%lon(ilon))
    ilat2 = minloc(abs(model%lat-lat),1, model%lat/=model%lat(ilat))

    if (lon.gt.model%lon(ilon2).and. lon.gt.model%lon(ilon)) then
    else
      array_aux (1, :) = [ model%lon(ilon) , model%lat(ilat) , model%data(ilon , ilat , ilevel) ]
      array_aux (2, :) = [ model%lon(ilon) , model%lat(ilat2), model%data(ilon , ilat2, ilevel) ]
      array_aux (3, :) = [ model%lon(ilon2), model%lat(ilat) , model%data(ilon2, ilat , ilevel) ]
      array_aux (4, :) = [ model%lon(ilon2), model%lat(ilat2), model%data(ilon2, ilat2, ilevel) ]

      do i=1,size(moreverbose)
        if (moreverbose(i)%dataname.eq."l") then
          write(moreverbose(i)%unit ,  '(3f15.4," l")') , &
            (array_aux(j,2),array_aux(j,1),array_aux(j,3), j = 1 ,4)
          write(moreverbose(i)%unit ,  '(">")')
        endif
      enddo
      val = bilinear ( lon , lat , array_aux )
      return
    endif
  endif

  ! if the last element is the neares then check if the firt is not nearer
  ! i.e. search in 0-357.5E for 359E
  if (ilon .eq. size (model%lon) ) then
    if (abs(model%lon(ilon)-lon).gt.abs(model%lon(1)+360.-lon)) ilon = 1
  endif

  do i=1,size(moreverbose)
    if (moreverbose(i)%dataname.eq."n") then
      write(moreverbose(i)%unit ,  '(3f15.4," n")') , &
        model%lat(ilat) , model%lon(ilon) , model%data(ilon,ilat,ilevel)
      write(moreverbose(i)%unit ,  '(">")')
    endif
  enddo
  val = model%data (ilon , ilat, ilevel)
end subroutine 

! =============================================================================
!> Performs bilinear interpolation
!! \author Marcin Rajner
!! \date 2013-05-07
! =============================================================================
function bilinear (x , y , aux )
  use mod_constants, only: dp
  real(dp) :: bilinear
  real(dp) :: x , y , aux(4,3) 
  real(dp) :: a , b , c
  a  = ( x - aux(1,1) ) /(aux(4,1)-aux(1,1))
  b = a * (aux(3,3)  - aux(1,3)) + aux(1,3) 
  c = a * (aux(4,3)  - aux(2,3)) + aux(2,3)
  bilinear = (y-aux(1,2))/(aux(4,2) -aux(1,2)) * (c-b) + b
end function

!
!!! delteme
!!subroutine invspt(alp,del,b,rlong)
!!  real alp, del , b ,rlong
!!!      data dtr/.01745329251/
!!!      ca = cos(alp*dtr)
!!!      sa = sin(alp*dtr)
!!!      cd = cos(del*dtr)
!!!      sd = sin(del*dtr)
!!!      cb = cd*ct + sd*st*ca
!!!      sb = sqrt(1.-cb*cb)
!!!      b = acos(cb)/dtr
!!!      if(sb.le.1.e-3) then
!!!   rlong = 0
!!!   return
!!!      endif
!!!      sg = sd*sa/sb
!!!      cg = (st*cd-sd*ct*ca)/sb
!!!      g = atan2(sg,cg)/dtr
!!!     rlong = dlugosc_stacji + g
!!!     return
!!end subroutine
! =============================================================================
!!> Counts number of properly specified models
!!!
!!! \date 2013-03-15
!!! \author M. Rajner
!!! TODO move to mod_data?
!! =============================================================================
!!integer function nmodels (model)
!!  type(file) , allocatable, dimension (:) :: model
!!  integer :: i
!!
!!  nmodels = 0
!!  do i = 1 , size (model)
!!    if (model(i)%if) nmodels =nmodels + 1
!!    if (model(i)%if_constant_value) nmodels =nmodels + 1
!!  enddo
!!end function

! =============================================================================
!> Attach full dataname by abbreviation
!!
!! \date 2013-03-21
!! \author M. Rajner
! =============================================================================
function dataname(abbreviation)
  character(len=40) :: dataname
  character(*) :: abbreviation

  dataname="unknown"
  if (abbreviation.eq."LS") dataname = "Land-sea mask"
  if (abbreviation.eq."SP") dataname = "Surface pressure"
  if (abbreviation.eq."SP") dataname = "Surface temperature"
  if (abbreviation.eq."RS") dataname = "Reference surface pressure"
  if (abbreviation.eq."GN") dataname = "Green newtonian"
end function

!! =============================================================================
!!> Put netCDF COARDS compliant 
!!!
!!! for GMT drawing
!! =============================================================================
!subroutine put_grd (model, time , level , filename_opt )
!  use netcdf
!  use mod_cmdline, only : file, form_separator, log
!  type (file) :: model
!  integer  :: time , level , ncid
!  integer :: londimid, latdimid, dimids(2), varid, latvarid,lonvarid
!  character (*) , intent (in) , optional :: filename_opt
!  character(60) :: filename = "tmp_.grd"
!
!  if (present(filename_opt)) filename = filename_opt
!  write(log%unit, form_separator)
!  call check ( nf90_create ( filename , 0 , ncid ))
!
!  ! Define the dimensions. NetCDF will hand back an ID for each. 
!  call check( nf90_def_dim(ncid, "lon", size(model%lon) , londimid) )
!  call check( nf90_def_dim(ncid, "lat", size(model%lat) , latdimid) )
!
!  dimids =  (/ londimid, latdimid /)
!
!  call check( nf90_def_var(ncid, "lat", NF90_float , latdimid, latvarid) )
!  call check( nf90_def_var(ncid, "lon", NF90_float , londimid, lonvarid) )
!  call check( nf90_def_var(ncid, "data", NF90_float , dimids, varid) )
!  call check( nf90_enddef(ncid) )
!
!  call check( nf90_put_var(ncid, latvarid, model%lat ))
!  call check( nf90_put_var(ncid, lonvarid, model%lon ))
!  call check( nf90_put_var(ncid, varid, model%data(:,:,1)) )
!  call check( nf90_close(ncid) )
!end subroutine
!
end module
