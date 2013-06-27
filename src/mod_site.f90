module mod_site
  use mod_constants, only:dp
  use mod_printing
  use, intrinsic :: iso_fortran_env

  implicit none
  !---------------------------------------------------
  ! site information
  !---------------------------------------------------
  type site_info 
    character(:), allocatable :: name
    real(dp)                  :: lat,lon,height
  end type

  type(site_info) , allocatable , dimension(:) :: site

contains
! =============================================================================
!> 
! =============================================================================
subroutine parse_site(cmd_line_entry)
  use mod_cmdline
  use mod_utilities, only: file_exists, is_numeric
  type(cmd_line_arg),intent(in):: cmd_line_entry
  integer :: start_index

  integer :: i, j
  if (allocated(site)) then
    call print_warning ("repeated")
    return
  endif

  do i = 1 , size (cmd_line_entry%field)
    write(log%unit, form%i2) , trim(cmd_line_entry%field(i)%full)


    if(index(cmd_line_entry%field(i)%subfield(1)%name, "/" ).ne.0 &
      .or.&
      (cmd_line_entry%field(i)%subfield(1)%name.eq. "g" )  &
      .or.&
      (cmd_line_entry%field(i)%subfield(1)%name.eq. "m" )  &
      ) &
      then
      call parse_GMT_like_boundaries (cmd_line_entry%field(i))
    else if ( &
      size(cmd_line_entry%field(i)%subfield).ge.3 &
      .and. is_numeric(cmd_line_entry%field(i)%subfield(2)%name) &
      .and. is_numeric(cmd_line_entry%field(i)%subfield(3)%name) &
      ) then
      call more_sites (1,start_index)
      site(start_index)%name = trim(cmd_line_entry%field(i)%subfield(1)%name)
      read (cmd_line_entry%field(i)%subfield(2)%name, * ) site(start_index)%lat
      if (abs(site(start_index)%lat).gt.90.) &
        site(start_index)%lat = sign(90.,site(start_index)%lat) 
      read (cmd_line_entry%field(i)%subfield(3)%name,*) site(start_index)%lon
      if (site(start_index)%lon.ge.360.) &
        site(start_index)%lon = mod(site(start_index)%lon,360.)
      if (is_numeric(cmd_line_entry%field(i)%subfield(4)%name)) then
        read (cmd_line_entry%field(i)%subfield(4)%name, * ) &
          site(start_index)%height
      endif
    else if (file_exists (cmd_line_entry%field(i)%subfield(1)%name))  then
      write(log%unit, form%i3) 'reading from file:', &
        cmd_line_entry%field(i)%subfield(1)%name
      call read_site_file (cmd_line_entry%field(i)%subfield(1)%name)
    else
      call print_warning ("site")
    endif
  enddo
  call print_site_summary()

end subroutine

! =============================================================================
!> 
! =============================================================================
subroutine print_site_summary()
  integer :: j
  if (size(site).ge.1) then
    write(log%unit, form%i2 ) "Processing:", size(site), "site(s)"
    if (size(site).le.15) then
      write(log%unit, '(t6,4a10)') &
        "Name" , "lat [deg]" , "lon [deg]" ,"H [m]"
      do j = 1,size(site)
        write(log%unit, '(t6,a10,3f10.4)') &
          site(j)%name, site(j)%lat, site(j)%lon , site(j)%height
      enddo
    endif
  endif
end subroutine

! =============================================================================
!> 
! =============================================================================
subroutine parse_GMT_like_boundaries (field)
  use mod_utilities, only : is_numeric
  use mod_cmdline, only : field_info
  type(field_info),intent(in) :: field

  real(dp) :: limits (4) , resolution (2) 
  real(dp) :: range_lon , range_lat , lat , lon
  character(10) :: dummy
  integer :: i , ii , indeks_slash
  character(:) ,allocatable :: text
  integer :: n_lon , n_lat , start_index

  resolution =[1,1]
  text = field%subfield(1)%name

  do i=1,4
    indeks_slash=index(text,"/")
    if (indeks_slash.eq.0) indeks_slash=len(text)+1
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

  if (size(field%subfield).ge.2) then
    if (is_numeric(field%subfield(2)%name)) then
      read (field%subfield(2)%name, * ) resolution(1)
      resolution(2) = resolution(1)
    endif
  endif
  if (size(field%subfield).ge.3) then
    if (is_numeric(field%subfield(3)%name)) then
      read (field%subfield(3)%name, * ) resolution(2)
    endif
  endif

  range_lon=limits(2) - limits(1)
  if (range_lon.lt.0) range_lon = range_lon + 360.
  range_lat=limits(4) - limits(3)
  n_lon = floor ( range_lon / resolution(1)) + 1
  n_lat = floor ( range_lat / resolution(2)) + 1  
  call more_sites ( n_lon * n_lat , start_index )

  do i = 1 , n_lon
    lon = limits (1) + (i-1) * resolution(1)
    if (lon.ge.360.) lon = lon - 360. 
    do ii = 1 , n_lat
      lat = limits (3) + (ii-1) * resolution (2)
      site( start_index -1 +  (i-1) * n_lat + ii  )%lon = lon
      site( start_index -1 + (i-1) * n_lat + ii  )%lat = lat
      site( start_index -1 + (i-1) * n_lat + ii  )%height = 0
      site( start_index -1 + (i-1) * n_lat + ii  )%name = "auto"
    enddo
  enddo
end subroutine

! =============================================================================
! =============================================================================
subroutine more_sites (number, start_index)
  integer, intent(in)  :: number
  integer, intent(out) :: start_index
  type(site_info),allocatable , dimension(:) :: tmpsite

  if (allocated(site)) then
    write(log%unit, form%i3) ,"added site(s):", number
    start_index=size(site) + 1
    call move_alloc(site,tmpsite)
    allocate(site(size(tmpsite)+number))
    site=tmpsite
    deallocate(tmpsite)
  else 
    allocate(site(number))
    start_index=1
  endif
end subroutine

! =============================================================================
!> Read site list from file
!!
!! checks for arguments and put it into array \c sites
! =============================================================================
subroutine read_site_file (file_name)
  use mod_utilities, only: is_numeric, ntokens
  !  use mod_cmdline,    only: cmd_line_arg, form_63 , log
  character(len=*) , intent(in) ::  file_name
  integer :: io_status , i , good_lines = 0 , number_of_lines = 0 , nloop
  integer :: fileunit_site , start_index
  character(len=255) ,dimension(4)  :: dummy
  character(len=255) :: line_of_file
  type(site_info) :: aux

  open ( newunit = fileunit_site , file = file_name, &
    iostat = io_status ,status = "old" , action="read" )

  ! two loops, first count good lines and print rejected
  ! second allocate array of sites and read coordinates into it
  do nloop = 1, 2
    if (nloop.eq.2) call more_sites(good_lines ,start_index)
    if (number_of_lines.ne.good_lines) then
      call print_warning ("site_file_format")
    endif
    good_lines=0
    do 
      read ( fileunit_site , '(a)' , iostat = io_status ) line_of_file 
      if ( io_status == iostat_end)  exit
      number_of_lines = number_of_lines + 1
      !  ! we need at least 3 parameter for site (name , B , L )
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
      !  ! check the values given
      if(    is_numeric(trim(dummy(2)))   &
        .and.is_numeric(trim(dummy(3)))   &
        .and.is_numeric(trim(dummy(4)))   &
        .and.ntokens(line_of_file).ge.3 ) &
        then
        aux%name= trim(dummy(1))
        read( dummy(2),*) aux%lat
        read(dummy(3),*) aux%lon 
        read(dummy(4),*) aux%height 
        if (aux%lat.ge.-90 .and. aux%lat.le.90) then
          if (aux%lon.ge.-180 .and. aux%lon.le.360) then
            good_lines=good_lines+1
            if (nloop.eq.2) then
              site(good_lines-1+start_index)%name= trim(dummy(1))
              read(dummy(2),*) site(good_lines-1+start_index)%lat 
              read(dummy(3),*) site(good_lines-1+start_index)%lon 
              read(dummy(4),*) site(good_lines-1+start_index)%height 
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
  do i =1, size (site)
    if (site(i)%lon.lt.0) site(i)%lon= site(i)%lon + 360.
    if (site(i)%lon.eq.360) site(i)%lon= 0.
  enddo
end subroutine

end module
