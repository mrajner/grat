module mod_site
  use, intrinsic :: iso_fortran_env
  use mod_constants, only:dp

  implicit none
  !---------------------------------------------------
  ! site information
  !---------------------------------------------------
  type site_data 
    character(:), allocatable :: name
    real(dp)                  :: lat,lon,height
  end type 

  type(site_data) , allocatable , dimension(:) :: site

  !----------------------------------------------------
  ! Site names file
  !----------------------------------------------------
  character(:), allocatable &
    :: filename_site 
  integer :: fileunit_site

contains
subroutine parse_site(cmd_line_entry)
  use mod_utilities, only: file_exists, is_numeric
  use mod_cmdline,   only: cmd_line_arg, form_63, log

  type (cmd_line_arg)  :: cmd_line_entry
  integer :: i, j

    ! check if format is proper for site
    ! i,e. -Sname,B,L[,H]
    if (.not. allocated(site)) then
      if(index(cmd_line_entry%field(1)%subfield(1)%name, "/" ).ne.0 &
        .or.(cmd_line_entry%field(1)%subfield(1)%name.eq. "g" ) ) &
        then
        call parse_GMT_like_boundaries ( cmd_line_entry )
      else if (size(cmd_line_entry%field).ge.3) then
        if (is_numeric(cmd_line_entry%field(2)%subfield(1)%name) &
          .and.is_numeric(cmd_line_entry%field(3)%subfield(1)%name) &
          .and.index(cmd_line_entry%field(1)%subfield(1)%name, "/" ).eq.0 &
          .and.(.not.cmd_line_entry%field(1)%subfield(1)%name.eq. "Rg" ) &
          ) then
!          allocate (site(1))
!          site(1)%name = trim(cmd_line_entry%field(1)%subfield(1)%name)
!          read ( cmd_line_entry%field(2)%subfield(1)%name , * ) site(1)%lat
!          if (abs(site(1)%lat).gt.90.) site(1)%lat = sign(90.,site(1)%lat) 
!          read ( cmd_line_entry%field(3)%subfield(1)%name , * ) site(1)%lon
!          if (site(1)%lon.ge.360.) site(1)%lon = mod(site(1)%lon,360.)
!          if (size(cmd_line_entry%field).ge.4) then
!            if (is_numeric (cmd_line_entry%field(4)%subfield(1)%name ) ) then
!              read (cmd_line_entry%field(4)%subfield(1)%name, * ) site(1)%height
!            endif
!          endif
        endif
!        !or read sites from file
      else if (file_exists (cmd_line_entry%field(1)%subfield(1)%name))  then
        write(log%unit, form_62) 'the site file was set:' , &
          cmd_line_entry%field(1)%subfield(1)%name
        call read_site_file (cmd_line_entry%field(1)%subfield(1)%name)
      else
!        call print_warning ( "site" )
      endif
!    else
!      call print_warning ( "repeated" , error_unit)
    endif
    !----------------------------------------------------
    ! Site summary
    !----------------------------------------------------
    if (size(site).ge.1) then
      write(log%unit, form_63 ) "Processing:", size(site), "site(s)"
      if (size(site).le.15) then
        write(log%unit, '(8x,a,t16,3a15)') &
          "Name" , "lat [deg]" , "lon [deg]" ,"H [m]"
        do j = 1,size(site)
          write(log%unit, '(8x,a,t16,3f15.4)') &
            site(j)%name, site(j)%lat, site(j)%lon , site(j)%height
        enddo
      endif
    endif
  end subroutine

  ! =============================================================================
  !> 
  ! =============================================================================
  subroutine parse_GMT_like_boundaries ( cmd_line_entry )
  use mod_cmdline,    only: cmd_line_arg
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
    allocate (site ( n_lon * n_lat ) )

    do i = 1 , n_lon
      lon = limits (1) + (i-1) * resolution(1)
      if (lon.ge.360.) lon = lon - 360. 
      do ii = 1 , n_lat
        lat = limits (3) + (ii-1) * resolution (2)
        site( (i-1) * n_lat + ii  )%lon = lon
        site( (i-1) * n_lat + ii  )%lat = lat
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
  use mod_cmdline,    only: cmd_line_arg, form_63 , log
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
    if (nloop.eq.2) allocate(site(good_lines))
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
              site(good_lines)%name= trim(dummy(1))
              read(dummy(2),*) site(good_lines)%lat 
              read(dummy(3),*) site(good_lines)%lon 
              read(dummy(4),*) site(good_lines)%height 
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
