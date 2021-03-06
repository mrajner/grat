module mod_site
  use mod_constants, only: dp
  use mod_printing
  use, intrinsic :: iso_fortran_env

  implicit none
  !---------------------------------------------------
  ! site information
  !---------------------------------------------------
  type more_site_heights
    real(dp) :: val
    logical  :: if=.false.
  end type

  type lp_info
    real(dp), allocatable, dimension(:,:) :: date
    real(dp), allocatable, dimension(:)   :: data
    logical :: if=.false.
  end type

  type site_info
    character(20) :: name
    real(dp)                 :: lat,lon,height
    type(more_site_heights)  :: hp, h, hrsp
    logical :: use_local_pressure=.false.
    type(lp_info) :: lp
  end type

  type(site_info), allocatable, dimension(:) :: site

  logical :: site_height_from_model=.false.

  ! if using -S @LP substitute model values till this distance
  real(dp) :: local_pressure_distance = 0.25

contains
! =============================================================================
!>
! =============================================================================
subroutine parse_site(cmd_line_entry)
  use mod_cmdline
  use mod_utilities, only: file_exists, is_numeric

  type(cmd_line_arg),intent(in):: cmd_line_entry
  integer :: start_index
  integer :: i

  if (allocated(site)) then
    call print_warning ("repeated")
    return
  endif

  do i = 1, ubound(cmd_line_entry%field,1)

    if (.not.log%sparse) then
      write(log%unit, form%i2), trim(cmd_line_entry%field(i)%full)
    endif

    if (file_exists (cmd_line_entry%field(i)%subfield(1)%name))  then

      write(log%unit, form%i3) 'reading from file:', &
        cmd_line_entry%field(i)%subfield(1)%name

      if(cmd_line_entry%field(i)%subfield(1)%dataname.eq."LP") then

        if (size(site).gt.1) then
          call print_warning("only one site with @LP allowed",error=.true.)
        end if
        call read_local_pressure(cmd_line_entry%field(i)%subfield(1)%name)
        return

      else
        call read_site_file (cmd_line_entry%field(i)%subfield(1)%name)
      endif

      continue

    else if(index(cmd_line_entry%field(i)%subfield(1)%name, "/" ).ne.0 &
      )                                                                &
      then
      call parse_GMT_like_boundaries (cmd_line_entry%field(i))
      continue

    else if (                                                    &
      size(cmd_line_entry%field(i)%subfield).ge.3                &
      .and. is_numeric(cmd_line_entry%field(i)%subfield(2)%name) &
      .and. is_numeric(cmd_line_entry%field(i)%subfield(3)%name) &
      ) then

      call more_sites (1,start_index)
      site(start_index)%name = trim(cmd_line_entry%field(i)%subfield(1)%name)
      read (cmd_line_entry%field(i)%subfield(2)%name, * ) site(start_index)%lat

      if (abs(site(start_index)%lat).gt.90.) &
        site(start_index)%lat = sign(90._dp,site(start_index)%lat)
      read (cmd_line_entry%field(i)%subfield(3)%name,*) site(start_index)%lon

      if (site(start_index)%lon.ge.360.) &
        site(start_index)%lon = mod(site(start_index)%lon,360.)

      if (site(start_index)%lon.lt.-180.) &
        site(start_index)%lon = mod(site(start_index)%lon-180,360.)+180

      if (is_numeric(cmd_line_entry%field(i)%subfield(4)%name)) then
        read (cmd_line_entry%field(i)%subfield(4)%name, * ) &
          site(start_index)%height
      endif

      continue

    else

      select case(cmd_line_entry%field(i)%subfield(1)%name)

      ! sone predefined sites for test purposes
      case ("j")
        ! this is shortcut for Józefosław -Sj
        call more_sites (1,start_index)
        site(start_index)%name   = "joze_a"
        site(start_index)%lat    = 52._dp
        site(start_index)%lon    = 21._dp
        site(start_index)%height = 110._dp
        continue

      case("b")
        ! and point on Baltic Sea -Sb
        call more_sites (1,start_index)
        site(start_index)%name   = "balt_a"
        site(start_index)%lat    = 57
        site(start_index)%lon    = 21
        site(start_index)%height = 0
        continue

      case("r")
        ! and point on Rysy -Sr
        call more_sites (1,start_index)
        site(start_index)%name   = "rysy_a"
        site(start_index)%lat    = 49.17944
        site(start_index)%lon    = 20.088333
        site(start_index)%height = 2499
        continue

      case("l")
        ! Lamkówko -Sl
        call more_sites (1,start_index)
        site(start_index)%name   = "lama_a"
        site(start_index)%lat    = 53.883
        site(start_index)%lon    = 20.667
        site(start_index)%height = 100
        continue

      case("o")
        ! Marian trench -So
        call more_sites (1,start_index)
        site(start_index)%name   = "mari_a"
        site(start_index)%lat    = 11.317
        site(start_index)%lon    = 142.25
        site(start_index)%height = -9910
        continue

      case("e")
        ! Mount Everest -Se
        call more_sites (1,start_index)
        site(start_index)%name   = "ever_a"
        site(start_index)%lat    = 27.988056
        site(start_index)%lon    = 86.925278
        site(start_index)%height = 8848.
        continue

      case("jb")
        ! Bajkal Lakea -Sb
        call more_sites (1,start_index)
        site(start_index)%name   = "bajk_a"
        site(start_index)%lat    = 53.699959
        site(start_index)%lon    = 108.379898
        site(start_index)%height = 0.
        continue

      case ("g","m","pl")
        ! computing grid for whole world, model boundaries, or Poland
        call parse_GMT_like_boundaries (cmd_line_entry%field(i))
        continue

      case default
        call print_warning ("site")
        continue
      end select

    endif
  enddo

  do i = 1, size(cmd_line_entry%field)
    if (any(cmd_line_entry%field(i)%subfield%dataname.eq."H")) then
      site_height_from_model=.true.
    endif
  enddo
end subroutine

! =============================================================================
!>
! =============================================================================
subroutine print_site_summary(site_parsing)
  use mod_data
  integer :: j
  logical, optional :: site_parsing

  if (ubound(site,1).ge.1) then
    write(log%unit, form%i2 ) "Processing:", size(site), "site(s)"
    if (size(site).le.15) then
      write(log%unit, '(t1, 3a10, 4a10)') &
        "Name", "lat [deg]", "lon [deg]", "H [m]", "Hp [m]", "H* [m]", "Hrsp[m]"

      do j = 1,size(site)

        write(log%unit, '(t1,a10,3f10.4)', advance="no")               &
          trim(site(j)%name), site(j)%lat, site(j)%lon, site(j)%height

        if(present(site_parsing).and.site_parsing)                     &
          write(output%unit, '(t6,a10,3f10.4)')                        &
          trim(site(j)%name), site(j)%lat, site(j)%lon, site(j)%height

        if (site(j)%hp%if) then
          write(log%unit, "(f10.4)", advance="no") site(j)%hp%val
        else
          write(log%unit, "(a10)", advance="no") "--"
        endif

        if (site(j)%h%if) then
          write(log%unit, "(f10.4)", advance="no") site(j)%h%val
        else
          write(log%unit, "(a10)", advance="no") "--"
        endif

        if (site(j)%hrsp%if) then
          write(log%unit, "(f10.4$)") site(j)%hrsp%val
        else
          write(log%unit, "(a10)", advance ="no") "--"
        endif

        write(log%unit,'(a)')
      enddo
    endif
  endif
end subroutine

! =============================================================================
!>
! =============================================================================
subroutine parse_GMT_like_boundaries (field)
  use mod_utilities, only: is_numeric
  use mod_cmdline, only: field_info
  use mod_data, only: model
  type(field_info),intent(in) :: field

  real(dp) :: limits (4), resolution (2)
  real(dp) :: range_lon, range_lat, lat, lon
  integer :: i, ii, indeks_slash
  character(:), allocatable :: text
  integer :: n_lon, n_lat, start_index

  resolution =[1,1]
  text = field%subfield(1)%name

  do i = 1, 4
    indeks_slash=index(text,"/")

    if (indeks_slash.eq.0) indeks_slash=len(text)+1

    if (is_numeric (text(1:indeks_slash-1))) then
      read (text(1:indeks_slash-1), *)  limits(i)
    else
      if (text.eq."g" ) then
        limits=[0., 359.9999, -90., 90.]
      else if (text.eq."pl") then
        limits=[14.0, 24.2, 48.7, 55.]
      endif
      exit
    endif
    text = text(index(text,"/")+1:)
  enddo

  do i = 1, 2
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
  if (text.eq."m" ) then
    limits = [ &
      minval(model(1)%lon), &
      maxval(model(1)%lon), &
      minval(model(1)%lat), &
      maxval(model(1)%lat) &
      ]
    if (size(field%subfield).eq.1) then
      call more_sites (size(model(1)%lon) * size(model(1)%lat), start_index)

      do i =1, size(model(1)%lon)
        do ii =1, size(model(1)%lat)
          site(start_index -1 + (i-1) * size(model(1)%lat) + ii)%lon = model(1)%lon(i)
          site(start_index -1 + (i-1) * size(model(1)%lat) + ii)%lat =  model(1)%lat(ii)
          site(start_index -1 + (i-1) * size(model(1)%lat) + ii)%name = "model"
          site(start_index -1 + (i-1) * size(model(1)%lat) + ii)%height = 0
        enddo
      enddo
      return
    endif
  endif

  range_lon = limits(2) - limits(1)
  if (range_lon.lt.0) range_lon = range_lon + 360.
  range_lat = limits(4) - limits(3)
  n_lon = floor (range_lon / resolution(1)) + 1
  n_lat = floor (range_lat / resolution(2)) + 1
  call more_sites (n_lon * n_lat, start_index)

  do i = 1, n_lon
    lon = limits (1) + (i-1) * resolution(1)
    if (lon.ge.360.) lon = lon - 360.
    do ii = 1, n_lat
      lat = limits (3) + (ii-1) * resolution (2)
      site(start_index -1 + (i-1) * n_lat + ii)%lon = lon
      site(start_index -1 + (i-1) * n_lat + ii)%lat = lat
      site(start_index -1 + (i-1) * n_lat + ii)%name = "auto"
      site(start_index -1 + (i-1) * n_lat + ii)%height = 0
    enddo
  enddo
end subroutine

! =============================================================================
! =============================================================================
subroutine more_sites (number, start_index)
  integer, intent(in)  :: number
  integer, intent(out) :: start_index
  type(site_info), allocatable, dimension(:) :: tmpsite

  if (allocated(site)) then
    write(log%unit, form%i3), "added site(s):", number
    start_index=size(site) + 1
    call move_alloc(site,tmpsite)
    allocate(site(size(tmpsite)+number))
    site(1:size(tmpsite))=tmpsite
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
  use mod_utilities, only: is_numeric, ntokens, skip_header
  use mod_cmdline, only: method
  character(len=*), intent(in) ::  file_name
  integer :: io_status, i, good_lines, number_of_lines, nloop
  integer :: fileunit_site, start_index
  character(len=255), dimension(4)  :: dummy
  character(len=255) :: line_of_file
  type(site_info) :: aux

  number_of_lines=0

  open ( newunit = fileunit_site, file = file_name, &
    iostat = io_status, status = "old", action="read" )

  ! two loops, first count good lines and print rejected
  ! second allocate array of sites and read coordinates into it
  do nloop = 1, 2
    if (nloop.eq.2) call more_sites(good_lines, start_index)
    if (number_of_lines.ne.good_lines) then
      call print_warning ("site_file_format")
    endif
    good_lines=0
    do
      call skip_header(fileunit_site)
      read (fileunit_site, '(a)', iostat=io_status) line_of_file
      if (io_status==iostat_end)  exit
      number_of_lines = number_of_lines + 1
      !  ! we need at least 3 parameter for site (name, B, L )
      if (ntokens(line_of_file).ge.3) then
        ! but no more than 4 parameters (name, B, L, H)
        if (ntokens(line_of_file).gt.4) then
          read (line_of_file, *) dummy(1:4)
        else
          read (line_of_file, *) dummy(1:3)
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
              if (number_of_lines-good_lines.lt.15) then
                call print_warning("rejecting: " // line_of_file // "  [lon limits]")
              endif
            endif
          endif
        else
          if (nloop.eq.2) then
            if (number_of_lines-good_lines.lt.15) then
              call print_warning("rejecting: " // trim(line_of_file)//  " [lat limits]")
            endif
          endif
        endif
      else
        ! print it only once
        if (nloop.eq.2) then
          if (number_of_lines-good_lines.lt.15) then
            call print_warning("rejecting: " // trim(line_of_file) // " [args]")
          endif
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

! =============================================================================
! =============================================================================
subroutine gather_site_model_info()
  use mod_cmdline, only: ind, info
  use mod_data,    only: get_value, model, get_variable
  integer :: i

  if (site_height_from_model.and. ind%model%h.eq.0) then
    call print_warning("site @H but not model @H was given", error=.true.)
  endif

  do i = 1 , ubound(site,1)
    if (ind%model%hp.ne.0) then

      call get_variable(model(ind%model%hp))
      site(i)%hp%if=.true.

      call get_value (                   &
        model=model(ind%model%hp),     &
        lat=site(i)%lat,               &
        lon=site(i)%lon,               &
        val=site(i)%hp%val,            &
        level=1,                       &
        method = info(1)%interpolation &
        )
    endif

    if(ind%model%h.ne.0) then
      site(i)%h%if=.true.

      call get_variable(model(ind%model%h))

      call get_value (                 &
        model=model(ind%model%h),      &
        lat=site(i)%lat,               &
        lon=site(i)%lon,               &
        val=site(i)%h%val,             &
        level=1,                       &
        method = info(1)%interpolation &
        )
    endif

    if(ind%model%hrsp.ne.0) then
      site(i)%hrsp%if=.true.

      call get_variable(model(ind%model%hrsp))

      call get_value (                 &
        model=model(ind%model%hrsp),   &
        lat=site(i)%lat,               &
        lon=site(i)%lon,               &
        val=site(i)%hrsp%val,          &
        level=1,                       &
        method = info(1)%interpolation &
        )
    endif
  enddo

  if (site_height_from_model) then
    site%height=site%h%val
  endif

  write(log%unit, form%separator)

  if(.not.log%sparse) then
    call print_site_summary()
    write(log%unit, form%separator)
  endif
end subroutine

!
! use local pressure to enhance solution
!
subroutine read_local_pressure(file)
  use mod_date
  use mod_printing, only: print_warning
  character(*), intent(in) :: file
  integer :: unit, io_stat, datei(6), ilines
  real(dp) :: val
  character(20)::dates

  if (size(site).ne.1) then
    call print_warning( &
      "something wrong with @LP. &
      There should be oneand only one station declared prior to &
      local file pressure @LP [e.x. -S j , lp_joze@LS ...]" , &
      error=.true. &
      )
  endif
  site(1)%lp%if=.true.

  open (newunit=unit, file=file, action="read")

  ilines=0
  do
    read (unit,*, iostat=io_stat) dates, val
    if (io_stat.eq.iostat_end) exit
    call string2date(dates, datei)
    if (modulo(datei(4),6).ne.0) then
      continue
    else
      ilines=ilines+1
    endif
  enddo
  rewind(unit)
  write(log%unit, form%i3) "lines in file: ", ilines
  allocate(site(1)%lp%date(ilines,6))
  allocate(site(1)%lp%data(ilines))

  ilines=0
  do
    read (unit,*, iostat=io_stat) dates, val
    call string2date(dates, datei)

    if (io_stat.eq.iostat_end) exit

    if (modulo(datei(4),6).ne.0) then
      continue
    else
      ilines=ilines+1
      site(1)%lp%date(ilines,1:6)=datei
      site(1)%lp%data(ilines)=val*100.
    endif

  enddo

  close(unit)
end subroutine
end module
