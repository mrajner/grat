!> \file
module mod_green
  use mod_constants, only: dp

  implicit none
  !----------------------------------------------------
  ! Greens function
  !----------------------------------------------------
  type green_functions
    character (len=255) :: name
    character (len=25) :: dataname
    integer, dimension(2) :: column
    character(10), dimension(2) :: columndataname
    real(dp), allocatable,dimension(:) :: distance
    real(dp), allocatable,dimension(:) :: data
  end type
  type(green_functions), allocatable, dimension(:) :: green

  real(dp), allocatable, dimension(:) :: result

  type green_common_info
    real(dp), allocatable, dimension(:) :: distance
    real(dp), allocatable, dimension(:) :: start
    real(dp), allocatable, dimension(:) :: stop
    real(dp), allocatable, dimension(:,:) :: data
    character (len=25), allocatable, dimension(:) :: dataname
    logical, allocatable, dimension(:) :: elastic
  end type
  type(green_common_info), allocatable, dimension(:) :: green_common

  integer :: gnc_looseness=1

contains
! =============================================================================
!> This subroutine parse -G option -- Greens function.
!!
!! This subroutines takes the -G argument specified as follows:
!!   -G 
!! \author M. Rajner
!! \date 2013-03-06
! =============================================================================
subroutine parse_green (cmd_line_entry)
  use mod_utilities, only: file_exists, is_numeric
  use mod_cmdline
  use mod_printing
  type (cmd_line_arg), optional  :: cmd_line_entry
  integer :: i, ii 

  if (allocated(green)) then
    call print_warning ("repeated")
    return
  endif

  if (method(3)) then
    if (present(cmd_line_entry)) then
      allocate (green (size(cmd_line_entry%field)+1))
    else
      allocate (green (1))
    endif
    ind%green%g3d=ubound(green,1)
    green(ind%green%g3d)%name="merriam"
    green(ind%green%g3d)%column=[1, 2]
    green(ind%green%g3d)%dataname="G3D"
    call read_green(green(ind%green%g3d))
  else
    allocate (green (size(cmd_line_entry%field)))
  endif

  if (present(cmd_line_entry)) then
    do i = 1, size(cmd_line_entry%field)

      write(log%unit, form%i2) trim(basename(trim(cmd_line_entry%field(i)%full)))
      green(i)%name = cmd_line_entry%field(i)%subfield(1)%name

      if (i.gt.1.and.cmd_line_entry%field(i)%subfield(1)%name.eq."") then
        green(i)%name = green(i-1)%name
      endif

      if (any(green%dataname.eq.cmd_line_entry%field(i)%subfield(1)%dataname )) then
        call print_warning("repeated dataname for Green")
        continue
      else
        green(i)%dataname = cmd_line_entry%field(i)%subfield(1)%dataname
      endif

      do ii=1, 2
        green(i)%column(ii) = green(i-1)%column(ii)
        green(i)%columndataname(ii) = green(i-1)%columndataname(ii) 
        if(is_numeric (cmd_line_entry%field(i)%subfield(ii+1)%name ) ) then
          read(cmd_line_entry%field(i)%subfield(ii+1)%name, *) green(i)%column(ii)
          green(i)%columndataname(ii) = cmd_line_entry%field(i)%subfield(ii+1)%dataname
        endif

      enddo
      if (green(i)%dataname.eq."GNc") then
        if(is_numeric(cmd_line_entry%field(i)%subfield(2)%name)) then
          read(cmd_line_entry%field(i)%subfield(2)%name, *) gnc_looseness
          if (gnc_looseness.lt.1) then
            call print_warning("gnc_looseness < 1", error=.true.) 
          endif
        endif
      endif

      call read_green(green(i))

    enddo
  endif

  ! check completness
  ! if ( &
  ! ! any(green%name.eq."/home/mrajner/src/grat/dat/merriam_green.dat" &
  ! ! .and. green%dataname.eq."GNdz" ) &
  ! ! .neqv. &
  ! any(green%name.eq."/home/mrajner/src/grat/dat/merriam_green.dat" &
  ! .and. green%dataname.eq."GNdz2" ) &
  ! ) call print_warning("-G: merriam@GNdz should go with merriam @GNdz2")
end subroutine

! =============================================================================
!> This subroutine read  green file
! =============================================================================
subroutine read_green (green, print)
  use mod_utilities, only: file_exists, skip_header, r2d, d2r
  use iso_fortran_env
  use mod_printing
  use mod_constants, only: earth, pi
  use mod_normalization, only: green_normalization

  integer :: lines, fileunit, io_status, i
  real (dp), allocatable, dimension(:) :: tmp
  type(green_functions) :: green
  logical, optional :: print 

  ! change the paths accordingly
  if (.not.file_exists(green%name)       & 
    .and. (.not. green%name.eq."merriam" & 
    .and.  .not. green%name.eq."huang"   & 
    .and.  .not. green%name.eq."rajner"  & 
    ! this will be feature added for hydrosphere loading later...
  ! .and.  .not. green%name.eq."GB" &
  )) then
    green%name="merriam"
  endif

  select case (green%name)
  case ("merriam", "compute", "/home/mrajner/src/grat/dat/merriam_green.dat")
    green%name="/home/mrajner/src/grat/dat/merriam_green.dat"
    select case (green%dataname)
    case("GN")
      green%column=[1, 2]
    case("GNdt") 
      green%column=[1, 3]
    case("GNdz")
      green%column=[1, 4]
    case("GNdz2")
      green%column=[1, 5]
    case("GE")
      green%column=[1, 6]
    case("GNc")
      green%column=[1, 2]
    case("G3D")
      green%column=[1, 2]
    case default
      call print_warning( &
        "green type not found", &
        more=trim(green%dataname), &
        error=.true.)
    endselect

  case ("huang", "/home/mrajner/src/grat/dat/huang_green.dat" ) 
    green%name="/home/mrajner/src/grat/dat/huang_green.dat"
    select case (green%dataname)
    case("GN")
      green%column=[1, 2]
    case("GNdt") 
      green%column=[1, 3]
    case("GNdh")
      green%column=[1, 4]
    case("GNdz")
      green%column=[1, 5]
    case default
      call print_warning ( &
        trim(green%dataname) //" not found in " &
        // trim(green%name), error=.true.)
    endselect

  case ("rajner", "/home/mrajner/src/grat/dat/rajner_green.dat")
    green%name="/home/mrajner/src/grat/dat/rajner_green.dat"
    select case (green%dataname)
    case("GN")
      green%column=[1, 2]
    case("GNdt") 
      green%column=[1, 3]
    case("GNdh")
      green%column=[1, 4]
    case("GNdz")
      green%column=[1, 5]
    case default
      call print_warning ( &
        trim(green%dataname) //" not found in " &
        // trim(green%name), error=.true.)
      call print_warning (green%dataname //"not found in " // green%name, &
        error=.true.)
    endselect
  endselect

  if(green%column(1).ne.0 .and. green%column(2).ne.0) then
    allocate(tmp(max(green%column(1), green%column(2))))
    lines = 0
    open (newunit =fileunit, file=green%name, action="read", status="old")
    do 
      call skip_header (fileunit)
      read (fileunit, *, iostat = io_status) tmp
      if (io_status == iostat_end) exit
      lines = lines + 1
    enddo

    allocate (green%distance(lines))
    allocate (green%data(lines))
    rewind(fileunit)
    lines = 0
    do 
      call skip_header (fileunit)
      lines = lines + 1
      read (fileunit, *, iostat = io_status) tmp
      if (io_status == iostat_end) then
        close(fileunit) 
        exit
      endif
      green%distance(lines) = tmp (green%column(1))
      green%data(lines)     = tmp (green%column(2))
    enddo
    deallocate(tmp)
  endif

  ! file specific 
  if (green%name.eq."/home/mrajner/src/grat/dat/merriam_green.dat") then
    select case(green%dataname)
    case("GNdz") 
      green%data = green%data * 1.e-3
    endselect
  endif

  if (.not.present(print)) then
    write(log%unit, form%i3) &
      trim(basename(trim(green%name))), trim(green%dataname), &
      "columns:", green%column, &
      "lines:", size(green%distance)
    if (green%dataname.eq."GNc") then 
      write(log%unit, form%i3) "gnc loosenes" , gnc_looseness
    endif
  endif

  if (green%columndataname(1).eq."R") then
    green%distance=(/ (r2d(green%distance(i)), i=1, size(green%distance)) /)
    write(log%unit, form_63) "conversion: radians --> to degrees"
  endif
  if (green%columndataname(2).eq."a2f") then
    green%data=green%data  / (earth%radius)*1e12 * earth%gravity%mean
    write(log%unit, form_63) "conversion: aplo --> to farrell"
  endif
  if (green%columndataname(2).eq."f2m") then
    green%data= &
      -green%data * green_normalization("f2m")
    write(log%unit, form_63) "conversion: farrell --> to merriam"
  endif
end subroutine

! =============================================================================
!> Unification:
! =============================================================================
subroutine green_unification ()
  use mod_utilities, only: size_ntimes_denser, spline_interpolation, d2r
  use mod_cmdline,   only: info, moreverbose, ind
  use mod_printing
  use mod_site, only: site

  type(green_functions) :: tmpgreen
  integer :: i, iinfo, imin, imax, j, ii
  integer, allocatable, dimension(:):: which_green, tmp

  allocate (green_common(size(info)))
  allocate (which_green(size(info)))
  allocate (tmp(size(green)))

  do iinfo=1, size(info)

    if (info(iinfo)%distance%step.eq.0) then
      do i=1, size(green)
        tmp(i) = count(                                          & 
          green(i)%distance.le.info(iinfo)%distance%stop       & 
          .and.green(i)%distance.ge.info(iinfo)%distance%start & 
          ) 
      enddo
      which_green(iinfo) = maxloc(tmp, 1)

      imin=minloc( & 
        abs(green(which_green(iinfo))%distance - info(iinfo)%distance%start), 1)-1 
      imax=minloc( &
        abs(green(which_green(iinfo))%distance - info(iinfo)%distance%stop), 1)+1

      if (imin.lt.1) imin = 1
      if (imax.gt.size(green(which_green(iinfo))%distance)) then
        imax = size(green(which_green(iinfo))%distance)
      endif

      if (info(iinfo)%distance%denser.ge.0) then
        allocate(tmpgreen%distance(                                   & 
          size_ntimes_denser(imax-imin+1, info(iinfo)%distance%denser) & 
          ))

        do ii = 1, imax-imin
          do j = 1, info(iinfo)%distance%denser
            tmpgreen%distance((ii-1)*info(iinfo)%distance%denser+j) = & 
              green(which_green(iinfo))%distance(imin+ii-1)           & 
              +(j-1)*(green(which_green(iinfo))%distance(imin+ii)     & 
              -green(which_green(iinfo))%distance(imin+ii-1))         & 
              /info(iinfo)%distance%denser
          enddo
        enddo

      else
        ! if @DD is negative make distance sparse
        allocate(tmpgreen%distance((imax-imin)/-info(iinfo)%distance%denser &
          +1+min(1,modulo(imax-imin,-info(iinfo)%distance%denser))))
        ii=0
        do j=1,imax-imin+1
          if (j.eq.imax-imin+1.or.modulo(j-1,info(iinfo)%distance%denser).eq.0) then
            ii=ii+1
            tmpgreen%distance(ii)=green(which_green(iinfo))%distance(j)
          endif
        enddo
      endif

      tmpgreen%distance(size(tmpgreen%distance)) = & 
        green(which_green(iinfo))%distance(imax)

      imin = count(tmpgreen%distance.le.info(iinfo)%distance%start) 
      imax = size(tmpgreen%distance) - &
        count(tmpgreen%distance.ge.info(iinfo)%distance%stop ) + 1

      allocate(green_common(iinfo)%distance(imax-imin+1))
      green_common(iinfo)%distance =       & 
        tmpgreen%distance(imin:imax)
      green_common(iinfo)%distance(1) =    & 
        (3/4.*info(iinfo)%distance%start+  & 
        green_common(iinfo)%distance(2)/4)
      green_common(iinfo)%distance(size(green_common(iinfo)%distance)) =      & 
        (3/4.*info(iinfo)%distance%stop+                                      & 
        green_common(iinfo)%distance(size(green_common(iinfo)%distance)-1)/4)

      allocate(green_common(iinfo)%start(size(green_common(iinfo)%distance)))
      allocate(green_common(iinfo)%stop(size(green_common(iinfo)%distance)))

      green_common(iinfo)%start=(green_common(iinfo)%distance)
      do i =1, size(green_common(iinfo)%distance)
        green_common(iinfo)%start(i)=(green_common(iinfo)%distance(i) + &
          green_common(iinfo)%distance(i-1) ) / 2.
        green_common(iinfo)%stop(i)=(green_common(iinfo)%distance(i) + &
          green_common(iinfo)%distance(i+1) ) / 2.
      enddo

      green_common(iinfo)%start(1)= info(iinfo)%distance%start
      green_common(iinfo)%stop(size(green_common(iinfo)%stop)) = &
        info(iinfo)%distance%stop
      deallocate(tmpgreen%distance)

      !@DS =/ 0
    else
      allocate(green_common(iinfo)%distance( &
        ceiling( &
        (info(iinfo)%distance%stop - info(iinfo)%distance%start) &
        /info(iinfo)%distance%step) &
        ))
      allocate(green_common(iinfo)%start(size(green_common(iinfo)%distance)))
      allocate(green_common(iinfo)%stop(size(green_common(iinfo)%distance)))

      green_common(iinfo)%start =                & 
        [                                        & 
        (info(iinfo)%distance%start +            & 
        (i-1)*info(iinfo)%distance%step,         & 
        i=1, size(green_common(iinfo)%distance)) & 
        ]

      green_common(iinfo)%stop = green_common(iinfo)%start(2:) 
      green_common(iinfo)%stop(ubound(green_common(iinfo)%stop)) = &
        info(iinfo)%distance%stop
      green_common(iinfo)%distance = &
        (green_common(iinfo)%stop + green_common(iinfo)%start)/2
    endif

    allocate(green_common(iinfo)%data(size(green_common(iinfo)%distance), size(green)))
    allocate(green_common(iinfo)%dataname(size(green)))

    do i = 1,  size(green_common(iinfo)%data, 2)
      call  spline_interpolation(          & 
        green(i)%distance,                 & 
        green(i)%data,                     & 
        size(green(i)%distance),           & 
        green_common(iinfo)%distance,      & 
        green_common(iinfo)%data(:, i),     & 
        size(green_common(iinfo)%distance) & 
        )
      where( &
          green_common(iinfo)%distance.gt.green(i)%distance(size(green(i)%distance)) &
          .or.green_common(iinfo)%distance.lt.green(i)%distance(1) &
          )
        green_common(iinfo)%data(:, i)=0
      end where
      green_common(iinfo)%dataname(i) = green(i)%dataname
    enddo
  enddo
end subroutine


! =============================================================================
!> Perform convolution
!!
!! \date 2013-03-15
!! \author M. Rajner
! =============================================================================
subroutine convolve(site, date)
  use mod_constants
  use iso_fortran_env
  use mod_site, only : site_info, local_pressure_distance
  use mod_cmdline
  use mod_utilities, &
    only: d2r, r2d, datanameunit, mmwater2pascal, countsubstring
  use mod_spherical
  use mod_data
  use mod_date, only : dateandmjd
  use mod_polygon
  use mod_printing
  use mod_normalization, only: green_normalization
  use mod_aggf, only: aggf
  use mod_atmosphere, only: &
    standard_pressure, standard_temperature, virtual_temperature
  use mod_3d

  type(site_info),  intent(in) :: site
  type(dateandmjd), intent(in), optional :: date

  integer  :: igreen, idist, iazimuth, nazimuth
  real(dp) :: azimuth, dazimuth
  real(dp) :: lat, lon, area, tot_area, tot_area_used
  real(dp) :: val(size(model)), old_val_sp, old_val_rsp
  integer  :: i, j, npoints, iheight, nheight
  integer(2) :: iok(size(polygon))

  real(dp) :: normalize, aux
  real(dp), allocatable, dimension(:) :: azimuths, &
    heights, pressures, temperatures
  logical :: header_p = .true.

  ! real(dp) :: h1,h2, v1,v2, p_int !temporary
  real(dp) :: rsp
  real(dp), dimension(:), allocatable :: result_partial

  logical :: first_reduction
  first_reduction=.true.


  if (transfer_sp%if) then
    if (ind%model%hp.eq.0) call print_warning("no @HP with -U", error=.true.)
    if (ind%model%h .eq.0) call print_warning("no @H  with -U", error=.true.)
  endif

  if(.not.allocated(green_common)) then
    call green_unification()
  endif

  val=0

  if (site%lp%if) then
    do i=1, size(site%lp%date)
      if(all(site%lp%date(i, 1:6).eq.date%date(1:6))) then
        val(ind%model%sp) = site%lp%data(i)
        exit
      endif
      val(ind%model%sp) = sqrt(-1.)
      if(i.eq.size(site%lp%date)) &
        call print_warning("date not found in @LP")
    enddo
  endif

  if (.not. allocated(result)) then
    if (any(green%dataname.eq."GE").and.inverted_barometer &
      .and. non_inverted_barometer) then
      allocate(result(size(green)+1))
    else
      allocate(result(size(green)))
    endif
  endif
  if(.not.allocated(result_partial)) allocate(result_partial(size(result)))

  npoints       = 0
  area          = 0
  tot_area      = 0
  tot_area_used = 0

  result=0
  rsp=0

  do igreen = 1, size(green_common)
    do idist = 1, size(green_common(igreen)%distance)
      if (allocated(azimuths)) deallocate (azimuths)
      if (info(igreen)%azimuth%step.eq.0) then
        nazimuth = &
          (info(igreen)%azimuth%stop-info(igreen)%azimuth%start)/360 * &
          max(int(360*sin(d2r(green_common(igreen)%distance(idist)))), 100) * &
          info(igreen)%azimuth%denser
        if (nazimuth.eq.0) nazimuth=1
        dazimuth= (info(igreen)%azimuth%stop-info(igreen)%azimuth%start)/nazimuth
      else
        dazimuth = info(igreen)%azimuth%step
        nazimuth= (info(igreen)%azimuth%stop-info(igreen)%azimuth%start)/dazimuth
      endif


      ! calculate area using spherical formulae
      area = spher_area(                        & 
        d2r(green_common(igreen)%start(idist)), & 
        d2r(green_common(igreen)%stop(idist)),  & 
        d2r(dazimuth),                          & 
        radius=earth%radius,                    & 
        alternative_method=.true.)

      ! normalization according to Merriam (1992) 
      normalize= 1e8 / &
        (green_normalization("m", psi = d2r(green_common(igreen)%distance(idist))))

      allocate(azimuths(nazimuth))
      azimuths = [(info(igreen)%azimuth%start + (i-1) * dazimuth, i= 1, nazimuth)] 

      do iazimuth  = 1, nazimuth
        azimuth = azimuths(iazimuth)

        npoints = npoints + 1
        tot_area=tot_area+area

        ! get lat and lon of point
        call spher_trig &
          (d2r(site%lat), d2r(site%lon), &
          d2r(green_common(igreen)%distance(idist)), d2r(azimuth), lat, lon, domain=.true.)

        ! read polygons
        if (ind%polygon%e.ne.0 .or. ind%polygon%n.ne.0) then
          do i =1, size(polygon)
            if (polygon(i)%if) then
              call chkgon (r2d(lon), r2d(lat), polygon(i), iok(i))
            endif
          enddo
        endif

        ! get LS
        if (ind%model%ls.ne.0.and.inverted_barometer) then
          call get_value ( & 
            model(ind%model%ls), r2d(lat), r2d(lon), val(ind%model%ls), & 
            level=1, method=info(igreen)%interpolation, date=date%date)
        endif

        if (iok(1).eq.1 & .and. int(val(ind%model%ls)).eq.1) then
          tot_area_used = tot_area_used +area
        endif

        ! GE, GN, ...
        if (any([&
          ind%green%gn,   ind%green%ge, ind%green%gg, &
          ind%green%gndt, ind%green%gnc, ind%green%gegdt, ind%green%g3d &
          ].ne.0) &
          ) then

          if ( &
            ind%model%sp.ne.0.and.(model(ind%model%sp)%if &
            .or.model(ind%model%sp)%if_constant_value) &
            ) then

            ! get SP
            if (.not.(site%lp%if                      &
              .and.green_common(igreen)%distance(idist) &
              .lt.local_pressure_distance)) then
              call get_value (                                              & 
                model(ind%model%sp), r2d(lat), r2d(lon), val(ind%model%sp), & 
                level=1,                                                    & 
                method = info(igreen)%interpolation,                        & 
                date=date%date)
            endif
            old_val_sp=val(ind%model%sp)

            if (.not.isnan(val(ind%model%sp))) then

              ! get RSP if given
              if (ind%model%rsp.ne.0) then
                call get_value (                                                & 
                  model(ind%model%rsp), r2d(lat), r2d(lon), val(ind%model%rsp), & 
                  level=1, method = info(igreen)%interpolation)
              endif
              old_val_rsp=val(ind%model%rsp)

              if(transfer_sp%if.and..not.all([ind%model%rsp, ind%model%hrsp].ne.0)) then
                call print_warning("@RSP or @HRSP with -U is missing", error=.true.)
              else
                call get_value ( & 
                  model(ind%model%hrsp), r2d(lat), r2d(lon), val(ind%model%hrsp), & 
                  level=1, method = info(igreen)%interpolation)
              endif

              ! get T
              if (ind%model%t.ne.0 &
                .and.( &
                transfer_sp%if &
                .or.any(([ &
                ind%green%gndt, &
                ind%green%gegdt, &
                ind%green%gnc, &
                ind%green%g3d &
                ]).ne.0) &
                ) &
                ) then
                call get_value ( & 
                  model(ind%model%t), r2d(lat), r2d(lon), val(ind%model%t), & 
                  level=1, method=info(igreen)%interpolation, date=date%date)
              endif

              ! get HP
              if (ind%model%hp.ne.0 &
                .and.( &
                transfer_sp%if &
                .or. ind%green%g3d.ne.0 &
                ) &
                ) then
                call get_value ( & 
                  model(ind%model%hp), r2d(lat), r2d(lon), val(ind%model%hp), & 
                  level=1, method = info(igreen)%interpolation)
              endif

              ! get H
              if (ind%model%h.ne.0 & 
                .and.(             & 
                transfer_sp%if     & 
                .or.any(([         & 
                ind%green%gndt,    & 
                ind%green%gndz,    & 
                ind%green%gndz2,   & 
                ind%green%gndh,    & 
                ind%green%gnc,     & 
                ind%green%g3d      & 
                ]).ne.0)           & 
                )                  & 
                ) then

                if (optimize.and.green_common(igreen)%distance(idist).gt.3) then
                  val(ind%model%h)=val(ind%model%hp)
                else
                  call get_value ( & 
                    model(ind%model%h), r2d(lat), r2d(lon), val(ind%model%h), & 
                    level=1, method = info(igreen)%interpolation)
                endif
              endif

              if (ind%model%sp.ne.0) then
                ! transfer SP if necessary on terrain
                if (transfer_sp%if & 
                  .and.any ([      & 
                  ind%green%ge,    & 
                  ind%green%gnc,   & 
                  ind%green%g3d,   & 
                  ind%green%gegdt, & 
                  ind%green%gg     & 
                  ].ne.0)          & 
                  ) then

                  val(ind%model%sp) = standard_pressure( & 
                    height=val(ind%model%h),             & 
                    h_zero=val(ind%model%hp),            & 
                    p_zero=old_val_sp,                   & 
                    method=transfer_sp%method,           & 
                    temperature=val(ind%model%t),        & 
                    use_standard_temperature             & 
                    = ind%model%t.eq.0,                  & 
                    nan_as_zero=.false.)

                  if(all([ind%model%rsp, ind%model%hrsp].ne.0)) then
                    val(ind%model%rsp) = standard_pressure(          & 
                      height=val(ind%model%h),                       & 
                      h_zero=val(ind%model%hrsp),                    & 
                      p_zero=old_val_rsp,                            & 
                      method=transfer_sp%method,                     & 
                      temperature=val(ind%model%t),                  & 
                      use_standard_temperature                       & 
                      = ind%model%t.eq.0,                            & 
                      nan_as_zero=.false.)
                  endif
                endif

                if(                                         &
                  ind%model%rsp.ne.0                        &
                  .and.green_common(igreen)%distance(idist) &
                  .ge.local_pressure_distance               &
                  ) then
                  val(ind%model%sp) = val(ind%model%sp) - val(ind%model%rsp)
                endif

                if (site%lp%if.and.first_reduction.and. ind%model%rsp.ne.0) then
                  val(ind%model%sp) = val(ind%model%sp) - val(ind%model%rsp)
                  first_reduction=.false.
                endif

                ! if the cell is not over sea and inverted barometer assumption was not set 
                ! and is not excluded by polygon
                if ((ind%polygon%e.ne.0.and.iok(ind%polygon%e).ne.0).or.(ind%polygon%e.eq.0)) then 
                  !IB or NIB
                  if (.not.(ind%model%ls.ne.0.and.inverted_barometer.and.int(val(ind%model%ls)).eq.0)) then
                    ! GE
                    if (ind%green%ge.ne.0) then
                      result(ind%green%ge) = result(ind%green%ge)        & 
                        + val(ind%model%sp)                              & 
                        * green_common(igreen)%data(idist, ind%green%ge) & 
                        * area * normalize
                    endif

                    ! GEGdt pressure part from Guo 2004
                    if (ind%green%gegdt.ne.0) then
                      result(ind%green%gegdt) = result(ind%green%gegdt) +   & 
                        val(ind%model%sp) *                                 & 
                        val(ind%model%t) * 1e-4 *                           & 
                        green_common(igreen)%data(idist, ind%green%gegdt) * & 
                        area * normalize
                    endif

                    ! GG
                    if (ind%green%gg.ne.0) then
                      aux = mmwater2pascal(val(ind%model%sp), inverted=.true.) & 
                        * area/ (d2r(green_common(igreen)%distance(idist)) *   & 
                        earth%radius*1e18)

                      result(ind%green%gg) = result(ind%green%gg) +      & 
                        green_common(igreen)%data(idist, ind%green%gg) * & 
                        aux * 1e8 ! m s-2 -> microGal
                    endif
                  endif

                  ! ! GE NIB if both IB and NIB wanted
                  if (inverted_barometer.and.non_inverted_barometer) then
                    if (ind%green%ge.ne.0) then
                      result(ubound(result)) = result(ubound(result)) +        & 
                        val(ind%model%sp) *                              & 
                        green_common(igreen)%data(idist, ind%green%ge) * & 
                        area * normalize
                    endif
                  endif
                endif

                if (                                                     & 
                  (ind%polygon%n.ne.0.and.iok(ind%polygon%n).ne.0)     & 
                  .or.(ind%polygon%n.eq.0)                             & 
                  ) then

                  !3D 
                  if (method(3)) then

                    ! if distance%stop_3d was set restrict computation of 3D to this distance
                    if(green_common(igreen)%distance(idist).lt.info(igreen)%distance%stop_3d) then

                      if (ind%model%rsp.eq.0) then
                        call print_warning("3D but no RSP", error=.true.)
                      endif

                      if (allocated(heights))      deallocate(heights)
                      if (allocated(pressures))    deallocate(pressures)
                      if (allocated(temperatures)) deallocate(temperatures)

                      nheight= &
                        ceiling((info(igreen)%height%stop &
                        -max(info(igreen)%height%start,val(ind%model%h))) &
                        /info(igreen)%height%step)

                      allocate(heights(nheight))
                      allocate(pressures(nheight))
                      allocate(temperatures(nheight))

                      do iheight=1, nheight
                        heights(iheight)=max(info(igreen)%height%start, val(ind%model%h)) &
                          +(iheight-0.5)*info(igreen)%height%step
                      enddo

                      if (.not.allocated(level%height))      allocate (level%height(size(level%level)))
                      if (.not.allocated(level%temperature)) allocate (level%temperature(size(level%level)))
                      if (.not.allocated(level%humidity))    allocate (level%humidity(size(level%level)))

                      do i=1,size(level%level)
                        call get_value (                                                             & 
                          model(ind%model%gp), r2d(lat), r2d(lon), level%height(i),                  & 
                          level=level%level(i), method = info(igreen)%interpolation, date=date%date)

                        if (ind%model%vt.ne.0) then
                          call get_value (                           & 
                            model(ind%model%vt), r2d(lat), r2d(lon), & 
                            val    = level%temperature(i),           & 
                            level  = level%level(i),                 & 
                            method = info(igreen)%interpolation,     & 
                            date   = date%date                       & 
                            )
                        endif

                        if (ind%model%vsh.ne.0) then
                          call get_value (                            & 
                            model(ind%model%vsh), r2d(lat), r2d(lon), & 
                            val    = level%humidity(i),               & 
                            level  = level%level(i),                  & 
                            method = info(igreen)%interpolation,      & 
                            date   = date%date                        & 
                            )

                          if (.not.isnan(level%humidity(i))) then
                            level%temperature(i)= &
                              virtual_temperature(level%temperature(i),level%humidity(i))
                          endif

                        endif
                      enddo

                      i=1
                      do while(level%height(i).lt.heights(1).and.i.ne.size(level%level))
                        i=i+1
                      end do

                      do iheight=1, nheight

                        if (iheight.eq.1) then
                          ! h1=val(ind%model%h)
                          ! v1=val(ind%model%sp)+val(ind%model%rsp)
                          ! h2=level%height(i)
                          ! v2=1.e2*dble(level%level(i))

                          temperatures(iheight)= &
                            level%temperature(i)-6.5e-3*(val(ind%model%h)-val(ind%model%hp))

                          if (.not.isnan(level%humidity(1))) then
                            val(ind%model%t) = &
                              virtual_temperature(val(ind%model%t), level%humidity(1))
                          endif

                          pressures(iheight) = standard_pressure(        & 
                            heights(iheight),                            & 
                            p_zero=val(ind%model%sp)+val(ind%model%rsp), & 
                            h_zero=val(ind%model%h), &
                            method="standard",                           & 
                            use_standard_temperature=.true.,             & 
                            temperature=val(ind%model%t)                 & 
                            )

                        else
                          do while(level%height(i+1).lt.heights(iheight).and. i.ne.size(level%level))
                            i=i+1
                          end do

                          ! temperature linear interpolation
                          if(i.lt.size(level%level)) then
                            temperatures(iheight)= &
                              level%temperature(i) &
                              + (level%temperature(i+1)-level%temperature(i)) &
                              /(level%height(i+1)-level%height(i))*(heights(iheight)-level%height(i))
                          else
                            temperatures(iheight)= &
                              level%temperature(i) 
                          endif

                          if(heights(iheight-1).lt.level%height(i).and.(heights(iheight).gt.level%height(i))) then 
                            ! h1=level%height(i)
                            ! v1=1.e2*dble(level%level(i))
                            ! h2=level%height(i+1)
                            ! v2=1.e2*dble(level%level(i+1))

                            pressures(iheight) =                                    & 
                              standard_pressure(                                    & 
                              height                   = heights(iheight),          & 
                              p_zero                   = 1.e2*dble(level%level(i)), & 
                              h_zero                   = level%height(i),           & 
                              method                   = "standard",                & 
                              use_standard_temperature = .true.,                    & 
                              temperature              = temperatures(iheight),     & 
                              nan_as_zero              = .true.                     & 
                              )

                          else

                            pressures(iheight)=                                 & 
                              standard_pressure(                                & 
                              height                   = heights(iheight),      & 
                              p_zero                   = pressures(iheight-1),  & 
                              h_zero                   = heights(iheight-1),    & 
                              method                   = "standard",            & 
                              use_standard_temperature = .true.,                & 
                              temperature              = temperatures(iheight), & 
                              nan_as_zero              = .true.                 & 
                              )

                          endif
                        endif

                        ! if (i.lt.size(level%level)) then
                        ! p_int=exp(dlog(v1) +(dlog(v2)-dlog(v1))*(heights(iheight)-h1)/(h2-h1))
                        ! if (p_int.gt.1e29)   p_int=0
                        ! pressures(iheight)=p_int
                        ! endif

                        if (method3d(1).or.green_common(igreen)%distance(idist).gt.method3d_refinment_distance) then
                          result(ind%green%g3d) = result(ind%green%g3d) &
                            + geometry(psi=d2r(green_common(igreen)%distance(idist)), h=site%height, z=heights(iheight)) &
                            * pressures(iheight)/(temperatures(iheight))  &
                            * area * info(igreen)%height%step &
                            *(-gravity%constant)*1e8/R_air

                        else if (method3d(2)) then
                          result(ind%green%g3d) =                            & 
                            result(ind%green%g3d)                            & 
                            + potential(                                     & 
                            psi1=d2r(green_common(igreen)%start(idist)),     & 
                            psi2=d2r(green_common(igreen)%stop(idist)),      & 
                            dazimuth=d2r(dazimuth),                          & 
                            h=site%height,                                   & 
                            z1= heights(iheight)-info(igreen)%height%step/2, & 
                            z2= heights(iheight)+info(igreen)%height%step/2  & 
                            )                                                & 
                            * pressures(iheight)/(temperatures(iheight))     & 
                            *(-gravity%constant)*1e8/R_air
                          if (isnan(result(ind%green%g3d)))  then
                            ! small distances can cause numerical problems
                            result(ind%green%g3d)=0
                          endif

                        else if (method3d(3)) then
                          result(ind%green%g3d) =                            & 
                            result(ind%green%g3d)                            & 
                            + cylinder(                                      & 
                            psi1=d2r(green_common(igreen)%start(idist)),     & 
                            psi2=d2r(green_common(igreen)%stop(idist)),      & 
                            dazimuth=d2r(dazimuth),                          & 
                            h=site%height,                                   & 
                            z1= heights(iheight)-info(igreen)%height%step/2, & 
                            z2= heights(iheight)+info(igreen)%height%step/2  & 
                            )                                                & 
                            * pressures(iheight)/(temperatures(iheight))     & 
                            *(-gravity%constant)*1e8/R_air
                        endif
                      enddo
                    endif
                  endif

                  !C before GN GNdt etc because it needs SP on H not on site 
                  if(ind%green%gnc.ne.0) then
                    if ( &
                      any ([ &
                      ind%model%sp, &
                      ind%model%hp, &
                      ind%model%h, &
                      ind%model%t &
                      ].eq.0)) &
                      call print_warning ("with @GNc you need to give @T @HP @H", error=.true.)
                    result(ind%green%gnc) = result(ind%green%gnc)  & 
                      + val(ind%model%sp)                        & 
                      * aggf(                                    & 
                      d2r(green_common(igreen)%distance(idist)), & 
                      zmin=val(ind%model%h),                     & 
                      t_zero=val(ind%model%t),                   & 
                      h=site%height,                             & 
                      dz= gnc_looseness*10. &
                      *merge(10._dp, &
                      merge(0.1_dp,1._dp, &
                      green_common(igreen)%distance(idist).le.1e-5_dp ), &
                      green_common(igreen)%distance(idist).ge.1e-2_dp ), &
                      method="standard",                         & 
                      predefined=.true.)                         & 
                      * area * normalize

                    if (.not.quiet) then
                      open(unit=output_unit, carriagecontrol='fortran')
                      call progress(                                                 & 
                        100*igreen*idist                                           & 
                        /(size(green_common(igreen)%distance)*size(green_common)), & 
                        every=1 &
                        )
                    endif
                  endif

                  ! transfer SP if necessary on site level
                  if (transfer_sp%if &
                    .and.any ([ &
                    ind%green%gn, &
                    ind%green%gndt, &
                    ind%green%gndz, &
                    ind%green%gndz2, &
                    ind%green%gndh &
                    ].ne.0) &
                    ) then
                    val(ind%model%sp) = standard_pressure( & 
                      height=site%height,                  & 
                      h_zero=val(ind%model%hp),            & 
                      p_zero=old_val_sp,                   & 
                      method=transfer_sp%method,           & 
                      temperature=val(ind%model%t),        & 
                      use_standard_temperature             & 
                      = ind%model%t.eq.0,                  & 
                      nan_as_zero=.false.)

                    if(all([ind%model%rsp, ind%model%hrsp].ne.0)) then
                      val(ind%model%rsp) = standard_pressure(          & 
                        height=site%height,                            & 
                        h_zero=val(ind%model%hrsp),                    & 
                        p_zero=old_val_rsp,                            & 
                        method=transfer_sp%method,                     & 
                        temperature=val(ind%model%t),                  & 
                        use_standard_temperature                       & 
                        = ind%model%t.eq.0,                            & 
                        nan_as_zero=.false.)
                    endif
                    if(ind%model%rsp.ne.0) val(ind%model%sp) = val(ind%model%sp) - val(ind%model%rsp)
                  endif

                  ! GN
                  if (ind%green%gn.ne.0) then
                    result_partial(ind%green%gn) = &
                      val(ind%model%sp) *                              & 
                      green_common(igreen)%data(idist, ind%green%gn) * & 
                      area * normalize
                    result(ind%green%gn) = &
                      result(ind%green%gn) + result_partial(ind%green%gn)
                  endif

                  ! GNdt
                  if (ind%green%gndt.ne.0) then
                    if (any(                                                & 
                      [ind%model%sp, ind%model%t, ind%model%rsp           & 
                      ].eq.0)) &
                      call print_warning("not enough data model for GNdt", &
                      error=.true.)
                    result_partial(ind%green%gndt) =       & 
                      val(ind%model%sp)                                    & 
                      * green_common(igreen)%data(idist, ind%green%gndt)   & 
                      * (val(ind%model%t)-atmosphere%temperature%standard) & 
                      *  area * normalize
                    result(ind%green%gndt) = result(ind%green%gndt) +       & 
                      result_partial(ind%green%gndt) 
                  endif

                  ! GNdh
                  if (ind%green%gndh.ne.0) then
                    if (any(                                                & 
                      [ &
                      ind%model%sp, ind%model%h, ind%model%rsp           & 
                      ].eq.0)) &
                      call print_warning("not enough data model for GNdh", &
                      error=.true.)
                    result_partial(ind%green%gndh) = &
                      val(ind%model%sp)                                    & 
                      * green_common(igreen)%data(idist, ind%green%gndh)   & 
                      * (val(ind%model%h)-site%height) & 
                      *  area * normalize
                    result(ind%green%gndh) = result(ind%green%gndh) +       & 
                      result_partial(ind%green%gndh)
                  endif

                  ! GNdz
                  if (ind%green%gndz.ne.0) then
                    if (any(                                                & 
                      [ &
                      ind%model%sp, ind%model%h, ind%model%rsp           & 
                      ].eq.0)) &
                      call print_warning("not enough data model for GNdz", &
                      error=.true.)
                    result_partial(ind%green%gndz) =  +       & 
                      val(ind%model%sp)                                    & 
                      * green_common(igreen)%data(idist, ind%green%gndz)   & 
                      * (val(ind%model%h)-site%height) & 
                      *  area * normalize
                    result(ind%green%gndz) = result(ind%green%gndz) +       & 
                      result_partial(ind%green%gndz)
                  endif


                  ! GNdz2
                  if (ind%green%gndz2.ne.0) then
                    if (any(                                                & 
                      [ &
                      ind%model%sp, ind%model%h, ind%model%rsp           & 
                      ].eq.0)) &
                      call print_warning("not enough data model for GNdz2", &
                      error=.true.)
                    result_partial(ind%green%gndz2) =                   & 
                      val(ind%model%sp)                                                & 
                      * green_common(igreen)%data(idist, ind%green%gndz2)              & 
                      * ( (val(ind%model%h)-site%height)                               & 
                      /(earth%radius * d2r(green_common(igreen)%distance(idist))) )**2 & 
                      *  area * normalize
                    result(ind%green%gndz2) = result(ind%green%gndz2) +                  & 
                      result_partial(ind%green%gndz2)
                  endif

                  ! reference 2D for 3D method
                  if (ind%green%g3d.ne.0) then
                    if(green_common(igreen)%distance(idist).lt.info(igreen)%distance%stop_3d) then
                      rsp = rsp+ &
                        val(ind%model%rsp) *                              & 
                        green_common(igreen)%data(idist, ind%green%gn) * & 
                        area * normalize
                    else
                      result(ind%green%g3d) = result(ind%green%g3d) + & 
                        sum(result_partial, &
                        mask=( &
                        green%dataname.eq."GN" &
                        .or.green%dataname.eq."GNdt" &
                        .or.green%dataname.eq."GNdz" &
                        .or.green%dataname.eq."GNdz2" &
                        .or.green%dataname.eq."GNdh" &
                        ))
                    endif
                  endif

                endif
              endif
            else
              result=sqrt(-1.)
            endif
          elseif(ind%model%ewt.eq.0) then
            call print_warning("@SP is required with -M2D -G", error=.true.)
          endif
        endif

        ! surface loads from EWT
        if (                                                                                          & 
          ind%green%gr.ne.0                                                                         & 
          .or.ind%green%ghn.ne.0                                                                    & 
          .or.ind%green%ghe.ne.0                                                                    & 
          ) then
          if ((ind%polygon%e.ne.0.and.iok(ind%polygon%e).ne.0).or.(ind%polygon%e.eq.0)) then
            if (.not.(ind%model%ls.ne.0.and.inverted_barometer.and.int(val(ind%model%ls)).eq.0)) then
              call get_value (                                                                        & 
                model(ind%model%ewt), r2d(lat), r2d(lon), val(ind%model%ewt),                       & 
                level=1, method = info(igreen)%interpolation, date=date%date)
              aux = (val(ind%model%ewt))  *                                                           & 
                area/d2r(green_common(igreen)%distance(idist)) *                                    & 
                1./earth%radius/1e12* 1e3 ! m -> mm
              if (isnan(aux)) aux = 0
              if (ind%green%gr.ne.0) then
                result(ind%green%gr) = result(ind%green%gr) +       & 
                  green_common(igreen)%data(idist, ind%green%gr) & 
                  * aux

                if (ind%green%ghn.ne.0) then
                  result(ind%green%ghn) = result(ind%green%ghn) +      & 
                    green_common(igreen)%data(idist, ind%green%ghn) * & 
                    aux * (-cos(d2r(azimuth)))
                endif
                if (ind%green%ghe.ne.0) then
                  result(ind%green%ghe) = result(ind%green%ghe) +      & 
                    green_common(igreen)%data(idist, ind%green%ghe) * & 
                    aux * (-sin(d2r(azimuth)))
                endif
              endif
            endif
          endif
        endif

        ! moreverbose point: -L@p
        if(ind%moreverbose%p.ne.0) then
          if (header_p.and. output%header) then
            if(size(green_common).gt.1) &
              write(moreverbose(ind%moreverbose%p)%unit, "(a2, x$)") "i"

            write(moreverbose(ind%moreverbose%p)%unit, & 
              '(a8, 8a13, $)')                         & 
              "name", "lat", "lon",                  & 
              "distance", "azimuth",                 & 
              "lat", "lon",                          & 
              "area", "totarea"

            if (result_component) then
              write(moreverbose(ind%moreverbose%p)%unit, & 
                '(a13, $)')                & 
                (trim(green(i)%dataname), & 
                i=lbound(green, 1),       & 
                ubound(green, 1)          & 
                )
            endif

            if (result_total) then
              if (method(2)) then
                write(moreverbose(ind%moreverbose%p)%unit, & 
                  '(a13, $)') "G2D_t" 
              endif
              if (method(3)) then
                write(moreverbose(ind%moreverbose%p)%unit, & 
                  '(a13, $)') "G3D_t" 
              endif
            endif

            if (.not.moreverbose(ind%moreverbose%p)%sparse) then
              write(moreverbose(ind%moreverbose%p)%unit,                       & 
                '(<size(model)>a12)', advance='no' )                         & 
                (trim(model(i)%dataname), i=lbound(model, 1), ubound(model, 1))
            endif

            if (size(iok).gt.0) then
              write(moreverbose(ind%moreverbose%p)%unit, & 
                '(<size(iok)>(a3, i1))'),               & 
                ("ok", i, i =1, ubound(iok, 1))
            else
              write(moreverbose(ind%moreverbose%p)%unit, *)
            endif
            header_p=.false.
          endif
          if (                                              & 
            .not.moreverbose(ind%moreverbose%p)%sparse    & 
            .or.                                          & 
            (moreverbose(ind%moreverbose%p)%sparse        & 
            .and.(azimuth==azimuths(ubound(azimuths, 1))) & 
            )                                             & 
            ) then

            if(size(green_common).gt.1) &
              write(moreverbose(ind%moreverbose%p)%unit, "(i2, x$)") igreen

            write(moreverbose(ind%moreverbose%p)%unit,         & 
              '(a8, 6' // output%form //',2 en13.3, $)'),       & 
              site%name, site%lat, site%lon,                 & 
              green_common(igreen)%distance(idist), azimuth, & 
              r2d(lat), r2d(lon), area, tot_area

            if (result_component)                          & 
              write(moreverbose(ind%moreverbose%p)%unit, & 
              '(' // output%form //'$)'),                & 
              (result(i), i =1, size(result))

            if (result_total) then
              if (method(2)) then
                write(moreverbose(ind%moreverbose%p)%unit, &
                  '(' // output%form //'$)'), &
                  sum(result, &
                  mask=( &
                  green%dataname.eq."GN" &
                  .or.green%dataname.eq."GE" &
                  .or.green%dataname.eq."GNdt" &
                  .or.green%dataname.eq."GNdz" &
                  .or.green%dataname.eq."GNdz2" &
                  .or.green%dataname.eq."GNdh" &
                  ))
              endif
              if (method(3)) then
                write(moreverbose(ind%moreverbose%p)%unit, &
                  '(' // output%form //'$)'), &
                  sum(result, &
                  mask=( &
                  green%dataname.eq."G3D" &
                  .or.green%dataname.eq."GE" &
                  ))
              endif

            endif
            if (.not.moreverbose(ind%moreverbose%p)%sparse) then
              do i=1, size(val)
                call get_value (                          & 
                  model(i), r2d(lat), r2d(lon), val(i), & 
                  level=1,                              & 
                  method = info(igreen)%interpolation,  & 
                  date=date%date)
              enddo
              write(moreverbose(ind%moreverbose%p)%unit, & 
                '(<size(model)>en12.2, $)') val
            endif
            if (size(iok).gt.0) then
              write(moreverbose(ind%moreverbose%p)%unit, & 
                '(<size(iok)>(i4))'), iok
            else
              write(moreverbose(ind%moreverbose%p)%unit, * )
            endif
          endif
        endif

        ! moreverbose auxilary to draw: -L@a
        if(ind%moreverbose%a.ne.0) then
          call printmoreverbose (                                        & 
            d2r(site%lat), d2r(site%lon), d2r(azimuth), d2r(dazimuth), & 
            d2r(green_common(igreen)%start(idist)),                    & 
            d2r(green_common(igreen)%stop(idist))                      & 
            )
        endif
      enddo
    enddo
  enddo

  if (ind%green%g3d.ne.0) result(ind%green%g3d)=result(ind%green%g3d)-rsp

  ! results to output
  if (result_component) write (output%unit, "(" // output%form // '$)') result
  if (result_total) then
    if (method(2)) then
      write(output%unit, &
        '(' // output%form //'$)'), &
        sum(result, &
        mask=( &
        green%dataname.eq."GN" &
        .or.green%dataname.eq."GE" &
        .or.green%dataname.eq."GNdt" &
        .or.green%dataname.eq."GNdz" &
        .or.green%dataname.eq."GNdz2" &
        .or.green%dataname.eq."GNdh" &
        ))
    endif
    if (method(3)) then
      write(output%unit, &
        '(' // output%form //'$)'), &
        sum(result, &
        mask=( &
        green%dataname.eq."G3D" &
        .or.green%dataname.eq."GE" &
        ))
    endif
  endif

  ! summary: -L@s
  if (ind%moreverbose%s.ne.0) then
    if (output%header) write(moreverbose(ind%moreverbose%s)%unit, '(2a8, 3a12)' ) &
      "station", "npoints", "area", "area/R2", "t_area_used"
    write(moreverbose(ind%moreverbose%s)%unit, '(a8, i8, 3en12.2)') &
      site%name, npoints, tot_area, tot_area/earth%radius**2, tot_area_used
  endif

  ! green values : -L@g
  if(ind%moreverbose%g.ne.0) then
    do i = 1, size(green_common)
      if (output%header) &
        write(moreverbose(ind%moreverbose%g)%unit, '(a3,100a14)') &
        "nr", "distance", "start", "stop", "data", "di(j)-di(j-1)"
      do j=1,size(green_common(i)%distance)
        write(moreverbose(ind%moreverbose%g)%unit, '(i3,f14.6, 100f14.7)'), &
          j, green_common(i)%distance(j), &
          green_common(i)%start(j), &
          green_common(i)%stop(j), &
          green_common(i)%data(j,:), &
          green_common(i)%distance(j)-green_common(i)%distance(j-1)
      enddo
    enddo
  endif
end subroutine

! =============================================================================
!> returns lat and lon of spherical trapezoid
!! \date 2013.07.03
!! \author Marcin Rajner
! =============================================================================
subroutine printmoreverbose (latin, lonin, azimuth, azstep, distancestart, distancestop)
  use mod_spherical, only : spher_trig
  use mod_cmdline,   only : moreverbose, ind
  use mod_utilities, only : r2d

  real(dp), intent(in) :: azimuth, azstep, latin, lonin
  real(dp) ::  lat, lon, distancestart, distancestop

  call spher_trig (latin, lonin, distancestart, azimuth - azstep/2, lat, lon)
  write(moreverbose(ind%moreverbose%a)%unit, '(8f12.6)'), r2d(lat), r2d(lon) 
  call spher_trig (latin, lonin, distancestop, azimuth - azstep/2, lat, lon)
  write(moreverbose(ind%moreverbose%a)%unit, '(8f12.6)'), r2d(lat), r2d(lon)
  call spher_trig (latin, lonin, distancestop, azimuth + azstep/2, lat, lon)
  write(moreverbose(ind%moreverbose%a)%unit, '(8f12.6)'), r2d(lat), r2d(lon)
  call spher_trig (latin, lonin, distancestart, azimuth + azstep/2, lat, lon)
  write(moreverbose(ind%moreverbose%a)%unit, '(8f12.6)'), r2d(lat), r2d(lon)
  write(moreverbose(ind%moreverbose%a)%unit, '(">")')
end subroutine

! =============================================================================
!! \date 2013-07-02
!! \author M. Rajner
!! \warning input spherical distance in radian
!! 
!! method:
!!   default see equation in Rajnerdr
!!   spotl   see \cite spotl manual
!!   olssson see \cite olsson2009
!! =============================================================================
function green_newtonian (psi, h, z, method)
  use mod_constants, only: earth, gravity
  use mod_normalization, only: green_normalization
  real(dp) :: green_newtonian
  real(dp), intent (in) :: psi
  real(dp), intent (in), optional :: h
  real(dp), intent (in), optional :: z
  character(*), optional :: method
  real(dp) :: h_, z_, eps, t
  if (present(h)) then
    h_=h
  else
    h_=0.
  endif
  if (present(z)) then
    z_=z
  else
    z_=0.
  endif
  if (present(method) &
    .and. (method.eq."spotl" .or. method.eq."olsson")) then
    if(method.eq."spotl") then
      eps = h_/ earth%radius
      green_newtonian =                                      & 
        1. /earth%radius**2                                  & 
        *(eps + 2. * (sin(psi/2.))**2 )                      & 
        /((4.*(1.+eps)* (sin(psi/2.))**2 + eps**2)**(3./2.)) & 
        * gravity%constant                                   & 
        * green_normalization("f",psi=psi)
      return
    else if (method.eq."olsson") then
      t = earth%radius/(earth%radius +h_)
      green_newtonian =                      & 
        1 / earth%radius**2 * t**2 *         & 
        (1. - t * cos (psi) ) /              & 
        ( (1-2*t*cos(psi) +t**2 )**(3./2.) ) & 
        * gravity%constant                   & 
        * green_normalization("f",psi=psi)
      return
    endif
  else
    green_newtonian =                                                 & 
      ((earth%radius + h_) - (earth%radius + z_) * cos(psi))        & 
      / ((earth%radius + h_)**2 + (earth%radius + z_)**2            & 
      -2*(earth%radius + h_)*(earth%radius + z_)*cos(psi))**(3./2.)

    green_newtonian = green_newtonian &
      * gravity%constant / earth%gravity%mean  * green_normalization("m", psi=psi)
    return
  endif
end function
end module
