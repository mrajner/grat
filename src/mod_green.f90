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
    integer,dimension(2) :: column
    character(10), dimension(2) :: columndataname
    real(dp),allocatable,dimension(:) :: distance
    real(dp),allocatable,dimension(:) :: data
  end type
  type(green_functions), allocatable , dimension(:) :: green

  real(dp) , allocatable, dimension(:) :: result

  type green_common_info
    real(dp),allocatable,dimension(:) :: distance
    real(dp),allocatable,dimension(:) :: start
    real(dp),allocatable,dimension(:) :: stop
    real(dp),allocatable,dimension(:,:) :: data
    character (len=25), allocatable, dimension(:) :: dataname
    logical ,allocatable,dimension(:) :: elastic
  end type
  type(green_common_info), allocatable, dimension(:) :: green_common

contains
! =============================================================================
!> This subroutine parse -G option -- Greens function.
!!
!! This subroutines takes the -G argument specified as follows:
!!   -G 
!!
!! \author M. Rajner
!! \date 2013-03-06
! =============================================================================
subroutine parse_green (cmd_line_entry)
  use mod_utilities, only: file_exists, is_numeric
  use mod_cmdline
  use mod_printing
  type (cmd_line_arg)  :: cmd_line_entry
  integer :: i , ii 

  if (allocated(green)) then
    call print_warning ("repeated")
    return
  endif

  allocate (green (size(cmd_line_entry%field)))
  do i = 1 , size(cmd_line_entry%field)
    write(log%unit, form%i2) trim(cmd_line_entry%field(i)%full)
    green(i)%name = cmd_line_entry%field(i)%subfield(1)%name
    if (i.gt.1.and.cmd_line_entry%field(i)%subfield(1)%name.eq."") then
      green(i)%name = green(i-1)%name
    endif
    if (cmd_line_entry%field(i)%subfield(1)%dataname.eq."") then
      green(i)%dataname = "NN"
    else
      green(i)%dataname = cmd_line_entry%field(i)%subfield(1)%dataname
    endif
    do ii=1,2
      green(i)%column(ii) =green(i-1)%column(ii)
      green(i)%columndataname(ii) = green(i-1)%columndataname(ii) 
      if(is_numeric (cmd_line_entry%field(i)%subfield(ii+1)%name ) ) then
        read(cmd_line_entry%field(i)%subfield(ii+1)%name, *) green(i)%column(ii)
        green(i)%columndataname(ii) = cmd_line_entry%field(i)%subfield(ii+1)%dataname
      endif
    enddo
    call read_green(green(i))
  enddo
end subroutine

! =============================================================================
!> This subroutine read  green file
! =============================================================================
subroutine read_green (green)
  use mod_utilities, only: file_exists, skip_header, r2d
  use iso_fortran_env
  use mod_printing
  use mod_constants, only:earth

  integer :: lines , fileunit, io_status, i
  real (dp) , allocatable , dimension(:) :: tmp
  type(green_functions) :: green

  ! change the paths accordingly
  if (.not.file_exists(green%name) &
    .and. (.not. green%name.eq."merriam" &
    .and.  .not. green%name.eq."huang" &
    .and.  .not. green%name.eq."rajner" )) then
    green%name="merriam"
  endif
  if (green%name.eq."merriam") then
    green%name="/home/mrajner/src/grat/dat/merriam_green.dat"
    if (green%dataname.eq."GN") then
      green%column=[1,2]
    else if &
      (green%dataname.eq."GNdt") then
      green%column=[1,3]
    else if &
      (green%dataname.eq."GNdz") then
      green%column=[1,4]
    else if &
      (green%dataname.eq."GNdz2") then
      green%column=[1,5]
    else if &
      (green%dataname.eq."GE") then
      green%column=[1,6]
    else
      green%column=[1,2]
    endif
  else if (green%name.eq."huang") then
    green%name="/home/mrajner/src/grat/dat/huang_green.dat"
    if (green%dataname.eq."GN") then
      green%column=[1,2]
    else if &
      (green%dataname.eq."GNdt") then
      green%column=[1,3]
    else if &
      (green%dataname.eq."GNdh") then
      green%column=[1,4]
    else if &
      (green%dataname.eq."GNdz") then
      green%column=[1,5]
    endif
  else if (green%name.eq."rajner") then
    green%name="/home/mrajner/src/grat/dat/rajner_green.dat"
    if (green%dataname.eq."GN") then
      green%column=[1,2]
    else if &
      (green%dataname.eq."GNdt") then
      green%column=[1,3]
    else if &
      (green%dataname.eq."GNdh") then
      green%column=[1,4]
    else if &
      (green%dataname.eq."GNdz") then
      green%column=[1,5]
    endif
  endif

  if(green%column(1).ne.0 .and. green%column(2).ne.0) then
    allocate(tmp(max(green%column(1),green%column(2))))
    lines = 0
    open (newunit =fileunit, file=green%name, action="read", status="old")
    do 
      call skip_header (fileunit)
      read (fileunit , * , iostat = io_status) tmp
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
      read (fileunit , * , iostat = io_status) tmp
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
  if (green%name.eq."/home/mrajner/src/grat/dat/merriam_green.dat".and. green%dataname.eq."GNdz") then
    green%data = green%data * (-1.)
  endif
  if (green%name.eq."/home/mrajner/src/grat/dat/huang_green.dat" .and. &
    (green%dataname.eq."GNdh".or.green%dataname.eq."GNdh")) &
    then
    green%data = green%data * 1000.
  endif

  write(log%unit, form_63) trim(green%name), trim(green%dataname), &
    "columns:",green%column ,&
    "lines:", size(green%distance)

  if (green%columndataname(1).eq."R") then
    green%distance=(/ (r2d(green%distance(i)), i=1,size(green%distance)) /)
    write(log%unit, form_63) "conversion: radians --> to degrees"
  endif
  if (green%columndataname(2).eq."aplo") then
    ! need some proof ?
    green%data=green%data  / (earth%radius)*1e12 * earth%gravity%mean
    write(log%unit, form_63) "conversion: aplo --> to gotic"
  endif

end subroutine

! =============================================================================
!> Unification:
! =============================================================================
subroutine green_unification ()
  use mod_utilities, only: size_ntimes_denser, spline_interpolation
  use mod_cmdline, only: info, moreverbose, ind
  type(green_functions) :: tmpgreen
  integer :: i , denser , iinfo , imin, imax , j, ii
  integer , allocatable, dimension(:):: which_green , tmp
  integer :: ndenser=10
  integer :: n
  real(dp) , allocatable, dimension(:):: b, c, d

  allocate (green_common(size(info)))
  allocate (which_green(size(info)))
  allocate (tmp(size(green)))
  do iinfo=1,size(info)
    if (info(iinfo)%distance%step.eq.0) then
      do i = 1, size(green)
        tmp(i)= count(                                         & 
          green(i)%distance.le.info(iinfo)%distance%stop       & 
          .and.green(i)%distance.ge.info(iinfo)%distance%start & 
          ) 
      enddo
      which_green(iinfo) = maxloc(tmp,1)

      imin=minloc( & 
        abs(green(which_green(iinfo))%distance - info(iinfo)%distance%start),1)-1 
      imax=minloc( &
        abs(green(which_green(iinfo))%distance - info(iinfo)%distance%stop),1)+1

      if (imin.lt.1) imin = 1
      if( imax.gt.size(green(which_green(iinfo))%distance)) then
        imax = size(green(which_green(iinfo))%distance)
      endif

      allocate(tmpgreen%distance(                                   & 
        size_ntimes_denser(imax-imin+1,info(iinfo)%distance%denser) & 
        ))
      do ii = 1, imax - imin
        do j = 1, info(iinfo)%distance%denser
          tmpgreen%distance((ii-1)*info(iinfo)%distance%denser+j) = & 
            green(which_green(iinfo))%distance(imin+ii-1)           & 
            +(j-1)*(green(which_green(iinfo))%distance(imin+ii)     & 
            -green(which_green(iinfo))%distance(imin+ii-1))         & 
            /info(iinfo)%distance%denser
        enddo
      enddo

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
      do i =1 , size(green_common(iinfo)%distance)
        green_common(iinfo)%start(i)=(green_common(iinfo)%distance(i) + &
          green_common(iinfo)%distance(i-1) ) / 2.
        green_common(iinfo)%stop(i)=(green_common(iinfo)%distance(i) + &
          green_common(iinfo)%distance(i+1) ) / 2.
      enddo

      green_common(iinfo)%start(1)= info(iinfo)%distance%start
      green_common(iinfo)%stop(size(green_common(iinfo)%stop)) = &
        info(iinfo)%distance%stop
      deallocate(tmpgreen%distance)
    else
      allocate(green_common(iinfo)%distance( &
        ceiling( &
        (info(iinfo)%distance%stop - info(iinfo)%distance%start) &
        /info(iinfo)%distance%step) &
        ))
      allocate(green_common(iinfo)%start(size(green_common(iinfo)%distance)))
      allocate(green_common(iinfo)%stop(size(green_common(iinfo)%distance)))

      green_common(iinfo)%start = &
        [(info(iinfo)%distance%start + &
        (i-1)*info(iinfo)%distance%step, &
        i=1,size(green_common(iinfo)%distance)) ]
      green_common(iinfo)%stop = green_common(iinfo)%start(2:) 
      green_common(iinfo)%stop(ubound(green_common(iinfo)%stop)) = info(iinfo)%distance%stop
      green_common(iinfo)%distance = &
        (green_common(iinfo)%stop + green_common(iinfo)%start)/2
    endif

    allocate(green_common(iinfo)%data(size(green_common(iinfo)%distance),size(green)))
    allocate(green_common(iinfo)%dataname(size(green)))

    do i = 1 ,  size(green_common(iinfo)%data,2)
      call  spline_interpolation(          & 
        green(i)%distance,                 & 
        green(i)%data,                     & 
        size(green(i)%distance),           & 
        green_common(iinfo)%distance,      & 
        green_common(iinfo)%data(:,i),     & 
        size(green_common(iinfo)%distance) & 
        )
      where( &
          green_common(iinfo)%distance.gt.green(i)%distance(size(green(i)%distance)) &
          .or.green_common(iinfo)%distance.lt.green(i)%distance(1) &
          )
        green_common(iinfo)%data(:,i)=0
      end where
      green_common(iinfo)%dataname(i) = green(i)%dataname
    enddo

    if(ind%moreverbose%g.ne.0) then
      do j = 1, size(green_common(iinfo)%distance)
        write(moreverbose(ind%moreverbose%g)%unit, '(i3,f14.6,100f14.7)'), &
          j, green_common(iinfo)%distance(j), &
          green_common(iinfo)%start(j), &
          green_common(iinfo)%stop(j), &
          green_common(iinfo)%data(j,:)
      enddo
    endif
  enddo
end subroutine

! =============================================================================
!> Perform convolution
!!
!! \date 2013-03-15
!! \author M. Rajner
! =============================================================================
subroutine convolve (site , date)
  use mod_constants
  use mod_site, only : site_info
  use mod_cmdline
  use mod_utilities, only: d2r, r2d, datanameunit, mmwater2pascal
  use mod_spherical
  use mod_data
  use mod_date, only : dateandmjd
  use mod_polygon
  use mod_printing
  type(site_info), intent(in) :: site
  type(dateandmjd),intent(in) , optional :: date

  integer  ::  ndenser , igreen , idist  , iazimuth , nazimuth
  integer :: imodel
  real(dp) :: azimuth ,dazimuth
  real(dp) :: lat , lon , area , tot_area  
  real(dp) :: val(size(model)) , ref_p
  integer :: i, j, npoints
  integer(2) :: iok(size(polygon))

  real(dp) :: normalize , aux
  real(dp), allocatable, dimension(:) :: azimuths
  logical :: header_p = .true. , tmp

  if(.not.allocated(green_common)) then
    call green_unification()
  endif

  if (.not. allocated(result)) allocate(result(size(green)))
  npoints  = 0
  area     = 0
  tot_area = 0

  result=0
  do igreen = 1 , size(green_common)
    do idist = 1 , size(green_common(igreen)%distance)
      if (allocated(azimuths)) deallocate (azimuths)
      if (info(igreen)%azimuth%step.eq.0) then
        nazimuth = &
          (info(igreen)%azimuth%stop-info(igreen)%azimuth%start)/360 * &
          max(int(360*sin(d2r(green_common(igreen)%distance(idist)))),100) * &
          info(igreen)%azimuth%denser
        if (nazimuth.eq.0) nazimuth=1
        dazimuth= (info(igreen)%azimuth%stop-info(igreen)%azimuth%start)/nazimuth
      else
        dazimuth = info(igreen)%azimuth%step
        nazimuth= (info(igreen)%azimuth%stop-info(igreen)%azimuth%start)/dazimuth
      endif

      allocate(azimuths(nazimuth))
      azimuths = [ (info(igreen)%azimuth%start + (i-1) * dazimuth , i =1, nazimuth)] 

      do iazimuth  = 1 , nazimuth
        npoints = npoints + 1
        azimuth = (iazimuth - 1) * dazimuth
        azimuth = azimuths(iazimuth)

        ! get lat and lon of point
        call spher_trig &
          (d2r(site%lat), d2r(site%lon), &
          d2r(green_common(igreen)%distance(idist)), d2r(azimuth), lat, lon, domain=.true.)

        ! read polygons
        if (ind%polygon%e.ne.0 .or. ind%polygon%n.ne.0) then
          do i =1,size(polygon)
            if (polygon(i)%if) then
              call chkgon (r2d(lon), r2d(lat) , polygon(i) , iok(i))
            endif
          enddo
        endif


        ! calculate area using spherical formulae
        area = spher_area(                        & 
          d2r(green_common(igreen)%start(idist)), & 
          d2r(green_common(igreen)%stop(idist)),  & 
          d2r(dazimuth),                          & 
          radius=earth%radius,                    & 
          alternative_method=.true.)

        tot_area=tot_area+ area

        ! get LS
        if (ind%model%ls.ne.0) then
          call get_value ( & 
            model(ind%model%ls), r2d(lat), r2d(lon), val(ind%model%ls), & 
            level=1, method = info(igreen)%interpolation)
        endif

        ! GE, GN, ...
        if (                    & 
          ind%green%gn.ne.0     & 
          .or.ind%green%ge.ne.0 & 
          .or.ind%green%gg.ne.0 & 
          ) then


          ! normalization according to Merriam (1992) 
          normalize= 1./ &
            (2.*pi*(1.-cos(d2r(dble(1.)))) * &
            d2r(green_common(igreen)%distance(idist)) * &
            1.e5 * &
            earth%radius**2 * &
            100) ! Pa into hPa

          ! get SP (and RP if given)
          if (ind%model%sp.ne.0) then
            call get_value ( & 
              model(ind%model%sp), r2d(lat), r2d(lon), val(ind%model%sp), & 
              level=1, method = info(igreen)%interpolation)
            if (ind%model%rsp.ne.0) then
              call get_value ( & 
                model(ind%model%rsp), r2d(lat), r2d(lon), val(ind%model%rsp), & 
                level=1, method = info(igreen)%interpolation)
              val(ind%model%sp) = val(ind%model%sp) - val(ind%model%rsp)
            endif
          endif

          ! get T
          if (ind%model%t.ne.0) then
            call get_value ( & 
              model(ind%model%t), r2d(lat), r2d(lon), val(ind%model%t), & 
              level=1, method = info(igreen)%interpolation)
          endif

          if ((ind%polygon%e.ne.0.and.iok(ind%polygon%e).ne.0).or.(ind%polygon%e.eq.0)) then 
            if (.not.(ind%model%ls.ne.0.and.inverted_barometer.and.val(ind%model%ls).eq.0)) then
              ! GE
              if (ind%green%ge.ne.0) then
                ! if the cell is not over sea and inverted barometer assumption was not set 
                ! and is not excluded by polygon
                result(ind%green%ge) = result(ind%green%ge) + & 
                  val(ind%model%sp) * &
                  green_common(igreen)%data(idist, ind%green%ge) * & 
                  area * normalize
              endif
              ! GG
              if (ind%green%gg.ne.0) then
                aux = mmwater2pascal(val(ind%model%sp),inverted=.true.) * & 
                  area/d2r(green_common(igreen)%distance(idist)) * & 
                  1./earth%radius/1e18 
                result(ind%green%gg) = result(ind%green%gg) + & 
                  green_common(igreen)%data(idist, ind%green%gg) * & 
                  aux * 1e8 ! m s-2 -> microGal
              endif
            endif
          endif

          if ((ind%polygon%n.ne.0.and.iok(ind%polygon%n).ne.0).or.(ind%polygon%n.eq.0)) then 
            ! GN
            if (ind%green%gn.ne.0) then
              result(ind%green%gn) = result(ind%green%gn) + & 
                val(ind%model%sp) *                                       & 
                green_common(igreen)%data(idist, ind%green%gn) *          & 
                area * normalize
            endif

            ! GNdt
            if (ind%green%gndt.ne.0) then
              result(ind%green%gndt) = result(ind%green%gndt) +    & 
                val(ind%model%sp) *                                & 
                green_common(igreen)%data(idist, ind%green%gndt) * & 
                area * normalize
            endif

          endif
        endif

        ! surface loads from EWT
        if ( &
          ind%green%gr.ne.0 &
          .or.ind%green%ghn.ne.0 &
          .or.ind%green%ghe.ne.0 &
          ) then
          if ((ind%polygon%e.ne.0.and.iok(ind%polygon%e).ne.0).or.(ind%polygon%e.eq.0)) then 
            call get_value (                                  & 
              model(ind%model%ewt), r2d(lat), r2d(lon), val(ind%model%ewt), & 
              level=1, method = info(igreen)%interpolation)
            aux = (val(ind%model%ewt))  *                      & 
              area/d2r(green_common(igreen)%distance(idist)) * & 
              1./earth%radius/1e12
            if (ind%green%gr.ne.0) then
              if (.not.(ind%model%ls.ne.0.and.inverted_barometer.and.val(ind%model%ls).eq.0)) then
                result(ind%green%gr) = result(ind%green%gr) +     & 
                  green_common(igreen)%data(idist,ind%green%gr) * & 
                  aux * 1e3 ! m -> mm
              endif
              if (ind%green%ghn.ne.0) then
                result(ind%green%ghn) = result(ind%green%ghn) +    & 
                  green_common(igreen)%data(idist,ind%green%ghn) * & 
                  aux * - cos (d2r(azimuth))
              endif
              if (ind%green%ghe.ne.0) then
                result(ind%green%ghe) = result(ind%green%ghe) +    & 
                  green_common(igreen)%data(idist,ind%green%ghe) * & 
                  aux * - sin (d2r(azimuth))
              endif
            endif
          endif
        endif

        ! moreverbose point: -L@p
        if(ind%moreverbose%p.ne.0) then
          if (header_p.and. output%header) then
            write(moreverbose(ind%moreverbose%p)%unit,                                         & 
              '(a8,8a12,<size(result)>a12)' , advance='no' )                                   & 
              "name", "lat", "lon", "distance", "azimuth", "lat", "lon",                       & 
              "area", "totarea",  (trim(green(i)%dataname), i=lbound(green,1),ubound(green,1))
            if (.not.moreverbose(ind%moreverbose%p)%sparse) then
              write(moreverbose(ind%moreverbose%p)%unit,                     & 
                '(<size(model)>a12)' , advance='no' )                        & 
                (trim(model(i)%dataname), i=lbound(model,1),ubound(model,1))
            endif
            if (size(iok).gt.0) then
              write(moreverbose(ind%moreverbose%p)%unit ,             & 
                '(<size(iok)>(a3,i1))'), ("ok",i, i =1,ubound(iok,1))
            else
              write(moreverbose(ind%moreverbose%p)%unit , * )
            endif
            header_p=.false.
          endif
          if (                                           & 
            .not.moreverbose(ind%moreverbose%p)%sparse   & 
            .or.                                         & 
            (moreverbose(ind%moreverbose%p)%sparse       & 
            .and.(azimuth==azimuths(ubound(azimuths,1))) & 
            )                                            & 
            ) then
            write(moreverbose(ind%moreverbose%p)%unit ,                                     & 
              '(a8,6f12.6,2en12.2,<size(result)>en12.2,a)', advance = 'no'),                & 
              site%name, site%lat, site%lon, green_common(igreen)%distance(idist), azimuth, & 
              r2d(lat),r2d(lon), area, tot_area, result
            if (.not.moreverbose(ind%moreverbose%p)%sparse) then
              write(moreverbose(ind%moreverbose%p)%unit,     & 
                '(<size(model)>en12.2)' , advance='no' ) val
            endif
            if (size(iok).gt.0) then
              write(moreverbose(ind%moreverbose%p)%unit ,    & 
                '(<size(iok)>(i4))'), iok
            else
              write(moreverbose(ind%moreverbose%p)%unit , * )
            endif
          endif
        endif

        ! moreverbose auxilary to draw: -L@a
        if(ind%moreverbose%a.ne.0) then
          call printmoreverbose (                                      & 
            d2r(site%lat), d2r(site%lon), d2r(azimuth), d2r(dazimuth), & 
            d2r(green_common(igreen)%start(idist)),                    & 
            d2r(green_common(igreen)%stop(idist))                      & 
            )
        endif
      enddo
    enddo
  enddo 

  ! results to output
  if (present(date)) then
    write (output%unit, '(f15.3,x,i4.4,5(i2.2))', advance = "no" ) date%mjd, date%date 
  endif
  write (output%unit, '(a8,3f15.4,10en15.4)' ), site%name, site%lat, site%lon, site%height, result

  ! summury: -L@s
  if (ind%moreverbose%s.ne.0) then
    if (output%header) write(moreverbose(ind%moreverbose%s)%unit, '(2a8,2a12)' ) &
      "station", "npoints" ,"area" ,"area/R2"
    write(moreverbose(ind%moreverbose%s)%unit,'(a8,i8,2en12.2)') &
      site%name, npoints , tot_area, tot_area/earth%radius**2
  endif

end subroutine

subroutine printmoreverbose (latin, lonin, azimuth, azstep, distancestart, distancestop)
  use mod_spherical, only : spher_trig
  use mod_cmdline,   only : moreverbose, ind
  use mod_utilities, only : r2d

  real(dp), intent(in) :: azimuth ,azstep, latin, lonin
  real(dp) ::  lat , lon , distancestart ,distancestop

  call spher_trig (latin, lonin, distancestart, azimuth - azstep/2, lat, lon)
  write(moreverbose(ind%moreverbose%a)%unit, '(8f12.6)'), r2d(lat), r2d(lon) , r2d(latin), r2d(lonin)
  call spher_trig (latin, lonin, distancestop, azimuth - azstep/2, lat, lon)
  write(moreverbose(ind%moreverbose%a)%unit, '(8f12.6)'), r2d(lat), r2d(lon)
  call spher_trig (latin, lonin, distancestop, azimuth + azstep/2, lat, lon)
  write(moreverbose(ind%moreverbose%a)%unit, '(8f12.6)'), r2d(lat), r2d(lon)
  call spher_trig (latin, lonin, distancestart, azimuth + azstep/2, lat, lon)
  write(moreverbose(ind%moreverbose%a)%unit, '(8f12.6)'), r2d(lat), r2d(lon)
  write(moreverbose(ind%moreverbose%a)%unit, '(">")')
end subroutine


!! =============================================================================
!!> 
!!! chapter 4.1 of spotl manual \cite Agnew12
!!! 
!!! \date 2013-03-15
!!! \author M. Rajner
!! =============================================================================
!!subroutine integrated2pointmass(G_integrated,dist, delta, K, G_t)
!!  use mod_utilities, only: d2r 
!!  real(dp), intent (in) :: G_integrated, dist,delta
!!  integer , intent(in) :: K
!!  real(dp), intent(out) :: G_t
!!  real :: G_prim_t
!
!!  G_prim_t = G_integrated  / ( 4 *  cos( d2r(dist) / 2. ) * sin( d2r(delta) /4. ) )
!!  G_t = G_prim_t * ( ( 10.**K * a ) / ( a**2 * ( 2 *  sin (d2r(dist) /2  )/ d2r(dist)  ) ) )  
!  !/ ( 10.**K * a * d2r(dist) )
!!end subroutine
!
!!subroutine pointmass2integrated(G_t,dist, delta, K, G_integrated)
!!!  ! rozdział 4.1 spotlman
!!!  implicit none
!!!  real, intent (in) :: G_t, dist,delta
!!!  integer , intent(in) :: K
!!!  real, intent(out) :: G_integrated
!!!  real :: G_prim_t
!!
!!!  G_prim_t = G_t / ( ( 10.**K * a ) / ( a**2 * ( 2 *  sin (d2r(dist) /2  ) / d2r(dist)  ) ) )  
!!!  G_integrated = G_prim_t  * ( 4 *  cos( d2r(dist) / 2. ) * sin( d2r(delta) /4. ) )
!!!end subroutine

end module
