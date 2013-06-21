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

  real(dp) , allocatable, dimension(:,:) :: result

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
    green(i)%dataname = cmd_line_entry%field(i)%subfield(1)%dataname
    do ii=1,2
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
        ! todo why line below causes problems?
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
    green%data=green%data*earth%radius/ (earth%radius*earth%radius)*1e12 * 10
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
    do i = 1, size(green)
      tmp(i)= count( &
        green(i)%distance.le.info(iinfo)%distance%stop &
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

    allocate(tmpgreen%distance( &
      size_ntimes_denser(imax-imin+1,info(iinfo)%distance%denser) &
      ))
    do ii = 1, imax - imin
      do j = 1, info(iinfo)%distance%denser
        tmpgreen%distance((ii-1)*info(iinfo)%distance%denser+j) =  &
          green(which_green(iinfo))%distance(imin+ii-1) &
          +(j-1)*(green(which_green(iinfo))%distance(imin+ii) &
          -green(which_green(iinfo))%distance(imin+ii-1)) &
          /info(iinfo)%distance%denser
      enddo
    enddo

    tmpgreen%distance(size(tmpgreen%distance)) = &
      green(which_green(iinfo))%distance(imax)

    imin = count(tmpgreen%distance.le.info(iinfo)%distance%start) 
    imax = size(tmpgreen%distance) - &
      count(tmpgreen%distance.ge.info(iinfo)%distance%stop ) + 1

    allocate(green_common(iinfo)%distance(imax-imin+1))
    green_common(iinfo)%distance =  &
      tmpgreen%distance(imin:imax)
    green_common(iinfo)%distance(1) = &
      (3/4.*info(iinfo)%distance%start+ &
      green_common(iinfo)%distance(2)/4)
    green_common(iinfo)%distance(size(green_common(iinfo)%distance)) = &
      (3/4.*info(iinfo)%distance%stop+ &
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
    deallocate(tmpgreen%distance)
  enddo
end subroutine

! =============================================================================
!> Perform convolution
!!
!! \date 2013-03-15
!! \author M. Rajner
! =============================================================================
subroutine convolve (site ,  denserdist , denseraz)
  use mod_constants
  use mod_site, only : site_info
  use mod_cmdline
  use mod_utilities, only: d2r, r2d, datanameunit
  use mod_spherical
  use mod_data
  use mod_polygon
  use mod_printing
  type(site_info), intent(in) :: site
  integer , intent (in) , optional :: denserdist , denseraz

  integer  ::  ndenser , igreen , idist  , iazimuth , nazimuth
  integer :: imodel
  real(dp) :: azimuth ,dazimuth
  real(dp) :: lat , lon , area  
  real(dp) :: val(size(model)) , ref_p
  integer :: i,j , iok(size(polygon)) , npoints

  real(dp) :: normalize 

  real(dp), allocatable, dimension(:) :: azimuths

  if(.not.allocated(green_common)) then
    call green_unification()
  endif

  npoints = 0
  area = 0
  do igreen = 1 ,size(green_common)
    do idist = 1, size(green_common(igreen)%distance)
      if (allocated(azimuths)) deallocate (azimuths)
      nazimuth = &
        (info(igreen)%azimuth%stop-info(igreen)%azimuth%start)/360 * &
        max(int(360*sin(d2r(green_common(igreen)%distance(idist)))),100) * &
        info(igreen)%azimuth%denser
      if (nazimuth.eq.0) nazimuth=1
      dazimuth= (info(igreen)%azimuth%stop-info(igreen)%azimuth%start)/nazimuth

      allocate(azimuths(nazimuth))
      azimuths = [ (info(igreen)%azimuth%start + i * dazimuth - dazimuth/2 , i =1, nazimuth)] 

      do iazimuth  = 1 , nazimuth
        npoints = npoints + 1
        azimuth = (iazimuth - 1) * dazimuth

        ! get lat and lon of point
        call spher_trig &
          (d2r(site%lat), d2r(site%lon), &
          d2r(green_common(igreen)%distance(idist)) , d2r(azimuth) , lat , lon)

        ! get values of model
        do imodel = 1 , size(model)
          if(model(imodel)%if) then 
            call get_value (model(imodel) , r2d(lat) , r2d(lon) , val(imodel) , level=1 , method = info(igreen)%interpolation)
          else if (model(imodel)%if_constant_value)  then
            val(imodel) = model(imodel)%constant_value
          endif
        enddo

        do i =1,size(polygon)
          if (polygon(i)%if) then
            call chkgon (r2d(lon), r2d(lat) , polygon(i) , iok(i))
          else
            iok(i)=1
          endif
        enddo

        !moreverbose p option
        if(ind%moreverbose%p.ne.0) then
          write(moreverbose(ind%moreverbose%p)%unit , &
            '(2f10.4,<size(val)>f15.4)', advance ="no"), &
            r2d(lat),r2d(lon),val
          if (size(iok).gt.0) then
            write(moreverbose(ind%moreverbose%p)%unit , &
              '(<size(iok)>i2)'), iok
          else
            write(moreverbose(ind%moreverbose%p)%unit , *)
          endif
        endif

        ! calculate area using spherical formulae
        area= spher_area( &
          d2r(green_common(igreen)%start(idist)), &
          d2r(green_common(igreen)%stop(idist)), &
          d2r(dazimuth), &
          radius=earth%radius, &
          alternative_method=.true.) 

        ! normalization according to Merriam (1992) 
        normalize= 1./ &
          (2.*pi*(1.-cos(d2r(dble(1.)))) * &
          d2r(green_common(igreen)%distance(idist)) * &
          1.e5 * &
          earth%radius )

        !        if (any(green_common(igreen)%dataname.eq."GE")) then
        !          ! elastic part
        !          ! if the cell is not over sea and inverted barometer assumption was not set 
        !          ! and is not excluded by polygon
        !          !      if ((.not.((val(4).eq.0.and.inverted_barometer).or. iok(2).eq.0)).or.size(model).lt.4) then
        !          !        print * ,val
        !          !          results(1,1) = results(1,1) + &
        !          !            (val(1) / 100. -ref_p) * &
        !          !            green_common(igreen,7) * & 
        !          !            area * normalize
        !          !        result(1,1) = result(1,1)+ &
        !          !          (val(1) / 100. -1000. )  * &
        !          !          green_common(igreen)%data(idist,1) * & 
        !          !          area * normalize
        !          !!       print*, results%e , inverted_barometer , .not.((val(4).eq.0.and.inverted_barometer).or. iok(2).eq.0) ,val(4)
        !          !!       stop 

        !          stop
        !        endif
        !        if (any(green_common(igreen)%dataname.eq."GR")) then
        !                  result(1,1) = result(1,1)+ &
        !                    (val(1) )  * &
        !                    green_common(igreen)%data(idist,1) * & 
        !                    area  / d2r(green_common(igreen)%distance(idist)) * &
        !                    1./earth%radius /1e12 * &
        !                    1e3

        !        endif

        !        !      ! newtonian part
        !        !      if(.not. iok(1).eq.0) then
        !        !       results%n = results%n   + (val(1)/ 100.-ref_p) * green_common(igreen,3) * area * normalize

        !        !       if (model(2)%if.and.size(model).ge.2) then
        !        !          results%dt = results%dt + (val(1)/ 100.-ref_p) * &
        !        !            (green_common(igreen,4)*(val(2)- atmosphere%temperature%standard) ) * area * normalize
        !        !        endif

        !        !       results%dh = results%dh + (val(1)/ 100.-ref_p) * &
        !        !        (green_common(igreen,5)*(site%height/1000.) ) * area * normalize

        !        !       results%dz = results%dz + (val(1)/ 100.-ref_p) * &
        !        !        (green_common(igreen,6)*(val(3)/1000.) ) * area * normalize
        !        !     endif
        !        !    endif
        !        !        !!!      if (moreverbose%if.and. moreverbose%names(1).eq."g") then
        !        !        !!        !todo
        !        !        !!!        call convolve_moreverbose (site%lat,site%lon , azimuth , dble(360./ nazimuth) , green_common(igreen,1), green_common(igreen,1))
        !        !        !!!        write (moreverbose%unit, '(">")')
        !        !        !!!      endif
      enddo
    enddo
    !    write(log%unit,*)  , "npoints:", npoints ,"area", area
    !        write(output%unit, '(g20.4)') , result(1,1)
  enddo 
end subroutine

!
!!!> \todo site height from model 
!!

!subroutine convolve_moreverbose (latin , lonin , azimuth , azstep ,  distance , distancestep)
!  use mod_cmdline , only : moreverbose
!  use mod_utilities, only: spher_trig
! 
!  real(dp), intent(in) :: azimuth ,azstep, latin, lonin
!  real(dp) :: distance, lat , lon , distancestep
!
!!  call spher_trig ( latin , lonin , distance - distancestep/2. , azimuth - azstep/2. , lat , lon)
!!  write(moreverbose%unit, '(2f12.6)') , lat , lon
!!  call spher_trig ( latin , lonin , distance - distancestep/2. , azimuth + azstep/2. , lat , lon)
!!  write(moreverbose%unit, '(2f12.6)') , lat , lon
!!  call spher_trig ( latin , lonin , distance + distancestep/2. , azimuth + azstep/2. , lat , lon)
!!  write(moreverbose%unit, '(2f12.6)') , lat , lon
!!  call spher_trig ( latin , lonin , distance + distancestep/2. , azimuth - azstep/2. , lat , lon)
!!  write(moreverbose%unit, '(2f12.6)') , lat , lon
!end subroutine
!
!
subroutine wczytaj_linie_informacyjne
  !!     do i=1,size(linie_informacyjne);        linie_informacyjne(i)%j_l = i;      enddo
  !!      linie_informacyjne%Nj     = (/ 95    , 30    , 95    , 90    , 160    , 90       /)
  !!!      linie_informacyjne%deltal = (/ 0.0011, 0.0205, 0.0550, 1.0500, 10.2500, 90.5000  /)
  !!!      linie_informacyjne%deltah = (/ 0.0199, 0.0495, 0.9950, 9.9500, 89.7500, 179.5000 /)
  !!!      linie_informacyjne%delta  = (/ 0.0002, 0.0010, 0.0100, 0.1000, 0.5000 , 1.0000   /)
  !!!      linie_informacyjne%fine_l = (/ 'F'   , 'F'   , 'F'   , 'F'   , 'C'    , 'C'      /)
end subroutine
!
subroutine plot2green(green_file)
  character(len=*),intent (in) :: green_file
  !!!  integer                          :: ile_linii_komentarza , i, j , jj ,  ile_rekordow , io
  !!!  logical                          :: czy_komentarz=.true.
  !!!  character(len=1)                 :: fine
  !!!  real,dimension(:,:), allocatable :: values
  !!!  real, dimension(7)               :: values_interpolowane=0., values_interpolowane_integrated=0.
  !!!  real,dimension(:), allocatable   :: b,c,d
  !!!  real,dimension(3)                :: G_t
  !!!  real                             :: dist
  !!
  !!!ile_rekordow=0; ile_linii_komentarza=0
  !!!call wczytaj_linie_informacyjne
  !!
  !!
  !!!  open(1, file=trim(green_file)              , action='read'  , status='old')
  !!!  open(2, file=trim(green_file)//'.mrp02.dat', action='write')
  !!
  !!!  do while(czy_komentarz)
  !!!    read(1, *) dummy
  !!!    if( dummy(1:1).eq.'#') then
  !!!      ile_linii_komentarza=ile_linii_komentarza+1
  !!!    else
  !!!      czy_komentarz=.false.
  !!!    endif
  !!!  enddo
  !!!  rewind (1)
  !!!  do i=1, ile_linii_komentarza
  !!!    read (1,*) dummy
  !!!  enddo
  !!!  do while (io.eq.0)
  !!!    read(1,*, iostat=io) dummy
  !!!    ile_rekordow=ile_rekordow+1
  !!!  enddo
  !!!  ile_rekordow=ile_rekordow-1
  !!
  !!!  allocate(values(ile_rekordow,4))
  !!!  allocate(b(ile_rekordow))
  !!!  allocate(c(ile_rekordow))
  !!!  allocate(d(ile_rekordow))
  !!
  !!!  rewind(1)
  !!!  print *, ile_linii_komentarza,ile_rekordow
  !!!  do i=1, ile_linii_komentarza
  !!!    read (1,*) dummy
  !!!  enddo
  !!!  do i=1, ile_rekordow
  !!!    read (1,*) (values(i,j), j=1,4)
  !!  enddo
  !!
  !!
  !!!  write(2,'(a)'), '# program '
  !!!  do i=1,size(linie_informacyjne)
  !!!      write(2, '(i1, i3, 2i4, 3f10.4, 5x, a1)'), linie_informacyjne(i)
  !!!      write(*, '(i1, i3, 2i4, 3f10.4, 5x, a1)'), linie_informacyjne(i)
  !!!      do j= 1, linie_informacyjne(i)%Nj
  !!!         dist = linie_informacyjne(i)%deltal+(j-1)*linie_informacyjne(i)%delta
  !!!!         print * ,dist
  !!!        do jj=2,4
  !!!          call spline(values(:,1), values(:,jj) ,b,c,d,ile_rekordow)
  !!!          values_interpolowane(jj-1)  = ispline(dist , values(:,1), values(:,jj), b, c, d, ile_rekordow)
  !!!          call pointmass2integrated(values_interpolowane(jj-1), dist , linie_informacyjne(i)%delta , K(jj-1), values_interpolowane_integrated(jj-1) )
  !!!!          print*,ile_rekordow, values(1,1), values_interpolowane(jj-1),dist,values_interpolowane_integrated(jj-1)
  !!!!call exit
  !!!        enddo
  !!!        write(2,'(7e13.6)') (values_interpolowane_integrated(jj),jj=1,7)
  !!!      enddo
  !!!  enddo
  !!!  close(1)
  !!!  close(2)
  !!!  deallocate(values,b,c,d)
end subroutine

subroutine denormalize(filein)
  use mod_printing
  character(len=*),intent (in) :: filein
  !  character(len=1) :: fine
  character(len=80) :: header
  integer :: fileinunit , fileoutunit
  integer :: ngr, j, M, Nj, i, ii, iii, i_plik
  !!!  real :: deltal, deltah, delta,dist
  !!!  real, dimension(7) :: val
  !!!  real,dimension(3) :: G_t
  !!
  write(fileoutunit, form%separator)
  write(fileoutunit, form%i3), '# Converted with denormalize (from mod_green)'
  write(fileoutunit, form%i3), '#',trim(filein)

  open(newunit = fileinunit ,file=trim(filein),action='read',status='old')
  read (fileinunit,'(a70)') header

  read (fileinunit,'(i1,i3,2i4)') ngr,j,M,Nj
  !!!  do i=1,M
  !!!    read (1,'(i1,i3,2i4,3f10.4,5x,a1)') ngr,j,M,Nj,deltal, deltah,delta,fine
  !!!    do ii=1,Nj
  !!!      read (1,'(<ngr>e13.6)'), (val(iii),iii=1,7)
  !!!      dist=deltal+(ii-1)*delta
  !!!      write (2, '(f10.5,7e)'),dist,val
  !!
  !!!      do iii=1,3  ! dla vert_disp, hor_disp, gravity -- jest taka 
  !!!      call integrated2pointmass(val(iii),dist , delta, K(iii),  G_t(iii))
  !!!      enddo
  !!!      write (3,'(100(e20.11))') dist,(G_t(iii),iii=1,3)
  !!!    enddo
  !  enddo
  !!
end subroutine
!
!
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
!!
!!subroutine ignewt_(del,stp,grav_new)
!! width stp (ie, the interval [del-stp/2,del+stp/2],
!! del and stp both being in radians
!!  the height correction is included in the green functions,
!! the station height in meters being passed as ht in the common block
!! stloc
!!
!!!  $$$$$$calls only system routines
!!
!!implicit none
!!!      real ::  eps,eps1,eps2,s,gt,c1
!!!      real :: del,stp,g,g2,em ,plc
!!!      real , intent(out) :: grav_new
!!
!!!         eps = wysokosc_stacji/a
!!!         eps1=1.+eps
!!!         eps2=eps*eps
!!!         g2 = gn/(eps1*eps1)
!!!        g = 9.7803327*(1+.005279*ct*ct) - 3.08e-6*wysokosc_stacji
!!
!!!   em = gn/g
!!!   plc = 4*a*em
!!!      if(eps.ne.0) then
!!!        s=sin(d2r(del+stp/2)/2.d0)
!!!        gt=(2.d0*eps1*s**2-eps)/sqrt(4*eps1*s**2+eps2)
!!!        s=sin(d2r(del-stp/2)/2.d0)
!!!        grav_new=gt-(2.d0*eps1*s**2-eps)/sqrt(4*eps1*s**2+eps2)
!!!        grav_new=-g2*grav_new
!!!      endif
!!!      if(eps.eq.0) then
!!!        grav_new=-g2*(sin(( d2r ( del+stp/2 ) )/2.d0)-sin(( d2r(del-stp/2) )/2.d0))
!!!      endif
!!!      return
!!!      end subroutine
!!
!!
!!
!subroutine getgrf(num,ntot,ngr,fingrd)
!      character*80 grname
!      character*1 fingrd
!      integer llu,ngr,ntot,num
!         llu =  71
!         open(unit=llu,file='~/src/spotl/green/gr.gbaver.wef.p01.ce',status='old',action='read')
!!         open(unit=llu,file='~/src/spotl/working/tmpgr',status='old',access='sequential',form="formatted")
!!         open(unit=llu,file='~/dr/merriam/green.dat_zmienione_kolumny.mrp02.dat',status='old',access='sequential',form="formatted")
!!   read(llu,'(a)') grname
!!      endif
!!      read(llu,102) ngreen,num,ntot,ngr,beg,end,spc,fingrd
!!      fingrd='L' ! IB , tmp
!! 102  format(i1,i3,2i4,3f10.4,5x,a)
!!      read(llu,104) ((grfn(ii,j),j=1,7),ii=1,ngr)
!! 104  format(7e13.6)
!!      rin(num) = beg
!!      rout(num) = end
!!      rsz(num) = spc
!!     statgr(num) = fingrd
!!      nring=ntot
!end subroutine
!
!!real :: psi, gn , dgndt , dgndz, d2gndz2, ge
!!character(len=255)::  dummy
!
!
!!print *, 'sta'
!!open( unit=11, file='./green.dat'               , action='read'  )
!!open( unit=12, file='./green_denormalizacja.dat', action='write' )
!
!!read (11,*) dummy
!
!!do 
!!  read (11,*, iostat=io) , psi, gn , dgndt , dgndz, d2gndz2, ge
!!  if (.not.io.eq.0)  exit
!
!!  call denormfactor(psi, ge)
!!  
!!  write( 12 , '(2f15.5,3e20.10,e15.7)' ), psi, gn, dgndt, dgndz, d2gndz2, ge
!!  write( * , '(2f15.5,3e20.10,e15.7)' ), psi, gn, dgndt, dgndz, d2gndz2, ge
!!enddo
!!print*, cos(d2r(1.))
!!  
!
!
subroutine denormfactor(psi, val)
  real , intent(in) :: psi
  real,intent(inout) :: val
  real :: pi
  real :: d2r
  real , parameter :: a =6378137.0
  !
  !!pi = 4. * atan (1.0)
  !
  !!val = val / ( 2. * pi * ( 1. - cos ( d2r(1.) ) ) * d2r(psi)  *  a**2 * 1e5 * 1e8 * 1e2 )
  !!val = val * 1e18 * ( a * d2r(psi) )
  !
end subroutine

end module
