!> \file
!! module
module mod_green
  use mod_constants, only: dp
  use mod_utilities, only: spher_area
  implicit none

  private
  public :: results , convolve

  real(dp), allocatable , dimension(:,:)  :: green_common
  type result
    real(dp) :: N=0. , dt=0. ,E=0. , dh=0.,dz=0.
  end type
  type (result), allocatable, dimension(:) :: results
 
contains

! =============================================================================
!> Unification:
! =============================================================================
!subroutine green_unification (green , green_common , denser)
!  use mod_constants , only : dp 
!  use mod_cmdline, only: moreverbose, method ,  green_functions
!  use mod_aggf, only: size_ntimes_denser
!  use mod_utilities, only:spline_interpolation
!
!  type(green_functions), allocatable , dimension(:) , intent(in) :: green
!  integer, optional :: denser
!  integer :: i , ndenser , j ,ii
!  real(dp), allocatable , dimension(:) :: x , y , dist
!  real(dp), allocatable , dimension(:,:) , intent(out) :: green_common
!
!
!  ndenser=0
!  if (present(denser)) ndenser = denser
!
!  allocate (x (size_ntimes_denser(size(green(1)%distance),ndenser)-1))
!  allocate(dist(size(x)))
!  ii=0
!  do i = 1 , size(green(1)%distance)-1
!    do j = 0 , ndenser
!      ii=ii+1
!      x(ii) = green(1)%distance (i)  + (j +1./2.) * (green(1)%distance (i+1) -green(1)%distance (i) ) / ( ndenser + 1 )
!!      dist(ii) = (green(1)%distance (i+1) -green(1)%distance (i) ) / ( ndenser + 1 )
!    enddo
!  enddo
!  ! x(size(x)) = green(1)%distance(size(green(1)%distance))
!!  allocate(green_common (size(x) , 7))
!!  green_common(:,1) = x
!!  green_common(:,2) = dist
!!  do i = 1 , 5
!!    if (size(green).ge.i .and. allocated(green(i)%distance)) then
!!      call spline_interpolation (green(i)%distance , green(i)%data, &
!!        size(green(i)%data), x , y , size(x)) 
!!      green_common(:,i+2) = y
!!    else 
!!      green_common(:,i+2) = 0
!!    endif
!!  enddo
!!  !todo
!!!  if (moreverbose%if.and. moreverbose%names(1).eq."G") then
!!!    write(moreverbose%unit , '(7F13.6)' ) (green_common (i,:), i =1,ubound(green_common,1))
!!!  endif
!end subroutine
!
!
! =============================================================================
!> Perform convolution
!!
!! \date 2013-03-15
!! \author M. Rajner
! =============================================================================
subroutine convolve (site ,  green , results, denserdist , denseraz)
  use, intrinsic :: iso_fortran_env, only : error_unit
  use mod_constants, only: pi , dp,  atmosphere
  use mod_cmdline , only: site_data, green_functions , moreverbose , &
    inverted_barometer , model , polygons ,  method ,log
  use mod_utilities, only: d2r , spher_trig
  use mod_data, only: get_value
  use mod_polygon, only: chkgon
!
  type(site_data) , intent(in) :: site
  type(green_functions), allocatable , dimension(:) :: green
  integer , intent (in) :: denserdist , denseraz
  real(dp) :: latin , lonin
  integer ::  ndenser , igreen  , iazimuth , nazimuth
  real(dp) :: azimuth
  real(dp) :: lat , lon , area  
  real(dp) :: val(4) , ref_p
  integer :: i , iok(2) , npoints
  real(dp) :: normalize 
  type (result) ,intent(out)  :: results

  ! check if greens functions were specified
  if( size (green).eq.0) then
    stop "No green functions!"
  endif

  print * ,denseraz,":::"
  if (.not.allocated(green_common))  then
!    call green_unification (green , green_common , denser = denserdist-1)
  endif
!
!!  npoints=0
!!  do igreen = 1 ,size(green_common(:,1))
!!    nazimuth = max(int(360*sin(d2r(green_common(igreen,1)))),100) * denseraz
!!    do iazimuth  = 1 , nazimuth
!!      npoints = npoints + 1
!!      azimuth = (iazimuth - 1) * 360./nazimuth
!
!!      ! get lat and lon of point
!!      call spher_trig ( site%lat , site%lon , green_common(igreen,1) , azimuth , lat , lon)
!
!!      ! get values of model
!!      do i = 1 , size(model)
!!        if(model(i)%if) then 
!!          call get_value (model(i) , lat , lon , val(i) , level=1, method =model(i)%interpolation)
!!        else 
!!          val(i) = 0.
!!        endif
!!      enddo
!
!!      !todo
!!!      if (refpres%if) then
!!!       call get_value (refpres , lat , lon , ref_p , method =1)
!!!     else
!!       ref_p=0.
!!!     endif
!
!!      ! get polygons
!!      do i = 1 , 2
!!        if (polygons(i)%if) then
!!          call chkgon ( lon, lat , polygons(i) , iok(i) )
!!        else
!!          iok(i)=1
!!        endif
!!      end do
!
!!      ! calculate area using spherical formulae
!!      if (val(1).ne.0) then
!!        ! todo !!!!! spher area was changed to work with RADIANS
!!        call spher_area(green_common(igreen,1) , green_common(igreen,2), dble(360./nazimuth) , area) 
!
!!        ! force topography to zero over oceans
!!        if (val(4).eq.0.and.val(3).lt.0) val(3) = 0.
!
!!        ! normalization according to Merriam (1992) 
!!        normalize= 1. / ( 2. * pi * ( 1. - cos ( d2r(dble(1.)) ) ) * d2r(green_common(igreen,1)) *1.e5  )
!
!!       ! elastic part
!!        ! if the cell is not over sea and inverted barometer assumption was not set 
!!        ! and is not excluded by polygon
!!        if ((.not.((val(4).eq.0.and.inverted_barometer).or. iok(2).eq.0)).or.size(model).lt.4) then
!!          results%e = results%e + (val(1) / 100. -ref_p) * green_common(igreen,7) * area * normalize
!!        endif
!!  !       print*, results%e , inverted_barometer , .not.((val(4).eq.0.and.inverted_barometer).or. iok(2).eq.0) ,val(4)
!!  !       stop 
!
!!        ! newtonian part
!!        if(.not. iok(1).eq.0) then
!!         results%n = results%n   + (val(1)/ 100.-ref_p) * green_common(igreen,3) * area * normalize
!
!!         if (model(2)%if.and.size(model).ge.2) then
!!            results%dt = results%dt + (val(1)/ 100.-ref_p) * &
!!              (green_common(igreen,4)*(val(2)- atmosphere%temperature%standard) ) * area * normalize
!!          endif
!
!!         results%dh = results%dh + (val(1)/ 100.-ref_p) * &
!!          (green_common(igreen,5)*(site%height/1000.) ) * area * normalize
!
!!         results%dz = results%dz + (val(1)/ 100.-ref_p) * &
!!          (green_common(igreen,6)*(val(3)/1000.) ) * area * normalize
!!       endif
!!      endif
!!!      if (moreverbose%if.and. moreverbose%names(1).eq."g") then
!!        !todo
!!!        call convolve_moreverbose (site%lat,site%lon , azimuth , dble(360./ nazimuth) , green_common(igreen,1), green_common(igreen,1))
!!!        write (moreverbose%unit, '(">")')
!!!      endif
!!    enddo
!!  enddo
!!  !todo
!!!  if (moreverbose%if.and. moreverbose%names(1).eq."i") then
!!!    write (moreverbose%unit, '(a,x,g0)') "Points used in convolution" ,npoints
!!!  endif
end subroutine
!
!!!> \todo site height from model 
!!
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
!subroutine wczytaj_linie_informacyjne
!!!     do i=1,size(linie_informacyjne);        linie_informacyjne(i)%j_l = i;      enddo
!!!      linie_informacyjne%Nj     = (/ 95    , 30    , 95    , 90    , 160    , 90       /)
!!!      linie_informacyjne%deltal = (/ 0.0011, 0.0205, 0.0550, 1.0500, 10.2500, 90.5000  /)
!!!      linie_informacyjne%deltah = (/ 0.0199, 0.0495, 0.9950, 9.9500, 89.7500, 179.5000 /)
!!!      linie_informacyjne%delta  = (/ 0.0002, 0.0010, 0.0100, 0.1000, 0.5000 , 1.0000   /)
!!!      linie_informacyjne%fine_l = (/ 'F'   , 'F'   , 'F'   , 'F'   , 'C'    , 'C'      /)
!end subroutine
!
!subroutine plot2green(green_file)
!  character(len=*),intent (in) :: green_file
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
!end subroutine
!
!subroutine green2plot(green_file)
!  character(len=*),intent (in) :: green_file
!!!  character(len=1) :: fine
!!!  integer :: ngr, j, M, Nj, i, ii, iii, i_plik
!!!  real :: deltal, deltah, delta,dist
!!!  real, dimension(7) :: val
!!!  real,dimension(3) :: G_t
!!
!!! print *,trim(green_file)
!!
!!!  open(1,file=trim(green_file),action='read',status='old')
!!!  open(2,file=trim(green_file)//'.dat_i',action='write')
!!!  open(3,file=trim(green_file)//'.dat',action='write')
!!
!!!  read (1,'(a70)') header
!!!  write(2,*),'# '//trim(header)
!!!  write(3,*),'# '//trim(header)
!!!  write(2,*),'# Przerobione z pliku w formacie spotl - mrajner'
!!!  write(3,*),'# Przerobione z pliku w formacie spotl - mrajner'
!!
!!
!!!  read (1,'(i1,i3,2i4)') ngr,j,M,Nj
!!!  rewind(1)
!!!  read (1,*) header
!!
!!
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
!!!  enddo
!!
!end subroutine
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
!!program denormalizacja
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
!!end program denormalizacja
!
!!subroutine denormfactor(psi, val)
!!  implicit none
!!  real , intent(in) :: psi
!!  real,intent(inout) :: val
!!  real :: pi
!!  real :: d2r
!!  real , parameter :: a =6378137.0
!
!!pi = 4. * atan (1.0)
!
!!val = val / ( 2. * pi * ( 1. - cos ( d2r(1.) ) ) * d2r(psi)  *  a**2 * 1e5 * 1e8 * 1e2 )
!!val = val * 1e18 * ( a * d2r(psi) )
!
!!end subroutine

end module
