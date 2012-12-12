module mod_green
  use constants
  use get_cmd_line
  use aggf
  use mod_data
  implicit none


contains
subroutine green_unification (green , green_common , denser)
  type(green_functions), allocatable , dimension(:) , intent(in) :: green
  integer, optional :: denser
  integer :: i , ndenser , j ,ii
  real(dp), allocatable , dimension(:) :: x , y , dist
  real(dp), allocatable , dimension(:,:) , intent(out) :: green_common

  ndenser=0
  if (present(denser)) ndenser = denser
  allocate (x (size_ntimes_denser(size(green(1)%distance),ndenser)-1))
  allocate(dist(size(x)))
  ii=0
  do i = 1 , size(green(1)%distance)-1
    do j = 0 , ndenser
      ii=ii+1
      x(ii) = green(1)%distance (i)  + (j +1./2.) * (green(1)%distance (i+1) -green(1)%distance (i) ) / ( ndenser + 1 )
      dist(ii) = (green(1)%distance (i+1) -green(1)%distance (i) ) / ( ndenser + 1 )
    enddo
  enddo
!  x(size(x)) = green(1)%distance(size(green(1)%distance))
  allocate(green_common (size(x) , 6))
  green_common(:,1) = x
  green_common(:,2) = dist
  do i = 1 , 4
    if (allocated(green(i)%distance)) then
      call spline_interpolation (green(i)%distance , green(i)%data, x , y)
      green_common(:,i+2) = y
    else 
      green_common(:,i+2) = 0
    endif
  enddo
end subroutine
! =============================================================================
! =============================================================================
subroutine spher_area (distance ,ddistance, azstp,  area )
  real(dp),intent(out) :: area
  real(dp), intent(in) :: distance,ddistance 
  real(sp):: azstp
  area = sin ( d2r(distance) ) * d2r(ddistance) * d2r(dble(azstp))

end subroutine

subroutine spher_trig ( latin , lonin , distance , azimuth , latout , lonout)
  real(dp) , intent(in)  :: distance 
  real(sp) , intent(in)  :: latin , lonin , azimuth
  real(dp) , intent(out) :: latout, lonout 
  real(dp):: sg, cg , saz ,caz , st ,ct , cd ,sd  , cb , sb

  ct  = cos (d2r(90.-dble(latin)))
  st  = sin (d2r(90.-dble(latin)))
  cd  = cos (d2r(distance))
  sd  = sin (d2r(distance))
  saz = sin (d2r(dble(azimuth)))
  caz = cos (d2r(dble(azimuth)))
  cb = cd*ct + sd*st*caz
  !todo !if(abs(cb).gt.1) cb = cb/abs(cb)
  sb = sqrt(1.-cb*cb)
  latout = 90 - r2d(acos(cb))
  lonout = 0.
  if(sb.gt.1.e-3) then
    sg = sd*saz/sb
    cg = (st*cd - sd*ct*caz)/sb
    lonout = lonin + r2d(atan2(sg,cg))
  endif
end subroutine

! =============================================================================
! =============================================================================
subroutine convolve (site,  green , denserdist , denseraz )
  type(green_functions), allocatable , dimension(:) :: green
  integer, optional :: denserdist , denseraz
  integer ::  ndenser , igreen  , iazimuth , nazimuth
  real(dp), allocatable , dimension(:,:)  :: green_common
  real(sp) :: azimuth
  type(site_data), intent(in) :: site
  real(dp) :: lat , lon , area 
  real(sp) :: val 

  ndenser =   0
  nazimuth= 1 !< todo set to 150 at least
  if (present(denserdist))    ndenser = denserdist
  if (present(denseraz))     nazimuth = nazimuth * denseraz
  call green_unification (green , green_common , denser = ndenser)

  write(* , "( 6f15.7 )" ) ,( green_common (ndenser,:) , ndenser = 1,1 )
  write(* , "( 6f15.7 )" ) ,  green_common (size(green_common(:,1)),:) 

  
  open (22, file = "tmp.dat" , action ="write")

  do igreen = 1 ,size(green_common(:,1)), 1 !todo 
  
    do iazimuth  = 1 , nazimuth
      azimuth = (iazimuth - 1) * 360./nazimuth
      call spher_trig ( site%lat , site%lon , green_common(igreen,1) , azimuth , lat , lon)
      call get_value (model(1) , real(lat) , real(lon) , val )
      call spher_area(green_common(igreen,1) , green_common(igreen,2), 360./nazimuth , area) 
      if (moreverbose%if) then
        call convolve_moreverbose (site , azimuth , green_common(igreen,1))
!        call ldbxdr(azimuth,dist,azstpd,spc)
      endif

    write (22, *)  lat , lon 
    write (*, '(10(go,x))')  lat , lon, val * real(area) * green_common(igreen,3)
    enddo
  enddo
  print * , site%lat
end subroutine

subroutine convolve_moreverbose (site , azimuth , distance)
  type(site_data),intent(in) :: site
  real(sp), intent(in) :: azimuth
  real(dp) :: distance
  real(sp) :: ca , sa

      ca = cos(d2r(dble(azimuth)))
      sa = sin(d2r(dble(azimuth)))
!        write(moreverbose%unit , *) "D"
!      subroutine invspt(alp,del,b,rlong)
!c  solves the inverse spherical triangle problem - given a station whose
!c  colatitude is t, and east longitude rlam (in degrees), returns the
!c  colatitude b and east longitude rlong of the place which is at a distance
!c  of delta degrees and at an azimuth of alp (clockwise from north).
!c  the common block stloc holds the cosine and sine of the station colatitude,
!c  and its east longitude.
!      sa = sin(alp*dtr)
!      cd = cos(del*dtr)
!      sd = sin(del*dtr)
!      cb = cd*ct + sd*st*ca
!      sb = sqrt(1.-cb*cb)
!      b = acos(cb)/dtr
!      if(sb.le.1.e-3) then
!c  special case - the point is at the poles
!   rlong = 0
!   return
!      endif
!      sg = sd*sa/sb
!      cg = (st*cd-sd*ct*ca)/sb
!      g = atan2(sg,cg)/dtr
!      rlong = rlam + g
end subroutine



!      if (lnd.eq.2) wartosc_komorki_h = 0.

!        if(wartosc_komorki.ne.0.) then
!          normalizacja= 1 / ( 2. * pi * ( 1. - cos ( d2r(1.) ) ) * d2r(dist) *1e5  )
!          if (lnd.eq.1) then
!            grav_merriam_e = grav_merriam_e  + values_interpolowane(5) * wartosc_komorki * pole * normalizacja 
!          endif
!          grav_merriam_e_nib = grav_merriam_e_nib  + values_interpolowane(5) * wartosc_komorki * pole * normalizacja 
!          grav_merriam_n = grav_merriam_n  + values_interpolowane(1) * wartosc_komorki * pole * normalizacja 
!          grav_merriam_n_t = grav_merriam_n_t  + ( values_interpolowane(1) +values_interpolowane(2)* (wartosc_komorki_t/10-15)) * wartosc_komorki * pole * normalizacja 
!          grav_merriam_n_h =&
!            grav_merriam_n_h  + (&
!              values_interpolowane(1) &
!             +values_interpolowane(2)* (wartosc_komorki_t/10-15) &
!             +values_interpolowane(3)*((wartosc_komorki_h - wysokosc_stacji_etopo2) /1000 ) &
!             +values_interpolowane(4)*((wartosc_komorki_h - wysokosc_stacji_etopo2) /1000 /( d2r(dist) * a/1000 )**2 ) &
!              ) &
!            * wartosc_komorki * pole * normalizacja 
!        endif
!      azimuth = azimuth + azstpd
!      xx = saz*caztp + caz*saztp
!      caz = caz*caztp - saz*saztp
!      saz = xx
!      cale_pole = cale_pole + pole
!      licznik=licznik+1
! 16   continue

!  enddo
!print '(2(f14.6,x))' , (table(i), green(1)%distance(i) , i =1,20) 
!    print * , table2


!subroutine wczytaj_linie_informacyjne
!     do i=1,size(linie_informacyjne);        linie_informacyjne(i)%j_l = i;      enddo
!      linie_informacyjne%Nj     = (/ 95    , 30    , 95    , 90    , 160    , 90       /)
!      linie_informacyjne%deltal = (/ 0.0011, 0.0205, 0.0550, 1.0500, 10.2500, 90.5000  /)
!      linie_informacyjne%deltah = (/ 0.0199, 0.0495, 0.9950, 9.9500, 89.7500, 179.5000 /)
!      linie_informacyjne%delta  = (/ 0.0002, 0.0010, 0.0100, 0.1000, 0.5000 , 1.0000   /)
!      linie_informacyjne%fine_l = (/ 'F'   , 'F'   , 'F'   , 'F'   , 'C'    , 'C'      /)
!end subroutine

!subroutine plot2green(green_file)
!  implicit none
!  character(len=*),intent (in) :: green_file
!  integer                          :: ile_linii_komentarza , i, j , jj ,  ile_rekordow , io
!  logical                          :: czy_komentarz=.true.
!  character(len=1)                 :: fine
!  real,dimension(:,:), allocatable :: values
!  real, dimension(7)               :: values_interpolowane=0., values_interpolowane_integrated=0.
!  real,dimension(:), allocatable   :: b,c,d
!  real,dimension(3)                :: G_t
!  real                             :: dist

!ile_rekordow=0; ile_linii_komentarza=0
!call wczytaj_linie_informacyjne


!  open(1, file=trim(green_file)              , action='read'  , status='old')
!  open(2, file=trim(green_file)//'.mrp02.dat', action='write')

!  do while(czy_komentarz)
!    read(1, *) dummy
!    if( dummy(1:1).eq.'#') then
!      ile_linii_komentarza=ile_linii_komentarza+1
!    else
!      czy_komentarz=.false.
!    endif
!  enddo
!  rewind (1)
!  do i=1, ile_linii_komentarza
!    read (1,*) dummy
!  enddo
!  do while (io.eq.0)
!    read(1,*, iostat=io) dummy
!    ile_rekordow=ile_rekordow+1
!  enddo
!  ile_rekordow=ile_rekordow-1

!  allocate(values(ile_rekordow,4))
!  allocate(b(ile_rekordow))
!  allocate(c(ile_rekordow))
!  allocate(d(ile_rekordow))

!  rewind(1)
!  print *, ile_linii_komentarza,ile_rekordow
!  do i=1, ile_linii_komentarza
!    read (1,*) dummy
!  enddo
!  do i=1, ile_rekordow
!    read (1,*) (values(i,j), j=1,4)
!  enddo


!  write(2,'(a)'), '# program '
!  do i=1,size(linie_informacyjne)
!      write(2, '(i1, i3, 2i4, 3f10.4, 5x, a1)'), linie_informacyjne(i)
!      write(*, '(i1, i3, 2i4, 3f10.4, 5x, a1)'), linie_informacyjne(i)
!      do j= 1, linie_informacyjne(i)%Nj
!         dist = linie_informacyjne(i)%deltal+(j-1)*linie_informacyjne(i)%delta
!!         print * ,dist
!        do jj=2,4
!          call spline(values(:,1), values(:,jj) ,b,c,d,ile_rekordow)
!          values_interpolowane(jj-1)  = ispline(dist , values(:,1), values(:,jj), b, c, d, ile_rekordow)
!          call pointmass2integrated(values_interpolowane(jj-1), dist , linie_informacyjne(i)%delta , K(jj-1), values_interpolowane_integrated(jj-1) )
!!          print*,ile_rekordow, values(1,1), values_interpolowane(jj-1),dist,values_interpolowane_integrated(jj-1)
!!call exit
!        enddo
!        write(2,'(7e13.6)') (values_interpolowane_integrated(jj),jj=1,7)
!      enddo
!  enddo
!  close(1)
!  close(2)
!  deallocate(values,b,c,d)
!end subroutine

!subroutine green2plot(green_file)
!implicit none
!  character(len=*),intent (in) :: green_file
!  character(len=1) :: fine
!  integer :: ngr, j, M, Nj, i, ii, iii, i_plik
!  real :: deltal, deltah, delta,dist
!  real, dimension(7) :: val
!  real,dimension(3) :: G_t

! print *,trim(green_file)

!  open(1,file=trim(green_file),action='read',status='old')
!  open(2,file=trim(green_file)//'.dat_i',action='write')
!  open(3,file=trim(green_file)//'.dat',action='write')

!  read (1,'(a70)') header
!  write(2,*),'# '//trim(header)
!  write(3,*),'# '//trim(header)
!  write(2,*),'# Przerobione z pliku w formacie spotl - mrajner'
!  write(3,*),'# Przerobione z pliku w formacie spotl - mrajner'


!  read (1,'(i1,i3,2i4)') ngr,j,M,Nj
!  rewind(1)
!  read (1,*) header


!  do i=1,M
!    read (1,'(i1,i3,2i4,3f10.4,5x,a1)') ngr,j,M,Nj,deltal, deltah,delta,fine
!    do ii=1,Nj
!      read (1,'(<ngr>e13.6)'), (val(iii),iii=1,7)
!      dist=deltal+(ii-1)*delta
!      write (2, '(f10.5,7e)'),dist,val

!      do iii=1,3  ! dla vert_disp, hor_disp, gravity -- jest taka 
!      call integrated2pointmass(val(iii),dist , delta, K(iii),  G_t(iii))
!      enddo
!      write (3,'(100(e20.11))') dist,(G_t(iii),iii=1,3)
!    enddo
!  enddo

!end subroutine green2plot

!subroutine integrated2pointmass(G_integrated,dist, delta, K, G_t)
!  ! rozdział 4.1 spotlman
!  implicit none
!  real, intent (in) :: G_integrated, dist,delta
!  integer , intent(in) :: K
!  real, intent(out) :: G_t
!  real :: G_prim_t

!  G_prim_t = G_integrated  / ( 4 *  cos( d2r(dist) / 2. ) * sin( d2r(delta) /4. ) )
!  G_t = G_prim_t * ( ( 10.**K * a ) / ( a**2 * ( 2 *  sin (d2r(dist) /2  )/ d2r(dist)  ) ) )  
!  !/ ( 10.**K * a * d2r(dist) )
!end subroutine
!subroutine pointmass2integrated(G_t,dist, delta, K, G_integrated)
!  ! rozdział 4.1 spotlman
!  implicit none
!  real, intent (in) :: G_t, dist,delta
!  integer , intent(in) :: K
!  real, intent(out) :: G_integrated
!  real :: G_prim_t

!  G_prim_t = G_t / ( ( 10.**K * a ) / ( a**2 * ( 2 *  sin (d2r(dist) /2  ) / d2r(dist)  ) ) )  
!  G_integrated = G_prim_t  * ( 4 *  cos( d2r(dist) / 2. ) * sin( d2r(delta) /4. ) )
!end subroutine

!subroutine ignewt_(del,stp,grav_new)
!!c width stp (ie, the interval [del-stp/2,del+stp/2],
!!c del and stp both being in radians
!!c  the height correction is included in the green functions,
!!c the station height in meters being passed as ht in the common block
!!c stloc
!!c
!!c  $$$$$$calls only system routines
!!c
!implicit none
!      real ::  eps,eps1,eps2,s,gt,c1
!      real :: del,stp,g,g2,em ,plc
!      real , intent(out) :: grav_new

!         eps = wysokosc_stacji/a
!         eps1=1.+eps
!         eps2=eps*eps
!         g2 = gn/(eps1*eps1)
!        g = 9.7803327*(1+.005279*ct*ct) - 3.08e-6*wysokosc_stacji

!   em = gn/g
!   plc = 4*a*em
!      if(eps.ne.0) then
!        s=sin(d2r(del+stp/2)/2.d0)
!        gt=(2.d0*eps1*s**2-eps)/sqrt(4*eps1*s**2+eps2)
!        s=sin(d2r(del-stp/2)/2.d0)
!        grav_new=gt-(2.d0*eps1*s**2-eps)/sqrt(4*eps1*s**2+eps2)
!        grav_new=-g2*grav_new
!      endif
!      if(eps.eq.0) then
!        grav_new=-g2*(sin(( d2r ( del+stp/2 ) )/2.d0)-sin(( d2r(del-stp/2) )/2.d0))
!      endif
!      return
!      end subroutine



!subroutine getgrf(num,ntot,ngr,fingrd)
!      character*80 grname
!      character*1 fingrd
!      save newf,llu
!      data newf/0/
!      if(newf.eq.0) then
!          newf = 1
!         llu =  71
!!         open(unit=llu,file='~/src/spotl/green/gr.gbaver.wef.p01.ce',status='old',access='sequential',form="formatted")
!         open(unit=llu,file='~/src/spotl/working/tmpgr',status='old',access='sequential',form="formatted")
!!         open(unit=llu,file='~/dr/merriam/green.dat_zmienione_kolumny.mrp02.dat',status='old',access='sequential',form="formatted")
!   read(llu,'(a)') grname
!      endif
!      read(llu,102) ngreen,num,ntot,ngr,beg,end,spc,fingrd
!      fingrd='L' ! IB , tmp
! 102  format(i1,i3,2i4,3f10.4,5x,a)
!      read(llu,104) ((grfn(ii,j),j=1,7),ii=1,ngr)
! 104  format(7e13.6)
!      rin(num) = beg
!      rout(num) = end
!      rsz(num) = spc
!     statgr(num) = fingrd
!      nring=ntot
!      return
!end subroutine

end module
