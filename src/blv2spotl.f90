!============================================================
subroutine blv2spotl(plik_aux)
  implicit none

  character(len=*),intent(in)     :: plik_aux
  character(len=255)              :: polecenie, plik_aux_wynikowy , plik_aux_wynikowy_b
  real                            :: B, L,v
  real                            :: minl,minb,minv,maxb,maxl,maxv,aux
  real :: skala=1.0
  integer                         :: ileB, ileL,i,j 
  real,dimension(:,:),allocatable :: values


  write(polecenie, '(f19.2)') interwal_press2spotl
    polecenie=trim(polecenie)//'='
  if (.not.log_N) then
    polecenie=trim(polecenie)//' -F'
  endif
  if (.not.log_lbv) then
    polecenie=trim(polecenie)//' -:'
  endif
  call system( 'xyz2grd '//plik_aux//' -Gtmp.grd -I'//trim(adjustl(polecenie))//' -Rg' )

  write(polecenie, '(f19.5)') interwal_press2spotl_wyjscie
  call system( 'grdsample tmp.grd  -Gtmp2.grd -F -I'//trim(adjustl(polecenie)))
  call system( 'grdsample /home/mrajner/pub/2012_grat/ref_pres/refpres0p5.grd -Gtmp.grd  -F -I'//trim(adjustl(polecenie)))

  ! *10 zm. ciś. = mm H2O eq.
  if (log_R.and..not.log_Rval) then !mrajner 2012-10-03 10:40
    call system( 'grdmath tmp2.grd tmp.grd NEG ADD 10 MUL = tmp3.grd ' )
  elseif (log_R.and.log_Rval) then
    write(polecenie, '(f19.5)') Rval
    call system( 'grdmath tmp2.grd '//trim(adjustl(polecenie))//' ADD 10 MUL = tmp3.grd ' )
  else
    call system( 'grdmath tmp2.grd 10 MUL = tmp3.grd ' )
  endif

  call system( 'grd2xyz -: tmp3.grd > '//trim(plik_aux)//'.norm' )
  call system( 'grdinfo tmp3.grd -C > tmp.txt')
  open(13 , file='./tmp.txt',action='read')
  read(13 , * ),polecenie, minl, maxl, minb, maxb, minv, maxv,aux,aux,ileL,ileB
  close(13)

  plik_aux_wynikowy  = trim(plik_aux)//'.norm.spotl'
  plik_aux_wynikowy_b= trim(plik_aux)//'.norm.spotl_b'

  open (12 , file=plik_aux_wynikowy ,action='write')
  write(12 ,'(a4)') ,'z0'
  write(12 ,'(6(i3))') ,0,0,0,0,0,0
  write(12 ,'(2i8)'), int(maxb           ),int((maxb           -int(maxb           ))*1000)
  write(12 ,'(2i8)'), int(minb           ),int((minb           -int(minb           ))*1000)
  write(12 ,'(2i8)'), int(maxl           ),int((maxl           -int(maxl           ))*1000)
  write(12 ,'(2i8)'), int(minl           ),int((minl           -int(minl           ))*1000)
  write(12 ,'(2i8)'), ileb,ilel
  write(12 ,'(a6,a44)') ,'./blv2spotl ',trim(plik_aux)
  open(11, file=trim(plik_aux)//'.norm',action='read')
  allocate(values(ileB,ileL))
  do i=1,ileB
  do j=1,ileL
  read(11,*) B,L,values(i,j)
  if (abs(values(i,j)).gt.1000) values(i,j)=0 !przy grdsample wyrzuca zmiany większe niż 100 hPa
  enddo
  enddo

  write(12,'(10(i7))') ( ( int((values(i,j))*1),  j=1,ileL ) , i=1,ileB )
  write(12,'(10(i7))') ( ( int(0),  j=1,ileL ) , i=1,ileB )
  close(11)
  close(12)

!  call system ('cat '//plik_aux_wynikowy//' | /home/mrajner/src/spotl/bin/modcon_allocatable f '//plik_aux_wynikowy_b)
  call system ('cat '//plik_aux_wynikowy//' | /home/mrajner/src/spotl/bin/modcon f '//plik_aux_wynikowy_b)
!  call system ('rm tmp.*') ! zmienić żeby w ścieżce odpowiedniej usuwał
end subroutine


