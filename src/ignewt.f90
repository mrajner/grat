program ignewt
use green

implicit none
real ::dist,stp,grav
integer :: i, ii, j, ile_wysokosci, ile_argumentow
real,dimension(:,:), allocatable :: wartosci
real,dimension(7):: wysokosc=(/0,1,100,1000, -1,-10,-1000/)


do ii=1,7


wysokosc_stacji=wysokosc(ii)
write (dummy,'(i9)') int(wysokosc_stacji)
open(unit=2, file="../green/ignewt.dat"//trim(adjustl(dummy)), action="write")



call wczytaj_linie_informacyjne

  write(2,'(a)'), '# Linia nagłówkowa'
  do i=1,size(linie_informacyjne)
      write(2, '(i1, i3, 2i4, 3f10.4, 5x, a1)'), linie_informacyjne(i)
!      write(*, '(i1, i3, 2i4, 3f10.4, 5x, a1)'), linie_informacyjne(i)
      do j= 1, linie_informacyjne(i)%Nj
         dist = linie_informacyjne(i)%deltal+(j-1)*linie_informacyjne(i)%delta
          call ignewt_(dist,linie_informacyjne(i)%delta,grav)
        write(2,'(7e13.6)') grav
      enddo
  enddo

close(2)
print *, "../green/ignewt.dat"//trim(adjustl(dummy))
call green2plot ("../green/ignewt.dat"//trim(adjustl(dummy)) )
enddo
end program


