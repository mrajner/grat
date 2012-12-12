program symuluj_dane
implicit none
real B, L, val 
character(len=255) :: plik,sciezka
real ::interwal
integer :: i,j

plik='dane.blv'
sciezka='../dane/symulacja/'

interwal=1.
open (4,file=trim(sciezka)//trim(plik), action='write')

do i = 1, 180/interwal+1
  B = -90  + ( i-1 )*interwal
  do j = 1, 360/interwal +1
    L = -180  + ( j-1 )*interwal
  write (4, * ) L, B,  B+L/10
  enddo
enddo
end program

