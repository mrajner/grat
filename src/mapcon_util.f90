module mapcon_util

contains 
subroutine mapaascii2mapablv
  implicit none
  character*1 cdir
  integer :: index , i , j , ncells
  integer map
  real :: B, L 
  integer :: val
  !c index is 360x720 integers, map 32x32 bits (64/degree). The following
  !c dimension and data statements would need to be changed for a different
  !c cell size and resolution.

  dimension index(259200),map(32)

    ncells=0
    open(unit=1,file='../lndsea/lndsea.ascii_aux',status='old',action='read')
    open(unit=2,file='../lndsea/ind.blv')

    read(1,'(12(i6))') index

    B=90.0
    L=0.
         do i=1,360
         B= 89.75 -  ( i -1 ) * 0.5
         do j=1,720
         L= 0.25 +  ( j -1 ) * 0.5
         val=( index(i*720 + j-1))
         if (val.eq.-2) then
           val=0.0
        elseif (val.eq.-1) then
           val=2
         else
           val=1
         endif
          write(2,'(f10.4,f10.4,i10)')  B, L, val
        enddo
        enddo
        close(2)


         open(unit=3,file='../lndsea/bit.blv')
13         read(1,'(6(i12))',end=17) map
   ncells = ncells+1
   write(3,'(i)') map
   go to 13
 17      continue
    close(1)
end subroutine
end module
