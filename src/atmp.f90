program tmp
  use mod_utilities
  use mod_constants
  use mod_aggf
  real(dp):: x, y
  integer :: n
  real(dp) :: dpres, dpres2,dpres3

  x=1e-6
  y=10000
  n=6
!  print * , x , y , n , linspace(x,y,n)
!  print '(2f13.5)' , logspace(x,y,n), 

do ih = 0,10000,500
!  call standard_pressure(dble(ih), dpres3, method="simple")
  print '(10f12.3)' , real(ih) ,&
    standard_pressure(dble(ih)) , &
    standard_pressure(dble(ih), method="simple") , &
    standard_pressure(dble(ih), method="berg")

enddo
end program

