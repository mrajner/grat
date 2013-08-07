program check_transfer
!  use mod_aggf, only:standard_pressure, standard_temperature
  use mod_constants
!  use mod_green
  use mod_utilities ,only: linspace
  use mod_atmosphere
  implicit none

  real (dp), allocatable, dimension(:) :: heights
  integer :: i, nheight


  nheight=25
  allocate(heights(nheight))
  heights = linspace(real(0,dp),real(30000,dp),nheight)

  do i = 1 , nheight


    if (i.eq.1) print '(10a18)' , "h", "--", "berg", "simple" , "full"
    print '(10f18.9)' , heights(i),  &
      standard_pressure(heights(i)), &
      standard_pressure(heights(i),method="berg") , &
      standard_pressure(heights(i),method="simple"), & 
      standard_pressure(heights(i),method="full",dz=dble(1)), &
      standard_pressure(heights(i),method="full",dz=dble(100))
  enddo


end program
