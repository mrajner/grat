program check_transfer
!  use mod_aggf, only:standard_pressure, standard_temperature
  use mod_constants
!  use mod_green
  use mod_utilities ,only: linspace
  use mod_atmosphere
  implicit none

  real (dp), allocatable, dimension(:) :: heights , pressures
  integer :: i, nheight

  real(dp) :: cpu(2)
  call cpu_time(cpu(1))

  nheight=25
  allocate(heights(nheight))
  allocate(pressures(nheight))

  heights = linspace(real(0,dp),real(48000,dp),nheight)

  pressures(1) = standard_pressure(heights(1),method="full")
  do i = 2 , nheight
!    pressures(i) = standard_pressure(heights(i),p_zero=pressures(i-1),h_zero = heights(i-1),method="full")
    pressures(i) = standard_pressure(heights(i),p_zero=pressures(i-1),h_zero = heights(i-1))
  enddo

  do i = 1 , nheight
    if (i.eq.1) print '(10a18)' , "h", "--", "berg", "--", "simple" , "full"
    print '(10f18.9)' , heights(i),  &
      standard_pressure(heights(i)), &
      standard_pressure(heights(i),method="berg") , &
      pressures(i), &
      standard_pressure(heights(i),method="simple"), & 
      standard_pressure(heights(i),method="full") 
  enddo

  call cpu_time(cpu(2))
  print '(f10.3)' , cpu(2)-cpu(1)

end program
