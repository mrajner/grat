program check_transfer
!  use mod_aggf, only:standard_pressure, standard_temperature
  use mod_constants
!  use mod_green
  use mod_utilities ,only: linspace
  use mod_atmosphere
  use mod_aggf
  implicit none

  real (dp), allocatable, dimension(:) :: heights , pressures, pressures2
  integer :: i, nheight

  real(dp) :: dz
  real(dp) :: cpu(2)
  character(20):: method

  call cpu_time(cpu(1))

  dz =4000
  nheight=nint(60000./dz)
  allocate(heights(nheight))
  allocate(pressures(nheight))
  allocate(pressures2(nheight))

  heights = linspace(real(0,dp),real(60000,dp),nheight) 

  pressures(1) = standard_pressure(heights(1), h_zero=heights(1))
  do i = 2 , nheight
    pressures(i) = standard_pressure(heights(i),p_zero=pressures(i-1),h_zero = heights(i-1))
  enddo
  pressures2(1) = standard_pressure(heights(1), h_zero=heights(1), method="full")
  do i = 2 , nheight
    pressures2(i) = standard_pressure(heights(i),p_zero=pressures2(i-1),h_zero = heights(i-1),method="full" )
  enddo

  do i = max(1,nheight-40) , nheight
    print '(10f18.9)' , heights(i),  &
      standard_pressure(heights(i)), &
      standard_pressure(heights(i),method="berg") , &
      pressures(i), &
      pressures2(i), &
      standard_pressure(heights(i),method="simple"), & 
      standard_pressure(heights(i),method="full", dz=dble(0.1)) 
  enddo
    print '(10a18)' , "h", "std", "berg", "pred", "pred2", "simple" , "full"

  call cpu_time(cpu(2))
  print '(a,f10.3)' , "execution time [s]:", cpu(2)-cpu(1)


end program
