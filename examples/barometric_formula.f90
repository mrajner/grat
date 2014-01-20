program barometric_formula
  use mod_constants
  use mod_utilities ,only: linspace, spline, ispline
  use mod_atmosphere
  use mod_aggf
  implicit none

  real (dp), allocatable, dimension(:) :: heights , pressures, pressures2
  integer :: i, nheight

  real(dp) :: dz
  real(dp) :: cpu(2)

  call cpu_time(cpu(1))

  dz=1
  nheight=nint(60000./dz)
  allocate(heights(nheight))
  allocate(pressures(nheight))
  allocate(pressures2(nheight))
  heights = linspace(real(0,dp),real(60000,dp),nheight) 

  

  pressures(1) = standard_pressure(heights(1), h_zero=heights(1),method="standard")
  do i = 2 , nheight
    pressures(i) = standard_pressure(heights(i),p_zero=pressures(i-1),h_zero = heights(i-1),method="standard")
  enddo
  pressures2(1) = standard_pressure(heights(1), h_zero=heights(1), method="full", use_standard_temperature=.true.)
  do i = 2 , nheight
    pressures2(i) = standard_pressure(heights(i),p_zero=pressures2(i-1),h_zero = heights(i-1),method="full",use_standard_temperature=.true. )
  enddo

   print '(10a18)' , "h", "simple", "berg", "pred", "pred2", "std" , "full"
   do i = 1, nheight,200
     ! if (i.gt. 20 .and. i.lt.nheight-20) then
       ! cycle
     ! endif
     ! if (heights(i).lt.1000.or.heights(i).gt.1001) cycle
     print '(10f18.9)' ,                                         & 
       heights(i),                                               & 
       1e-2*standard_pressure(heights(i),method="simple"),            & 
       1e-2*standard_pressure(heights(i),method="berg") ,             & 
       1e-2*pressures(i),                                             & 
       1e-2*pressures2(i),                                            & 
       1e-2*standard_pressure(heights(i),method="standard"),                            & 
       1e-2*standard_pressure(heights(i),method="full", use_standard_temperature=.true.)
   enddo

  call cpu_time(cpu(2))
  print '(a,f10.3)' , "execution time [s]:", cpu(2)-cpu(1)
end program
