program check_transfer
  use aggf

  real*8 :: pressure ,x , x2 , pressure2 , temperature , pressure3, pressure4
  real*8 :: h(23)

  i=0
  do x =0,11,0.5
    i=i+1
    h (i) = x
  enddo


  do i = 1 , size(h)

  if (i.gt.1) call standard_pressure(h(i), pressure3 , h_zero = h(i-1) ,p_zero = pressure )
  if (i.gt.1) call standard_pressure(h(i), pressure4 , h_zero = h(i-1) ,p_zero = pressure2 )

  call standard_pressure(h(i), pressure,if_simplificated=.true.)
  call standard_pressure(h(i), pressure2)
 

  print '(10f10.3)' ,h(i) , pressure , pressure2 , pressure3 ,pressure4
  enddo

  call standard_temperature(dble(25.) , temperature)
  print *, temperature

!  x=1.
!  call standard_pressure(x, pressure)
!  print * ,x , pressure 

!  call standard_pressure(x+x2, pressure2)
!  print * ,x+x2 , pressure2 

!  call standard_temperature(x, temperature)
!  call standard_pressure(x2, pressure2, p_zero = pressure, t_zero= temperature, if_simplificated=.true.)
!  print * ,x+x2 , pressure2 , temperature

!  call transfer_pressure ( x , x+x2, pressure , pressure2)
!  print * ,x+x2 , pressure2 , temperature


end program
