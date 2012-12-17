program check_transfer
  use aggf

  real*8 :: pressure ,x , x2 , pressure2 , temperature

  x =1.
  x2 =2.


  do x =0,4

  call standard_pressure(x, pressure)
  print * ,x , pressure 
  enddo
  print *

  x=1.
  call standard_pressure(x, pressure)
  print * ,x , pressure 


  call standard_pressure(x+x2, pressure2)
  print * ,x+x2 , pressure2 


  call standard_temperature(x, temperature)
  call standard_pressure(x2, pressure2, p_zero = pressure, t_zero= temperature, if_simplificated=.true.)
  print * ,x+x2 , pressure2 , temperature

  call transfer_pressure ( x , x+x2, pressure , pressure2)
  print * ,x+x2 , pressure2 , temperature


end program
