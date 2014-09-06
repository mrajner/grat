program a
  use mod_atmosphere
  integer:: dp=8

  
  print * , standard_pressure (height=300._8, method="full", h_zero=15.22222_8, temperature=300._8, use_standard_temperature=.false.)
  print * , standard_pressure (height=300._8, method="full", h_zero=55._8, temperature=300.2_8, use_standard_temperature=.true.)
  print * , standard_pressure (height=100.04_8, method="full", h_zero=55._8, temperature=300.2_8, use_standard_temperature=.true.)

  print *
  print *, "98081.6445982020"
  print *, "98531.6468205247" 
  print *, "100807.186135159"
  end program

