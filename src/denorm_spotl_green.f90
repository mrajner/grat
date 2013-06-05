program denorm
  use mod_green
  character(255):: filein, fileout

  filein = "/home/mrajner/src/spotl/green/gr.gbaver.wef.p01.ce"
  !fileout= "tmp.green"
  
  call denormalize(filein)

end program
