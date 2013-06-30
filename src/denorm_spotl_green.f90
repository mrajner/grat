program denorm
  use mod_green
  use mod_parser, only : intro
  use mod_data, only : model

  character (100) :: line
  integer :: fileinunit ,fileoutunit

  call intro & 
    (program_calling = "denorm" , &
    accepted_switches="dd" , &
    cmdlineargs=.true. &
    )

  print * , model%name
  open (newunit = fileinunit, file=model(1)%name, action="read")
  read (fileinunit, '(a)') line
  write(fileoutunit, '(a)') line

  read (fileinunit, *)
  

end program
