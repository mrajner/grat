program denorm
  use mod_green
  use mod_constants, only:dp ,earth
  use mod_parser, only : intro
  use mod_data, only : model
  use mod_utilities
  use mod_spherical

  character (70) :: line , fileout, &
    lineform  = '(i1,i3,2i4,3f10.4,5x,a1)', &
    greenform = '(7e13.6)'
  character (1) :: fine
  integer :: fileinunit ,fileoutunit 
  real (dp) :: greenf(7) , greenf_de(7)
  real (dp) :: dl, delta

  call intro & 
    (program_calling = "denorm" , &
    accepted_switches="GSF" , &
    cmdlineargs=.true. &
    )

  open (newunit = fileinunit, file=model(1)%name, action="read")
  
  do 
    i = index(model(1)%name,"/")
    model(1)%name = model(1)%name(i+1:)
    if (i.eq.0) exit
  enddo
 fileout = "/home/mrajner/dat/green/"//model(1)%name
 open (newunit=fileoutunit, file = trim(fileout)//".deintegrated", action = "write")


  read (fileinunit, '(a)') line
  line(1:1)="#"
  write(fileoutunit, '(a)') line

  read (fileinunit, lineform)  ngr , j , m 
  backspace(fileinunit)
  
  do irange = 1, m
    read (fileinunit, lineform)  ngr , j , m  , n  , dl , dh , delta , fine
    do idistance = 1 , n
      read (fileinunit, greenform ) , greenf

      ! for strain and tilt different denormalization
      greenf(1:3) = greenf(1:3) / ( 4 * cos (d2r(dl)/2.)*sin(d2r(delta)/4) )
      greenf(1:3) = greenf(1:3) / ( earth%radius**2 * 2 * sin(d2r(dl)/2.)/ d2r(dl)) *1e12 * earth%radius
      greenf(3) = greenf(3) * 1e6

      write(fileoutunit, '(f10.4 , 3e12.4)') , dl , greenf(1:3) 

      if (abs(dl -dh + (n- idistance)*delta).gt.1e-5) stop "alarm!"
      dl = dl + delta
    enddo
  enddo

  ! integrated
!  dstep=10
!  do i = 1,180
!    print * , i , spher_area(d2r(dble(i)), &
!   d2r(dble(dstep)), dble(2*pi)) , sin(d2r(dble(i))) *2*pi * d2r(dble(dstep))
!  enddo

end program
