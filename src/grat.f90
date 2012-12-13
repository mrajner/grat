! ==============================================================================
!> \file
!! \mainpage Grat overview
!! \section Purpose
!! This program was created to make computation of atmospheric gravity
!! correction more easy.
!!
!! \version v. 1.0
!! \date 2012-12-12
!! \author Marcin Rajner\n 
!! Politechnika Warszawska\n
!! (Warsaw University of Technology)
!! \line program
!!
!! \warning This program is written in Fortran90 standard but uses some featerus
!! of 2003 specification (e.g., \c 'newunit='). It was also written
!! for <tt>Intel Fortran Compiler</tt> hence some commands can be unavailable
!! for yours (e.g., \c <integer_parameter> for \c IO statements. This should be
!! easily modifiable according to your output needs.>
!! Also you need to have \c iso_fortran_env module available to guess the number
!! of output_unit for your compiler.
!! When you don't want a \c log_file and you don't switch \c verbose all 
!! unneceserry information whitch are normally collected goes to \c /dev/null
!! file. This is *nix system default trash. For other system or file system
!! organization, please change this value in \c get_cmd_line module.
! ==============================================================================
program grat
  use iso_fortran_env
  use get_cmd_line
  use mod_polygon
  use mod_data
  use mod_green


  implicit none
  real(sp) :: x , y , z , lat ,lon ,val !tmp variables
  integer :: i , j , ii
  integer :: d(6)

  !> program starts here with time stamp
  call cpu_time(cpu_start)

  ! gather cmd line option decide where to put output
  call intro ( program_calling = "grat" )

  ! print header to log: version, date and summary of command line options
  call print_settings (program_calling = "grat")

  
  ! read polygons
  do i =1 , 2
   call read_polygon (polygons(i))
  enddo

  ! read models into memory
  do i =1 , size(model)
    if (model(i)%if) call read_netCDF ( model(i) )
  enddo

  refpres%name="/home/mrajner/src/grat/data/refpres/vienna_p0.grd"
  call read_netCDF (refpres)
   

    

  do j = 1 , max(size (dates),1)
  
    do ii = 1 , 2
      call get_variable ( model(ii) , date = dates(j)%date)
    enddo

    do i = 1 , size(sites)
      call convolve (sites(1)  , green , denserdist = 0 , denseraz =1)
    enddo
  enddo
  





  call cpu_time(cpu_finish)
  write(log%unit, '(/,"Execution time:",1x,f16.9," seconds")') cpu_finish - cpu_start
  write(log%unit, form_separator)

end program 
