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
  real(sp) :: x , y , z , lat ,lon ,val(0:100) !tmp variables
  integer :: i , j , ii, iii

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
   
  allocate (results(size(sites)*max(size(dates),1)))
  iii=0
  do j = 1 , max(size (dates),1)
    if(size(dates).gt.0)  write(output%unit, '(i4,5(i2.2))', advance ="no") dates(j)%date
  
    do ii = 1 , min(2,size(model))
      if (model(ii)%if) call get_variable ( model(ii) , date = dates(j)%date)
    enddo



!todo
    do i = 1 , size(sites)
      write(output%unit, '(2f15.5f)', advance ="no") sites(i)%lat ,sites(i)%lon
      iii=iii+1
      call convolve (sites(i) , green , results(iii), denserdist = denser(1) , denseraz = denser(2))
      write (output%unit,'(15f13.5)') , results(iii)%e ,results(iii)%n  ,results(iii)%dt , results(iii)%dh, results(iii)%dz
    enddo
  enddo

! print '(15f13.5)',  results(maxloc (results%e))%e - results(minloc (results%e))%e       ,&
!           results(maxloc (results%n))%n - results(minloc (results%n))%n       ,&
!           results(maxloc (results%dh))%dh - results(minloc (results%dh))%dh   ,&
!           results(maxloc (results%dz))%dz - results(minloc (results%dz))%dz   ,&
!           results(maxloc (results%dt))%dt - results(minloc (results%dt))%dt
  

  call cpu_time(cpu_finish)
  write(log%unit, '(/,"Execution time:",1x,f16.9," seconds")') cpu_finish - cpu_start
  write(log%unit, form_separator)

  print * , model(6)%level
  print *
  lat =00
  lon = 00
  call get_value(model(7),lat,lon, val(0))
  do i =1, size(model(6)%level)
  call get_value(model(6),lat,lon, val(i), level = i, method=2)
  enddo
  print  '(30f10.2)', lat , lon , (val(i), i=0,size(model(6)%level))
  print  '(30f10.2)' , lat , lon , (geop2geom(val(i)/1000)*1000., i=0,size(model(6)%level))

end program 
