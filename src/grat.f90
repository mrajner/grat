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
!  character(255) :: dummy
!  real :: del, grav=0. ,cd ,sd, rlato ,rlong , ddist, pole ,cale_pole, normalizacja, cisnienie_stacja, temperatura_stacja
!  integer :: ii , naz , jj , i , j
!  integer , parameter :: minaz =50 !mrajner 2012-10-03 14:24
!  !integer , parameter :: minaz =1
!  integer , parameter :: ile  = 5 !mrajner 2012-10-03 14:24
!  !integer , parameter :: ile   = 1
!  real :: azstp, azstpd, azimuth ,caz ,saz,saztp, caztp,stpfac ,cb , sb ,sg ,cg
!  real ::  xx 
!  real :: grav_merriam_e=0. ,grav_merriam_n=0. , grav_merriam_s=0. ,grav_merriam_e_nib=0.
!  real ::grav_merriam_n_t=0. 
!  real ::grav_merriam_n_h=0. 
!  real :: admit3
!  real,dimension(85) :: b,c,d
!  integer:: przebieg ,licznik
!  real, dimension(6) :: values_interpolowane
!  real , dimension(:,:), allocatable :: tablica
!  integer :: ile_plikow
!  real :: szerokosc_zmienna , dlugosc_zmienna , wysokosc_stacji_etopo2
!  logical :: czy_otworzyc_nowy_plik=.true.


  real(sp) :: x , y , z , lat ,lon ,val !tmp variables
  integer :: i , j
  integer :: d(6)

  !> program starts here with time stamp
  call cpu_time(cpu_start)

  ! gather cmd line option decide where to put output
  call intro ( program_calling = "grat" )

  ! print header to log: version, date and summary of command line options
  call print_settings (program_calling = "grat")

  ! read polygons into memory
!  call read_polygon (polygons(1))
!  call read_polygon (polygons(2))
  
  ! read models into memory
  do i =1 , size(model)
    if (model(i)%if) call read_netCDF ( model(i) )
  enddo

    

  do j = 1 , size (dates)
      call get_variable ( model(1) , date = dates(j)%date)

    do i = 1 , size(sites)
      call get_value(model(1), sites(i)%lat, sites(i)%lon , val)
      write(output%unit ,  '(f15.4,2x,i4,5i2.2,3f13.4)') ,mjd (dates(j)%date) , dates(j)%date , sites%lat, sites%lon, val
    enddo
  enddo
  
  ! todo wysokosci nad wodą ustaw na 0. Głębokość nie jest interesująca

  ! read Green function into memory




  call convolve (green)




  call cpu_time(cpu_finish)
  write(log%unit, '(/,"Execution time:",1x,f16.9," seconds")') cpu_finish - cpu_start
  write(log%unit, form_separator)

end program 
