!> This program can be used to check the default behaviour of point selection
!! used by module grat_polygon
!! \page polygon_check-h polygon_check
!!    \include polygon_check.hlp

program polygon_check
  use iso_fortran_env
  use mod_cmdline
  use mod_constants
  use mod_data
  use mod_polygon
  
  implicit none
  integer i , j
  real x , y , z
  integer iok 

  ! gather cmd line option decide where to put output
  call intro (program_calling = "polygon_check" )

  ! print header to log: version, date and summary of command line options
  call print_settings ("polygon_check")

  call read_polygon ( polygons(1) )

  if (size (polygons(1)%polygons).eq.0 ) then
    do i=1 , size (sites)
      write (output%unit , '(2f10.5)' ) sites(i)%lon, sites(i)%lat   
    enddo
  else
    do i=1 , size (sites)
      call chkgon ( sites(i)%lon, sites(i)%lat , polygons(1) , iok )
      write (output%unit , '(2f10.5,i4)' ) sites(i)%lon, sites(i)%lat   , iok
    enddo
  endif
end program

