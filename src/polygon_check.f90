!> This program can be used to check the default behaviour of point selection
!! used by module grat_polygon
!! \page polygon_check-h polygon_check
!!    \include polygon_check.hlp

program polygon_check
  use mod_cmdline, only: polygons , sites , output , intro , print_settings
  use mod_polygon, only: read_polygon,chkgon
  
  implicit none
  integer i , j
  integer iok 

  ! gather cmd line option decide where to put output
  call intro ( program_calling = "polygon_check" , accepted_switches = "VfABLPoShvIiR" , cmdlineargs=.true.)

  ! read polygon - only first one
  call read_polygon (polygons(1))

  if (size (polygons(1)%polygons).eq.0) then
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

