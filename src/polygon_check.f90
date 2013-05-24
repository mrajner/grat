!> This program can be used to check the default behaviour of point selection
!! used by module grat_polygon
!! \page polygon_check-h polygon_check
!!    \include polygon_check.hlp

program polygon_check
  use mod_polygon
  use mod_parser
  use mod_site

!  use mod_polygon, only: read_polygon,chkgon, polygon
    

  implicit none
  integer i , j
  integer iok 

  ! gather cmd line option decide where to put output
  call intro ( &
    program_calling = "polygon_check" , &
      accepted_switches = "VfABLPoShvIiR" , &
      cmdlineargs=.true. &
      )


    do j = 1 , size(polygon)
      if (size (polygon(j)%polygon).eq.0) then
        do i=1 , size (site)
          write (output%unit , '(2f10.5)' ) site(i)%lon, site(i)%lat   
        enddo
      else
        do i=1 , size (site)
          call chkgon (site(i)%lon, site(i)%lat, polygon(j), iok )
          write (output%unit, '(2f10.5,i4)' ) site(i)%lat, site(i)%lon   , iok
        enddo
      endif 
    enddo
  end program

