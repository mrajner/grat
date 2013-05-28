!> This program can be used to check the default behaviour of point selection
!! used by module grat_polygon
!! \page polygon_check-h polygon_check
!!    \include polygon_check.hlp

program polygon_check
  use mod_polygon
  use mod_parser
  use mod_site


  implicit none
  integer i , j
  integer , allocatable , dimension (:) :: iok 

  call intro  ( &
    program_calling = "polygon_check" ,  &
      accepted_switches = "VfABLPoShvIiR" , &
      cmdlineargs=.true. &
      )

    allocate(iok(size(polygon)))

    do i=1 , size (site)
      write (output%unit , '(a8,1x,2f10.5)' , advance="no" ) site(i)%name, site(i)%lon, site(i)%lat   
      if (allocated(polygon)) then
        do j = 1 , size(polygon)
          call chkgon (site(i)%lon, site(i)%lat, polygon(j), iok(j) )
        enddo
        write (output%unit, '(<size(iok)>i4)' ) , iok
      else
        write(output%unit, *)
      endif
    enddo
  end program
