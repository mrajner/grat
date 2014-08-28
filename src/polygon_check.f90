!> This program can be used to check the default behaviour of point selection
!! used by module grat_polygon
!! \page polygon_check-h polygon_check
!!    \include polygon_check.hlp

program polygon_check
  use mod_polygon
  use mod_parser
  use mod_site

  implicit none
  integer(2) :: i , j
  integer(2) , allocatable , dimension (:) :: iok

  call intro  (                          &
    program_calling   = "polygon_check", &
    accepted_switches = "VfABLPoShvIiR", &
    version           = __VERSION__,     &
    cmdlineargs       = .true.           &
    )

  allocate(iok(size(polygon)))

  if (output%header) then
    write (output%unit , '(a8,1x,2a10)' , advance="no" ) "name", "lat", "lon"

    if (allocated(polygon)) then
      write (output%unit, '(<size(iok)>(2x,a1,i1))' ) , ("p",i,i=1,size(iok))
    else
      write(output%unit, *)
    endif

  endif

  do i=1 , size (site)
    write (output%unit, '(a8,1x,2f10.5$)') site(i)%name, site(i)%lon, site(i)%lat

    if (allocated(polygon)) then
      do j = 1 , size(polygon)
        call chkgon (site(i)%lon, site(i)%lat, polygon(j), iok(j))
      enddo
      write (output%unit, '(<size(iok)>i4)' ) , iok
    else
      write(output%unit, *)
    endif

  enddo
end program
