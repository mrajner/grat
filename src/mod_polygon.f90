! ==============================================================================
!> \file
!! Some routines to deal with inclusion or exclusion of polygons
!!
!! \author M.Rajner
!! \date 2012-12-20
!! \date 2013-03-19
!!    added overriding of poly use by command line like in \cite spotl
! ==============================================================================
module mod_polygon
  use mr_constants, only : dp

  implicit none

  !----------------------------------------------------
  ! polygons
  !----------------------------------------------------
  type polygon_data
    logical :: use
    real(dp), allocatable , dimension (:,:) :: coords
  end type

  type polygon_info
    integer :: unit
    character(len=200) :: name
    character(len=25) :: dataname
    type(polygon_data), dimension (:), allocatable :: polygon
    logical :: if

    ! global setting (+|-) which override the one specified in polygon file
    character(1):: pm
  end type

  type(polygon_info) , allocatable, dimension (:) :: polygon

contains

! =============================================================================
!> This subroutine parse polygon information from command line entry
!!
!! \author M. Rajner
!! \date 2013.05.20
! =============================================================================
subroutine parse_polygon(cmd_line_entry)
  use mod_printing
  use mod_cmdline
  use mr_utilities, only: file_exists

  type(cmd_line_arg), intent(in) :: cmd_line_entry
  integer :: i

  if (allocated(polygon)) then
    call print_warning ("repeated")
    return
  endif

  allocate(polygon(size(cmd_line_entry%field)))

  do i=1, size(cmd_line_entry%field)
    polygon(i)%name=cmd_line_entry%field(i)%subfield(1)%name

    if(i.gt.1.and.cmd_line_entry%field(i)%subfield(1)%name.eq."") then
      polygon(i)%name= polygon(i-1)%name
    endif

    polygon(i)%dataname=cmd_line_entry%field(i)%subfield(1)%dataname
    write(log%unit, form%i2) 'polygon file:' , trim(polygon(i)%name)

    if (file_exists((polygon(i)%name))) then

      polygon(i)%if=.true.

      if(cmd_line_entry%field(i)%subfield(2)%name.eq."+" &
        .or.cmd_line_entry%field(i)%subfield(2)%name.eq."-" ) then
        polygon(i)%pm = cmd_line_entry%field(i)%subfield(2)%name
        write(log%unit, form%i3) "global override:", polygon(i)%pm
      endif
      call read_polygon (polygon(i))

    else
      stop 'file do not exist. Polygon file PROBLEM'
    endif
  enddo

end subroutine

subroutine read_polygon(polygon)

  use, intrinsic :: iso_fortran_env
  use mr_utilities, only: skip_header
  use mod_printing

  type(polygon_info) :: polygon
  integer :: i, j, number_of_polygons, nvertex
  character (1) :: pm

  if (polygon%if) then
    ! polygon file
    open (newunit = polygon%unit , action = "read", file = polygon%name )

    ! first get the number of polygon
    call skip_header(polygon%unit)
    read(polygon%unit, *) number_of_polygons
    allocate (polygon%polygon(number_of_polygons))

    ! loop over all polygons in file
    do  i=1, number_of_polygons

      call skip_header(polygon%unit)
      read(polygon%unit, *) nvertex
      allocate (polygon%polygon(i)%coords(nvertex, 2 ))

      call skip_header(polygon%unit)
      read(polygon%unit, *) pm

      if (pm.eq."+") polygon%polygon(i)%use=.true.
      if (pm.eq."-") polygon%polygon(i)%use=.false.

      ! override file +|- with global given with command line
      if (polygon%pm.eq."+") polygon%polygon(i)%use=.true.
      if (polygon%pm.eq."-") polygon%polygon(i)%use=.false.

      do j = 1 , nvertex
        call skip_header(polygon%unit)
        ! lon lat , checks while reading
        read (polygon%unit, * ) polygon%polygon(i)%coords(j,1:2)
        if ( polygon%polygon(i)%coords(j,1).lt.-180.        &
          .or.polygon%polygon(i)%coords(j,1).gt.360.        &
          .or.polygon%polygon(i)%coords(j,2).lt.-90.        &
          .or.polygon%polygon(i)%coords(j,2).gt. 90. ) then
          write (error_unit , form_63) "Somethings wrong with coords in polygon file"
          polygon%if=.false.
          return

        elseif ( polygon%polygon(i)%coords(j,1).lt.0. ) then
          polygon%polygon(i)%coords(j,1) = polygon%polygon(i)%coords(j,1) + 360.
        endif

      enddo
    enddo
    close (polygon%unit)

    ! print summary to log file
    write (log%unit, form_63) "name:", trim(polygon%name)
    write (log%unit, form_63) "number of polygons:" , size (polygon%polygon)

    do i = 1 , size (polygon%polygon)
      if (polygon%pm.eq."+".or.polygon%pm.eq."-") write (log%unit, form_63) &
        "Usage overwritten with command line option", polygon%pm
      write (log%unit, form_63) "use [true/false]:" , &
        polygon%polygon(i)%use
      write (log%unit, form_63) "number of coords:" , &
        size (polygon%polygon(i)%coords(:,1))
    enddo
  endif

end subroutine

! ==============================================================================
!> Check if point is in closed polygon
!!
!! From spotl \cite Agnew97
!! adopted to \c grat and Fortran90 syntax
!! From original description
!!  returns iok=0 if
!!     1. there is any polygon (of all those read in) in which the
!!        coordinate should not fall, and it does
!!             or
!!     2. the coordinate should fall in at least one polygon
!!        (of those read in) and it does not
!!    otherwise returns iok=1
!! \author D.C. Agnew \cite Agnew96
!! \author adopted by Marcin Rajner
!! \date 2013-03-04
!!
!! The ilustration explain exclusion idea\n
!! \image latex /home/mrajner/src/grat/doc/figures/polygon_ilustration.pdf "capt" width=\textwidth
!! \image html /home/mrajner/src/grat/doc/figures/polygon_ilustration.png

!! all values in decimal degrees
! ==============================================================================
subroutine chkgon(rlong, rlat, polygon, iok)
  real(dp),intent (in) :: rlong, rlat
  integer :: i, ianyok
  integer(2), intent (out) :: iok
  real(dp) :: rlong2
  type(polygon_info), intent (in) :: polygon

  !  ! Check first if we need to use this soubroutine
  if (size(polygon%polygon).eq.0) then
    iok=0
    return
  endif

  if(rlong.gt.180) rlong2 = rlong - 360.
  ! loop over polygons
  do i=1,size(polygon%polygon)
    ! loop twice for elastic and newtonian
    ! polygon is one we should not be in
    if(.not.polygon%polygon(i)%use) then
      if (  if_inpoly(rlong  ,rlat,polygon%polygon(i)%coords).ne.0   &
        .or.if_inpoly(rlong2 ,rlat,polygon%polygon(i)%coords).ne.0 ) &
        then
        iok=0
        return
      endif
    endif
  enddo
  ianyok=0
  ! polygon is one we should be in; test to see if we are, and if so set
  ! iok to 1 and return
  do i=1,size(polygon%polygon)
    if(polygon%polygon(i)%use) then
      ianyok = ianyok+1
      if (  if_inpoly(rlong  ,rlat,polygon%polygon(i)%coords).ne.0   &
        .or.if_inpoly(rlong2 ,rlat,polygon%polygon(i)%coords).ne.0 ) &
        then
        iok=1
        return
      endif
    endif
  enddo
  ! not inside any polygon%polygons; set iok to 0 if there are any we should have
  ! been in
  iok = 1
  if(ianyok.gt.0) iok = 0
  return
end subroutine

! ==============================================================================
!! taken from spotl \cite Agnew97
!! \par oryginal comment:
!!  Rewritten by D. Agnew from the version by Godkin and Pulli,
!!  in BSSA, Vol 74, pp 1847-1848 (1984)
!! adopted and slightly modified M. Rajner
!! cords is x, y (lon, lat) 2 dimensional array
! ==============================================================================
pure function if_inpoly(x,y,coords)
  use mr_constants, only: dp
  real(dp), allocatable, dimension (:,:), intent (in) :: coords
  real(dp), intent (in) :: x , y
  integer :: i , isc
  integer :: if_inpoly
  ! Returns 1 if point at (x,y) is inside polygon whose nv vertices
  ! Returns 0 if point is outside
  ! Returns 2 if point is on edge or vertex

  if_inpoly = 0
  do  i=1, size(coords(:,1))-1
    isc = ncross( &
      coords(i,1)   - x,  &
      coords(i,2)   - y,  &
      coords(i+1,1) - x,  &
      coords(i+1,2) - y )
    !  on edge - know the answer
    if(isc.eq.4) then
      if_inpoly = 2
      return
    endif
    if_inpoly = if_inpoly + isc
  enddo
  ! check final segment
  isc = ncross(                       &
    coords(size(coords(:,1)), 1) - x, &
    coords(size(coords(:,2)), 2) - y, &
    coords(1, 1) - x,                 &
    coords(1, 2) - y)
  if(isc.eq.4) then
    if_inpoly = 2
    return
  endif
  if_inpoly = if_inpoly + isc
  if_inpoly = if_inpoly/2
  ! convert to all positive (a departure from the original)
  if_inpoly = iabs(if_inpoly)
  return
end function

! ==============================================================================
!> \brief finds whether the segment from point 1 to point 2 crosses
!!   the negative x-axis or goes through the origin (this is
!!   the signed crossing number)
!!
!!     return value       nature of crossing
!!         4               segment goes through the origin
!!         2               segment crosses from below
!!         1               segment ends on -x axis from below
!!                          or starts on it and goes up
!!         0               no crossing
!!        -1               segment ends on -x axis from above
!!                          or starts on it and goes down
!!        -2               segment crosses from above
!!
!! taken from spotl \cite Agnew97
!! slightly modified
! ==============================================================================
pure function ncross(x1,y1,x2,y2)
  real(dp) , intent(in) :: x1 , y1, x2 , y2
  real(dp) :: c12 , c21
  integer :: ncross

  ! all above (or below) axis
  if(y1*y2.gt.0) then
    ncross = 0
    return
  endif

  c12 = x1*y2
  c21 = x2*y1

  ! through origin
  if(c12.eq.c21.and.x1*x2.le.0.) then
    ncross = 4
    return
  endif

  ! touches +x axis; crosses +x axis; lies entirely on -x axis
  if(   (y1.eq.0.and.x1.gt.0)                          &
    .or.(y2.eq.0.and.x2.gt.0)                          &
    .or.((y1.lt.0).and.(c12.gt.c21))                   &
    .or.((y1.gt.0).and.(c12.lt.c21))                   &
    .or.(y1.eq.0.and.y2.eq.0.and.x1.lt.0.and.x2.lt.0)) &
    then
    ncross = 0
    return
  endif

  ! cross axis
  if(y1.ne.0.and.y2.ne.0) then
    if(y1.lt.0) ncross = 2
    if(y1.gt.0) ncross = -2
    return
  endif

  ! one end touches -x axis - goes which way?
  if(y1.eq.0) then
    if(y2.lt.0) ncross = -1
    if(y2.gt.0) ncross = 1
  else
    ! y2=0 - ends on x-axis
    if(y1.lt.0) ncross = 1
    if(y1.gt.0) ncross = -1
  endif

  return
end function

end module

!\appendix
! \chapter{Polygon}
!  This examples show how the exclusion of~selected polygons works
!  \begin{figure}[htb]
!    \includegraphics[width=0.5\textwidth]{../mapa1}
!    \caption{If only excluded polygons (red area) are given
!    all points falling in~it will be excluded (red points) all other
!    will be included}
!  \end{figure}
!  \begin{figure}
!    \includegraphics[width=0.5\textwidth]{../mapa2}
!    \caption{If at least one included are are given
!    (green area) than all points which not fall into included area will
!    be excluded}
!  \end{figure}
!  \begin{figure}
!    \includegraphics[width=0.5\textwidth]{../mapa3}
!    \caption{If there is overlap of~polygons the exclusion has higher
!    priority}
!  \end{figure}
!  \chapter{Interpolation}
!  \begin{figure}
!  \input{/home/mrajner/src/grat/doc/interpolation_ilustration.tex}
!  \caption{Interpoloation}
!  \end{figure}
