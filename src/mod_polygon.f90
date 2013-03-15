module mod_polygon

  implicit none
  private

  public :: read_polygon ,chkgon

contains

! ==============================================================================
!> Reads polygon data
!!
!! inspired by spotl \cite Agnew97
! ==============================================================================
subroutine read_polygon (polygon)
  use mod_cmdline
  use mod_utilities, only: skip_header
  type(polygon_info):: polygon
  integer :: i , j , number_of_polygons , nvertex
  character(80) :: dummy
  character (1)  :: pm

  if (polygon%if) then
    ! polygon file
    open (newunit = polygon%unit , action="read", file=polygon%name )
      
      ! first get the number of polygons
      call skip_header(polygon%unit)
      read (polygon%unit , * ) number_of_polygons
      allocate (polygon%polygons(number_of_polygons))

      ! loop over all polygons in file
      do  i=1, number_of_polygons
        call skip_header(polygon%unit)
        read (polygon%unit, * ) nvertex
        allocate (polygon%polygons(i)%coords(nvertex, 2 ))
        call skip_header(polygon%unit)
        read (polygon%unit, * ) pm
          if (pm.eq."+") polygon%polygons(i)%use=.true.
          if (pm.eq."-") polygon%polygons(i)%use=.false.
        do j = 1 , nvertex
          call skip_header(polygon%unit)
          ! lon lat , checks while reading
          read (polygon%unit, * ) polygon%polygons(i)%coords(j,1:2)
          if ( polygon%polygons(i)%coords(j,1).lt.-180. &
           .or.polygon%polygons(i)%coords(j,1).gt.360.  & 
           .or.polygon%polygons(i)%coords(j,2).lt.-90.  & 
           .or.polygon%polygons(i)%coords(j,2).gt. 90. ) then 
            write (error_unit , form_63) "Somethings wrong with coords in polygon file"
            call exit
          elseif ( polygon%polygons(i)%coords(j,1).lt.0. ) then
            polygon%polygons(i)%coords(j,1) = polygon%polygons(i)%coords(j,1) + 360.
          endif
        enddo
      enddo
    close (polygon%unit)

    ! print summary to log file
    write (log%unit, form_separator) 
    write (log%unit, form_60) "Summary of polygon file", trim(polygon%name)
    write (log%unit, form_61) "number of polygons:" , size (polygon%polygons)
    do i = 1 , size (polygon%polygons)
      write (log%unit, form_62) "[true/false] number of coords :" , &
        polygon%polygons(i)%use , size (polygon%polygons(i)%coords(:,1)) 
    enddo
  endif

end subroutine


! ==============================================================================
!> Check if point is in closed polygon
!!
!! If it is first call it loads the model into memory
!! inspired by spotl \cite Agnew97
!! adopted to grat and Fortran90 syntax
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
! ==============================================================================
subroutine chkgon (rlong , rlat , polygon , iok)
  use mod_constants, only: dp , dp
  use mod_cmdline
  real(dp),intent (in) :: rlong, rlat
  integer :: i ,ii , ianyok
  integer , intent (out) :: iok
  real(dp) :: rlong2
  type( polygon_info ) , intent (in) :: polygon
  
  ! Check first if we need to use this soubroutine
  if (size(polygon%polygons).eq.0) then
    iok=0
    return
  endif

  if(rlong.gt.180) rlong2 = rlong - 360.
  ! loop over polygons
  do i=1,size(polygon%polygons)
    ! loop twice for elastic and newtonian
    ! polygon is one we should not be in
    if(.not.polygon%polygons(i)%use) then
      if (  if_inpoly(rlong  ,rlat,polygon%polygons(i)%coords).ne.0 &
        .or.if_inpoly(rlong2 ,rlat,polygon%polygons(i)%coords).ne.0 ) then
        iok=0
        return
      endif
    endif
  enddo
    ianyok=0
  ! polygon is one we should be in; test to see if we are, and if so set
  ! iok to 1 and return
  do i=1,size(polygon%polygons)
    if(polygon%polygons(i)%use) then
       ianyok = ianyok+1
      if (  if_inpoly(rlong  ,rlat,polygon%polygons(i)%coords).ne.0 &
        .or.if_inpoly(rlong2 ,rlat,polygon%polygons(i)%coords).ne.0 ) then
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
integer function if_inpoly(x,y,coords) 
  use mod_constants, only: dp, dp
  real(dp) ,allocatable , dimension (:,:) , intent (in) :: coords  
  real(dp) , intent (in) :: x , y
  integer :: i , isc  
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
  isc = ncross( &
    coords (size(coords(:,1)) , 1 ) - x , &
    coords (size(coords(:,2)) , 2 ) - y , &
    coords (1 , 1 ) - x , & 
    coords (1 , 2 ) - y )
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
integer function ncross(x1,y1,x2,y2)
  use mod_constants, only: dp, dp
  use mod_cmdline
  real(dp) , intent(in) :: x1 , y1, x2 , y2 
  real(dp) :: c12 , c21

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
  if(   (y1.eq.0.and.x1.gt.0)    &
    .or.(y2.eq.0.and.x2.gt.0) &
    .or.((y1.lt.0).and.(c12.gt.c21)) & 
    .or.((y1.gt.0).and.(c12.lt.c21)) &
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
