module mod_spherical
  use mod_constants, only: dp

contains
! =============================================================================
!> Calculate area of spherical segment 
!!
!! Computes spherical area on unit (default if optional argument \c radius is
!! not given) sphere given by:
!!   - method 1 (\c alternative_method not given or \c alternative_method .false.)
!!    - distance from station, segment size in spher distance and angle
!!   - method 2 (\c alternative_method .true.)
!!    - distance from station, segment size in spher distance and angle
!!
!! 
!! The ilustration explain optional \c method argument
!! \latexonly
!! \begin{center}
!!  \tikzsetfigurename{spher_area} 
!!  \input{/home/mrajner/src/grat/doc/rysunki/spher_area}
!! \end{center}
!! \endlatexonly
!! \image html /home/mrajner/src/grat/doc/rysunki/spher_area.svg
!!
!! \warning All input angles in radiana output area on unit sphere or 
!! in square units of given (optionally) \c radius.
! =============================================================================
subroutine spher_area (distance ,ddistance, azstp,  area, radius, alternative_method )
  real(dp), intent(out) :: area
  real(dp), intent(in)  :: distance,ddistance 
  real(dp), intent(in)  :: azstp
  logical, intent(in), optional :: alternative_method
  real(dp), intent(in), optional :: radius

  if (present(alternative_method).and.alternative_method) then
    area =  (-cos (ddistance) + cos (distance))*(azstp)
  else
    area =  (-cos (distance+ddistance/2.)+cos(distance-ddistance/2.))*(azstp)
  endif
  if(present(radius)) area = area * radius**2

end subroutine

! =============================================================================
!> This soubroutine gives the latitude and longitude of the point at the 
!! specified distance and azimuth from site latitude and longitude.
!!
!! all parameters in decimal degree
!! \author D.C. Agnew \cite Agnew96
!! \date 2012
!! \author M. Rajner - modification
!! \date 2013-03-06
! =============================================================================
subroutine spher_trig ( latin , lonin , distance , azimuth , latout , lonout)
  use mod_utilities, only: d2r, r2d
  real(dp) , intent(in)  :: distance 
  real(dp) , intent(in)  :: latin , lonin , azimuth
  real(dp) , intent(out) :: latout, lonout 
  real(dp):: sg, cg , saz ,caz , st ,ct , cd ,sd  , cb , sb

  ct  = cos (d2r(90.-latin))
  st  = sin (d2r(90.-latin))
  cd  = cos (d2r(distance))
  sd  = sin (d2r(distance))
  saz = sin (d2r(azimuth))
  caz = cos (d2r(azimuth))
  cb = cd*ct + sd*st*caz
  !  todo !if(abs(cb).gt.1) cb = cb/abs(cb)
  sb = sqrt(1.-cb**2)
  latout = 90 - r2d(acos(cb))
  lonout = lonin + r2d(atan2(sd*saz/sb,(st*cd - sd*ct*caz)/sb))
end subroutine

! =============================================================================
!> For given coordinates for two points on sphere calculate distance and
!! azimuth in radians
!!
!! Input coordinates ub
!! \author M. Rajner
!! \date 2013-03-04
!! for small spherical distances you should always use havesine=.true.
!!
!! All arguments in radians
! =============================================================================
subroutine spher_trig_inverse (lat1, lon1, lat2 , lon2 , distance , azimuth, haversine)

  real(dp) , intent (in)         :: lat1 , lon1 , lat2 , lon2
  real(dp) , intent (out)        :: distance , azimuth
  real(dp)                       :: dlat , dlon ,  distancetmp , a
  logical, intent(in) , optional :: haversine

  dlon = lon2 - lon1
  dlat = lat2 - lat1

  if (dlon > pi) dlon=dlon-pi  !todo check if this is necessary

  if (present(haversine).and.haversine.eq..true.) then
    ! the formula below from Robert Chamberlain
    ! http://www.usenet-replayer.com/faq/comp.infosystems.gis.html
    a = (sin(dlat/2))**2 + cos(lat1) * cos(lat2) * (sin(dlon/2))**2
    distance = 2 * atan2( sqrt(a), sqrt(1-a) )
  else 
    distance = acos ( sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(dlon))
  endif

  azimuth = atan2( (sin(dlon)*cos(lat2)/sin(distance)) ,  ((sin(lat2)*cos(lat1) - cos(lat2)*sin(lat1)*cos(dlon))/sin(distance))  )
  if (azimuth.lt.0) azimuth = azimuth + 2 * pi
end subroutine

end module 
