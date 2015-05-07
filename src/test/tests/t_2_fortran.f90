program test2
  use mod_spherical
  use mod_utilities, only: d2r
  
  print * ,                     &
    spher_area(                 &
    distance  = 0._dp,          &
    ddistance = d2r(180._dp),   &
    azstp     = d2r(360._dp),   &
    alternative_method = .true. &
  ) - 4 * pi

  print * ,                     &
    spher_area(                 &
    distance  = pi/2.,          &
    ddistance = d2r(180._dp),   &
    azstp     = d2r(360._dp),   &
    alternative_method = .false. &
    ) - 4 * pi


  dtot  = 0
  dtot2  = 0
  idlat = 60
  idlon = 120
  idlat = 1
  idlon = 1

  do ilat = -90, 90-idlat, idlat
    do ilon = 0 , 360-idlon, idlon

      ! print*, ilat, ilon, 90 - ilat -idlat , 90 -ilat
      dtot = dtot +                           &
        spher_area(                           &
        distance  = d2r(90._dp - ilat-idlat), &
        ddistance = d2r(90._dp -ilat),        &
        azstp     = d2r(real(idlon,dp)),      &
        alternative_method = .true.           &
        )

      dtot2 = dtot2 +                           &
        spher_area(                             &
        distance  = d2r(90._dp - ilat-idlat/2), &
        ddistance = d2r(real(idlat,dp)),        &
        azstp     = d2r(real(idlon,dp)),        &
        alternative_method = .false.            &
        )

    enddo
  enddo

  print '(f10.7)', dtot  - 4*pi , dtot2 - 4*pi


end program
