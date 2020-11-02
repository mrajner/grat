! this program will create data set for unit tests
program createdata
  use netcdf
  use mod_data, only: nc_error
  use mr_utilities, only: d2r, setnan
  use mr_conversion, only: celsius_to_kelvin
  use mr_constants, only: dp

  implicit none

  integer(2) :: i, ilevel, itime, ilat, ilon
  integer :: ncid,                          &
  londimid, latdimid, timedimid,leveldimid, &
  lonvarid, latvarid, timevarid,levelvarid, &
     gpvarid, spvarid, tvarid, lsvarid
   integer :: dimids_constants(2), dimids_surface(3), dimids_vertical(4)

  real, parameter :: resolution = 10
  real, parameter :: sp_scale=1., sp_offset=95000

  integer, parameter ::                  &
    nlon   = ceiling(360. / resolution), &
    nlat   = ceiling(180. / resolution), &
    nlevel = 5,                          &
    ntime  = 10

  real, parameter ::                                                            &
    lats   (nlat   )  = [ ( -89.75 + ( i-1 ) *resolution , i = 1 , nlat   ) ] , &
    lons   (nlon   )  = [ ( 0.25 +   ( i-1 ) *resolution , i = 1 , nlon   ) ] , &
    levels (nlevel )  = [ ( 1000-    ( i-1 ) *50         , i = 1 , nlevel ) ] , &
    times(ntime)= [ ((i-1)*6 , i=1, ntime) ]

  real , dimension(nlon, nlat, nlevel, ntime) :: gp 
  real , dimension(nlon, nlat, ntime) ::  sp, t
  real , dimension(nlon, nlat) :: ls
  real :: mix


  call nc_error (nf90_create(path = "data/test_data.nc", cmode = nf90_hdf5, ncid = ncid))

  call nc_error (nf90_def_dim(ncid = ncid , name = "lon"   , len = nlon           , dimid = londimid))
  call nc_error (nf90_def_dim(ncid = ncid , name = "lat"   , len = nlat           , dimid = latdimid))
  call nc_error (nf90_def_dim(ncid = ncid , name = "level" , len = nlevel         , dimid = leveldimid))
  call nc_error (nf90_def_dim(ncid = ncid , name = "time"  , len = nf90_unlimited , dimid = timedimid))

  call nc_error( nf90_def_var(ncid , "lat"   , NF90_REAL  , latdimid   , latvarid) )
  call nc_error( nf90_def_var(ncid , "lon"   , NF90_REAL  , londimid   , lonvarid) )
  call nc_error( nf90_def_var(ncid , "level" , nf90_short , leveldimid , levelvarid) )
  call nc_error( nf90_def_var(ncid , "time"  , nf90_short , timedimid  , timevarid) )

  dimids_constants =  [ londimid, latdimid  ]
  dimids_surface   =  [ londimid, latdimid , timedimid ]
  dimids_vertical  =  [ londimid, latdimid, leveldimid, timedimid ]

  call nc_error(nf90_def_var(ncid , "sp", NF90_short , dimids_surface,   spvarid))
  call nc_error(nf90_def_var(ncid , "t" , NF90_real  , dimids_surface,   tvarid))
  call nc_error(nf90_def_var(ncid , "ls", NF90_byte  , dimids_constants, lsvarid))

  call nc_error(nf90_def_var(ncid , "gp", NF90_int , dimids_vertical, gpvarid) )


  call nc_error(nf90_put_att(ncid, latvarid, "units", "degree") )
  call nc_error(nf90_put_att(ncid, lonvarid, "units", "degree") )

  call nc_error(nf90_put_att(ncid, spvarid, "units", "Pascal") )
  call nc_error(nf90_put_att(ncid, spvarid, "actual_range", [90000,105000]))
  call nc_error(nf90_put_att(ncid, spvarid, "scale_factor", sp_scale) )
  call nc_error(nf90_put_att(ncid, spvarid, "add_offset", sp_offset) )

  call nc_error(nf90_put_att(ncid, tvarid, "units", "Kelvin") )

  call nc_error(nf90_put_att(ncid, latvarid, "actual_range", [-90,90]) )
  call nc_error(nf90_put_att(ncid, lonvarid, "actual_range", [0,340]) )

  call nc_error(nf90_put_att(ncid, timevarid, "units", "hours since 2012-1-1 00:00:0.0") )
  call nc_error(nf90_enddef(ncid))

  call nc_error (nf90_put_var(ncid, latvarid, lats ))
  call nc_error (nf90_put_var(ncid, lonvarid, lons ))
  call nc_error (nf90_put_var(ncid, levelvarid, levels ))
  call nc_error (nf90_put_var(ncid, timevarid, times ))

  do ilat  = 1 , nlat
    do ilon  = 1 , nlon

      ls(ilon,ilat) = nint((sin(d2r(real(lons(ilon),dp)-90))**2)+0.3)

      do itime = 1 , ntime

        mix = cos(d2r(real(lats(ilat),dp)))*cos(d2r(real(lons(ilat),dp))) * sin(real(itime))

        sp(ilon,ilat,itime) = 101325  + 5000*mix

        t(ilon,ilat,itime) =  celsius_to_kelvin(15.0_dp)  + 30 * mix



        do ilevel = 1 , nlevel

          gp(ilon,ilat,ilevel,itime) = &
            lons(ilon) + 1* lats(ilat) + 0 *levels(ilevel) + 0 *times(itime)

        enddo

      enddo
    enddo
  enddo

  ! pack sp variable
  sp = nint((sp - sp_offset) / sp_scale)

  call nc_error (nf90_put_var(ncid, spvarid, sp))
  call nc_error (nf90_put_var(ncid, tvarid, t))
  call nc_error (nf90_put_var(ncid, lsvarid, ls))
  call nc_error (nf90_put_var(ncid, gpvarid, gp))

  call nc_error (nf90_close(ncid=ncid))
  call system("ncdump -hs data/test_data.nc ")

end program
