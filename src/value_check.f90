! =============================================================================
!>  \file
!! \mainpage
!! \brief ...put...
!! \page value_check-h value_check 
!!    \include value_check.hlp
!! \date 2013-01-09
!! \author M. Rajner
!!
!! \date 2013-03-19 added -P (if point is excluded all values are zero)
! =============================================================================

program value_check 
  use mod_cmdline  , only: output      , sites , model , dates , print_settings , intro,nmodels , polygons
  use mod_data     , only: get_variable, get_value,read_netCDF
  use mod_constants, only: dp
  use mod_polygon  , only: read_polygon, chkgon
!  use ieee_arithmetic

  implicit none
  real (dp) , allocatable , dimension(:) :: val
  integer :: i,ii ,j ,start , imodel, iok

  call intro (program_calling = "value_check" )
  call print_settings (program_calling = "value_check")

  do i = 1 , size(model)
    if (model(i)%if) call read_netCDF(model(i))
  enddo

  ! check of exclusion or inclusion in polygon file
  ! for every site
  call read_polygon (polygons(1))

  allocate (val (nmodels(model)))

  start =0 
  if (size(dates).gt.0) start=1

  do j = start , size (dates)
    do i = 1 , size(model)
      if (model(i)%if) then
        call get_variable ( model(i) , date = dates(j)%date)
      endif
    enddo

    do i = 1 , size(sites)
      ! add time stamp if -D option was specified
      if (j.gt.0) then
        write (output%unit , '(f15.3,x,i4.4,5(i2.2))' , advance = "no" ) dates(j)%mjd , dates(j)%date
      endif


      ! if this point should not be used (polygon) leave as zero
      ! get polygons
      if (polygons(1)%if) then
        call chkgon( sites(i)%lon , sites(i)%lat , polygons(1) , iok)
      else
        iok=1
      endif

      imodel = 0
      do ii = 1 , size (model)
        if (model(ii)%if .or. model(ii)%if_constant_value) then
          imodel = imodel + 1
          if (model(ii)%if) then 
            if (iok.eq.1) then
              call get_value ( model(ii) , sites(i)%lat , sites(i)%lon , val(imodel) , method = model(ii)%interpolation )
            else
              val (imodel) = 0
            endif
          elseif (model(ii)%if_constant_value) then
            val(imodel) = model(ii)%constant_value
          endif
        endif
      enddo

      write (output%unit ,   '(30f15.4, 1x)') , sites(i)%lat, sites(i)%lon, val

    enddo

  enddo

end program
