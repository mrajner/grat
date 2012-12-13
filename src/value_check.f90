program value_check 
  use get_cmd_line
  use mod_data
use ieee_arithmetic
  implicit none

  real (sp) , allocatable , dimension(:) :: val
  integer :: i,ii ,j ,start , imodel

  call intro (program_calling = "value_check" )
  call print_settings (program_calling = "value_check")

  do i = 1 , size(model)
    if (model(i)%if) call read_netCDF(model(i))
  enddo

  allocate (val (nmodels(model)))

  start =0 
  if (size(dates).gt.0) start=1

  print * , val
  do j = start , size (dates)
    do i = 1 , size(model)
      if (model(i)%if) call get_variable ( model(i) , date = dates(j)%date)
    enddo



    do i = 1 , size(sites)
      ! add time stamp if -D option was specified
      if (j.gt.0) then
        write (output%unit , '(f15.3,x,i4.4,5(i2.2))' , advance = "no" ) dates(j)%mjd , dates(j)%date
      endif

      imodel = 0
      do ii = 1 , size (model)
        if (model(ii)%if .or. model(ii)%if_constant_value) then
          imodel = imodel + 1
          if (model(ii)%if) then 
            call get_value ( model(ii) , sites(i)%lat , sites(i)%lon , val(imodel) , model(ii)%interpolation )
          elseif (model(ii)%if_constant_value) then
            val(imodel) = model(ii)%constant_value
          endif
        endif
      enddo

      write (output%unit ,   '(30f15.4, 1x)') , sites(i)%lon , sites(i)%lat , val

      if (moreverbose%if.and.moreverbose%names(1).eq."c") then
        write (moreverbose%unit ,   '(30f15.4)'), sites(i)%lon , sites(i)%lat , val
      endif

    enddo

  enddo

end program
