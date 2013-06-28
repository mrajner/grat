! =============================================================================
!> \file
!! \date 2013-01-09
!! \author M. Rajner
! =============================================================================

program value_check 
  use mod_cmdline 
  use mod_parser
  use mod_data     
  use mod_date
  use mod_site
  use mod_constants, only: dp
  use mod_polygon  , only: read_polygon, chkgon, polygon

  implicit none
  real (dp) , allocatable , dimension(:) :: val
  integer(2) :: i,ii ,j ,start , imodel, iok 
  character(1) :: interpolation

  call intro (program_calling = "value_check", &
    accepted_switches="VFoShvIDLPR" , &
    cmdlineargs=.true.)

  call get_index()

  write(log%unit, form_separator) 
  allocate (val (size(model)))

  start =0 
  if (size(date).gt.0) then
    start=1
    ! print header
    if (output%header) then
    write (output%unit , '(a15,x,a14)' , advance = "no" ) "#mjd" , "date"
  endif
  endif

  ! print header
  if (output%header) then
    write (output%unit , '(a8,30a15)', advance ="no"  ) "name", "lat" , "lon"
  endif
  do i = 1 ,size(model)
    if ((model(i)%if .or. model(i)%if_constant_value) .and. output%header ) then
      write (output%unit,'(a15)',advance='no') , trim( model(i)%dataname)
    endif
  enddo
  if(output%header) write (output%unit , *)

  do j = start , size (date)
    do i = 1 , size(model)
      if (model(i)%if) then
        ! only read from multidate files for specific date
        ! for 'static' data files get_variable was performed
        ! during read_netCDF
        if (allocated(date)) then
           call get_variable (model(i), date = date(j)%date , huge= model(i)%dataname)
        else
          call get_variable (model(i) , huge = model(i)%dataname)
        endif
      endif
    enddo

    do i = 1 , size(site)
      ! add time stamp if -D option was specified
      if (j.gt.0) then
        write (output%unit , '(f15.3,x,i4.4,5(i2.2))' , advance = "no" ) date(j)%mjd , date(j)%date
      endif


      ! if this point should not be used (polygon) leave as zero
      if (allocated(polygon).and.polygon(1)%if) then
        call chkgon( site(i)%lon, site(i)%lat, polygon(1), iok)
      else
        iok=1
      endif
      imodel = 0
      do ii = 1 , size (model)
        if (model(ii)%if .or. model(ii)%if_constant_value) then
          imodel = imodel + 1
          if (iok.eq.1) then
            call get_value (model(ii), site(i)%lat, site(i)%lon, val(imodel), &
              method=info(1)%interpolation, huge=model(ii)%dataname, date=date(j)%date)
          else
            val (imodel) = 0
          endif
        endif
      enddo
      write (output%unit , '(a8,2f15.4,20en15.4)') , site(i)%name, site(i)%lat, site(i)%lon, val
    enddo
  enddo

  if (ind%moreverbose%d.ne.0) then
    do i = 1 , size(model)
      do  j =1,size(model(i)%time)
        write (moreverbose(ind%moreverbose%d)%unit ,  '(g0,1x,i4,5i2.2)')  model(i)%time(j), model(i)%date(j,:)
      enddo
    enddo
  endif

end program
