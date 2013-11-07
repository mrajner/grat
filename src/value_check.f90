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
  real (dp)    :: cpu(2)
  integer      :: i,ii ,j ,start , imodel, iprogress = 0
  integer(2)   :: iok

  call cpu_time(cpu(1))

  call intro ( &
    program_calling   = "value_check",   &
    accepted_switches = "VFoShvIDLPRqwHM",&
    version           = "beta",          &
    cmdlineargs       = .true.           &
    )

  call get_index()

  write(log%unit, form_separator) 

  ! for progress bar
  if (output%unit.ne.output_unit.and..not.quiet) open (unit=output_unit, carriagecontrol='fortran')

  allocate (val (size(model)))

  start =0 
  if (size(date).gt.0) then
    start=1
    ! print header
    if (output%header) then
      write (output%unit , '(a15,1x,a14,1x)' , advance = "no" ) "#mjd", "date"
    endif
  endif

  ! print header
  if (output%header.and.size(site).gt.0) then
    write (output%unit, '(a8,2a10)', advance ="no"  ) "name", "lat", "lon"
  endif
  do i = 1 ,size(model)
    if ((model(i)%if .or. model(i)%if_constant_value) .and. output%header ) then
      write (output%unit,'(a15)',advance='no') trim( model(i)%dataname)
    endif
  enddo
  if(output%header) write(output%unit, *)

  do j = start, size(date)
    do i = 1 , size(model)
      if (model(i)%if) then
        if ( &
            j.eq. 1 .and. model(i)%autoload &
            .or. (model(i)%autoload &
            .and. .not. date(j)%date(1).eq.date(j-1)%date(1)) &
            ) then
          call model_aliases(model(i), year= date(j)%date(1))
        endif
        if (allocated(date).and.model(i)%exist) then
          call get_variable (model(i), date = date(j)%date)
        elseif (model(i)%exist) then
          call get_variable (model(i))
        endif
      endif
    enddo

    ! print only dates if no site given
    if (j.gt.0 .and. size(site).lt.1) then
      if (method.eq."n") then
        write (output%unit , '(i4.4,5(i2.2),$)') date(j)%date
        if (j.lt.size(date)) write (output%unit , '(", ",$)')
      else
        write (output%unit , '(f15.3,1x,i4.4,5(i2.2))'  ) date(j)%mjd , date(j)%date
      endif
    endif
    do i = 1 , size(site)
      iprogress = iprogress + 1
      ! add time stamp if -D option was specified
      if (j.gt.0) then
        write (output%unit , '(f15.3,1x,i4.4,5(i2.2),1x)' , advance = "no" ) date(j)%mjd , date(j)%date
      endif

      ! if this point should not be used (polygon) leave as zero
      if (allocated(polygon).and.polygon(1)%if) then
        call chkgon(site(i)%lon, site(i)%lat, polygon(1), iok)
      else
        iok=1
      endif
      imodel = 0
      do ii = 1 , size (model)
        if (model(ii)%if .or. model(ii)%if_constant_value) then
          imodel = imodel + 1
          if (iok.eq.1) then
            call get_value (model(ii), site(i)%lat, site(i)%lon, val(imodel), &
              method=info(1)%interpolation, date=date(j)%date)
          else
            val (imodel) = 0
          endif
          if (model(ii)%dataname.eq."LS") val(ii)=int(val(ii))
        endif
      enddo
      write (output%unit , '(a8,2f10.4,20en15.4)') site(i)%name, site(i)%lat, site(i)%lon, val
      if (output%unit.ne.output_unit.and..not.quiet) then 
        call cpu_time(cpu(2))
        call progress(100*iprogress/(max(size(date),1)*max(size(site),1)), cpu(2)-cpu(1))
      endif
    enddo
  enddo

  if (ind%moreverbose%d.ne.0) then
    do i = 1, size(model)
      do j = 1, size(model(i)%time)
        write (moreverbose(ind%moreverbose%d)%unit, '(g0,1x,i4,5i2.2)') &
          model(i)%time(j), model(i)%date(j,:)
      enddo
    enddo
  endif

  call cpu_time(cpu(2))
  if (output%unit.ne.output_unit.and..not.quiet) then 
    call progress(100*iprogress/(max(size(date),1)*max(size(site),1)), cpu(2)-cpu(1))
    close(output_unit) 
  endif
  write(log%unit, '(/,"Execution time:",1x,f16.9," seconds")') cpu(2)-cpu(1)
  write(log%unit, form_separator)
end program
