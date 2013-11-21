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
  use mod_constants, only: dp, R_air
  use mod_polygon  , only: read_polygon, chkgon, polygon
  use mod_atmosphere, only: standard_pressure, standard_temperature

  implicit none
  real (dp) , allocatable , dimension(:) :: val
  real (dp)    :: cpu(2)
  integer      :: i, ii, j ,start, imodel, iprogress = 0
  integer(2)   :: iok
  integer(2)   :: ilevel, start_level

  call cpu_time(cpu(1))

  call intro ( &
    program_calling   = "value_check",      & 
    accepted_switches = "VFoShvIDLPRqwHMJ", & 
    version           = "beta",             & 
    cmdlineargs       = .true.              & 
    )

  ! for progress bar
  if (output%unit.ne.output_unit.and..not.quiet) open (unit=output_unit, carriagecontrol='fortran')

  allocate (val (size(model)))

  start=0 
  if (size(date).gt.0) then
    start=1
    ! print header
    if (output%header) then
      write (output%unit , '(a10,1x,a14,1x)' , advance = "no" ) "#mjd", "date"
    endif
  endif

  ! print header
  if (output%header.and.size(site).gt.0) then
    write (output%unit, '(a8,2a10$)') "name", "lat", "lon"
    if (output%height) then
      write (output%unit, '(a10$)') "height"
    endif
    if (output%level) then
      write (output%unit, '(a6$)') "level"
    endif
  endif
  do i = 1, size(model)
    if (output%header) then
      write (output%unit,'(a13)', advance='no') trim(model(i)%dataname)
    endif
  enddo
  if(output%header) write(output%unit, *)

  do j = start, size(date)
    do i = 1 , size(model)
      if (model(i)%if) then
        if ( &
            .not.( model(i)%autoloadname.eq."ERA" &
            .and.(model(i)%dataname.eq."GP".or.model(i)%dataname.eq."VT")) &
            .and.(j.eq.1.and. model(i)%autoload &
            .or. (  &
            model(i)%autoload &
            .and. .not. date(j)%date(1).eq.date(j-1)%date(1) &
            ) &
            ) &
            ) then
          call model_aliases(model(i), year=date(j)%date(1))
        else if ( &
            j.eq.1.and. model(i)%autoload &
            .or. (  &
            model(i)%autoload &
            .and. .not.( &
            date(j)%date(1).eq.date(j-1)%date(1) &
            .and.date(j)%date(2).eq.date(j-1)%date(2) &
            ) &
            ) &
            ) then
          call model_aliases( &
              model(i), year=date(j)%date(1), month=date(j)%date(2))
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
      if (dryrun) then
        write (output%unit , '(i4.4,5(i2.2),$)') date(j)%date
        if (j.lt.size(date)) write (output%unit , '(", ",$)')
      else
        write (output%unit , '(f10.3,1x,i4.4,5(i2.2))'  ) date(j)%mjd , date(j)%date
      endif
    endif

    if (level%all.and..not.allocated(level%level)) then
      allocate(level%level(size(model(1)%level)))
      level%level=model(1)%level
    endif

    if (size(level%level).lt.1) then
      start_level=0
    else
      start_level=1
    endif

    do ilevel=start_level, size(level%level)
      do i = 1 , size(site)
        iprogress = iprogress + 1
        ! add time stamp if -D option was specified
        if (j.gt.0) then
          write (output%unit , '(f10.3,1x,i4.4,5(i2.2),1x)' , advance = "no" ) date(j)%mjd , date(j)%date
        endif

        ! if this point should not be used (polygon) leave as zero
        if (allocated(polygon).and.polygon(1)%if) then
          call chkgon(site(i)%lon, site(i)%lat, polygon(1), iok)
        else
          iok=1
        endif

        imodel = 0
        do ii = 1 , size (model)
          if (model(ii)%if.or.model(ii)%if_constant_value) then
            imodel = imodel + 1
            if (iok.eq.1) then
              if (j.eq.0) then
                call get_value (model(ii), site(i)%lat, site(i)%lon, val(imodel), &
                    method=info(1)%interpolation, level=level%level(ilevel))
              else
                call get_value (model(ii), site(i)%lat, site(i)%lon, val(imodel), &
                    method=info(1)%interpolation, date=date(j)%date, level=level%level(ilevel))
              endif
            else
              val (imodel) = 0
            endif
            if (model(ii)%dataname.eq."LS") val(ii)=int(val(ii))
          endif
        enddo
        write (output%unit , '(a8,2f10.4$)') site(i)%name, site(i)%lat, site(i)%lon
        if (output%height) then
          write (output%unit, '(f10.3$)') site(i)%height
        endif
        if (output%level.and. allocated(level%level)) then
          write (output%unit, '(i6$)') level%level(ilevel)
        elseif(output%level) then
          write (output%unit, '(i6$)') ilevel
        endif

        if (ind%model%tp.ne.0) then
          if (ind%model%gp.eq.0) call print_warning("need @GP with @TP")
          val(ind%model%tp)= &
              standard_pressure ( &
              val(ind%model%gp), &
              use_standard_temperature=.true., &
              method = model(ind%model%tp)%name &
              ) 
          if (output%rho) then
            val(ind%model%tp)=val(ind%model%tp)/(R_air * standard_temperature(val(ind%model%gp)))
          endif
          val(ind%model%tp)= &
              variable_modifier(val(ind%model%tp),model(ind%model%tp)%datanames(1))
        endif
        if (ind%model%tpf.ne.0) then
          if (any([ind%model%gp,ind%model%sp,ind%model%hp,ind%model%t].eq.0)) &
              call print_warning("not enough with @TPF")
          val(ind%model%tpf)= &
              standard_pressure ( &
              val(ind%model%gp), &
              p_zero=val(ind%model%sp), &
              temperature=val(ind%model%t), &
              use_standard_temperature=.true., &
              h_zero=val(ind%model%hp), &
              method = model(ind%model%tpf)%name &
              )
          if (output%rho) then
            val(ind%model%tpf)=val(ind%model%tpf)/(R_air * standard_temperature(val(ind%model%gp)))
          endif
          val(ind%model%tpf)=variable_modifier(val(ind%model%tpf),model(ind%model%tpf)%datanames(1))
        endif
        if (ind%model%rho.ne.0) then
          if (any([ind%model%gp,ind%model%sp,ind%model%hp,ind%model%t,ind%model%vt].eq.0)) &
              call print_warning("not enough with @rho")
          val(ind%model%rho)= &
              100.*level%level(ilevel)/(R_air * val(ind%model%vt))
        endif


        write (output%unit , "("// output%form // '$)') val

        if (output%unit.ne.output_unit.and..not.quiet) then 
          call cpu_time(cpu(2))
          call progress(100*iprogress/(max(size(date),1)*max(size(site),1)*size(level%level)), cpu(2)-cpu(1))
        endif
        if (size(val).gt.0) write (output%unit , *)
      enddo
    enddo
  enddo

  if (ind%moreverbose%d.ne.0) then
    do i=1, size(model)
      do j=1, size(model(i)%time)
        write (moreverbose(ind%moreverbose%d)%unit, '(g0,1x,i4,5i2.2)') &
            model(i)%time(j), model(i)%date(j,:)
      enddo
    enddo
  endif

  if (ind%moreverbose%j.ne.0) then
    do i = 1, size(model)
      do j = 1, size(model(i)%level)
        write (moreverbose(ind%moreverbose%j)%unit, '(i5)') &
            model(i)%level(j) 
      enddo
    enddo
  endif

  call cpu_time(cpu(2))
  if (output%unit.ne.output_unit.and..not.quiet) then 
    call progress(100*iprogress/(max(size(date),1)*max(size(site),1)*size(level%level)), cpu(2)-cpu(1))
    close(output_unit) 
  endif
  write(log%unit, '(/,"Execution time:",1x,f16.9," seconds")') cpu(2)-cpu(1)
  write(log%unit, form_separator)
end program
