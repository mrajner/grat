! =============================================================================
!> \file
!! \date 2013-01-09
!! \author M. Rajner
! =============================================================================
program value_check
  use mod_constants, only: dp, sp, R_air, earth
  use mod_cmdline,   only: info, quiet_step, ind, dryrun, moreverbose, quiet
  use mod_parser
  use mod_data
  use mod_date
  use mod_site
  use mod_polygon,    only: read_polygon, chkgon, polygon
  use mod_atmosphere, only: standard_pressure, standard_temperature, geop2geom
  use mod_utilities,  only: d2r, setnan

  implicit none
  real(dp) , allocatable , dimension(:) :: val
  real(sp) :: cpu(2)
  real(dp)  :: sh
  integer :: execution_time(3)
  integer    :: i, ii, j ,start, imodel, iprogress = 0, lprogress
  integer(2) :: iok
  integer    :: ilevel, start_level

  call cpu_time(cpu(1))
  call system_clock(execution_time(1))

  quiet_step = 50000

  call intro (                                 &
    program_calling   = "value_check",         &
    accepted_switches = "VFoShvIDLPRqwHMJ!.-", &
    version           = __GRAT_VERSION__,      &
    gdate             = __GDATE__,             &
    cdate             = __CDATE__,             &
    fflags            = __FFLAGS__,            &
    compiler          = __COMPILER__,          &
    cmdlineargs       = .true.                 &
    )

  allocate (val (ubound(model,1)))

  ! initialize values
  val = setnan()

  start=0
  if (ubound(date,1).gt.0) then
    start=1

    ! print header
    if (output%header) then
      if (.not.output%prune) then
        write (output%unit , '(a9,1x,a14,1x)' , advance = "no" ) "#mjd", "date"
      endif
    endif
  endif

  ! print header
  if (output%header.and.ubound(site,1).gt.0) then

    if (.not.output%prune) then
      write (output%unit, '(a8,2(a10))' , advance = "no") "name", "lat", "lon"

      if (output%height) then
        write (output%unit, '(a10)', advance = "no") "height"
      endif

    endif

    if (output%level) then
      write (output%unit, '(a6)' , advance = "no") "level"
    endif

  endif

  do i = 1, ubound(model,1)
    if (output%header) then
      if (model(i)%dataname.eq."custom") then
        write (output%unit,'(a6,"@custom")', advance='no') trim(model(i)%name)
      else
        write (output%unit,'(a13)', advance='no') trim(model(i)%dataname)
      endif
    endif
  enddo

  if(output%header) write(output%unit, '(a)')

  do j = start, ubound(date,1)
    do i = 1 , ubound(model,1)
      if (model(i)%if) then
        if (model(i)%autoload                                &
          .and.                                              &
          .not.(                                             &
          model(i)%autoloadname(1:3).eq."ERA"                     &
          .and.(any(model(i)%dataname.eq.["GP ","VT ","VSH"])) &
          )) then

          if (                                              &
            (j.eq.1                                         &
            .or. .not. date(j)%date(1).eq.date(j-1)%date(1) &
            )                                               &
            ) then
            call model_aliases(model(i), year=date(j)%date(1))
          endif

        else if (model(i)%autoload) then

          if (                                         &
            (j.eq.1                                    &
            .or. .not.(                                &
            date(j)%date(1).eq.date(j-1)%date(1)       &
            .and.date(j)%date(2).eq.date(j-1)%date(2)) &
            )                                          &
            ) then

            call model_aliases(                                      &
              model(i), year=date(j)%date(1), month=date(j)%date(2))
          endif
        endif

        if (allocated(date).and.model(i)%exist) then
          call get_variable (model(i), date = date(j)%date)
        elseif (model(i)%exist) then
          call get_variable (model(i))
        endif

      endif
    enddo

    ! print only dates if no site given
    if (j.gt.0 .and. ubound(site,1).lt.1) then
      if (dryrun) then
        write (output%unit, '(i4.4,5(i2.2),$)') date(j)%date
        ! if (j.lt.size(date)) write (output%unit , '(", ",$)')
      else
        write (output%unit , '(f9.3,1x,i4.4,5(i2.2))'  ) date(j)%mjd , date(j)%date
      endif
    endif

    if (level%all.and..not.allocated(level%level)) then
      allocate(level%level(size(model(1)%level)))
      level%level = model(1)%level
    endif

    if (ubound(level%level,1).lt.1) then
      start_level=0
    else
      start_level=1
    endif

    lprogress = max(size(date),1)*max(size(site),1)*max(size(level%level),1)
    do ilevel = start_level, ubound(level%level,1)
      do i = 1, ubound(site,1)
        iprogress = iprogress + 1

        ! add time stamp if -D option was specified
        if (j.gt.0) then
          if (.not.output%prune) then
            write (output%unit, '(f9.3,1x,i4.4,5(i2.2),1x)', advance = "no" ) &
              date(j)%mjd, date(j)%date
          endif
        endif

        ! if this point should not be used (polygon) leave as zero
        if (allocated(polygon).and.polygon(1)%if) then
          call chkgon(site(i)%lon, site(i)%lat, polygon(1), iok)
        else
          iok = 1
        endif

        imodel = 0
        do ii = 1 , ubound (model,1)

          imodel = imodel + 1

          if (model(ii)%if.or.model(ii)%if_constant_value) then
            if (iok.eq.1) then
              if (j.eq.0) then
                call get_value (                  &
                  model(ii),                      &
                  site(i)%lat,                    &
                  site(i)%lon,                    &
                  val(imodel),                    &
                  method = info(1)%interpolation, &
                  level  = level%level(ilevel)    &
                  )
              else
                call get_value (                &
                  model(ii),                      &
                  site(i)%lat,                    &
                  site(i)%lon,                    &
                  val(imodel),                    &
                  method = info(1)%interpolation, &
                  date   = date(j)%date,          &
                  level  = level%level(ilevel)    &
                  )
              endif
            else
              val(imodel) = 0
            endif

            if (model(ii)%dataname.eq."LS") then
              val(ii)=int(val(ii))
            endif

          else if (model(ii)%dataname.eq."custom") then
            if(ilevel.eq.1) sh=val(ind%model%vsh)

            call customfile_value(             &
              what  = model(imodel)%name,      &
              sp    = val(ind%model%sp),       &
              t     = val(ind%model%t),        &
              hp    = val(ind%model%hp),       &
              sh    = sh,                      &
              gp    = val(ind%model%gp),       &
              vsh   = val(ind%model%vsh),      &
              vt    = val(ind%model%vt),       &
              level = level%level(ilevel),     &
              val   = val(imodel),             &
              rho   = any(model%name.eq."RHO") &
              )
          else
            val (imodel) = setnan()
          endif
        enddo

        if (.not.output%prune) then

          write (output%unit , '(a8,2f10.4)' , advance="no") trim(site(i)%name), site(i)%lat, site(i)%lon

          if (output%height) then
            write (output%unit, '(f10.3$)') site(i)%height
          endif
        endif

        if (output%level.and. allocated(level%level)) then
          write (output%unit, '(i6$)') level%level(ilevel)
        elseif(output%level) then
          write (output%unit, '(i6$)') ilevel
        endif

        write (output%unit , "(*("//output%form//'))') val

        if (.not.quiet.or.iprogress==lprogress) then

          call cpu_time(cpu(2))
          call system_clock(execution_time(2),execution_time(3))

          call progress(                                      &
            100*iprogress/lprogress,                          &
            time  = real(execution_time(2)-execution_time(1) &
            /execution_time(3),sp),                               &
            cpu   = cpu(2)-cpu(1),                            &
            every = quiet_step                                &
            )
        endif


      enddo
    enddo
  enddo

  if (ind%moreverbose%d.ne.0) then
    do i=1, ubound(model,1)
      do j=1, ubound(model(i)%time,1)
        write (moreverbose(ind%moreverbose%d)%unit, '(f0.3,1x,i4,5i2.2)') &
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

  write(log%unit, form_separator)
end program
