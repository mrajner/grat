!! \mainpage grat overview
!! \section Purpose
!! This program was created to make computation of atmospheric gravity
!! correction easier. Still developing. Consider visiting later...
!!
!! \version beta
!! \date 2014-06-30
!! \version pre-alpha
!! \date 2013-01-12
!! \author Marcin Rajner\n
!! Politechnika Warszawska | Warsaw University of Technology
!!
!! \warning This program is written in Fortran90 standard but uses some featerus
!! of 2003 specification (e.g., \c 'newunit='). It was also written
!! for <tt>Intel Fortran Compiler</tt> hence some commands can be unavailable
!! for other compilers (e.g., \c <integer_parameter> for \c IO statements).
!! This should be easily modifiable according to your output needs.
!! Also you need to have \c iso_fortran_env module available to guess the number
!! of output_unit for your compiler.
!! When you don't want a \c log_file and you don't switch \c verbose all
!! unnecesarry information whitch are normally collected goes to \c /dev/null
!! file. This is *nix system default trash. For other system or file system
!! organization, please change this value in \c mod_cmdline module.
!!
!! \attention
!! \c grat and value_check needs a \c netCDF library \cite netcdf
!> \copyright
!! Copyright 2013 by Marcin Rajner\n
!! This program is free software: you can redistribute it and/or modify
!! it under the terms of the GNU General Public License as published by
!! the Free Software Foundation, either version 3 of the License, or
!! (at your option) any later version.
!! \n\n
!! This program is distributed in the hope that it will be useful,
!! but WITHOUT ANY WARRANTY; without even the implied warranty of
!! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!! GNU General Public License for more details.
!! \n\n
!! You should have received a copy of the GNU General Public License
!! along with this program.
!! If not, see <http://www.gnu.org/licenses/>.
!! \page License
!! \include LICENSE
!!
!! \section Usage
!! After sucsesfull compiling make sure the executables are in your search path
!!
!! There is main program \c grat and some utilities program. For the options see

!> \page intro_sec External resources
!!   - <a href="https://code.google.com/p/grat">project page</a> (git repository)
!!   - \htmlonly <a href="../latex/refman.pdf">[pdf]</a> version of this manual\endhtmlonly
!!   \latexonly \href{https://grat.googlecode.com/git/doc/html/index.html}{html} version of this manual\endlatexonly
!! \TODO give source for grant presentation
!!   - <a href="">[pdf]</a> command line options (in Polish)
!! \example example_aggf.f90
!! \example grat_usage.sh
! ==============================================================================
program grat
#ifdef WITH_MONTE_CARLO
  use lib_random
#endif
  use mod_parser,    only: intro
  use mod_data
  use mod_date
  use mod_green,     only: convolve, green
  use mod_site,      only: print_site_summary, site
  use mod_cmdline
  use mod_admit,     only: admit
  use mod_utilities, only: Bubble_Sort, mean, stdev

  implicit none
  real    :: cpu(2)
  integer :: execution_time(3)
  integer :: isite, i, idate, start, iprogress = 0, lprogress, j
  logical :: first_waning = .true.

#ifdef WITH_MONTE_CARLO
  real(dp), allocatable, dimension(:,:) :: monte_carlo_results
  real(dp), allocatable, dimension(:) :: results
#endif


  ! program starts here with time stamp
  call cpu_time(cpu(1))
  call system_clock(execution_time(1))

  ! gather cmd line option decide where to put output
  call intro (                                           &
    program_calling   = "grat",                          &
    version           = __VERSION__,                     &
    cdate             = __CDATE__,                       &
    fflags            = __FFLAGS__,                      &
    compiler          = __COMPILER__,                    &
    accepted_switches = "VSBLGPqoFIDLvhRrMOAHUwJQ&!n-mC", &
    cmdlineargs       = .true.                           &
    )

  start = 0

  if (dryrun) then
    call print_site_summary (site_parsing=.true.)
    call exit (0)
  endif

#ifdef WITH_MONTE_CARLO
  if(monte_carlo) then
    call set_seed(10)
  endif
#endif

  if (ubound(date,1).gt.0) then

    if(output%header) then
      write (output%unit, '(a9,x,a14,x)', advance = "no" ) "mjd", "date"
    endif

    start = 1
  endif

  if(output%header) then
    write (output%unit, '(a8,3(x,a9)$)') "name", "lat", "lon", "h"
  endif

  if(output%header) then

    if (method(1)) then
      do i=1, max(1,ubound(admitance%value,1))
        if (i.gt.1) then
          write (output%unit,'(a11,"_",i1)', advance='no'), "G1D", i
        else
          write (output%unit,'(a13)', advance='no'), "G1D"
        endif

#ifdef WITH_MONTE_CARLO
        if (monte_carlo) then
          write (output%unit,'(2a8)', advance='no') "mean", "std"
        endif
#endif
      enddo
    endif


    if (method(2).or.method(3)) then

      if (result_component) then
        do i = 1, size(green)
          if (green(i)%dataname.eq."GE") then
            if (inverted_barometer) then
              write (output%unit,'(a13$)'), trim(green(i)%dataname)//"_IB"
            else
              write (output%unit,'(a13$)'), trim(green(i)%dataname)//"_NIB"
            endif
          else
            write (output%unit,'(a13$)'), trim(green(i)%dataname)
          endif
        enddo

        if (inverted_barometer.and.non_inverted_barometer.and.any(green%dataname.eq."GE")) then
          write (output%unit,'(a13)' , advance = "no"), "GE_NIB"
        endif
      endif

      if (result_total) then
        if (method(2)) then
          if (                                                &
            all([inverted_barometer, non_inverted_barometer]) &
            .and. result_total_all                            &
            ) then
            write (output%unit,'(a13)',advance='no'), "G2D_t_IB"
            write (output%unit,'(a13)',advance='no'), "G2D_t_NIB"
          else
            write (output%unit,'(a13)',advance='no'), "G2D_t"
          endif
        endif
        if (method(3)) then
          if ( &
            all([inverted_barometer, non_inverted_barometer]) &
            .and. result_total_all &
            ) then
            write (output%unit,'(a13)',advance='no'), "G3D_t_IB"
            write (output%unit,'(a13)',advance='no'), "G3D_t_NIB"
          else
            write (output%unit,'(a13)',advance='no'), "G3D_t"
          endif
        endif
      endif
    endif
  endif

  if(output%header) then
    write (output%unit, *)
  endif

  ! read only once Land-sea, reference surface pressure
  if (ind%model%ls.ne.0) then
    call get_variable (model(ind%model%ls))
  endif

  if (ind%model%rsp.ne.0) then
    call get_variable (model(ind%model%rsp))
  endif

  if (ind%model%hrsp.ne.0) then
    call get_variable (model(ind%model%hrsp))
  endif

  if (inverted_landsea_mask.and.ind%model%ls.ne.0) then
    stop "CHECK HERE"
    ! czy tu rzeczywiście .and.ind%model%ls.ne.0 ma sens
    model(ind%model%ls)%data = int(abs(model(ind%model%ls)%data-1))
  endif

  do idate=start, ubound(date,1)
    if (idate.ge.1) then
      if(.not.(output%nan).and.modulo(date(idate)%date(4),6).ne.0) then

        if (first_waning) then
          call print_warning (                           &
            "hours not matching model dates (0,6,12,18)" &
            //" are rejecting and not shown in output"   &
            )
        endif

        first_waning=.false.
        cycle
      endif
    endif

    do i = 1, ubound(model,1)
      if(model(i)%if) then

        select case (model(i)%dataname)
        case ("SP", "T", "GP", "VT", "VSH")
          if (                                                   &
            model(i)%autoload                                    &
            .and.                                                &
            .not.(                                               &
            model(i)%autoloadname(1:3).eq."ERA"                  &
            .and.(any(model(i)%dataname.eq.["GP ","VT ","VSH"])))) &
            then

            if (                                                      &
              (idate.eq.1                                             &
              .or. .not. date(idate)%date(1).eq.date(idate-1)%date(1) &
              )) then

              call model_aliases(model(i), year=date(idate)%date(1))
            endif

          else if (model(i)%autoload) then
            if (                                                 &
              (idate.eq.1                                        &
              .or. .not.(                                        &
              date(idate)%date(1).eq.date(idate-1)%date(1)       &
              .and.date(idate)%date(2).eq.date(idate-1)%date(2)) &
              )                                                  &
              ) then

              call model_aliases( &
                model(i), year=date(idate)%date(1), month=date(idate)%date(2))
            endif
          endif

          if (ubound(date,1).eq.0.and.model(i)%exist) then
            call get_variable (model(i))
          elseif (model(i)%exist) then
            call get_variable (model(i), date = date(idate)%date)
          endif

        end select
      endif
    enddo

    if (any(.not.model(1:ubound(model,1))%exist).and..not.output%nan) cycle

    if (level%all.and..not.allocated(level%level)) then
      allocate(level%level(size(model(ind%model%gp)%level)))
      level%level=model(ind%model%gp)%level
    endif

    ! sort levels for 3D method
    call Bubble_Sort(level%level)

    ! if ocean mass should be conserved (-O C)
    if (ocean_conserve_mass) then

      if (ind%model%sp.ne.0 .and. ind%model%ls.ne.0) then
        if(size(date).eq.0) then
          call conserve_mass(model(ind%model%sp), model(ind%model%ls), &
            inverted_landsea_mask = inverted_landsea_mask)
        else
          call conserve_mass(model(ind%model%sp), model(ind%model%ls), &
            date=date(idate)%date, &
            inverted_landsea_mask = inverted_landsea_mask)
        endif
      endif
    endif

    ! calculate total mass if asked for
    if (ind%moreverbose%t.ne.0) then
      if (size(date).eq.0) then
        call total_mass(model(ind%model%sp))
      else
        call total_mass(model(ind%model%sp), date=date(idate)%date)
      endif
    endif

    lprogress = max(size(date),1)*max(size(site),1)
    do isite = 1, ubound(site,1)
      iprogress = iprogress + 1

      if (idate.gt.0) then
        write(output%unit, '(f9.3,x,i4.4,5(i2.2),x$)', advance="no") &
          date(idate)%mjd, date(idate)%date
      endif

      write (output%unit, '(a8,2(x,f9.4),x,f9.3,$)'), &
        trim(site(isite)%name),                       &
        site(isite)%lat,                              &
        site(isite)%lon,                              &
        site(isite)%height

      if (method(1)) then
        do j=1, max(1,ubound(admitance%value(:),1))

          write (output%unit, "("// output%form // ')' , advance = "no"), &
            admit(                                                        &
            site(isite),                                                  &
            date   = date(idate)%date,                                    &
            number = j                                                    &
            )

#ifdef WITH_MONTE_CARLO
          if(monte_carlo) then
            if(allocated(monte_carlo_results)) then
              deallocate(monte_carlo_results)
            endif
            allocate(monte_carlo_results(monte_carlo_samples,1))

            do i = 1, monte_carlo_samples
              monte_carlo_results(i,1) = admit( &
                site(isite),                    &
                date      = date(idate)%date,   &
                number    = j,                  &
                randomize = .true.              &
                )
            enddo

            write(output%unit, "(2f8.3)" , advance = "no" ) &
              mean(monte_carlo_results,monte_carlo_samples) , &
              stdev(monte_carlo_results,monte_carlo_samples)
          endif
#endif

        enddo
      endif

      if (method(2).or.method(3)) then
        ! perform convolution
        call convolve (site(isite), date = date(idate))

#ifdef WITH_MONTE_CARLO
        if (monte_carlo) then
          if(allocated(monte_carlo_results)) then
            deallocate(monte_carlo_results)
          endif

          ! TODO
          ! change 9 in line below
          allocate(monte_carlo_results(monte_carlo_samples,9))

          do i = 1,monte_carlo_samples
            call convolve (site(isite), date = date(idate), randomize=monte_carlo, results = results)
            monte_carlo_results(i,:) = results
          enddo

          do i = 1, size(results)
            write(output%unit, "(2f8.3)" , advance = "no" )      &
              mean(monte_carlo_results(:,i),monte_carlo_samples) , &
              stdev(monte_carlo_results(:,i),monte_carlo_samples)
          enddo

          ! print *
          ! do i = 1 , monte_carlo_samples
          ! print * , monte_carlo_results(i,:)
          ! enddo
        endif
#endif
      endif

      write(output%unit,'("")')

      if (.not.(quiet).or.iprogress==lprogress) then
        call cpu_time(cpu(2))
        call system_clock(execution_time(2),execution_time(3))

        call progress(                                      &
          100*iprogress/lprogress ,                         &
          time  = real(execution_time(2)-execution_time(1)) &
          /execution_time(3),                               &
          cpu   = cpu(2)-cpu(1),                            &
          every = quiet_step                                &
          )
      endif

    enddo
  enddo

  close(output_unit)
end program
