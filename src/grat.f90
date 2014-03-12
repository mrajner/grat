!> \file
!! \mainpage grat overview
!! \section Purpose
!! This program was created to make computation of atmospheric gravity
!! correction easier. Still developing. Consider visiting later...
!!
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
  ! use omp_lib parallel computation not yet enabled
  use mod_parser, only: intro
  use mod_data
  use mod_date
  use mod_green, only: convolve, green
  use mod_site, only: print_site_summary, site
  use mod_cmdline
  use mod_admit, only: admit
  use mod_utilities, only: Bubble_Sort

  implicit none
  real(dp) :: cpu(2)
  integer :: execution_time(3)
  integer  :: isite, i, idate, start, iprogress = 0
  logical  :: first_waning = .true.

  ! program starts here with time stamp
  call cpu_time(cpu(1))
  call system_clock(execution_time(1))

  ! gather cmd line option decide where to put output
  call intro (                                         & 
    program_calling   = "grat",                        &
    version           = "pre-alpha",                   &
    accepted_switches = "VSBLGPqoFIDLvhRrMOAHUwJQ&!n", &
    cmdlineargs       = .true.                         &
    )

  start = 0

  if (dryrun) then
    call print_site_summary (site_parsing=.true.)
    call exit (0)
  endif

  if (size(date).gt.0) then
    if(output%header) then
      write (output%unit, '(a12,x,a14,x)', advance = "no" ) "mjd", "date"
    endif
    start = 1
  endif

  if(output%header) then
    write (output%unit, '(a8,3(x,a9$))') "name", "lat", "lon", "h"
  endif

  if(output%header) then

    if (method(1)) then
      write (output%unit,'(a13)', advance='no'), "G1D"
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
        if (inverted_barometer.and.non_inverted_barometer) then
          write (output%unit,'(a13$)'), "GE_NIB"
        endif
      endif

      if (result_total) then
        if (method(2)) then
          write (output%unit,'(a13)',advance='no'), "G2D_t"
        endif
        if (method(3)) then
          write (output%unit,'(a13)',advance='no'), "G3D_t"
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
    model(ind%model%ls)%data = int(abs(model(ind%model%ls)%data-1))
  endif

  do idate=start, size (date)
    if (idate.ge.1) then
      if(.not.(output%nan).and.modulo(date(idate)%date(4),6).ne.0) then

        if (first_waning) then
          call print_warning (                           &
            "hours not matching model dates (0,6,12,18)" &
            //"are rejecting and not shown in output"    &
            )
        endif

        first_waning=.false.
        cycle
      endif
    endif

    do i = 1, size(model)
      if(model(i)%if) then

        select case (model(i)%dataname)
        case ("SP", "T", "GP", "VT", "VSH")
          if (                                                   &
            model(i)%autoload                                    &
            .and.                                                &
            .not.(                                               &
            model(i)%autoloadname.eq."ERA"                       &
            .and.(any(model(i)%dataname.eq.["GP","VT","VSH"])))) &
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

          if (size(date).eq.0.and.model(i)%exist) then
            stop "temporary"
            call get_variable (model(i))
          elseif (model(i)%exist) then
            call get_variable (model(i), date = date(idate)%date)
          endif

        end select
      endif
    enddo

    if (any(.not.model%exist).and..not.output%nan) cycle

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

    do isite = 1, size(site)
      iprogress = iprogress + 1

      if (idate.gt.0) then
        write(output%unit, '(f12.3,x,i4.4,5(i2.2),x$)', advance="no") &
          date(idate)%mjd, date(idate)%date
      endif

      write (output%unit, '(a8,2(x,f9.4),x,f9.3,$)'), &
        site(isite)%name,                             &
        site(isite)%lat,                              &
        site(isite)%lon,                              &
        site(isite)%height

      if (method(1)) then
        write (output%unit, "("// output%form // '$)'), &
          admit(                                        &
          site(isite),                                  &
          date=date(idate)%date                         &
          )
      endif

      if (method(2).or.method(3)) then
        ! perform convolution
        call convolve (site(isite), date = date(idate))
      endif

      write(output%unit,*)

      if (output%unit.ne.output_unit.and..not.(quiet.and.quiet_step.eq.0)) then
        open(unit=output_unit, carriagecontrol='fortran')
        call cpu_time(cpu(2))
        call system_clock(execution_time(2),execution_time(3))

        call progress(                     &
          100*iprogress/(max(size(date),1) &
          *max(size(site),1)),             &
          time  = cpu(2)-cpu(1),           &
          cpu   = cpu(2)-cpu(1),           &
          every = quiet_step               &
          )
      endif

    enddo
  enddo

  ! execution time-stamp
  call cpu_time(cpu(2))
  call system_clock(execution_time(2),execution_time(3))

  if (output%unit.ne.output_unit.and..not.(quiet.and.quiet_step.eq.0)) then
    call progress(                     &
      100*iprogress/(max(size(date),1) &
      *max(size(site),1)),             &
      time  = cpu(2)-cpu(1),           &
      cpu   = cpu(2)-cpu(1),           &
      every = quiet_step               &
      )
    close(output_unit)
  endif

  write(log%unit, '("Execution time:",1x,f10.4," seconds (proc time:",1x,f10.4,1x,"s)")') &
    real(execution_time(2)-execution_time(1))/(execution_time(3)), &
    cpu(2)-cpu(1)
end program
