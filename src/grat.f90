! ==============================================================================
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
!! for other compilers (e.g., \c <integer_parameter> for \c IO statements. This should be
!! easily modifiable according to your output needs.
!! Also you need to have \c iso_fortran_env module available to guess the number
!! of output_unit for your compiler.
!! When you don't want a \c log_file and you don't switch \c verbose all 
!! unneceserry information whitch are normally collected goes to \c /dev/null
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

  use mod_parser
  use mod_data
  use mod_date
  use mod_green, only : convolve, green, result
  use mod_site
  use mod_polygon
  use mod_cmdline
  use mod_admit

  implicit none
  real(dp) :: x, y, z, lat, lon, cpu(2)
  integer :: isite, i, ii , iii , idate, start , iok  , iprogress = 0

  ! program starts here with time stamp
  call cpu_time(cpu(1))

  ! gather cmd line option decide where to put output
  call intro & 
    ( &
    program_calling = "grat" , &
      version = "pre-alpha" , &
      accepted_switches="VSBLGPpoFIDLvhRQOA" , &
      cmdlineargs=.true. &
      )

    ! for progress bar
    if (output%unit.ne.output_unit) open(unit=output_unit, carriagecontrol='fortran')

    start = 0

    if (size(date).gt.0) then
      if(output%header) then
        write (output%unit , '(a12,x,a14)' , advance = "no" ) "mjd" , "date"
      endif
      start = 1
    endif
    if (size(info).gt.1) then
      if(output%header) write (output%unit , '(a2)' , advance = "no" ) "i"
    endif
    if(output%header) then
      write (output%unit , '(a8,30a15)', advance ="no"  ) "name", "lat" , "lon" , "h"
    endif

    if(output%header) then
      if (method.eq."1D") then
        write (output%unit,'(a15)',advance='no') , "admitance"
        elseif (method.eq."2D") then
        do i = 1 ,size(green)
          write (output%unit,'(a15)',advance='no') , trim(green(i)%dataname)
        enddo
      endif
    endif

    if(output%header) then
      write (output%unit , *)
    endif

    do idate = start, size (date)
      do isite = 1, size(site)
        iprogress = iprogress + 1

        ! if ocean land mask should be inverted
        !todo
        ! force topography to zero over oceans
        !          if (val(4).eq.0.and.val(3).lt.0) val(3) = 0.
        do i = 1, size(model)
          if(model(i)%if) then
            select case (model(i)%dataname)
            ! read only once Land-sea, reference surface pressure and heights
          case ("LS", "RSP", "H", "HP")
            if (idate.gt.start) then
              cycle
            else
              call get_variable (model(i))
              select case (model(i)%dataname)
              case ("LS")
                if (inverted_landsea_mask) then
                  model(ind%model%ls)%data = abs(model(ind%model%ls)%data-1)
                endif
              endselect
            endif
          case default
             if (idate.eq.1 .and. model(i)%autoload &
             .or.( model(i)%autoload &
                 .and. .not. date(idate)%date(1).eq.date(idate-1)%date(1)) &
                 ) then
             call model_aliases(model(i), year= date(idate)%date(1))
             endif
            if (size(date).eq.0) then
              call get_variable (model(i))
            else
              call get_variable (model(i), date = date(idate)%date)
            endif
          endselect
        endif
      enddo

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


      if (method.eq."1D") then 
        if (idate.gt.0) then
          write(output%unit, '(f12.3,x,i4.4,5(i2.2))', advance="no") &
              date(idate)%mjd, date(idate)%date
        endif
        write (output%unit, '(a8,3f15.4,10en15.5)' ), &
            site(isite)%name, &
            site(isite)%lat,  &
            site(isite)%lon,  &
            site(isite)%height, &
            admit( &
            lat=site(isite)%lat, &
            lon=site(isite)%lon, &
            height=site(isite)%height  &
            )

      elseif (method.eq."2D") then 
        ! perform convolution
        if (idate.gt.0) then
          call convolve (site(isite), date = date(idate))
        else
          call convolve (site(isite))
        endif
        if (output%unit.ne.output_unit) then 
          call cpu_time(cpu(2))
          call progress(                     & 
              100*iprogress/(max(size(date),1) & 
              *max(size(site),1)),             & 
              cpu(2)-cpu(1))
        endif
      endif
    enddo
  enddo

  ! execution time-stamp
  call cpu_time(cpu(2))
  if (output%unit.ne.output_unit) then 
    call progress(100*iprogress/(max(size(date),1)*max(size(site),1)), cpu(2)-cpu(1))
    close(output_unit) 
  endif
  write(log%unit, '(/,"Execution time:",1x,f16.9," seconds")') cpu(2)-cpu(1)
  write(log%unit, form_separator)
end program 
