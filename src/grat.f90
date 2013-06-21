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
  use mod_utilities, only: datanameunit

  implicit none
  real(dp) :: x , y , z , lat ,lon , cpu(2)
  integer :: isite, i, ii , iii , idate, start , iok

  ! program starts here with time stamp
  call cpu_time(cpu(1))

  ! gather cmd line option decide where to put output
  call intro & 
    (program_calling = "grat" , &
    accepted_switches="VSBLGPpoFIDLvhRQ" , &
    cmdlineargs=.true. &
    )

  call get_index()

  allocate (result(size(site)*max(size(date),1), size(green) ))

  start=0
  if (size(date).gt.0) then
    if(output%header) then
      write (output%unit , '(a15,x,a14)' , advance = "no" ) "#mjd" , "date"
    endif
    start = 1
  endif

  if(output%header) then
    write (output%unit , '(a8,30a15)', advance ="no"  ) "name", "lat" , "lon"
  endif

  do i = 1 ,size(green)
    if(output%header) then
      write (output%unit,'(a15)',advance='no') , trim(green(i)%dataname)
    endif
  enddo

  if(output%header) then
    write (output%unit , *)
  endif

  do idate = start , size (date)
    do isite = 1 , size(site)
      if (idate.gt.0) then
        write (output%unit, '(f15.3,x,i4.4,5(i2.2))', advance = "no" ) date(idate)%mjd, date(idate)%date
      endif

      write (output%unit, '(a8,30f15.4)' ,advance='no'), site(isite)%name, site(isite)%lat, site(isite)%lon
      !!        print *
      !do i = 1 , size(polygon)
      !  call chkgon( site(isite)%lon , site(isite)%lat , polygon(i) , iok)
      !enddo

      do i = 1 , size(model)

        if(model(i)%if) then
          select case (model(i)%dataname)
          case ("LS","RS")
            if (idate.gt.start) then
              cycle
            else
              call get_variable (model(i))
            endif
          case ("T")
            if (idate.gt.start) then
              cycle
            else
              call get_variable (model(i))
              !todo
              ! force topography to zero over oceans
              !        !!        if (val(4).eq.0.and.val(3).lt.0) val(3) = 0.
            endif
          case default
            if( size(date).eq.0) then
            call get_variable (model(i))
          else
            call get_variable (model(i), date = date(idate)%date)
            endif
          endselect
        endif
      enddo
      result=0.
      call convolve (site(isite))
    enddo
  enddo


  !  if (any (moreverbose%dataname.eq."s")) then
  !    print '(15f13.5)', &
  !      results ( maxloc ( results%e  )  ) %e  - results ( minloc ( results%e  ) ) %e  ,  & 
  !      results ( maxloc ( results%n  )  ) %n  - results ( minloc ( results%n  ) ) %n  ,  & 
  !      results ( maxloc ( results%dh )  ) %dh - results ( minloc ( results%dh ) ) %dh ,  & 
  !      results ( maxloc ( results%dz )  ) %dz - results ( minloc ( results%dz ) ) %dz ,  & 
  !      results ( maxloc ( results%dt )  ) %dt - results ( minloc ( results%dt ) ) %dt
  !  endif

  ! execution time-stamp
  call cpu_time(cpu(2))
  write(log%unit, '(/,"Execution time:",1x,f16.9," seconds")') cpu(2)-cpu(1)
  write(log%unit, form_separator)
end program 
