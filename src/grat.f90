! ==============================================================================
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

  use mod_constants , only : dp
  use mod_cmdline   
  use mod_green     , only : results ,convolve
  use mod_polygon   , only : read_polygon
  use mod_data      , only : read_netCDF , get_variable

  implicit none
  real(dp) :: x , y , z , lat ,lon 
  integer :: i , j , ii, iii


  ! program starts here with time stamp
  call cpu_time(cpu_start)

  ! gather cmd line option decide where to put output
  call intro (program_calling = "grat" , &
    accepted_switches="VSBLGPpoFIDLvhRQ" , cmdlineargs=.true.)

  ! read polygons
  do i =1 , 2
    call read_polygon (polygons(i))
  enddo

  ! read models into memory
  do i =1 , size(model)
    if (model(i)%if) call read_netCDF (model(i))
    print *, i, model(i)%name , model(i)%if_constant_value
  enddo


  allocate (results(size(sites)*max(size(dates),1)))

!  iii=0
!  do j = 1 , max(size (dates),1)
!    if(size(dates).gt.0)  write(output%unit, '(i4,5(i2.2))', advance ="no") dates(j)%date
!
!    do ii = 1 , min(2,size(model))
!      if (model(ii)%if) call get_variable ( model(ii) , date = dates(j)%date)
!    enddo
!
!    write(log%unit, form_separator)
!    write(log%unit, form_60) "Results:"
!    if (output%if.and.(output%name /= "")) write(log%unit, form_61) "written into file:" , trim(output%name)
!    do i = 1 , size(sites)
!      write(output%unit, '(2f15.5f)', advance ="no") sites(i)%lat ,sites(i)%lon
!      iii=iii+1
!      call convolve (sites(i) , green , results(iii), denserdist = denser(1) , denseraz = denser(2))
!      write (output%unit,'(15f13.5)') , results(iii)%e ,results(iii)%n  ,results(iii)%dt , results(iii)%dh, results(iii)%dz
!    enddo
!  enddo
!
!
!  !todo
!!  if (moreverbose%if .and. moreverbose%names(1).eq."s") then
!!    print '(15f13.5)', &
!!      results ( maxloc ( results%e  )  ) %e  - results ( minloc ( results%e  ) ) %e  ,  & 
!!      results ( maxloc ( results%n  )  ) %n  - results ( minloc ( results%n  ) ) %n  ,  & 
!!      results ( maxloc ( results%dh )  ) %dh - results ( minloc ( results%dh ) ) %dh ,  & 
!!      results ( maxloc ( results%dz )  ) %dz - results ( minloc ( results%dz ) ) %dz ,  & 
!!      results ( maxloc ( results%dt )  ) %dt - results ( minloc ( results%dt ) ) %dt
!!  endif
!
!
!  ! execution time-stamp
!  call cpu_time(cpu_finish)
!  write(log%unit, '(/,"Execution time:",1x,f16.9," seconds")') cpu_finish - cpu_start
!  write(log%unit, form_separator)
end program 
