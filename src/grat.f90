! ==============================================================================
!> \file
!! \mainpage grat overview
!! \section Purpose
!! This program was created to make computation of atmospheric gravity
!! correction easier. Still developing. Consider visiting later...
!!
!! \version TESTING!
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
!!
!! \section Usage
!! After sucsesfull compiling make sure the executables are in your search path
!! 
!! There is main program \c grat and some utilities program. For the options see
!! the appropriate help:
!!  - \link grat-h grat\endlink
!!  - \link value_check-h value_check\endlink
!!  - \link polygon_check-h polygon_check\endlink
!!
!! \page grat-h grat
!!    \include grat.hlp

!> \page ilustration
!! \image latex /home/mrajner/src/grat/doc/interpolation_ilustration.pdf "example"
!! \image latex /home/mrajner/src/grat/doc/mapa1
!! \image latex /home/mrajner/src/grat/doc/mapa2
!! \image latex /home/mrajner/src/grat/doc/mapa3
!! 
!! \image html /home/mrajner/src/grat/doc/interpolation_ilustration.png "interpolation example" width=\textwidth
!! \image html /home/mrajner/src/grat/doc/mapa1.png
!! \image html /home/mrajner/src/grat/doc/mapa2.png
!! \image html /home/mrajner/src/grat/doc/mapa3.png

!> \page intro_sec External resources
!!   - <a href="https://code.google.com/p/grat">project page</a> (git repository)
!!   - \htmlonly <a href="../latex/refman.pdf">[pdf]</a> version of this manual\endhtmlonly
!!   \latexonly \href{https://grat.googlecode.com/git/doc/html/index.html}{html} version of this manual\endlatexonly
!! \TODO give source for grant presentation
!!   - <a href="">[pdf]</a> command line options (in Polish)

!> \example example_aggf.f90
!! \example grat_usage.sh
! ==============================================================================
program grat

  use mod_constants , only : dp
  use mod_cmdline   , only : cpu_start , cpu_finish,  intro , print_settings , &
    polygons , model , refpres, form_separator , log ,dates , sites, output, &
    moreverbose, form_60 , form_61, green ,denser
  use mod_green     , only : results ,convolve
  use mod_polygon   , only : read_polygon
  use mod_data      , only : read_netCDF , get_variable

  implicit none
  real(dp) :: x , y , z , lat ,lon ,val(0:100) !tmp variables
  integer :: i , j , ii, iii

  ! program starts here with time stamp
  call cpu_time(cpu_start)

  ! gather cmd line option decide where to put output
  call intro (program_calling = "grat" , accepted_switches="VSBLGPpoFIDLvhRQ" , cmdlineargs=.true.)

  ! for grat set default for Green functions if not given in command line
  ! options
!  if (.not.allocated(green)) then
!    dummy="-G,,,"
!    call mod_cmdline_entry(dummy,cmd_line_entry,program_calling="grat")
!  endif

!  if (size(model) .eq. 0) then
!    write(error_unit, * ) "ERROR:", program_calling, " -- model file not specified!"
!    call exit
!  endif


  ! read polygons
  do i =1 , 2
    call read_polygon (polygons(i))
  enddo

  ! read models into memory
  do i =1 , size(model)
    if (model(i)%if) call read_netCDF (model(i))
  enddo

  ! todo refpres in get_cmd-line
  if (refpres%if) then
    refpres%name="/home/mrajner/src/grat/data/refpres/vienna_p0.grd"
    call read_netCDF (refpres)
  endif


  allocate (results(size(sites)*max(size(dates),1)))
  iii=0
  do j = 1 , max(size (dates),1)
    if(size(dates).gt.0)  write(output%unit, '(i4,5(i2.2))', advance ="no") dates(j)%date

    do ii = 1 , min(2,size(model))
      if (model(ii)%if) call get_variable ( model(ii) , date = dates(j)%date)
    enddo

    write(log%unit, form_separator)
    write(log%unit, form_60) "Results:"
    if (output%if.and.(output%name /= "")) write(log%unit, form_61) "written into file:" , trim(output%name)
    do i = 1 , size(sites)
      write(output%unit, '(2f15.5f)', advance ="no") sites(i)%lat ,sites(i)%lon
      iii=iii+1
      call convolve (sites(i) , green , results(iii), denserdist = denser(1) , denseraz = denser(2))
      write (output%unit,'(15f13.5)') , results(iii)%e ,results(iii)%n  ,results(iii)%dt , results(iii)%dh, results(iii)%dz
    enddo
  enddo


  if (moreverbose%if .and. moreverbose%names(1).eq."s") then
    print '(15f13.5)', &
      results ( maxloc ( results%e  )  ) %e  - results ( minloc ( results%e  ) ) %e  ,  & 
      results ( maxloc ( results%n  )  ) %n  - results ( minloc ( results%n  ) ) %n  ,  & 
      results ( maxloc ( results%dh )  ) %dh - results ( minloc ( results%dh ) ) %dh ,  & 
      results ( maxloc ( results%dz )  ) %dz - results ( minloc ( results%dz ) ) %dz ,  & 
      results ( maxloc ( results%dt )  ) %dt - results ( minloc ( results%dt ) ) %dt
  endif


  call cpu_time(cpu_finish)
  write(log%unit, '(/,"Execution time:",1x,f16.9," seconds")') cpu_finish - cpu_start
  write(log%unit, form_separator)
end program 
