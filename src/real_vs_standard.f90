! ==============================================================================
!> \file
!! This program
!!
!! \todo put description
! ==============================================================================
program real_vs_standard
!  use mod_constants, only :dp
  use mod_parser
  use mod_data
!  use mod_aggf,      only : geop2geom
  
  call cpu_time(cpu_start)

!  call intro (program_calling="real_vs_standard")
  

!    do ii = 1 , min(2,size(model))
!      if (model(ii)%if) call get_variable ( model(ii) , date = dates(j)%date)
!    enddo
!
!
!
!!\todo
!    do i = 1 , size(sites)
!      write(output%unit, '(2f15.5f)', advance ="no") sites(i)%lat ,sites(i)%lon
!      iii=iii+1
!!      call convolve (sites(i) , green , results(iii), denserdist = denser(1) , denseraz = denser(2))
!      write (output%unit,'(15f13.5)') , results(iii)%e ,results(iii)%n  ,results(iii)%dt , results(iii)%dh, results(iii)%dz
!    enddo
!  enddo
!
!
!  call cpu_time(cpu_finish)
!  write(log%unit, '(/,"Execution time:",1x,f16.9," seconds")') cpu_finish - cpu_start
!  write(log%unit, form_separator)
!
!  print * , model(1)%level
!  print *
!  lat =00
!  lon = 00
!  call get_value(model(1),lat,lon, val(0))
!
!  do i =1, size(model(2)%level)
!    call get_value(model(2),lat,lon, val(i), level = i, method=1)
!  enddo
!  print  '(2f10.2)', lat , lon , (val(i),geop2geom(val(i)/1000)*1000., i=0,size(model(2)%level))
end program
