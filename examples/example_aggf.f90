! ============================================================================
!! This program shows some example of using AGGF module
!! 
!! \author Marcin Rajner
!! \date 20121108
! ============================================================================
program example_aggf
  implicit none

!  print *, "...standard1976 ()"
!  call standard1976 ()

!  print *, "...aggf_resp_hmax ()"
!  call aggf_resp_hmax ()

!  print *, "...aggf_resp_dz ()"
!  call aggf_resp_dz ()

!  print *, "...aggf_resp_t ()"
!  call aggf_resp_t ()

!  print *, "...aggf_resp_h ()"
!  call aggf_resp_h ()

!  print *, "...aggfdt_resp_dt ()"
!  call aggfdt_resp_dt ()

!  print *, "...compare_fels_profiles ()"
!  call compare_fels_profiles ()

!  print *, "...compute_tabulated_green_functions ()"
!  call compute_tabulated_green_functions ()

!  print *, "...aggf_thin_layer ()"
!   call aggf_thin_layer ()

!  print *, "...aggf_resp_fels_profiles ()"
!  call aggf_resp_fels_profiles ()

!  print *, "...compare_tabulated_green_functions ()"
!  call compare_tabulated_green_functions ()

!  print *, "...simple_atmospheric_model()"
!  call simple_atmospheric_model()

contains 
 
! =============================================================================
!> Reproduces data to Fig.~3 in \cite Warburton77
!!
!! \date 2013-03-18
!! \author M. Rajner
!!
! =============================================================================
subroutine simple_atmospheric_model ()
  use mod_constants, only:dp
  use mod_aggf, only:simple_def, bouger

  real(dp) :: R ! km
  integer :: iunit

  open (newunit=iunit,file="/home/mrajner/dr/rysunki/simple_approach.dat" ,&
    action = "write")
    do R = 0. , 25*8
    write ( iunit ,  * ) , R , bouger ( R_opt= R) * 1e8, & !conversion to microGal
      simple_def(R) * 1e8
  enddo

end subroutine

! =============================================================================
!> Compare tabulated green functions from different authors
!!
!! \date 2013-03-18
!! \author M. Rajner
! =============================================================================
subroutine compare_tabulated_green_functions ()
  use mod_constants, only : dp
  use mod_aggf, only:size_ntimes_denser,read_tabulated_green
  use mod_utilities, only : spline_interpolation

  integer :: i , j , file_unit , ii , iii
  real(dp), dimension(:,:), allocatable :: table , results
  real(dp), dimension(:,:), allocatable :: parameters
  real(dp), dimension(:), allocatable :: x1, y1 ,x2 , y2 , x, y , x_interpolated, y_interpolated
  integer :: how_many_denser
  character(len=255), dimension(3) :: authors 
  integer , dimension(3) :: columns

  authors=["rajner", "merriam" , "huang"] 
  ! selected columns for comparison in appropriate tables
  columns=[2 , 2, 2]

  how_many_denser=0

  ! reference author 
  call read_tabulated_green (table , author = authors(1) )
  allocate (results (size_ntimes_denser(size(table(:,1)), how_many_denser) , 0 : size(authors) ))

  ! fill abscissa in column 0
  ii = 1
  do i = 1 ,  size (table (:,1) ) - 1
    do j = 0 , how_many_denser
        results(ii,0) = table (i,1 ) + j * (table (i+1, 1) -table (i,1) ) / ( how_many_denser + 1 )
        ii=ii+1
    enddo
  enddo
  ! and the last element
  results ( size (results (:,0) )  , 0) =  table ( size(table(:,1)) ,1 ) 

  ! take it as main for all series
  allocate(x_interpolated ( size ( results(:,0))))
  x_interpolated = results(:,0)

  open (newunit = file_unit , file = "../examples/compare_aggf.dat", action="write")

  ! for every author 
  do i= 1, size(authors)
    print * , trim ( authors ( i ) )
    call read_tabulated_green (table , author = authors(i) )
    allocate(x ( size (table (:,1))))
    allocate(y ( size (table (:,2))))
    x = table (:,1)
    y = table (:, columns(i))
    call spline_interpolation ( x , y , size(x), x_interpolated, y_interpolated , size(x_interpolated) ) 
    if (i.gt.1) then
      y_interpolated = ( y_interpolated - results(:,1) ) / results(:,1)  * 100.
    endif

    results(:, i ) = y_interpolated
    deallocate(x,y)
  enddo

  write (file_unit , '(<size(results(1,:))>f20.5)' ) ( results (i , :) , i = 1 , size(results ( :,1)) )  
  close(file_unit)
end subroutine

! ============================================================================
!> Compute AGGF and derivatives
!!
!! \author M. Rajner
!! \date 2013-03-18
! ============================================================================
subroutine compute_tabulated_green_functions ()
  use mod_constants, only:dp
  use mod_aggf, only: read_tabulated_green , compute_aggf, compute_aggfdt
  integer :: i , file_unit
  real(dp) :: val_aggf , val_aggfdt ,val_aggfdh, val_aggfdz
  real(dp), dimension(:,:), allocatable :: table , results 

  ! Get the spherical distances from Merriam92
  call read_tabulated_green ( table , author = "merriam")

  open  ( newunit = file_unit, &
          file    = '../dat/rajner_green.dat', &
          action  = 'write' &
        )

  ! print header
  write ( file_unit,*) '# This is set of AGGF computed using module ', &
  'aggf from grat software'
  write ( file_unit,*) '# Normalization according to Merriam92'
  write ( file_unit,*) '# Marcin Rajner'
  write ( file_unit,*) '# For detail see www.geo.republika.pl'
  write ( file_unit,'(10(a23))')  '#psi[deg]', &
    'GN[microGal/hPa]'       , 'GN/dT[microGal/hPa/K]' , &
    'GN/dh[microGal/hPa/km]' , 'GN/dz[microGal/hPa/km]'

  do i= 1, size(table(:,1))
    call compute_aggf   ( table(i,1) , val_aggf   )
    call compute_aggfdt ( table(i,1) , val_aggfdt )
    call compute_aggf   ( table(i,1) , val_aggfdh , first_derivative_h=.true. )
    call compute_aggf   ( table(i,1) , val_aggfdz , first_derivative_z=.true. )
    write ( file_unit, '(10(e23.5))' ) &
      table(i,1) , val_aggf , val_aggfdt , val_aggfdh, val_aggfdz
  enddo
  close(file_unit)
end subroutine

! ============================================================================
!> Compare different vertical temperature profiles impact on AGGF
! ============================================================================
subroutine aggf_resp_fels_profiles ()
  use mod_constants, only: dp
  use mod_aggf, only : read_tabulated_green , compute_aggf
  character (len=255) ,dimension (6) :: fels_types
  real (dp) :: val_aggf
  integer :: i , j, file_unit
  real(dp), dimension(:,:), allocatable :: table  

  ! All possible optional arguments for standard_temperature
  fels_types = (/ "US1976"             , "tropical",   &
                  "subtropical_summer" , "subtropical_winter" , &
                  "subarctic_summer"   , "subarctic_winter"    /)

  open  ( newunit = file_unit, &
          file    = '../examples/aggf_resp_fels_profiles.dat' , &
          action  = 'write' &
        )

  call read_tabulated_green (table, "merriam")

  ! print header
  write ( file_unit , '(100(a20))' ) &
    'psi', ( trim ( fels_types (i) ) , i = 1 , size (fels_types) )

  ! print results
  do i = 1 , size (table(:,1))
    write (file_unit, '(f20.6$)') table(i,1)
    do j = 1 , size(fels_types)
      call compute_aggf(table (i,1), val_aggf ,fels_type=fels_types(j))
      write (file_unit, '(f20.6$)') val_aggf
    enddo
    write(file_unit, *)
  enddo
  close(file_unit)
end subroutine


! ============================================================================
!> Compare different vertical temperature profiles
!!
!! Using tables and formula from \cite Fels86
!! \author M. Rajner
!! \date 2013-03-19
! ============================================================================
subroutine compare_fels_profiles ()
  use mod_constants, only: dp
  use mod_aggf, only : standard_temperature
  character (len=255) ,dimension (6) :: fels_types
  real (dp) :: height , temperature
  integer :: i , file_unit , i_height

  ! All possible optional arguments for standard_temperature
  fels_types = (/ "US1976"             , "tropical",   &
                  "subtropical_summer" , "subtropical_winter" , &
                  "subarctic_summer"   , "subarctic_winter"    /)

  open  ( newunit = file_unit, &
          file    = '../examples/compare_fels_profiles.dat' , &
          action  = 'write' &
        )

  ! Print header
  write ( file_unit , '(100(a20))' ) &
    'height', ( trim ( fels_types (i) ) , i = 1 , size (fels_types) )

  ! Print results
  do i_height = 0 , 70 , 1
    height=dble(i_height)
    write ( file_unit , '(f20.3$)' ) , height
    do i = 1 , size (fels_types)
      call standard_temperature (height, temperature, fels_type=fels_types(i))
      write ( file_unit , '(f20.3$)' ),  temperature 
    enddo
    write ( file_unit , * )
  enddo
  close(file_unit)
end subroutine

! ============================================================================
!> Computes AGGF for different site height (h)
! ============================================================================
subroutine aggf_resp_h ()
  use mod_constants, only : dp
  use mod_aggf , only : read_tabulated_green , compute_aggf
  real(dp), dimension(:,:), allocatable :: table , results
  integer :: i, j, file_unit , ii
  real(dp) :: val_aggf

  ! Get the spherical distances from Merriam92
  call read_tabulated_green ( table , author = "merriam")

  ! Specify the output table and put station height in first row
  allocate ( results ( 0 : size (table(:,1)) , 7 ) )
  results(0,1) = 1./0     ! Infinity in first header
  results(0,3) = 0.0      !   0 m
  results(0,3) = 0.001    !   1 m
  results(0,4) = 0.01     !  10 m
  results(0,5) = 0.1      ! 100 m 
  results(0,6) = 1.       !   1 km
  results(0,7) = 10.      !  10 km

  ! write results to file
  open (                                      &
    newunit = file_unit,                      &
    file    = '../examples/aggf_resp_h.dat',  &
    action  = 'write'                         &
    )

  write (file_unit, '(8(F20.8))' ) results (0, :) 
  do i =1 , size (table(:,1))
    ! denser sampling 
    do ii = 0,8 
      results ( i , 1 )  = table(i,1) + ii * (table (i+1,1) - table (i,1)) / 9. 
      ! only compute for small spherical distances
      if (results (i, 1) .gt. 0.2 ) exit
      write (file_unit, '(F20.7,$)') , results (i,1)
      do j =  2 , size(results(1,: ) )
        call compute_aggf(results(i,1) , val_aggf, dh=dble(0.0001), h =results(0,j))
        results (i,j) = val_aggf
        write (file_unit,'(f20.7,1x,$)') results(i,j)
      enddo
      write (file_unit,*)
    enddo
  enddo
  close (file_unit)
end subroutine

! ============================================================================
!> This computes AGGF for different surface temperature
!!
!! \author M. Rajner
!! \date 2013-03-18
! ============================================================================
subroutine aggf_resp_t ()
  use mod_constants, only : dp , atmosphere
  use mod_aggf, only : read_tabulated_green , compute_aggf
  real(dp), dimension(:,:), allocatable :: table , results
  integer :: i, j , file_unit
  real(dp) :: val_aggf

  ! read spherical distances from Merriam
  call read_tabulated_green ( table , "merriam" )

  ! Header in first row with surface temperature [K]
  allocate ( results (0 : size (table(:,1)) , 4 ) )
  results(0,1) = 1./0
  results(0,2) = atmosphere%temperature%standard +   0. 
  results(0,3) = atmosphere%temperature%standard +  15.0 
  results(0,4) = atmosphere%temperature%standard + -45.0 
  do i =1 , size (table(:,1))
    results ( i , 1 )  = table(i,1)
    do j =  2 , 4
    call compute_aggf ( results (i , 1 ) , val_aggf, dh = dble(0.00001), t_zero = results(0, j) )
    results (i,j) = val_aggf
    enddo
  enddo

  ! Print results to file
  open ( newunit = file_unit , &
         file    = '../examples/aggf_resp_t.dat' , &
         action  = 'write')
  write (file_unit , '(4F20.5)' ) &
    ( (results (i,j) , j=1,4) , i = 0, size ( table (:,1) ) )
  close (file_unit)
end subroutine

! ============================================================================
!> \brief This computes AGGFDT for different dT
! ============================================================================
subroutine aggfdt_resp_dt ()
  use mod_constants, only : dp
  use mod_aggf , only : read_tabulated_green, compute_aggfdt
  real(dp), dimension(:,:), allocatable :: table , results
  integer :: i, j , file_unit
  real(dp) :: val_aggf

  ! read spherical distances from Merriam
  call read_tabulated_green ( table , "merriam" )

  ! Header in first row with surface temperature [K]
  allocate ( results (0 : size (table(:,1)) , 6 ) )
  results(0,1) = 1./0
  results(0,2) = 1.
  results(0,3) = 5. 
  results(0,4) = 10. 
  results(0,5) = 20. 
  results(0,6) = 50. 
  do i =1 , size (table(:,1))
    results ( i , 1 )  = table(i,1)
    do j =  2 , 6
      call compute_aggfdt ( results (i , 1 ) , val_aggf, results(0, j) )
      results (i,j) = val_aggf
    enddo
  enddo

  ! Print results to file
  open ( newunit = file_unit , &
         file    = '../examples/aggfdt_resp_dt.dat' , &
         action  = 'write')
  write (file_unit , '(6F20.5)' ) &
    ( (results (i,j) , j=1,6) , i = 0, size ( table (:,1) ) )
  close (file_unit)
end subroutine

! ============================================================================
!> \brief This computes AGGF for different height integration step 
! ============================================================================
subroutine aggf_resp_dz ()
  use mod_constants, only : dp
  use mod_aggf , only : read_tabulated_green, compute_aggf
  real(dp), dimension(:,:), allocatable :: table , results
  integer :: file_unit , i , j
  real(dp) :: val_aggf

  open ( newunit = file_unit, &
         file    = '../examples/aggf_resp_dz.dat', & 
         action='write')

  ! read spherical distances from Merriam
  call read_tabulated_green (table, "merriam")

  ! Differences in AGGF(dz) only for small spherical distances
  allocate ( results ( 0 : 29 , 0: 5 ) )
  results = 0.

  ! Header in first row [ infty and selected dz follow on ]
  results(0,0) = 1./0 
  results(0,1:5)=(/ 0.0001, 0.001, 0.01, 0.1, 1./)

  do i = 1 , size ( results (:,1) ) - 1
    results (i,0) = table (i , 1 )
    do j = 1 , size (results(1,:) ) - 1
    call compute_aggf ( results (i,0) , val_aggf , dh = results(0,j) )
    results (i, j) =  val_aggf
    enddo

    ! compute relative errors from column 2 for all dz with respect to column 1
    results(i,2:) = abs((results(i,2:) - results (i,1)) / results (i,1) * 100 )
  enddo

  ! write result to file
  write ( file_unit , '(<size(results(1,:))>f14.6)' ) &
    ((results (i,j), j=0,size(results (1,:)) - 1), i=0,size(results(:,1)) - 1)
  close(file_unit)
end subroutine

! ============================================================================
!> \brief This computes standard atmosphere parameters
!!
!! It computes temperature, gravity, pressure, pressure (simplified formula)
!! density for given height
! ============================================================================
subroutine standard1976  !()
  use mod_constants, only : dp
  use mod_aggf, only : standard_temperature, standard_pressure , &
    standard_gravity , standard_density
  real(dp) :: height , temperature , gravity , pressure , pressure2 , density
  integer :: file_unit

  open ( newunit = file_unit , &
         file    = '../examples/standard1976.dat', &
         action  = 'write' )
  ! print header
  write ( file_unit , '(6(a12))' ) &
    'height[km]', 'T[K]' , 'g[m/s2]' , 'p[hPa]', 'p_simp[hPa]' , 'rho[kg/m3]'
  do height=0.,98.
    call standard_temperature ( height , temperature )
    call standard_gravity     ( height , gravity )
    call standard_pressure    ( height , pressure )
    call standard_pressure    ( height , pressure2 , if_simplificated = .true. )
    call standard_density     ( height , density )
    ! print results to file
    write( file_unit,'(5f12.5, e12.3)'), &
    height,temperature , gravity , pressure , pressure2 , density 
  enddo
  close( file_unit )
end subroutine

! ============================================================================
!> \brief This computes relative values of AGGF for different atmosphere
!! height integration
! ============================================================================
subroutine aggf_resp_hmax ()
  use mod_constants, only : dp
  use mod_aggf, only : compute_aggf
  real (dp) , dimension (10) :: psi
  real (dp) , dimension (:)   , allocatable :: heights 
  real (dp) , dimension (:,:) , allocatable :: results
  integer :: file_unit , i , j
  real(dp) :: val_aggf

  ! selected spherical distances
  psi=(/0.000001, 0.000005,0.00001, 1,  2, 3 , 5, 10 , 90 ,  180 /)

  ! get heights (for nice graph) - call auxiliary subroutine
  call aux_heights ( heights )

  open ( newunit = file_unit , &
         file    = '../examples/aggf_resp_hmax.dat', & 
         action  = 'write')

  allocate ( results ( 0:size(heights)-1 , 1+size(psi) ) ) 

  do j=0 , size (results (:,1))
      results ( j , 1 ) = heights(j)

    do i = 1 , size(psi)
      call compute_aggf ( psi (i) , val_aggf , hmax = heights(j) )
      results(j,i+1) = val_aggf

      !> Relative value of aggf depending on integration height
      if (j.gt.0) then
        results(j,i+1) = results (j,i+1) / results (0,i+1) * 100 
      endif
    enddo
  enddo

  ! print header
  write(file_unit , '(a14,SP,100f14.5)' ),"#wys\psi", (psi(j) , j= 1,size(psi))
  ! print results
  do i=1, size (results (:,1))-1
    write(file_unit, '(100f14.3)' ) (results(i,j), j = 1, size(psi)+1 )
  enddo
  close(file_unit)
end subroutine

! ============================================================================
!> Auxiliary subroutine -- height sampling for semilog plot
! ============================================================================
subroutine aux_heights ( table )
  use mod_constants, only : dp
  real(dp) , dimension (:), allocatable, intent(inout) :: table
  real(dp) , dimension (0:1000) :: heights
  real(dp) :: height
  integer :: i , count_heights

  heights(0) =60
  i=0
  height=-0.001
  do while (height.lt.60)
    i=i+1
    if (height.lt.0.10) then
      height=height+2./1000
    elseif (height.lt.1) then
      height=height+50./1000
    else
      height=height+1
    endif
    heights(i)= height
    count_heights=i
  enddo
  allocate ( table ( 0 : count_heights ) )
  table (0 : count_heights ) = heights ( 0 : count_heights )
end subroutine

subroutine aggf_thin_layer ()
  use mod_constants, only : dp 
  use mod_aggf, only : read_tabulated_green, GN_thin_layer
  integer :: file_unit , i
  real(dp) , dimension (:,:), allocatable :: table

  ! read spherical distances from Merriam
  call read_tabulated_green (table, "merriam")
  do i = 1 , size (table (:,1))
    write(*,*) table(i,1:2) , GN_thin_layer (table (i,1))
  enddo
end subroutine

end program 
