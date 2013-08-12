! ============================================================================
!! This program shows some example of using AGGF module
!! 
!! \author Marcin Rajner
!! \date 20121108
! ============================================================================
program example_aggf
  use mod_atmosphere
  use mod_constants,only :dp
  use mod_utilities
  implicit none
  character(20):: host
  real(dp) :: x

  call standard1976 ('/home/mrajner/src/grat/examples/standard1976.dat')
  call compare_fels_profiles ('/home/mrajner/src/grat/examples/compare_fels_profiles.dat')
  call simple_atmospheric_model ("/home/mrajner/dr/rysunki/simple_approach.dat")
  call green_newtonian_compute( &
    ["green_newtonian_olsson.dat","green_newtonian_spotl.dat","green_newtonian.dat"])
  call admit_niebauer("/home/mrajner/src/grat/examples/admit_niebauer.dat")
  call aggf_thin_layer ("/home/mrajner/src/grat/examples/tmp")



  ! run only on server
  !call hostnm(host)
  !if (host.eq."grat") then
!  do x =1 , 1
    call compute_tabulated_green_functions ('/home/mrajner/src/grat/dat/rajner_green.dat', "full")
!  enddo
  !endif

  !  call aggf_resp_hmax ()
  !  call aggf_resp_dz ()
  !  call aggf_resp_t ()
  !  call aggf_resp_h ()
  !  call aggfdt_resp_dt ()
  !  call aggf_resp_fels_profiles ()


  !  call mass_vs_height() !'/home/mrajner/src/grat/examples/mass_vs_height.dat')



contains 
! =============================================================================
!> Mass of atmosphere respect to height
! =============================================================================
subroutine mass_vs_height (filename)
  use, intrinsic:: iso_fortran_env
  use mod_utilities, only: file_exists
  use mod_constants, only : dp,pi,earth
  use mod_atmosphere
  character(*) , intent (in) , optional:: filename
  real(dp) :: max_height,dh, percent
  real(dp) , allocatable, dimension(:):: mass, mass2, height
  integer::i,j,file_unit

  if (present (filename)) then
    if (file_exists(filename)) return
    open ( &
      newunit = file_unit, & 
      file    = filename,  & 
      action  = 'write'    & 
      )
  else
    file_unit = output_unit
  endif
  write(*,*), "mass_vs_height ---> ",filename

  max_height=50000.
  dh=10

  allocate(height(int(max_height/dh)+1))
  allocate(mass(size(height)))
  do i =1,size(height)
    height(i) = dh*(i-1) 
    mass  (i) = standard_density (height(i), method="full")
    !    mass  (i) = standard_density (height(i))
  enddo

  do i =0,50000,1000
    percent=0
    do j = 1 , size(height)
      if (height(j).le.dble(i)) percent=percent+mass(j)
    enddo
    percent = percent /sum(mass)*100
    write(file_unit, '(i6,2f19.9,es10.3)' ) , i ,percent , &
      100-(earth%radius+dble(1))**2 * standard_pressure(dble(i)) / standard_gravity(dble(i))&
      /earth%radius**2/standard_pressure(dble(0)) * standard_gravity(dble(0))*100

  enddo
end subroutine

! =============================================================================
!> Reproduces data to Fig.~3 in \cite Warburton77
!!
!! \date 2013-03-18
!! \author M. Rajner
!!
! =============================================================================
subroutine simple_atmospheric_model (filename)
  use, intrinsic:: iso_fortran_env
  use mod_utilities, only: file_exists
  use mod_constants
  use mod_aggf, only:simple_def, bouger

  real(dp) :: R ! km
  integer :: file_unit
  character(*) , intent (in) , optional:: filename
  real(dp) :: h =9.

  if (present (filename)) then
    if (file_exists(filename)) return
    open ( &
      newunit = file_unit, & 
      file    = filename,  & 
      action  = 'write'    & 
      )
  else
    file_unit = output_unit
  endif

  write(*,*), "simple_atmospheric_model ---> ",filename

  do R = 0. , 25*8
    write (file_unit,  * ), R,-100*bouger(h=h,R=R)/(earth%gravity%mean*h)  * 1e8, & !conversion to microGal
      -simple_def(R) * 1e8
  enddo

end subroutine


! ============================================================================
!> Compute AGGF and derivatives
!!
!! \author M. Rajner
!! \date 2013-03-18
! ============================================================================
subroutine compute_tabulated_green_functions (filename, method,dz)
  use mod_constants, only:dp
  use mod_aggf , only: aggf, aggfd
  use mod_green, only: green, read_green
  use mod_utilities, only: d2r , file_exists
  use mod_atmosphere
  integer :: i , file_unit
  character(*), intent(in) :: filename
  real(dp), optional :: dz
  real(dp) ::  t_zero , z
  character(100) :: fels_type
  character(*), optional :: method

  if (file_exists(filename)) then
    return
  else
    print '(a,a)' , "compute_tabulated_green_functions ---> ", trim(filename)
  endif
  ! Get the spherical distances from Merriam92
  if(allocated(green)) deallocate(green)
  allocate(green(1))
  green(1)%name="/home/mrajner/src/grat/dat/merriam_green.dat"
  green(1)%column=[1,1]
  call read_green(green(1))

  open (                                 & 
    newunit = file_unit,                 & 
    file    = filename, & 
    action  = 'write'                    & 
    )

  !print header
  write ( file_unit,*) '# This is set of AGGF computed using module ', & 
    'aggf from grat software'
  write ( file_unit,*) '# Normalization according to Merriam92'
  write ( file_unit,*) '# Marcin Rajner'
  write ( file_unit,*) '# For detail see www.geo.republika.pl'
  write ( file_unit,'(10(a23))')  '#psi[deg]',                         & 
    'GN[microGal/hPa]'       , 'GN/dT[microGal/hPa/K]' ,               & 
    'GN/dh[microGal/hPa/m]' , 'GN/dz[microGal/hPa/km]'


  !todo
  !  file_unit=6
  do i= 1, size(green(1)%distance)
    write(file_unit, '(13f15.6)'), &
      green(1)%distance(i), &
      aggf(d2r(green(1)%distance(i)),method=method, dz=dz), &
      aggfd(d2r(green(1)%distance(i)), method=method ,aggfdt=.true. , predefined=.false.) , &
      aggf(d2r(green(1)%distance(i)),method=method, dz=dz,first_derivative_h=.true.) , &
      aggf(d2r(green(1)%distance(i)),method=method, dz=dz,first_derivative_z=.true.)
  enddo
  close(file_unit)
end subroutine

!! ============================================================================
!!> Compare different vertical temperature profiles impact on AGGF
!! ============================================================================
!subroutine aggf_resp_fels_profiles ()
!  use mod_constants, only: dp
!  use mod_aggf, only : read_tabulated_green , compute_aggf
!  character (len=255) ,dimension (6) :: fels_types
!  real (dp) :: val_aggf
!  integer :: i , j, file_unit
!  real(dp), dimension(:,:), allocatable :: table  

!  ! All possible optional arguments for standard_temperature
!  fels_types = (/ "US1976"             , "tropical",   &
!                  "subtropical_summer" , "subtropical_winter" , &
!                  "subarctic_summer"   , "subarctic_winter"    /)

!  open  ( newunit = file_unit, &
!          file    = '../examples/aggf_resp_fels_profiles.dat' , &
!          action  = 'write' &
!        )

!  call read_tabulated_green (table, "merriam")

!  ! print header
!  write ( file_unit , '(100(a20))' ) &
!    'psi', ( trim ( fels_types (i) ) , i = 1 , size (fels_types) )

!  ! print results
!  do i = 1 , size (table(:,1))
!    write (file_unit, '(f20.6$)') table(i,1)
!    do j = 1 , size(fels_types)
!      call compute_aggf(table (i,1), val_aggf ,fels_type=fels_types(j))
!      write (file_unit, '(f20.6$)') val_aggf
!    enddo
!    write(file_unit, *)
!  enddo
!  close(file_unit)
!end subroutine


!! ============================================================================
!!> Compare different vertical temperature profiles
!!!
!!! Using tables and formula from \cite Fels86
!!! \author M. Rajner
!!! \date 2013-03-19
!! ============================================================================
subroutine compare_fels_profiles (filename)
  use iso_fortran_env
  use mod_utilities, only: file_exists
  use mod_constants, only: dp
  use mod_atmosphere, only : standard_temperature
  character (len=255) ,dimension (6) :: fels_types
  real (dp) :: height , temperature
  integer :: i , file_unit , i_height
  character(*), intent (in),optional:: filename

  ! All possible optional arguments for standard_temperature
  fels_types = (/ "US1976"             , "tropical",   &
    "subtropical_summer" , "subtropical_winter" , &
    "subarctic_summer"   , "subarctic_winter"    /)

  if (present (filename)) then
    if (file_exists(filename)) return
    open ( newunit = file_unit , &
      file =filename , &
      action  = 'write' )
  else
    file_unit = output_unit
  endif

  print * , "compare_fels_profiles --->", filename

  ! Print header
  write ( file_unit , '(100(a20))' ) &
    'height', ( trim ( fels_types (i) ) , i = 1 , size (fels_types) )

  ! Print results
  do i_height = 0 , 70 , 1
    height=dble(i_height)
    write ( file_unit , '(f20.3$)' ) , height
    do i = 1 , size (fels_types)
      write ( file_unit , '(f20.3$)' ),  standard_temperature (height, fels_type=fels_types(i))
    enddo
    write ( file_unit , * )
  enddo
  close(file_unit)
end subroutine

!! ============================================================================
!!> Computes AGGF for different site height (h)
!! ============================================================================
!subroutine aggf_resp_h ()
!  use mod_constants, only : dp
!  use mod_aggf  !, only : read_tabulated_green , compute_aggf
!  real(dp), dimension(:,:), allocatable :: table , results
!  integer :: i, j, file_unit , ii
!  real(dp) :: val_aggf

!  ! Get the spherical distances from Merriam92
!  call read_tabulated_green ( table , author = "merriam")

!  ! Specify the output table and put station height in first row
!  allocate ( results ( 0 : size (table(:,1)) , 7 ) )
!  results(0,1) = 1./0     ! Infinity in first header
!  results(0,3) = 0.0      !   0 m
!  results(0,3) = 0.001    !   1 m
!  results(0,4) = 0.01     !  10 m
!  results(0,5) = 0.1      ! 100 m 
!  results(0,6) = 1.       !   1 km
!  results(0,7) = 10.      !  10 km

!  ! write results to file
!  open (                                      &
!    newunit = file_unit,                      &
!    file    = '../examples/aggf_resp_h.dat',  &
!    action  = 'write'                         &
!    )

!  write (file_unit, '(8(F20.8))' ) results (0, :) 
!  do i =1 , size (table(:,1))
!    ! denser sampling 
!    do ii = 0,8 
!      results ( i , 1 )  = table(i,1) + ii * (table (i+1,1) - table (i,1)) / 9. 
!      ! only compute for small spherical distances
!      if (results (i, 1) .gt. 0.2 ) exit
!      write (file_unit, '(F20.7,$)') , results (i,1)
!      do j =  2 , size(results(1,: ) )
!        call compute_aggf(results(i,1) , val_aggf, dh=dble(0.0001), h =results(0,j))
!        results (i,j) = val_aggf
!        write (file_unit,'(f20.7,1x,$)') results(i,j)
!      enddo
!      write (file_unit,*)
!    enddo
!  enddo
!  close (file_unit)
!end subroutine

!! ============================================================================
!!> This computes AGGF for different surface temperature
!!!
!!! \author M. Rajner
!!! \date 2013-03-18
!! ============================================================================
!subroutine aggf_resp_t ()
!  use mod_constants, only : dp , atmosphere
!  use mod_aggf !, only : read_tabulated_green , compute_aggf
!  real(dp), dimension(:,:), allocatable :: table , results
!  integer :: i, j , file_unit
!  real(dp) :: val_aggf

!  ! read spherical distances from Merriam
!  call read_tabulated_green ( table , "merriam" )

!  ! Header in first row with surface temperature [K]
!  allocate ( results (0 : size (table(:,1)) , 4 ) )
!  results(0,1) = 1./0
!  results(0,2) = atmosphere%temperature%standard +   0. 
!  results(0,3) = atmosphere%temperature%standard +  15.0 
!  results(0,4) = atmosphere%temperature%standard + -45.0 
!  do i =1 , size (table(:,1))
!    results ( i , 1 )  = table(i,1)
!    do j =  2 , 4
!    call compute_aggf ( results (i , 1 ) , val_aggf, dh = dble(0.00001), t_zero = results(0, j) )
!    results (i,j) = val_aggf
!    enddo
!  enddo

!  ! Print results to file
!  open ( newunit = file_unit , &
!         file    = '../examples/aggf_resp_t.dat' , &
!         action  = 'write')
!  write (file_unit , '(4F20.5)' ) &
!    ( (results (i,j) , j=1,4) , i = 0, size ( table (:,1) ) )
!  close (file_unit)
!end subroutine

!! ============================================================================
!!> \brief This computes AGGFDT for different dT
!! ============================================================================
!subroutine aggfdt_resp_dt ()
!  use mod_constants, only : dp
!  use mod_aggf  !, only : read_tabulated_green, compute_aggfdt
!  real(dp), dimension(:,:), allocatable :: table , results
!  integer :: i, j , file_unit
!  real(dp) :: val_aggf

!  ! read spherical distances from Merriam
!  call read_tabulated_green ( table , "merriam" )

!  ! Header in first row with surface temperature [K]
!  allocate ( results (0 : size (table(:,1)) , 6 ) )
!  results(0,1) = 1./0
!  results(0,2) = 1.
!  results(0,3) = 5. 
!  results(0,4) = 10. 
!  results(0,5) = 20. 
!  results(0,6) = 50. 
!  do i =1 , size (table(:,1))
!    results ( i , 1 )  = table(i,1)
!    do j =  2 , 6
!      call compute_aggfdt ( results (i , 1 ) , val_aggf, results(0, j) )
!      results (i,j) = val_aggf
!    enddo
!  enddo

!  ! Print results to file
!  open ( newunit = file_unit , &
!         file    = '../examples/aggfdt_resp_dt.dat' , &
!         action  = 'write')
!  write (file_unit , '(6F20.5)' ) &
!    ( (results (i,j) , j=1,6) , i = 0, size ( table (:,1) ) )
!  close (file_unit)
!end subroutine

!! ============================================================================
!!> \brief This computes AGGF for different height integration step 
!! ============================================================================
!subroutine aggf_resp_dz ()
!  use mod_constants, only : dp
!  use mod_aggf  !, only : read_tabulated_green, compute_aggf
!  real(dp), dimension(:,:), allocatable :: table , results
!  integer :: file_unit , i , j
!  real(dp) :: val_aggf

!  open ( newunit = file_unit, &
!         file    = '../examples/aggf_resp_dz.dat', & 
!         action='write')

!  ! read spherical distances from Merriam
!  call read_tabulated_green (table, "merriam")

!  ! Differences in AGGF(dz) only for small spherical distances
!  allocate ( results ( 0 : 29 , 0: 5 ) )
!  results = 0.

!  ! Header in first row [ infty and selected dz follow on ]
!  results(0,0) = 1./0 
!  results(0,1:5)=(/ 0.0001, 0.001, 0.01, 0.1, 1./)

!  do i = 1 , size ( results (:,1) ) - 1
!    results (i,0) = table (i , 1 )
!    do j = 1 , size (results(1,:) ) - 1
!    call compute_aggf ( results (i,0) , val_aggf , dh = results(0,j) )
!    results (i, j) =  val_aggf
!    enddo

!    ! compute relative errors from column 2 for all dz with respect to column 1
!    results(i,2:) = abs((results(i,2:) - results (i,1)) / results (i,1) * 100 )
!  enddo

!  ! write result to file
!  write ( file_unit , '(<size(results(1,:))>f14.6)' ) &
!    ((results (i,j), j=0,size(results (1,:)) - 1), i=0,size(results(:,1)) - 1)
!  close(file_unit)
!end subroutine

! ============================================================================
!> \brief This computes standard atmosphere parameters
!!
!! It computes temperature, gravity, pressure, pressure (simplified formula)
!! density for given height
! ============================================================================
subroutine standard1976(filename)
  use, intrinsic :: iso_fortran_env
  use mod_utilities, only: file_exists
  use mod_constants, only : dp
  use mod_atmosphere, only: &
    standard_temperature, standard_pressure , &
    standard_gravity,     standard_density
  integer :: file_unit
  character(*) , intent (in) , optional:: filename
  real(dp) :: height

  if (present (filename)) then
    if (file_exists(filename)) return
    open ( newunit = file_unit , &
      file =filename , &
      action  = 'write' )
  else
    file_unit = output_unit
  endif

  print * , "standard atmosphere --->", filename
  ! print header
  write ( file_unit , '(6(a15))' ) &
    'height', 'T' , 'g' , 'p', 'rho'
  do height=0.,68000. , 1000
    ! print results to file
    write( file_unit,'(5f15.5, e12.3)'), & 
      height/1000.,                        & 
      standard_temperature(height),        & 
      standard_gravity(height),            & 
      standard_pressure(height)/100.,      &  ! --> hPa
      standard_density (height)
  enddo
  close( file_unit )
end subroutine

!! ============================================================================
!!> \brief This computes relative values of AGGF for different atmosphere
!!! height integration
!! ============================================================================
!subroutine aggf_resp_hmax ()
!  use mod_constants, only : dp
!  use mod_aggf, only : compute_aggf
!  real (dp) , dimension (10) :: psi
!  real (dp) , dimension (:)   , allocatable :: heights 
!  real (dp) , dimension (:,:) , allocatable :: results
!  integer :: file_unit , i , j
!  real(dp) :: val_aggf

!  ! selected spherical distances
!  psi=(/0.000001, 0.000005,0.00001, 1,  2, 3 , 5, 10 , 90 ,  180 /)

!  ! get heights (for nice graph) - call auxiliary subroutine
!  call aux_heights ( heights )

!  open ( newunit = file_unit , &
!    file    = '../examples/aggf_resp_hmax.dat', & 
!    action  = 'write')

!  allocate ( results ( 0:size(heights)-1 , 1+size(psi) ) ) 

!  do j=0 , size (results (:,1))
!    results ( j , 1 ) = heights(j)

!    do i = 1 , size(psi)
!      call compute_aggf ( psi (i) , val_aggf , hmax = heights(j) )
!      results(j,i+1) = val_aggf

!      !> Relative value of aggf depending on integration height
!      if (j.gt.0) then
!        results(j,i+1) = results (j,i+1) / results (0,i+1) * 100 
!      endif
!    enddo
!  enddo

!  ! print header
!  write(file_unit , '(a14,SP,100f14.5)' ),"#wys\psi", (psi(j) , j= 1,size(psi))
!  ! print results
!  do i=1, size (results (:,1))-1
!    write(file_unit, '(100f14.3)' ) (results(i,j), j = 1, size(psi)+1 )
!  enddo
!  close(file_unit)
!end subroutine

! ============================================================================
!> Auxiliary subroutine -- height sampling for semilog plot
! ============================================================================
!subroutine aux_heights ( table )
!  use mod_constants, only : dp
!  real(dp) , dimension (:), allocatable, intent(inout) :: table
!  real(dp) , dimension (0:1000) :: heights
!  real(dp) :: height
!  integer :: i , count_heights

!  heights(0) =60
!  i=0
!  height=-0.001
!  do while (height.lt.60)
!    i=i+1
!    if (height.lt.0.10) then
!      height=height+2./1000
!      elseif (height.lt.1) then
!      height=height+50./1000
!    else
!      height=height+1
!    endif
!    heights(i)= height
!    count_heights=i
!  enddo
!  allocate ( table ( 0 : count_heights ) )
!  table (0 : count_heights ) = heights ( 0 : count_heights )
!end subroutine

! ============================================================================
! ============================================================================
subroutine aggf_thin_layer (filename)
  use, intrinsic:: iso_fortran_env
  use mod_constants, only : dp , pi
  use mod_aggf, only : GN_thin_layer
  use mod_utilities, only: d2r, file_exists
  use mod_green

  integer :: file_unit , i
  real(dp) , dimension (:,:), allocatable :: table
  character(*) , intent (in) , optional:: filename

  if (file_exists(filename)) return

  allocate (green(1))
  green(1)%name="merriam"
  green(1)%column=[1, 2]
  call read_green(green(1))

  write(*,*), "aggf_thin_layer ---> ",filename
  if (present (filename)) then
    open (newunit = file_unit , &
      file =filename , &
      action  = 'write' )
  else
    file_unit = output_unit
  endif
  do i = 1 , size (green(1)%distance)
    write(file_unit,*) green(1)%distance(i) ,green(1)%data(i), &
      GN_thin_layer (d2r(green(1)%distance(i)))
  enddo
end subroutine

subroutine admit_niebauer(filename)
  use mod_constants
  use mod_utilities
  real(dp) :: a
  real(dp) :: theta
  real(dp) :: b , f
  character(*), intent(in) :: filename
  integer::iun

  if (file_exists(filename)) return
  print * , "admit_niebauer ---> ", filename

  open (newunit=iun, file=filename, action = 'write')

  f=earth%radius/9500
  do theta=0.5 , 180, 0.01
    b= 2*f*sin(d2r(theta/2))
    a= 2*pi * gravity%constant / earth%gravity%mean* &
      (1 - b/(2*f) -1/b + 2/f)
    write(iun, *) , theta , a *1e10
  enddo
end subroutine

! =============================================================================
!> compute green newtonian function
! =============================================================================
subroutine green_newtonian_compute(filenames)
  use mod_utilities, only: file_exists
  use mod_green
  use mod_utilities, only : logspace , d2r
  integer:: iun , n , i , j , k
  real (dp) , allocatable , dimension(:) :: psi , h
  character(12) , allocatable , dimension(:) :: column_name
  character(*) ,  optional :: filenames(3)
  character(20) :: method
  character(40) :: prefix

  prefix="/home/mrajner/src/grat/examples/"

  iun = 6

  n = 9 * 50
  allocate(psi(n))
  psi = logspace(real(1e-6,dp) , real(180,dp),n) 

  allocate(h(11))
  h = [ 0. , 1. , 10. , 100., 1000. , 10000., -1., -10. , -100., -1000., -10000]

  allocate(column_name(size(h)))
  write(column_name, '(f0.0)' ) (h(i),i=1,11)

  do k =1,3
    if (file_exists(trim(prefix)//trim(filenames(k)))) cycle
    print *, "green_newtonian_compute ---> " , trim(prefix)//trim(filenames(k))
    open (newunit=iun, file=trim(prefix)//filenames(k), action = 'write')

    method = filenames(k)(17:index(filenames(k),".")-1)
    write(iun, '(a12,<size(h)>a12)') "#psi" ,( "h"//trim(column_name(i)) , i = 1 ,11)
    write(iun, '(<size(h)+1>en12.2)') , (psi(i), &
      (green_newtonian(d2r(psi(i)), h= h(j), method = method), j=1,size(h)) , &
      i=1,size(psi))
    close(iun)
  enddo



end subroutine

end program 
