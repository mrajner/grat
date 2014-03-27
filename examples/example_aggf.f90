! ============================================================================
!! This program shows some example of using AGGF module
!! 
!! \author Marcin Rajner
!! \date 20121108
! ============================================================================
program example_aggf
  use mod_atmosphere
  use mod_constants, only: dp
  use mod_utilities
  use mod_printing, only: log
  implicit none
  real(dp) :: cpu(2)
  integer :: execution_time(3)


  call cpu_time(cpu(1))
  call system_clock(execution_time(1))

  call standard1976 ('/home/mrajner/src/grat/examples/standard1976.dat')
  call compare_fels_profiles ('/home/mrajner/src/grat/examples/compare_fels_profiles.dat')
  call simple_atmospheric_model ("/home/mrajner/dr/rysunki/simple_approach.dat")
  call green_newtonian_compute( &
    ["green_newtonian_olsson.dat","green_newtonian_spotl.dat","green_newtonian.dat"])
  call admit_niebauer("/home/mrajner/src/grat/examples/admit_niebauer.dat")
  call aggf_thin_layer ("/home/mrajner/src/grat/examples/aggf_thin_layer.dat")
  call compute_tabulated_green_functions ('/home/mrajner/src/grat/dat/rajner_green_full.dat'  , method="full"       , predefined=.false.)
  call compute_tabulated_green_functions ('/home/mrajner/src/grat/dat/rajner_green_rough.dat' , predefined=.false., rough=.true.)
  call compute_tabulated_green_functions ('/home/mrajner/src/grat/dat/rajner_green_simple.dat', method="simple"     , predefined=.false.)
  call compute_tabulated_green_functions ('/home/mrajner/src/grat/dat/rajner_green.dat'       , predefined=.false. )
  call aggf_resp_fels_profiles ('/home/mrajner/src/grat/examples/aggf_resp_fels_profiles.dat')
  call mass_vs_height('/home/mrajner/src/grat/examples/mass_vs_height.dat')
  call aggf_resp_hmax ('/home/mrajner/src/grat/examples/aggf_resp_zmax.dat')
  call aggf_resp_dz ('/home/mrajner/src/grat/examples/aggf_resp_dz.dat')
  call aggf_resp_t ('/home/mrajner/src/grat/examples/aggf_resp_t.dat')
  call aggf_resp_h ('/home/mrajner/src/grat/examples/aggf_resp_h.dat')

  call cpu_time(cpu(2))
  call system_clock(execution_time(2),execution_time(3))
  write(*,                                                                              &
    '("Execution time:",1x,f10.4," seconds (proc time:",1x,f6.2,1x,"s | %", f6.2,")")') &
    real(execution_time(2)-execution_time(1))/(execution_time(3)),                      &
    cpu(2)-cpu(1),                                                                      &
    100.*(cpu(2)-cpu(1))/ (real(execution_time(2)-execution_time(1))/(execution_time(3)) )

contains 
! =============================================================================
!> Mass of atmosphere respect to height
! =============================================================================
subroutine mass_vs_height (filename)
  use, intrinsic:: iso_fortran_env
  use mod_utilities, only: file_exists
  use mod_constants, only : dp, pi, earth, R_air
  use mod_atmosphere
  character(*), intent (in), optional:: filename
  real(dp) :: max_height,dh, percent
  real(dp), allocatable, dimension(:):: mass, height
  integer::i,j,file_unit

  if (present (filename)) then
    if (file_exists(filename)) return
    open ( &
      newunit = file_unit, & 
      file    = filename,  & 
      action  = 'write'  & 
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
    mass  (i) = standard_pressure ( &
      height(i), &
      method="standard", &
      use_standard_temperature=.true., &
      nan_as_zero=.true.) &
      / (R_air * standard_temperature(height(i)))
  enddo

  do i =0,50000,1000
    percent=0
    do j = 1, size(height)
      if (height(j).le.real(i,dp)) percent=percent+mass(j)
    enddo
    percent = percent / sum(mass)  * 100.
    write(file_unit, '(i6,2f19.9,es10.3)' ), i, percent, &
      100-(earth%radius+dble(1))**2 &
      * standard_pressure(real(i,dp),method="standard", use_standard_temperature=.true.) &
      / standard_gravity(real(i,dp))&
      /earth%radius**2/standard_pressure(real(0,dp),method="standard") * standard_gravity(real(0,dp))*100
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
  character(*), intent(in), optional:: filename
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

  do R = 0., 25*8
    write (file_unit, *) &
      R, &
      -100*bouger(h=h,R=R)/(earth%gravity%mean*h) * 1e8, & !conversion to microGal
      -simple_def(R) * 1e8
  enddo
end subroutine

! ============================================================================
!> Compute AGGF and derivatives
!!
!! \author M. Rajner
!! \date 2013-03-18
! ============================================================================
subroutine compute_tabulated_green_functions ( &
    filename, method, dz, &
    predefined,fels_type, rough)
  use mod_constants, only: dp
  use mod_aggf,     only: aggf, aggfd
  use mod_green,     only: green
  use mod_utilities, only: d2r, file_exists
  use mod_atmosphere

  integer :: i, file_unit
  character(*), intent(in) :: filename
  real(dp), optional :: dz
  character(*), optional :: fels_type
  character(*), optional :: method
  logical, optional, intent(in) :: predefined, rough

  if (file_exists(filename)) then
    return
  else
    print '(a,a)', "compute_tabulated_green_functions --> ", trim(filename)
  endif

  call get_green_distances

  open (                 & 
    newunit = file_unit, & 
    file    = filename,  & 
    action  = 'write'    & 
    )

  !print header
  write (file_unit,*) '# This is set of AGGF computed using module ', & 
    'aggf from grat software'
  write (file_unit,*) '# Normalization according to Merriam92'
  write (file_unit,*) '# Marcin Rajner'
  write (file_unit,*) '# For detail see www.geo.republika.pl'
  write (file_unit,'(10(a23))')                       & 
    '#psi[deg]',                                       & 
    'GN[microGal/hPa]'     , 'GN/dT[microGal/hPa/K]' , & 
    'GN/dh[microGal/hPa/m]', 'GN/dz[microGal/hPa/m]'

  do i= 1, size(green(1)%distance)
    write(file_unit, '(13f15.6)'), &
      green(1)%distance(i), &
      aggf (d2r(green(1)%distance(i)), method=method, dz=dz                           , predefined=predefined, fels_type=fels_type, rough=rough), &
      aggfd(d2r(green(1)%distance(i)), method=method, dz=dz, aggfdt=.true.            , predefined=predefined, fels_type=fels_type, rough=rough), &
      aggf (d2r(green(1)%distance(i)), method=method, dz=dz, first_derivative_h=.true., predefined=predefined, fels_type=fels_type, rough=rough), &
      aggf (d2r(green(1)%distance(i)), method=method, dz=dz, first_derivative_z=.true., predefined=predefined, fels_type=fels_type, rough=rough)
  enddo
  close(file_unit)
end subroutine

! ============================================================================
!> Compare different vertical temperature profiles impact on AGGF
! ============================================================================
subroutine aggf_resp_fels_profiles (filename)
  use mod_constants, only: dp
  use mod_aggf,  only: aggf
  use mod_green, only: green
  character (len=255), dimension (6) :: fels_types
  integer :: i, j, file_unit
  character(*), intent(in), optional :: filename

  if (present (filename)) then
    if (file_exists(filename)) return
    open ( newunit = file_unit, &
      file =filename, &
      action  = 'write' )
  else
    file_unit = output_unit
  endif
  print *, "aggf_resp_fels_profiles -->", filename

  ! Get the spherical distances from Merriam92
  call get_green_distances()

  ! ! All possible optional arguments for standard_temperature
  fels_types = (/ &
    "US1976"             , "tropical",   &
    "subtropical_summer" , "subtropical_winter" , &
    "subarctic_summer"   , "subarctic_winter"     &
    /)
  ! print header
  write (file_unit, '(100(a20))') &
    'psi', (trim(fels_types (i)), i = 1, size(fels_types))

  ! print results
  do i = 1, size(green(1)%distance)
    write(file_unit, '(<size(fels_types)+1>f20.5)'), &
      green(1)%distance(i), &
      (aggf( &
      d2r(green(1)%distance(i)), &
      method="standard", &
      fels_type=fels_types(j)), j=1,size(fels_types) &
      )
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
subroutine compare_fels_profiles (filename)
  use iso_fortran_env
  use mod_utilities, only: file_exists
  use mod_constants, only: dp
  use mod_atmosphere, only : standard_temperature
  character (len=255), dimension (6) :: fels_types
  real (dp) :: height
  integer :: i, file_unit, i_height
  character(*), intent (in),optional:: filename

  ! All possible optional arguments for standard_temperature
  fels_types = (/ "US1976"             , "tropical",   &
    "subtropical_summer" , "subtropical_winter" , &
    "subarctic_summer"   , "subarctic_winter"    /)

  if (present (filename)) then
    if (file_exists(filename)) return
    open ( newunit = file_unit, &
      file =filename, &
      action  = 'write' )
  else
    file_unit = output_unit
  endif

  print *, "compare_fels_profiles --->", filename

  ! Print header
  write (file_unit, '(100(a20))' ) &
    'height', ( trim ( fels_types (i) ), i = 1, size (fels_types) )

  ! Print results
  do i_height = 0, 70, 1
    height=dble(i_height)
    write ( file_unit, '(f20.3$)'), height
    do i = 1, size (fels_types)
      write (file_unit, '(f20.3$)'),  standard_temperature (height*1000, fels_type=fels_types(i))
    enddo
    write ( file_unit, *)
  enddo
  close(file_unit)
end subroutine

! ============================================================================
!> Computes AGGF for different site height (h)
! ============================================================================
subroutine aggf_resp_h (filename)
  use mod_green, only: green
  use mod_aggf, only: aggf
  real(dp) :: heights(6)
  character(*), intent(in), optional :: filename
  integer :: file_unit, i, ii, j
  real(dp) :: aux

  if (present (filename)) then
    if (file_exists(filename)) return
    open ( newunit = file_unit, &
      file =filename, &
      action  = 'write' )
  else
    file_unit = output_unit
  endif
  print *, "aggf_resp_h --->", filename

  call get_green_distances()

  heights=[0.,1.,10.,100.,1000.,10000.]


  write (file_unit, "(a12,6(x,'h',f0.0))") "distance", heights(1:6)
  do i =1, size (green(1)%distance)
    ! denser sampling 
    do ii = 0,8 
      aux  = green(1)%distance(i) + ii * (green(1)%distance(i+1) - green(1)%distance(i)) / 9. 
      if (aux.gt.0.2 ) exit
      write (file_unit, '(F12.6$)'), aux
      do j =  1, size(heights)
        write (file_unit,'(f12.4,1x,$)') aggf(d2r(aux), method="standard", h=heights(j))
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
subroutine aggf_resp_t (filename)
  use mod_green, only: green
  ! use mod_constants, only : dp, atmosphere
  use mod_aggf, only : aggf
  real(dp), dimension(:,:), allocatable :: results
  integer :: i, j
  character(*), intent(in), optional :: filename
  integer :: file_unit
  real(dp) :: temperatures(3)

  if (present (filename)) then
    if (file_exists(filename)) return
    open ( newunit = file_unit, &
      file =filename, &
      action  = 'write' )
  else
    file_unit = output_unit
  endif
  call get_green_distances()

  allocate(results(size(green(1)%distance), 3))

  temperatures=[0., 15., -45]

  write(file_unit, '(4a12)') "distance","T0+0", "T0+15", "T0-45"
  do i = 1, size(green(1)%distance)
    write(file_unit, '(f12.5$)') green(1)%distance(i)
    do j=1, size(temperatures)
      write(file_unit, '(f12.5$)') &
        aggf(d2r(green(1)%distance(i)), method="standard", t_zero=temperatures(j))
    enddo
    write(file_unit, *)
  enddo
  close (file_unit)
end subroutine

! ============================================================================
!> \brief This computes AGGF for different height integration step 
! ============================================================================
subroutine aggf_resp_dz (filename)
  use mod_green, only: green
  use mod_aggf, only: aggf
  use mod_utilities, only: logspace
  real(dp), dimension(:,:), allocatable :: results
  real(dp), dimension(:), allocatable :: dzs

  integer :: file_unit, i, j
  integer, parameter :: n=10
  character(*), intent (in), optional :: filename

  if (present (filename)) then
    if (file_exists(filename)) return
    open ( newunit = file_unit, &
      file =filename, &
      action  = 'write' )
  else
    file_unit = output_unit
  endif

  call get_green_distances()
  ! green(1)%distance(1:n)=logspace(green(1)%distance(1), green(1)%distance(10),n) 

  allocate(dzs(5))
  dzs=(/ 0.01, 0.1, 1., 10., 100./)

  allocate (results(size(green(1)%distance(1:n)),size(dzs)))
  results = 0.

  do i = 1, size (results (:,1))
    do j=1,size(dzs)
      results(i,j) = i+j
      results(i,j) =               &
        aggf(                      &
        d2r(green(1)%distance(i)), &
        method = "standard",       &
        dz     = 1._dp* dzs(j)            &
        )

      print *, results(i,j)
    enddo
    ! compute relative errors from column 2 for all dz with respect to column 1
    results(i,2:) = abs((results(i,2:) - results (i,1)) / results (i,1)*100.  )
  enddo

  write(file_unit, '(a16,<size(dzs)>f16.5)') "psi_dz", dzs
  write(file_unit, '(f16.7,<size(dzs)>e16.5)') &
    (green(1)%distance(i), results(i,:), i=1,size(results(:,1)))
  close(file_unit)
end subroutine

! ============================================================================
!> \brief This computes standard atmosphere parameters
!!
!! It computes temperature, gravity, pressure, pressure (simplified formula)
!! density for given height
! ============================================================================
subroutine standard1976(filename)
  use, intrinsic :: iso_fortran_env
  use mod_utilities, only: file_exists
  use mod_constants, only : dp, R_air
  use mod_atmosphere, only: &
    standard_temperature, standard_pressure, &
    standard_gravity,     standard_density
  integer :: file_unit
  character(*), intent (in), optional:: filename
  real(dp) :: height

  if (present (filename)) then
    if (file_exists(filename)) return
    open ( newunit = file_unit, &
      file =filename, &
      action  = 'write' )
  else
    file_unit = output_unit
  endif

  print *, "standard atmosphere --->", filename
  ! print header
  write ( file_unit, '(6(a15))' ) &
    'height', 'T', 'g', 'p', 'rho'
  do height=0.,68000., 1000
    ! print results to file
    write( file_unit,'(5f15.5, e12.3)'),                                     & 
      height/1000.,                                                          & 
      standard_temperature(height),                                          & 
      standard_gravity(height),                                              & 
      standard_pressure(height, method="standard", nan_as_zero=.true.)/100., & 
      standard_pressure(height, method="standard", nan_as_zero=.true.)       & 
      /(R_air*standard_temperature(height))
  enddo
  close( file_unit )
end subroutine

! ============================================================================
!> \brief This computes relative values of AGGF for different atmosphere
!! height integration
! ============================================================================
subroutine aggf_resp_hmax (filename)
  use mod_utilities, only: file_exists, logspace, d2r
  ! use mod_constants, only : dp
  use mod_aggf, only : aggf
  real (dp), dimension (2) :: psi
  real (dp), dimension (:), allocatable :: heights 
  real (dp), dimension (:,:), allocatable :: results
  integer :: file_unit, n, i, j
  character(*), intent (in), optional:: filename

  if (present (filename)) then
    if (file_exists(filename)) return
    open ( newunit = file_unit, &
      file =filename, &
      action  = 'write' )
  else
    file_unit = output_unit
  endif

  print *, "standard atmosphere ---> ", filename
  psi=(/0.0001,10/)

  n = 90
  allocate(heights(n))

  heights= logspace(real(1e-1,dp), real(60000,dp),n) 

  allocate (results(size(heights), size(psi))) 
  results=0

  do j=1, size(heights)
    do i=1, size(psi)
       results(j,i) =aggf(d2r(psi(i)),method="standard", zmax=heights(j))
    enddo
  enddo

  do i=1, size(psi)
     results(:,i) = - ((results(:,i)-results(size(heights),i))/results(size(heights),i)) * 100. ! in %
  enddo

  write(file_unit, '(a14,SP,100f14.5)' ),"#heght\psi", (psi(j), j= 1,size(psi))
  do i=1, size (results (:,1))
    write(file_unit, '(100f14.4)' ) heights(i)/1000, (results(i,j), j = 1, size(psi) )
  enddo
  close(file_unit)
end subroutine

! ============================================================================
! ============================================================================
subroutine aggf_thin_layer (filename)
  use, intrinsic:: iso_fortran_env
  use mod_constants, only: dp, pi
  use mod_aggf, only: GN_thin_layer
  use mod_utilities, only: d2r, file_exists
  use mod_green

  integer :: file_unit, i
  character(*), intent (in), optional:: filename

  if (file_exists(filename)) return

  call get_green_distances()

  write(*,*), "aggf_thin_layer ---> ",filename
  if (present (filename)) then
    open (newunit = file_unit, &
      file =filename, &
      action  = 'write' )
  else
    file_unit = output_unit
  endif
  do i = 1, size (green(1)%distance)
    write(file_unit,*) green(1)%distance(i), green(1)%data(i), &
      GN_thin_layer (d2r(green(1)%distance(i)))
  enddo
end subroutine

! =============================================================================
! =============================================================================
subroutine admit_niebauer(filename)
  use mod_constants
  use mod_utilities
  real(dp) :: a
  real(dp) :: theta
  real(dp) :: b, f
  character(*), intent(in) :: filename
  integer::iun

  if (file_exists(filename)) return
  print *, "admit_niebauer ---> ", filename

  open (newunit=iun, file=filename, action = 'write')

  f=earth%radius/9500
  do theta=0.5, 180, 0.01
    b= 2*f*sin(d2r(theta/2))
    a= 2*pi * gravity%constant / earth%gravity%mean* &
      (1 - b/(2*f) -1/b + 2/f)
    write(iun, *), theta, a *1e10
  enddo
end subroutine

! =============================================================================
!> compute green newtonian function
! =============================================================================
subroutine green_newtonian_compute(filenames)
  use mod_utilities, only: file_exists
  use mod_green
  use mod_utilities, only: logspace, d2r
  integer:: iun, n, i, j, k
  real (dp), allocatable, dimension(:) :: psi, h
  character(12), allocatable, dimension(:) :: column_name
  character(*),  optional :: filenames(3)
  character(20) :: method
  character(40) :: prefix

  prefix="/home/mrajner/src/grat/examples/"

  iun = 6

  n = 9 * 50
  allocate(psi(n))
  psi = logspace(real(1e-6,dp), real(180,dp),n) 

  allocate(h(11))
  h = [0., 1., 10., 100., 1000., 10000., -1., -10., -100., -1000., -10000.]

  allocate(column_name(size(h)))
  write(column_name, '(f0.0)' ) (h(i),i=1,11)

  do k =1,3
    if (file_exists(trim(prefix)//trim(filenames(k)))) cycle
    print *, "green_newtonian_compute ---> ", trim(prefix)//trim(filenames(k))
    open (newunit=iun, file=trim(prefix)//filenames(k), action = 'write')

    method = filenames(k)(17:index(filenames(k),".")-1)
    write(iun, '(a12,<size(h)>a12)') "#psi", ( "h"//trim(column_name(i)), i = 1, 11)
    write(iun, '(<size(h)+1>en12.2)'), (psi(i), &
      (green_newtonian(d2r(psi(i)), h= h(j), method = method), j=1,size(h)), &
      i=1,size(psi))
    close(iun)
  enddo
end subroutine

! =============================================================================
! =============================================================================
subroutine get_green_distances()
  use mod_green
  if (allocated(green)) deallocate(green)
  allocate (green(1))
  green(1)%name="merriam"
  green(1)%column=[1, 2]
  green(1)%dataname="GN"
  call read_green(green(1),print=.false.)
end subroutine
end program 
