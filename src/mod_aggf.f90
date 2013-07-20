! ==============================================================================
!> \file
!! \brief This module contains utitlities for computing
!! Atmospheric Gravity Green Functions
!!
!! In this module there are several subroutines for computing
!! AGGF and standard atmosphere parameters
! ==============================================================================
module mod_aggf
  implicit none

contains

! ==============================================================================
!> Compute first derivative of AGGF with respect to temperature
!! for specific angular distance (psi)
!!
!! optional argument define (-dt;-dt) range
!! See equation 19 in \cite Huang05
!! Same simple method is applied for aggf(gn) if \c aggf optional parameter
!! is set to \c .true. 
!! \warning Please do not use \c aggf=.true. this option was added only
!! for testing some numerical routines
!! \author M. Rajner
!! \date 2013-03-19
! ==============================================================================
!subroutine compute_aggfdt ( psi , aggfdt , delta_ , aggf )
!  real(dp) , intent (in) :: psi
!  real(dp) , intent (in) , optional :: delta_
!  logical , intent (in) , optional :: aggf
!  real(dp) , intent (out) :: aggfdt
!  real(dp) :: deltat , aux , h_

!  deltat = 10. ! Default value
!  if (present(delta_))  deltat = delta_
!  if (present(aggf) .and. aggf ) then
!    h_ = 0.001 ! default if we compute dggfdh using this routine
!    if (present(delta_))  h_ = deltat
!    call compute_aggf ( psi , aux , h = + h_ )
!    aggfdt = aux
!    call compute_aggf (psi, aux, h= -h_)
!    aggfdt = aggfdt - aux
!    aggfdt = aggfdt / ( 2. * h_ )
!  else
!    call compute_aggf (psi, aux, &
!      t_zero = atmosphere%temperature%standard + deltat )
!   aggfdt = aux
!    call compute_aggf (psi, aux,&
!      t_zero = atmosphere%temperature%standard - deltat )
!    aggfdt = aggfdt - aux
!    aggfdt = aggfdt / ( 2. * deltat)
!  endif
!end subroutine

! ==============================================================================
!> Read AGGF
!! \li merriam \cite Merriam92
!! \li huang   \cite Huang05
!! \li rajner  \cite Rajnerdr
!!
!! This is just quick solution for \c example_aggf program
!! in \c grat see the more general routine \c parse_green()
!! TODO Make it obsolete
! ==============================================================================
subroutine read_tabulated_green ( table , author )
  use mod_utilities, only: skip_header , count_records_to_read
  use mod_constants, only: dp
  real(dp), intent (inout),dimension(:,:), allocatable :: table
  character ( len = * ) , intent (in)                  :: author
  integer                                              :: i , j
  integer                                              :: rows , columns , file_unit 
  character (len=255)                                  :: file_name

  if ( author .eq. "huang" ) then
    rows    = 80
    columns = 5
    file_name = '../dat/huang_green.dat'
  else if ( author .eq. "rajner" ) then
    rows    = 85
    columns = 5
    file_name = '../dat/rajner_green.dat'
  else if ( author .eq. "merriam" ) then
    rows      = 85
    columns   = 6
    file_name = '../dat/merriam_green.dat'
  else if ( author .eq. "farrell" ) then
    file_name = '/home/mrajner/src/gotic2/data/grn1.data'
    call count_records_to_read(file_name, rows = rows, columns = columns)
  else
    write ( * , * ) 'cannot find specified tables, using merriam instead'
  endif

  if (allocated (table) ) deallocate (table)
  allocate ( table ( rows , columns ) )

  open (newunit = file_unit , file = file_name , action='read', status='old')

  call skip_header (file_unit)

  do i = 1 , rows
    read (file_unit,*) ( table ( i , j ), j = 1 , columns )
  enddo
  close(file_unit)
end subroutine

! ==============================================================================
!> This subroutine computes the value of atmospheric gravity green functions
!! (AGGF) on the basis of spherical distance (psi)
!! \author Marcin Rajner
!! \date 2013.07.15
!! \warning psi in radians h in meter
! ==============================================================================
function aggf (psi, zmin, zmax, dz, &
    t_zero, h,   first_derivative_h, first_derivative_z, fels_type, &
    standard_pressure_method)

  use mod_constants,  only: dp, pi, earth, gravity, atmosphere, R_air
  use mod_utilities, only: d2r
  use mod_atmosphere !, only: standard_density
  use mod_green, only : green_normalization

  real(dp), intent(in)          :: psi       ! spherical distance from site   [degree]
  real(dp), intent(in),optional :: & 
    zmin ,  & ! minimum height, starting point [m]     (default=0)
    zmax ,  & ! maximum height. eding point    [m]     (default=60000)
    dz ,    & ! integration step               [m]     (default=0.1 -> 10 cm)
    t_zero, & ! temperature at the surface     [K]      (default=288.15=t0)
    h         ! station height                 [m]     (default=0)
  logical, intent(in), optional ::  first_derivative_h , first_derivative_z
  character (len=*) , intent(in), optional  :: fels_type , standard_pressure_method
  real(dp) :: aggf
  real(dp) :: zmin_, zmax_, dz_ , h_
  real(dp) :: dA, z_ , rho , l , z

  zmin_ =     0.
  zmax_ =    60000.
  dz_   =     0.1
  h_    =     0.

  if (present(zmin)) zmin_ = zmin
  if (present(zmax)) zmax_ = zmax
  if (present(  dz))   dz_ = dz
  if (present(   h))    h_ = h

  
  aggf = 0.
  do z = zmin_, zmax_, dz_

    l =  ((earth%radius + z)**2 + (earth%radius + h_)**2 & 
      - 2.*(earth%radius + h_)*(earth%radius+z)*cos(psi))**(0.5)
      

    ! first derivative (respective to station height)
    ! micro Gal height / km
    if ( present ( first_derivative_h) .and. first_derivative_h ) then
!      ! see equation 22, 23 in \cite Huang05
!      !J_aux =  (( earth%radius + z )**2)*(1.-3.*((cos(psir))**2)) -2.*(earth%radius + h_station )**2  &
!      !  + 4.*(earth%radius+h_station)*(earth%radius+z)*cos(psir)
!      ! aggf =  aggf -  rho * (  J_aux  /  r**5  ) * dz 

!      ! direct derivative of equation 20 \cite Huang05      
!      J_aux = (2.* (earth%radius ) - 2 * (earth%radius +z )*cos(psir)) / (2. * r)
!      J_aux =  -r - 3 * J_aux * ((earth%radius+z)*cos(psir) - earth%radius)
!      aggf =  aggf +  rho * (  J_aux  /  r**4  ) * dz 
!    else
!      ! first derivative (respective to column height)
!      ! according to equation 26 in \cite Huang05
!      ! micro Gal / hPa / km
!      if (present (first_derivative_z) .and. first_derivative_z) then
!        if (z.eq.h_min) then
!          aggf = aggf  &
!            + rho*( ((earth%radius + z)*cos(psir) - ( earth%radius + h_station ) ) / ( r**3 ) ) 
!        endif
      else
        ! GN microGal/hPa
        aggf = aggf +  &
!           standard_density (z, t_zero = t_zero , fels_type = fels_type , &
!         standard_pressure_method = standard_pressure_method ) &
          standard_pressure(z,method="simple")/ R_air / standard_temperature(z,fels_type="tropical")  &
          * ((earth%radius +z)*cos(psi) - (earth%radius + h_)) / (l**3.)  * dz_ 
!      endif
    endif
  enddo

  aggf = -aggf /atmosphere%pressure%standard *gravity%constant * green_normalization("m", psi = psi) 
end function

! ==============================================================================
!> Compute AGGF GN for thin layer
!!
!! Simple function added to provide complete module
!! but this should not be used for atmosphere layer
!! See eq p. 491 in \cite Merriam92
!! \author M. Rajner
!! \date 2013-03-19
!! \warning psi in radian
!! \todo explanaition ?? 
! ==============================================================================
function GN_thin_layer (psi)
  use mod_constants, only: dp
  real(dp), intent(in) :: psi
  real(dp) :: GN_thin_layer

  GN_thin_layer = 1.627 * psi / sin ( psi / 2. )
end function


! ==============================================================================
!> \brief Bouger plate computation
!!
! ==============================================================================
real(dp) function bouger (h, R )
  use mod_constants, only: dp, gravity, pi
  real(dp), intent(in), optional :: R !< height of point above the cylinder
  real(dp), intent(in) ::  h 

  if (present( R ) ) then
    bouger = h + R - sqrt(R**2+H**2)
  else
    bouger = h
  endif
  bouger = 2 * pi * gravity%constant * bouger
  return
end function

! ==============================================================================
!> Bouger plate computation
!!
!! see eq. page 288 \cite Warburton77
!! \date 2013-03-18
!! \author M. Rajner 
! ==============================================================================
function simple_def (R)
  use mod_constants, only: dp, earth
  real(dp) :: R ,delta
  real(dp) :: simple_def

  delta = 0.22e-11 * R 
  simple_def = earth%gravity%mean / earth%radius *1000 * &
    delta * ( 2. - 3./2. * earth%density%crust / earth%density%mean &
    -3./4. * earth%density%crust / earth%density%mean * sqrt (2* (1. )) &
    ) * 1000
end function

end module
