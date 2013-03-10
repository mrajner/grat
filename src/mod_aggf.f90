! ==============================================================================
!> \file
!! \brief This module contains utitlities for computing
!! Atmospheric Gravity Green Functions
!!
!! In this module there are several subroutines for computing
!! AGGF and standard atmosphere parameters
! ==============================================================================
module mod_aggf

  use mod_constants
  implicit none
  private

  public:: size_ntimes_denser , read_tabulated_green

contains

! ==============================================================================
!> \brief Compute first derivative of AGGF with respect to temperature
!! for specific angular distance (psi)
!!
!! optional argument define (-dt;-dt) range
!! See equation 19 in \cite Huang05
!! Same simple method is applied for aggf(gn) if \c aggf optional parameter
!! is set to \c .true. 
!! \warning Please do not use \c aggf=.true. this option was added only
!! for testing some numerical routines
! ==============================================================================
subroutine compute_aggfdt ( psi , aggfdt , delta_ , aggf )
  implicit none
  real(dp) , intent (in) :: psi
  real(dp) , intent (in) , optional :: delta_
  logical , intent (in) , optional :: aggf
  real(dp) , intent (out) :: aggfdt
  real(dp) :: deltat , aux , h_

  deltat = 10. !< Default value
  if (present ( delta_) )  deltat = delta_
  if (present ( aggf ) .and. aggf ) then
    h_ = 0.001 ! default if we compute dggfdh using this routine
      if (present ( delta_) )  h_ = deltat
    call compute_aggf ( psi , aux , h = + h_ )
    aggfdt = aux
    call compute_aggf ( psi , aux , h= -h_ )
    aggfdt = aggfdt - aux
    aggfdt = aggfdt / ( 2. * h_ )
  else
    call compute_aggf ( psi , aux , t_zero = T0 + deltat )
    aggfdt = aux
    call compute_aggf ( psi , aux , t_zero = T0 - deltat )
    aggfdt = aggfdt - aux
    aggfdt = aggfdt / ( 2. * deltat)
  endif
end subroutine

! ==============================================================================
!> Wczytuje tablice danych AGGF
!! \li merriam \cite Merriam92
!! \li huang   \cite Huang05
!! \li rajner  \cite Rajnerdr
!!
!! This is just quick solution for \c example_aggf program
!! in \c grat see the more general routine \c parse_green()
! ==============================================================================
subroutine read_tabulated_green ( table , author )
  use mod_utilities, only: skip_header , count_records_to_read
  real(dp), intent (inout),dimension(:,:), allocatable :: table
  character ( len = * ) , intent (in)                  :: author
  integer                                              :: i , j
  integer                                              :: rows , columns , file_unit 
  character (len=255)                                  :: file_name

  if ( author .eq. "huang" ) then
    rows    = 80
    columns = 5
    file_name = '../dat/huang_green.dat'
  elseif ( author .eq. "rajner" ) then
    rows    = 85
    columns = 5
    file_name = '../dat/rajner_green.dat'
  elseif ( author .eq. "merriam" ) then
    rows      = 85
    columns   = 6
    file_name = '../dat/merriam_green.dat'
  elseif ( author .eq. "farrell" ) then
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
! ==============================================================================
subroutine compute_aggf (psi , aggf_val , hmin , hmax , dh , if_normalization, &
                      t_zero , h ,  first_derivative_h , first_derivative_z , fels_type )
  implicit none
  real(dp), intent(in)          :: psi       !< spherical distance from site   [degree]
  real(dp), intent(in),optional :: hmin ,  & !< minimum height, starting point [km]     (default=0)
                               hmax ,  & !< maximum height. eding point    [km]     (default=60)
                               dh ,    & !< integration step               [km]     (default=0.0001 -> 10 cm)
                               t_zero, & !< temperature at the surface     [K]      (default=288.15=t0)
                               h         !< station height                 [km]     (default=0)
  logical, intent(in), optional :: if_normalization , first_derivative_h , first_derivative_z
  character (len=*) , intent(in), optional  :: fels_type 
  real(dp), intent(out)         :: aggf_val
  real(dp)                      :: r , z , psir , dA , dz , rho , h_min , h_max , h_station , J_aux

  h_min = 0.
  h_max = 60.
  dz    = 0.0001 !mrajner 2012-11-08 13:49
  h_station = 0.

  if ( present(hmin) ) h_min    = hmin
  if ( present(hmax) ) h_max    = hmax
  if ( present(  dh) )    dz    = dh
  if ( present(   h) ) h_station = h


  psir = psi * pi / 180.

  dA = 2 * pi * r0**2 * ( 1 - cos (1. *pi/180.) )


  aggf_val=0.
  do z = h_min , h_max , dz

    r = ( ( r0 + z )**2 + (r0 + h_station)**2 & 
      - 2.*(r0 + h_station ) *(r0+z)*cos(psir) )**(0.5)
    call standard_density ( z , rho , t_zero = t_zero , fels_type = fels_type )

    !> first derivative (respective to station height)
    !> micro Gal height / km
    if ( present ( first_derivative_h) .and. first_derivative_h ) then

      !! see equation 22, 23 in \cite Huang05
      !J_aux =  (( r0 + z )**2)*(1.-3.*((cos(psir))**2)) -2.*(r0 + h_station )**2  &
      !  + 4.*(r0+h_station)*(r0+z)*cos(psir)
      ! aggf_val =  aggf_val -  rho * (  J_aux  /  r**5  ) * dz 

      !> direct derivative of equation 20 \cite Huang05      
      J_aux = (2.* (r0 ) - 2 * (r0 +z )*cos(psir)) / (2. * r)
      J_aux =  -r - 3 * J_aux * ((r0+z)*cos(psir) - r0)
      aggf_val =  aggf_val +  rho * (  J_aux  /  r**4  ) * dz 
    else
      !> first derivative (respective to column height)
      !! according to equation 26 in \cite Huang05
      !! micro Gal / hPa / km
      if ( present ( first_derivative_z) .and. first_derivative_z ) then
        if (z.eq.h_min) then
            aggf_val = aggf_val  &
              + rho*( ((r0 + z)*cos(psir) - ( r0 + h_station ) ) / ( r**3 ) ) 
        endif
      else
        !> aggf GN
        !! micro Gal / hPa
        aggf_val = aggf_val  &
         + rho * ( ( (r0 + z ) * cos ( psir ) - ( r0 + h_station ) ) / ( r**3 ) ) * dz
      endif
    endif
  enddo

  aggf_val = -G * dA * aggf_val * 1e8 * 1000 

  !> if you put the optional parameter \c if_normalization=.false.
  !! this block will be skipped
  !! by default the normalization is applied according to \cite Merriam92
  if ( (.not.present(if_normalization)) .or. (if_normalization)) then
    aggf_val= psir * aggf_val * 1e5  / p0
  endif

end subroutine 

! ==============================================================================
!> Compute air density for given altitude for standard atmosphere
!!
!! using formulae 12 in \cite Huang05
! ==============================================================================
subroutine standard_density ( height , rho , t_zero ,fels_type )

  implicit none
  real(dp) , intent(in)  ::  height !< height [km]
  real(dp) , intent(in), optional  :: t_zero !< if this parameter is given 
  character(len = 22) , optional :: fels_type
  !! surface temperature is set to this value, 
  !! otherwise the T0 for standard atmosphere is used
  real(dp) , intent(out) :: rho 
  real(dp)  :: p ,t

  call standard_pressure    (height , p , t_zero = t_zero, fels_type=fels_type)
  call standard_temperature (height , t , t_zero = t_zero, fels_type=fels_type)

  ! pressure in hPa --> Pa
  rho= 100 * p / ( R_air * t )
end subroutine

! =============================================================================
!> \brief Computes pressure [hPa] for specific height
!!
!! See \cite US1976 or  \cite Huang05 for details.
!! Uses formulae 5 from \cite Huang05.
!! Simplified method if optional argument if_simplificated = .true.
! =============================================================================
subroutine standard_pressure (height, pressure , &
          p_zero , t_zero , h_zero,  if_simplificated ,fels_type , inverted)
  implicit none
  real(dp) , intent(in)            :: height
  real(dp) , intent(in) , optional :: t_zero , p_zero , h_zero
  character(len = 22) , optional :: fels_type
  logical         , intent(in) , optional :: if_simplificated
  logical         , intent(in) , optional :: inverted
  real(dp), intent(out) :: pressure
  real(dp) ::  lambda , sfc_height , sfc_temperature , sfc_gravity , alpha , sfc_pressure

  sfc_temperature = T0
  sfc_pressure = p0
  sfc_height = 0.
  sfc_gravity = g0

  if (present(h_zero)) then
    sfc_height = h_zero
    call standard_temperature (sfc_height , sfc_temperature )
    call standard_temperature (sfc_height , sfc_temperature )
    call standard_gravity (sfc_height , sfc_gravity )
  endif

  if (present(p_zero)) sfc_pressure = p_zero
  if (present(t_zero)) sfc_temperature = t_zero

  lambda = R_air * sfc_temperature / sfc_gravity

  if (present (if_simplificated) .and. if_simplificated ) then
    ! use simplified formulae 
    alpha = -6.5 
    pressure = sfc_pressure  &
      * ( 1 + alpha / sfc_temperature * (height-sfc_height)) &
      ** ( -sfc_gravity / (R_air * alpha / 1000. ) )
  else
    ! use precise formulae
    pressure = sfc_pressure * exp ( -1000. * (height -sfc_height) / lambda ) 
  endif 
  if (present(inverted).and.inverted) then
    pressure = sfc_pressure  / ( exp ( -1000. * (height-sfc_height) / lambda ) )
  endif


  !todo incorporate this

!  Zdunkowski and Bott
!  p(z) = p0 (T0-gamm z )/T0


end subroutine

! =============================================================================
! > This will transfer pressure beetween different height using barometric
! formulae
! =============================================================================
!> \warning OBSOLETE ROUTINE -- use \c standard_pressure() instead with optional args
subroutine transfer_pressure (height1 , height2 , pressure1 , pressure2 , &
  temperature , polish_meteo )
  real (dp) , intent (in) :: height1 , height2 , pressure1
  real (dp) , intent (in), optional :: temperature
  real (dp) :: sfc_temp , sfc_pres
  logical , intent (in), optional :: polish_meteo
  real(dp) , intent(out) :: pressure2
  
  sfc_temp = t0

  ! formulae used to reduce press to sfc in polish meteo service
  if (present(polish_meteo) .and. polish_meteo) then
    sfc_pres = exp (log (pressure1) + 2.30259 * height1*1000. &
      /(18400.*(1+0.00366*((temperature-273.15) + 0.0025*height1*1000.)))  )
  else
  ! different approach
    if(present(temperature) ) then
      call surface_temperature( height1 , temperature , sfc_temp )
    endif
    call standard_pressure (height1 , sfc_pres , t_zero=sfc_temp , &
      inverted=.true. , p_zero = pressure1 )
  endif

  ! move from sfc to height2
  call standard_pressure (height2 , pressure2 , t_zero=sfc_temp , &
    p_zero = sfc_pres )
end subroutine

! =============================================================================
!> \brief Compute gravity acceleration of the Earth
!! for the specific height using formula
!! 
!! see \cite US1976 
! =============================================================================
subroutine standard_gravity ( height , g )
  implicit none
  real(dp), intent(in)  :: height
  real(dp), intent(out) :: g

  g= g0 * ( r0 / ( r0 + height ) )**2
end subroutine


! =============================================================================
!> \brief Compute geometric height from geopotential heights
! =============================================================================
real(dp) function geop2geom (geopotential_height)
  real (dp) :: geopotential_height

  geop2geom = geopotential_height * (R0 / ( R0 + geopotential_height ) )
end function


! =============================================================================
!> Iterative computation of surface temp. from given height using bisection
!! method
! =============================================================================
subroutine surface_temperature (height , temperature1 , &
  temperature2, fels_type , tolerance)
  real(dp) , intent(in)  :: height , temperature1
  real(dp) , intent(out) :: temperature2  
  real(dp) :: temp(3) , temp_ (3) , tolerance_ = 0.1
  character (len=*) , intent(in), optional  :: fels_type 
  real(dp) , intent(in), optional  :: tolerance
  integer :: i 

  if (present(tolerance)) tolerance_ = tolerance

  ! searching limits
  temp(1)=t0-150
  temp(3)=t0+ 50

  do 
    temp(2)= ( temp(1) + temp(3) ) /2.
  
    do i = 1,3
      call standard_temperature (height , temp_(i) , t_zero=temp(i) , fels_type = fels_type ) 
    enddo

    if (abs(temperature1 - temp_(2) ) .lt. tolerance_ ) then
      temperature2 = temp(2)
      return
    endif

    if ( (temperature1 - temp_(1) ) * (temperature1 - temp_(2) ) .lt.0 ) then
      temp(3) = temp (2)
    elseif ( (temperature1 - temp_(3) ) * (temperature1 - temp_(2) ) .lt.0 ) then
      temp(1) = temp (2)
    else
      stop "surface_temp"
    endif
  enddo
end subroutine
! =============================================================================
!> \brief Compute standard temperature [K] for specific height [km]
!!
!! if t_zero is specified use this as surface temperature
!! otherwise use T0.
!! A set of predifined temperature profiles ca be set using
!! optional argument \argument fels_type 
!! \cite Fels86
! ==============================================================================
subroutine standard_temperature ( height , temperature , t_zero , fels_type )
  real(dp) , intent(in)  :: height
  real(dp) , intent(out) :: temperature
  real(dp) , intent(in), optional  :: t_zero
  character (len=*) , intent(in), optional  :: fels_type 
    !< \li US standard atmosphere (default)
    !! \li tropical 
    !! \li subtropical_summer
    !! \li subtropical_winter
    !! \li subarctic_summer
    !! \li subarctic_winter
  real(dp) :: aux , cn , t
  integer :: i,indeks
  real , dimension (10) :: z,c,d

  !< Read into memory the parameters of temparature height profiles
  !! for standard atmosphere 
  !! From \cite Fels86
  z = (/11.0 , 20.1 , 32.1 , 47.4 , 51.4 , 71.7 , 85.7, 100.0, 200.0, 300.0/)
  c = (/-6.5,   0.0,   1.0,   2.75,  0.0,  -2.75, -1.97,  0.0,   0.0,   0.0/)
  d = (/ 0.3,   1.0,   1.0,   1.0,   1.0,   1.0,   1.0,   1.0,   1.0,   1.0/)
  t = T0

  if ( present (fels_type)) then
    if (fels_type .eq. "US1976" ) then
    elseif (fels_type .eq. "tropical" ) then
      z=(/ 2.0  , 3.0, 16.5 , 21.5 , 45.0 , 51.0, 70.0 , 100.0 , 200.0 , 300.0 /)
      c=(/-6.0 , -4.0, -6.7  , 4.0  , 2.2  , 1.0, -2.8  , -0.27  , 0.0   , 0.0 /)
      d=(/ 0.5  , 0.5 , 0.3  , 0.5  , 1.0  , 1.0 , 1.0   , 1.0   , 1.0   , 1.0 /)
      t=300.0
    elseif (fels_type .eq. "subtropical_summer" ) then
      z = (/ 1.5  , 6.5  , 13.0 , 18.0 , 26.0 , 36.0  , 48.0 ,  50.0 ,  70.0 , 100.0  /)
      c = (/-4.0 , -6.0  , -6.5  , 0.0  , 1.2  , 2.2  ,  2.5  ,  0.0   ,-3.0   ,-0.025/)
      d = (/ 0.5  , 1.0   , 0.5  , 0.5  , 1.0  , 1.0   , 2.5   , 0.5   , 1.0   , 1.0  /)
      t = 294.0
    elseif (fels_type .eq. "subtropical_winter" ) then
      z = (/ 3.0  ,10.0  , 19.0 , 25.0 , 32.0 , 44.5  , 50.0 ,  71.0 ,  98.0 , 200.0 /)
      c = (/-3.5 , -6.0  , -0.5  , 0.0  , 0.4  , 3.2  ,  1.6  , -1.8   , 0.7   , 0.0 /)
      d = (/ 0.5  , 0.5   , 1.0  , 1.0  , 1.0  , 1.0   , 1.0   , 1.0   , 1.0   , 1.0 /)
      t = 272.2
    elseif (fels_type .eq. "subarctic_summer" ) then
      z = (/  4.7 , 10.0 , 23.0 , 31.8 , 44.0 , 50.2  , 69.2 , 100.0 , 200.0 , 300.0 /)
      c = (/ -5.3 , -7.0 ,  0.0 ,  1.4 ,  3.0 , 0.7  , -3.3  , -0.2   , 0.0 ,   0.0 /)
      d = (/  0.5 ,  0.3 ,  1.0 ,  1.0 ,  2.0 , 1.0   , 1.5   , 1.0   , 1.0 ,   1.0 /)
      t = 287.0
    elseif (fels_type .eq. "subarctic_winter" ) then
      z = (/  1.0 ,  3.2 ,  8.5 , 15.5 , 25.0 , 30.0 , 35.0 , 50.0 , 70.0 , 100.0 /)
      c = (/  3.0 , -3.2 , -6.8 ,  0.0 , -0.6 ,  1.0 ,  1.2 ,  2.5 , -0.7 ,  -1.2 /)
      d = (/  0.4 ,  1.5 ,  0.3 ,  0.5 ,  1.0 ,  1.0 ,  1.0 ,  1.0 ,  1.0 ,   1.0 /)
      t = 257.1
    else
      print * , "unknown fels_type argument: &
        using US standard atmosphere 1976 instead"
    endif
  endif

  if (present (t_zero) ) then
    t=t_zero
  endif

  do i=1,10
    if (height.le.z(i)) then
      indeks=i
      exit
    endif
  enddo

  aux = 0.
  do i = 1 , indeks
    if (i.eq.indeks) then
      cn = 0.
    else
      cn = c(i+1)
    endif
      aux = aux + d(i) * ( cn - c(i) )  * log ( cosh ( (height - z(i)) / d(i) ) / cosh (z(i)/d(i)) ) 
  enddo
  temperature = t + c(1) * height/2. + aux/2.
end subroutine

! ==============================================================================
!> \brief Compute AGGF GN for thin layer
!!
!! Simple function added to provide complete module
!! but this should not be used for atmosphere layer
!! See eq p. 491 in \cite Merriam92
! ==============================================================================
real function GN_thin_layer (psi)
  implicit none
  real(dp) , intent(in) :: psi
  real(dp) :: psir

  psir = psi * pi / 180.
  GN_thin_layer = 1.627 * psir / sin ( psir / 2. )
end function


! ==============================================================================
!> \brief returns numbers of arguments for n times denser size
!!
!! i.e. * * * *  -->  * . . * . . * . . * (3 times denser)
! ==============================================================================
integer function size_ntimes_denser (size_original, ndenser)
  integer, intent(in) :: size_original , ndenser
  size_ntimes_denser= (size_original - 1 ) * (ndenser +1 ) + 1
end function

! ==============================================================================
!> \brief Bouger plate computation
!!
! ==============================================================================
real(dp) function bouger ( R_opt )
  real(dp), optional :: R_opt !< height of point above the cylinder
  real(dp) :: aux
  real(dp) :: R
  real(dp) ::  h = 8.84 ! scale height of standard atmosphere

  aux = 1

  if (present( R_opt ) ) then
    R = R_opt
    aux = h  + R  - sqrt(  R**2  + (h/2. ) ** 2 )
    bouger = 2 * pi * G  * aux 
  else
    aux = h
    bouger = 2 * pi * G * aux
    return
  endif
end function
! ==============================================================================
!> \brief Bouger plate computation
!! see eq. page 288 \cite Warburton77
! ==============================================================================
real(dp) function simple_def (R)
  real(dp) :: R ,delta

  delta = 0.22e-11 * R 
  
  simple_def = g0 / R0 * delta * ( 2. - 3./2. * rho_crust / rho_earth &
    -3./4. * rho_crust / rho_earth * sqrt (2* (1. )) ) * 1000
end function

!polish_meteo

end module
