program a
  use mod_atmosphere
  integer:: dp=8

  
  print * , standard_pressure (height=300._8, method="full", h_zero=55._8, temperature=300._8, use_standard_temperature=.false.)
  print * , standard_pressure (height=300._8, method="full", h_zero=55._8, temperature=300._8, use_standard_temperature=.true.)

  call compute_tabulated_green_functions ('rajner_green.dat'       , predefined=.false. , method="standard" )

contains

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

    print '(a,a)', "compute_tabulated_green_functions --> ", trim(filename)

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

  do i= 1, 4 ! size(green(1)%distance)
    write(file_unit, '(13f15.6)'), &
      green(1)%distance(i), &
      aggf (d2r(green(1)%distance(i)), method=method, dz=dz                           , predefined=predefined, fels_type=fels_type, rough=rough), &
      aggfd(d2r(green(1)%distance(i)), method=method, dz=dz, aggfdt=.true.            , predefined=predefined, fels_type=fels_type, rough=rough), &
      aggf (d2r(green(1)%distance(i)), method=method, dz=dz, first_derivative_h=.true., predefined=predefined, fels_type=fels_type, rough=rough), &
      aggf (d2r(green(1)%distance(i)), method=method, dz=dz, first_derivative_z=.true., predefined=predefined, fels_type=fels_type, rough=rough)
  enddo
  close(file_unit)
end subroutine

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
