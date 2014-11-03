module mod_montecarlo
  use mod_constants

  implicit none
  character(200) :: monte_carlo_settings = ""

  logical :: monte_carlo = .false.

  real(dp) :: random_value

  integer :: monte_carlo_samples = 10


  real(dp) :: &
    admitance_uncerteinty = 0.0_dp, &
    sp_uncerteinty        = 0.000_dp 


contains

subroutine get_monte_carlo_settings(file)
  use mod_utilities, only: file_exists
  use mod_printing, only: print_warning
  use iso_fortran_env

  character(*), intent(in) :: file
  integer :: i, io_stat
  character (30) :: key
  real (dp) :: value

  if (.not. file_exists(file)) then
    call print_warning(trim(file)// " not found!")
    return
  endif

  open(newunit=i, file=file, action="read")
  do 
    read(i,*, iostat=io_stat) key , value
    if(io_stat== iostat_end) exit
    select case(key)
    case("SP")
      sp_uncerteinty = value
    case("n")
      monte_carlo_samples = value
    case("A")
      admitance_uncerteinty = value
    case default
      stop "YYYYYYYYYYYYY"
    end select

    print *, trim(key), "::" ,value


  enddo



  close (i)
  ! stop "S"


end subroutine
end module
