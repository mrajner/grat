module mod_montecarlo
  use mod_constants
  use lib_random

  implicit none
  character(200) :: monte_carlo_settings = ""

  logical :: &
    monte_carlo = .false., &
    monte_carlo_systematic = .false., &
    monte_carlo_keep_cell = .false.


  real(dp) :: random_value

  integer :: monte_carlo_samples = 10


  real(dp) :: &
    admitance_uncerteinty = 0.0_dp, &
    sp_uncerteinty        = 0.000_dp , &
    t_uncerteinty         = 0.000_dp , &
    h_uncerteinty         = 0.000_dp , &
    rsp_uncerteinty       = 0.000_dp 

  real(dp), allocatable, dimension(:,:) :: monte_carlo_results
  real(dp), allocatable, dimension(:) :: results

contains

subroutine get_monte_carlo_settings(file)
  use mod_utilities, only: file_exists, skip_header
  use mod_printing, only: print_warning, log, form
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

    call skip_header(i)

    select case(key)
    case("SP")
      sp_uncerteinty = value
    case("n")
      monte_carlo_samples = value
    case("A")
      admitance_uncerteinty = value
    case("RSP")
      rsp_uncerteinty = value
    case("T")
      t_uncerteinty = value
    case("H")
      h_uncerteinty = value
    case("SYSTEMATIC")
      monte_carlo_systematic=.true.
    case default
      call print_warning("unknown key in monte carlo settings "// key , error =.true.)
    end select

    write(log%unit, '(3x,a6,":",g10.3)'), trim(key), value
  enddo
  close (i)
end subroutine


real(dp) function add_noise_to_value(val, dataname, ilon, ilat, ilevel)
  use mod_printing, only: print_warning
  use mod_cmdline, only: ind
  use mod_data, only : model

  real(dp), intent(in) :: val
  character(*), intent(in) :: dataname
  integer, intent(in), optional :: ilat, ilon, ilevel

  type val_data
    real(dp), dimension(:,:,:), allocatable :: sp
  end type
  type (val_data) :: vals
  
  integer::i

  if(monte_carlo_systematic) then
    random_value=1.
  else
    call random_gau(random_value,0._dp, 1._dp)
  endif

  select case (dataname)

  case ("SP")
    if (.not.allocated(vals%sp)) then 
      allocate(                          &
        vals%sp(                         &
        size(model(ind%model%sp)%lon),   &
        size(model(ind%model%sp)%lat),   &
        size(model(ind%model%sp)%level)) &
        )

      vals%sp = setnan()
    endif

    if(isnan(vals%sp(ilon,ilat,ilevel))) then
      vals%sp(ilon,ilat,ilevel) = val + val * random_value * sp_uncerteinty
    endif
    ! add_noise_to_value = vals%sp(ilon,ilat,ilevel)
    ! add_noise_to_value 
    ! old%spval = add_noise_to_value
    ! old%sp = [ ilat , ilon , ilevel ]
  ! endif
    ! print '(3i6,2f14.3)' , ilat, ilon,ilevel , val , add_noise_to_value
    ! print *, old%sp 
    ! print *
    ! print * ,all(old%date .eq. date)
  case ("RSP")
    ! add_noise_to_value = val + val * random_value * rsp_uncerteinty
    ! case ("T")
    ! add_noise_to_value = val + val * random_value * t_uncerteinty
    ! print '(3i6,2f14.3)' , ilat, ilon,ilevel , val , add_noise_to_value
    ! case ("H")
    ! add_noise_to_value = val + random_value * h_uncerteinty
  case default
    call print_warning (dataname // "randomize how?" , error=.true.)
  end select
end function
end module
