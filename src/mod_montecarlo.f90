module mod_montecarlo
  use mod_constants
  use lib_random

  implicit none
  character(200) :: monte_carlo_settings = ""

  logical :: &
    monte_carlo = .false., &
    monte_carlo_systematic = .false., &
    monte_carlo_progress = .false.


  real(dp) :: random_value

  integer :: monte_carlo_samples = 10


  real(dp) :: &
    admitance_uncerteinty = 0.0_dp, &
    sp_uncerteinty        = 0.000_dp , &
    t_uncerteinty         = 0.000_dp , &
    tv_uncerteinty        = 0.000_dp , &
    gp_uncerteinty        = 0.000_dp , & ! m
    vt_uncerteinty        = 0.000_dp , & ! m
    h_uncerteinty         = 0.000_dp  ! m

  real(dp), allocatable, dimension(:,:) :: monte_carlo_results
  real(dp), allocatable, dimension(:) :: results

  type val_data
    real(dp), dimension(:,:,:), allocatable :: sp, t, h , gp, vt
  end type
  type (val_data) :: mcval

contains

! =============================================================================
! =============================================================================
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
    case("T")
      t_uncerteinty = value
    case("H")
      h_uncerteinty = value
    case("GP")
      gp_uncerteinty = value
    case("VT")
      vt_uncerteinty = value
    case("SYSTEMATIC")
      monte_carlo_systematic=.true.
    case("PROGRESS")
      monte_carlo_progress=.true.
    case default
      call print_warning("unknown key in monte carlo settings "// key , error =.true.)
    end select

    write(log%unit, '(3x,a6,":",g10.3)'), trim(key), value
  enddo
  close (i)
end subroutine

! =============================================================================
! =============================================================================
real(dp) function add_noise_to_value(val, dataname, ilon, ilat, ilevel)
  use mod_printing, only: print_warning

  real(dp), intent(in) :: val
  character(*), intent(in) :: dataname
  integer, intent(in), optional :: ilat, ilon, ilevel

  integer::i

  if(monte_carlo_systematic) then
    random_value=1.
  else
    call random_gau(random_value,0._dp, 1._dp)
    ! print*,random_value
  endif

  select case (dataname)
  case ("SP")
    if( any([ilon,ilat,ilevel].gt.[(size(mcval%sp,i), i =1,3)])) then
      call print_warning ("PROBLEM montecarlo", error=.true.)
    endif

    if(isnan(mcval%sp(ilon,ilat,ilevel))) then
      mcval%sp(ilon,ilat,ilevel) = val + val * random_value * sp_uncerteinty
    endif

    add_noise_to_value = mcval%sp(ilon,ilat,ilevel)

  case ("T")
    if( any([ilon,ilat,ilevel].gt.[(size(mcval%t,i), i =1,3)])) then
      call print_warning ("PROBLEM montecarlo", error=.true.)
    endif

    if(isnan(mcval%t(ilon,ilat,ilevel))) then
      mcval%t(ilon,ilat,ilevel) = val + val * random_value * t_uncerteinty
    endif

    add_noise_to_value = mcval%t(ilon,ilat,ilevel)

  case ("H")
    if( any([ilon,ilat,ilevel].gt.[(size(mcval%h,i), i =1,3)])) then
      call print_warning ("PROBLEM montecarlo", error=.true.)
    endif

    if(isnan(mcval%h(ilon,ilat,ilevel))) then
      mcval%h(ilon,ilat,ilevel) = val + val * random_value * h_uncerteinty
    endif

    add_noise_to_value = mcval%h(ilon,ilat,ilevel)

  case ("GP")
    if( any([ilon,ilat,ilevel].gt.[(size(mcval%gp,i), i =1,3)])) then
      call print_warning ("PROBLEM montecarlo", error=.true.)
    endif

    if(isnan(mcval%gp(ilon,ilat,ilevel))) then
      mcval%gp(ilon,ilat,ilevel) = val + gp_uncerteinty
    endif
    add_noise_to_value = mcval%gp(ilon,ilat,ilevel)

  case ("VT")
    if( any([ilon,ilat,ilevel].gt.[(size(mcval%vt,i), i =1,3)])) then
      call print_warning ("PROBLEM montecarlo", error=.true.)
    endif

    if(isnan(mcval%vt(ilon,ilat,ilevel))) then
      mcval%vt(ilon,ilat,ilevel) = val + val * random_value * vt_uncerteinty
    endif
    add_noise_to_value = mcval%vt(ilon,ilat,ilevel)

  case default
    call print_warning (dataname // "randomize how?" , error=.true.)
  end select

end function

! =============================================================================
! =============================================================================
subroutine monte_carlo_reset()
  use mod_cmdline, only: ind
  use mod_data, only : model

  if(ind%model%sp.ne.0) then
    if(allocated(mcval%sp)) deallocate(mcval%sp)

    if (.not.allocated(mcval%sp)) then 
      allocate(                          &
        mcval%sp(                        &
        size(model(ind%model%sp)%lon),   &
        size(model(ind%model%sp)%lat),   &
        size(model(ind%model%sp)%level)) &
        )
      mcval%sp = setnan()
    endif
  endif

  if(ind%model%t.ne.0) then
    if(allocated(mcval%t)) deallocate(mcval%t)

    if (.not.allocated(mcval%t)) then 
      allocate(                          &
        mcval%t(                        &
        size(model(ind%model%t)%lon),   &
        size(model(ind%model%t)%lat),   &
        size(model(ind%model%t)%level)) &
        )
      mcval%t = setnan()
    endif
  endif

  if(ind%model%h.ne.0) then
    if(allocated(mcval%h)) deallocate(mcval%h)

    if (.not.allocated(mcval%h)) then 
      allocate(                          &
        mcval%h(                        &
        size(model(ind%model%h)%lon),   &
        size(model(ind%model%h)%lat),   &
        size(model(ind%model%h)%level)) &
        )
      mcval%h = setnan()
    endif
  endif

  if(ind%model%gp.ne.0) then
    if(allocated(mcval%gp)) deallocate(mcval%gp)

    if (.not.allocated(mcval%gp)) then 
      allocate(                          &
        mcval%gp(                        &
        size(model(ind%model%gp)%lon),   &
        size(model(ind%model%gp)%lat),   &
        size(model(ind%model%gp)%level)) &
        )
      mcval%gp = setnan()
    endif
  endif

  if(ind%model%vt.ne.0) then
    if(allocated(mcval%vt)) deallocate(mcval%vt)

    if (.not.allocated(mcval%vt)) then 
      allocate(                          &
        mcval%vt(                        &
        size(model(ind%model%vt)%lon),   &
        size(model(ind%model%vt)%lat),   &
        size(model(ind%model%vt)%level)) &
        )
      mcval%vt = setnan()
    endif
  endif

end subroutine
end module
