! ==============================================================================
!> \file
! ==============================================================================
module mod_normalization
  implicit none

contains
! =============================================================================
! =============================================================================
elemental function green_normalization(method, psi)
  use mod_constants, only: pi, earth, gravity, dp
  use mod_utilities, only: d2r

  real(dp) :: green_normalization
  character(*), intent(in) :: method
  real(dp), intent(in), optional :: psi

  if (method.eq."f2m") then
    green_normalization =                                                     &
        1e-3                                                                  &
        / earth%gravity%mean  * earth%radius * 2 * pi * (1.- cos(d2r(1._dp)))

  else if (method.eq."m") then ! merriam normalization
    green_normalization =                                             &
        psi * 1e15 * earth%radius**2 * 2 * pi * (1.- cos(d2r(1._dp)))

  else if (method.eq."f") then ! farrell normalization
    green_normalization = psi * 1e18 * earth%radius
  endif
end function

end module
