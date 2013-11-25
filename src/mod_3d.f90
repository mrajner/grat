module mod_3d
    use mod_constants, only: dp
    implicit none

contains



! see formula Neumeyer et al., 2004 p. 442-443
subroutine point_mass (site, date)
    use mod_site, only : site_info
    use mod_date, only : dateandmjd
    type (site_info) :: site
    type(dateandmjd),intent(in), optional :: date

    print *, present(date)
    ! call get_value
    print *, point_mass_a
end subroutine

real(dp) function point_mass_a ()
end function

! see formula Neumeyer et al., 2004 p. 442-443
subroutine potential ()

end subroutine
end module mod_3d
