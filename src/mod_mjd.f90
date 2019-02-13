! =============================================================================
!> \author M. Rajner
!! \date 2013.06.27
! =============================================================================
module mod_mjd
  use mr_constants, only: dp

  implicit none

contains
! ==============================================================================
!> Compute date from given Julian Day
!!
!! This subroutine computes date (as an six elements integer array) from
!! Modified Julian Day
!! \date 2013-03-04
! ==============================================================================
subroutine invmjd (mjd, date)
  real(dp), intent (in) :: mjd
  integer, intent (out):: date (6)
  integer  :: t1, t4, h, t2, t3, ih1, ih2
  real(dp) :: dayfrac

  date = 0

  t1      = 1+ int(mjd) + 2400000
  t4      = mjd - int (mjd)

  h       = int ((t1 - 1867216.25_dp)/36524.25_dp)
  t2      = t1 + 1 + h - int (h/4)
  t3      = t2 - 1720995
  ih1     = int ((t3 -122.1_dp)/365.25_dp)
  t1      = int (365.25_dp * ih1)
  ih2     = int ((t3 - t1)/30.6001_dp)
  date(3) = (t3 - t1 - int (30.6001_dp * ih2)) + t4
  date(2) = ih2 - 1

  if (ih2.gt.13) then
    date(2) = ih2 - 13
  endif

  date(1) = ih1

  if (date(2).le.2) then
    date(1) = date(1) + 1
  endif

  dayfrac = mjd - int(mjd) + 1._dp/ (60*60*1000)
  date(4) = int (dayfrac * 24._dp)
  date(5) = (dayfrac - date (4) / 24._dp) * 60 * 24
  date(6) = (dayfrac - date (4) / 24._dp - date(5)/(24._dp*60._dp)) &
    * 60 * 24 * 60

  if (date(6).ge.60) then
    stop "routine invmjd returned seconds ≥ 60"
  endif

end subroutine

! ==============================================================================
!> Compute Julian date for given date.
!!
!! Compute Julian Day (not MJD!). Seconds as integer!
!! \author  http://aa.usno.navy.mil/faq/docs/jd_formula.php
!! \date 2013-03-04
! ==============================================================================
function jd (year,month,day, hh,mm,ss)
  integer, intent(in) :: year,month,day
  integer, intent(in) :: hh,mm, ss
  integer :: i, j, k
  real(dp) :: jd

  i  = year
  j  = month
  k  = day
  jd =                            &
    k-32075                       &
    +1461*(i+4800+(j-14)/12)/4    &
    +367*(j-2-(j-14)/12*12)/12    &
    -3*((i+4900+(j-14)/12)/100)/4 &
    + (hh/24.)                    &
    + mm/(24.*60.)                &
    + ss/(24.*60.*60.)
  return
end function

! ==============================================================================
!> MJD from date.
!!
!! Compute Modified Julian date for given date. Iput is six element array of
!! !integers. Seconds also as integers!
!! \date 2013-03-04
! ==============================================================================
pure function mjd (date)
  integer, intent(in) :: date (6)
  integer  :: aux (6)
  integer  :: i, k
  real(dp) :: dayfrac, mjd

  aux = date

  if (aux(2).le.2) then
    aux(1) = date(1) -  1
    aux(2) = date(2) +  12
  endif

  i       = aux(1)/100
  k       = 2 - i + int(i/4)
  mjd     = int(365.25_dp * aux(1)) - 679006
  dayfrac = aux(4)/24._dp + date(5)/(24._dp*60._dp) + date(6)/(24._dp*3600._dp)
  mjd     = mjd + int(30.6001_dp*(aux(2) + 1)) + date(3) + k + dayfrac
end function

end module mod_mjd
