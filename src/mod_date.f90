module mod_date
  use mod_constants, only: dp
  use mod_printing

  implicit none
  !----------------------------------------------------
  ! dates
  !----------------------------------------------------
  type dateandmjd
    real(dp) :: mjd
    integer,dimension (6) :: date
  end type
  real(dp) :: cpu_start , cpu_finish  
  type(dateandmjd) , allocatable,dimension (:) :: date


contains

! =============================================================================
!> Parse date given as 20110503020103  to yy mm dd hh mm ss and mjd
!! 
!! \warning decimal seconds are not allowed
! =============================================================================
subroutine parse_date (cmd_line_entry) 
  use mod_cmdline
  integer , dimension(6) :: start , stop , swap 
  real (dp) :: step 
  integer :: i_ , i ,  start_index
  character(1) :: interval_unit
  type(cmd_line_arg)  :: cmd_line_entry

  if (allocated(date)) then
    call print_warning ("repeated")
    return
  endif
  do i_ = 1 , size(cmd_line_entry%field)
    interval_unit = "h"
    write(log%unit,form%i2) trim(cmd_line_entry%field(i_)%full)
    call string2date(cmd_line_entry%field(i_)%subfield(1)%name, start)

    if (size(cmd_line_entry%field(i_)%subfield).ge.2 )then
      call string2date(cmd_line_entry%field(i_)%subfield(2)%name, stop)
      select case (cmd_line_entry%field(i_)%subfield(2)%dataname)
      case('Y')
        stop(1)=start(1)+stop(1)
        stop(2:)=start(2:)
      case('M')
        stop(2)=start(2)+stop(1)
        stop(1)=start(1)
        stop(3:)=start(3:)
        if (stop(2).gt.12) then
          stop(1) = stop(1)+int(stop(2)/12)
          stop(2) = modulo(stop(2),12)
        else if (stop(2).lt.1) then
          stop(1) =stop(1)-int(-stop(2)/12+1)
          stop(2) =stop(2)+12*(1+int(-stop(2)/12))
        endif
      case('D')
        call invmjd ( mjd(start)+stop(1) , stop)
      case default
      endselect
    else
      stop = start
    endif
    if (size(cmd_line_entry%field(i_)%subfield).ge.3 )then
      read (cmd_line_entry%field(i_)%subfield(3)%name, *) step
      select case (cmd_line_entry%field(i_)%subfield(3)%dataname)
      case("M","D","Y")
        read (cmd_line_entry%field(i_)%subfield(3)%dataname,* ) interval_unit
      endselect
    else
      step=6
    endif

    write (log%unit , '('//form%t3//',a,x,i4,5(1x,i2.2))')  "start date:" , start
    if (mjd(start).ne.mjd(stop)) then
      write (log%unit , '('//form%t3//',a,x,i4,5(1x,i2.2))') "stop  date:" , stop
      write (log%unit , form%i3) "interval:" , step, interval_unit
    endif

    ! allow that stop is previous than start and list in reverse order
    ! chage the sign of step in dates if necessery
    if(mjd(stop).lt.mjd(start).and. step.gt.0) step = -step
    ! or if step is negative
    if(mjd(stop).gt.mjd(start).and. step.lt.0) then
      swap=start
      start=stop
      stop=swap
    endif

    if (interval_unit.eq."M".or.interval_unit.eq."Y") then
      if (interval_unit.eq."Y") then
        step=step*12
        interval_unit="M"
      endif
      if (interval_unit.eq."M") then
        call more_dates &
          ( int((12*(stop(1) - start(1))+stop(2)-start(2))/(step)) +1  , start_index)
        date(start_index)%date=start
        date(start_index)%mjd=mjd(date(start_index)%date)
        do i= start_index+1 ,size(date)
          date(i)%date=date(i-1)%date
          date(i)%date(2)=date(i-1)%date(2)+step
          if (date(i)%date(2).gt.12) then
            date(i)%date(1) =date(i)%date(1)+int(date(i)%date(2)/12)
            date(i)%date(2) =modulo(date(i)%date(2),12)
          else if (date(i)%date(2).lt.1) then
            date(i)%date(1) =date(i)%date(1)-int(-date(i)%date(2)/12+1)
            date(i)%date(2) =date(i)%date(2)+12*(1+int(-date(i)%date(2)/12))
          endif
          date(i)%mjd=mjd(date(i)%date)
        enddo
      endif
    else
      if (interval_unit.eq."D") step = 24. * step
      if (interval_unit.eq."m") step = step /60.
      if (interval_unit.eq."s") step = step /60./60.

      call more_dates (int((mjd(stop)-mjd(start)) / step * 24. + 1 ), start_index )
      do i = start_index , size(date)
        date(i)%mjd = mjd(start) + ( i - start_index ) * step / 24.
        call invmjd ( date(i)%mjd , date(i)%date)
      enddo
    endif
  enddo
  write (log%unit , form%i3) "dates total:" , size(date)
end subroutine
! =============================================================================
! =============================================================================
subroutine more_dates (number, start_index)
  integer, intent(in)  :: number
  integer, intent(out) :: start_index
  type(dateandmjd), allocatable , dimension(:) :: tmpdate

  if (allocated(date)) then
    write(log%unit, form%i3) ,"added date(s):", number
    start_index=size(date) + 1
    call move_alloc(date, tmpdate)
    allocate(date(size(tmpdate)+number))
    date=tmpdate
    deallocate(tmpdate)
  else 
    allocate(date(number))
    start_index=1
  endif
end subroutine


! =============================================================================
!> Convert dates given as string to integer (6 elements)
!! 
!! 20110612060302 --> [2011 , 6 , 12 , 6 , 3 , 2
!! you can omit
!! \warning decimal seconds are not allowed
! =============================================================================
subroutine string2date (string, date )
  use mod_utilities, only: is_numeric
  character (*) , intent(in) :: string
  integer , dimension(6) ,intent(out):: date 
  integer :: start_char , end_char , j

  ! this allow to specify !st Jan of year simple as -Dyyyy
  date = [2000 , 1 , 1 , 0 ,0 ,0]

  start_char = 1
  do j = 1 , 6 
    if (j.eq.1) then
      end_char=min(len(string),start_char+3)
    else
      end_char=min(len(string),start_char+1)
    endif
    if (is_numeric(string(start_char : end_char) )) then
      read(string(start_char : end_char),*) date(j)
    endif
    start_char=end_char+1
  enddo 
end subroutine

! ==============================================================================
!> Compute Julian date for given date.
!! 
!! Compute Julian Day (not MJD!). Seconds as integer!
!! \author  http://aa.usno.navy.mil/faq/docs/jd_formula.php
!! \todo mjd!
!! \date 2013-03-04
! ==============================================================================
function jd (year,month,day, hh,mm,ss)
  integer, intent(in) ::  year,month,day
  integer, intent(in) :: hh,mm, ss
  integer :: i , j , k
  real(dp) :: jd 

  i= year
  j= month
  k= day
  jd= k-32075+1461*(i+4800+(j-14)/12)/4+367*(j-2-(j-14)/12*12)/12-3*((i+4900+(j-14)/12)/100)/4  + (hh/24.) &
    + mm/(24.*60.) +ss/(24.*60.*60.) ! - 2400000.5
  return
end function

! ==============================================================================
!> MJD from date.
!! 
!! Compute Modified Julian date for given date. Iput is six element array of 
!! !integers. Seconds also as integers!
!! \date 2013-03-04
! ==============================================================================
function mjd  (date)
  integer ,intent(in) :: date (6)
  integer :: aux (6)
  integer :: i , k
  real(dp) :: dayfrac , mjd

  aux=date
  if ( aux(2) .le.  2) then
    aux(1)  = date(1) -  1
    aux(2) = date(2) +  12
  endif
  i = aux(1)/100
  k = 2 - i + int(i/4);
  mjd = int(365.25 * aux(1) ) - 679006
  dayfrac =  aux (4) / 24. + date(5)/(24. * 60. ) + date (6)/(24. * 3600. ) 
  mjd = mjd + int(30.6001*( aux(2) + 1)) + date(3) + k + dayfrac
end function

! ==============================================================================
!> Compute date from given Julian Day
!!
!! This subroutine computes date (as an six elements integer array) from 
!! Modified Julian Day
!! \date 2013-03-04
! ==============================================================================
subroutine invmjd (mjd , date)
  real(dp), intent (in) :: mjd
  integer , intent (out):: date (6)
  integer :: t1 ,t4 , h , t2 , t3 , ih1 , ih2
  real(dp) :: dayfrac

  date =0

  t1 = 1+ int(mjd) + 2400000
  t4 = mjd - int (mjd);
  h = int ((t1 - 1867216.25)/36524.25);
  t2 = t1 + 1 + h - int (h/4)
  t3 = t2 - 1720995
  ih1 = int ((t3 -122.1)/365.25)
  t1 = int (365.25 * ih1)
  ih2 = int ((t3 - t1)/30.6001);
  date(3) = (t3 - t1 - int (30.6001 * ih2)) + t4;
  date(2) = ih2 - 1;
  if (ih2 .gt. 13) date(2) = ih2 - 13
  date(1) = ih1
  if (date(2).le. 2) date(1) = date(1) + 1

  dayfrac = mjd - int(mjd) + 1./ (60*60*1000)
  date(4) = int (dayfrac * 24. )
  date(5) = ( dayfrac - date (4) / 24. ) * 60 * 24  
  date(6) = ( dayfrac - date (4) / 24. - date(5)/(24.*60.)  ) * 60 * 24 *60
  if (date (6) .eq. 60 ) then
    date (6)=0
    date (5)=date(5) + 1
  endif
end subroutine
end module mod_date
