module mod_date
  use mod_constants, only: dp
  use mod_printing

  implicit none
  !----------------------------------------------------
  ! dates
  !----------------------------------------------------
  type dateandmjd
    real(dp) :: mjd
    integer, dimension(6) :: date
  end type
  real(dp) :: cpu_start, cpu_finish  
  type(dateandmjd), allocatable, dimension (:) :: date

contains
! =============================================================================
!> Parse date given as 20110503020103  to yy mm dd hh mm ss and mjd
!! 
!! \warning decimal seconds are not allowed
! =============================================================================
subroutine parse_date (cmd_line_entry) 
  use mod_cmdline
  use mod_mjd, only: mjd, invmjd
  use mod_data
  integer, dimension(6) :: start, stop, swap 
  real (dp) :: step 
  integer :: i_, i, start_index, i_aux
  character(1) :: interval_unit
  type(cmd_line_arg) :: cmd_line_entry

  if (allocated(date)) then
    call print_warning ("repeated")
    return
  endif
  do i_ = 1, size(cmd_line_entry%field)
    interval_unit = "h"
    write(log%unit,form%i2) trim(cmd_line_entry%field(i_)%full)
    call string2date(cmd_line_entry%field(i_)%subfield(1)%name, start)

    if (cmd_line_entry%field(i_)%subfield(1)%name.eq."m") then
      if (size(model(1)%date).eq.0) then
        stop "NO dates in first model. -Dm is forbidden"
      else
        start = model(1)%date(lbound(model(1)%date,1),1:6)
      endif
    endif
    stop = start

    ! tilde
    if (cmd_line_entry%field(i_)%subfield(1)%dataname=="~") then
      swap = model(1)%date(1,:)
      do i=2,size(model(1)%date(:,:),1)
        if (abs(mjd(model(1)%date(i,:))-mjd(start)).lt.abs(mjd(swap)-mjd(start)))  then
          swap = model(1)%date(i,:)
        endif
      enddo
      start = swap
    endif

    if (size(cmd_line_entry%field(i_)%subfield).ge.2  &
      .and. cmd_line_entry%field(i_)%subfield(2)%name.ne.""  &
      ) then
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
        call invmjd (mjd(start)+stop(1) , stop)
      case('H')
        call invmjd (mjd(start)+stop(1)/24. , stop)
      case('')
      case default
        call print_warning ("unit not valid", error=.true.)
      endselect
    else
      stop = start
    endif
    if (size(cmd_line_entry%field(i_)%subfield).ge.3)then
      read (cmd_line_entry%field(i_)%subfield(3)%name, *) step
      select case (cmd_line_entry%field(i_)%subfield(3)%dataname)
      case("M","D","Y", "H")
        read (cmd_line_entry%field(i_)%subfield(3)%dataname,* ) interval_unit
      case default
        call print_warning ("interval unit not valid", error=.true.)
      endselect
    else
      step=6
    endif

    if (cmd_line_entry%field(i_)%subfield(2)%name.eq."m") then
      stop = model(1)%date(ubound(model(1)%date,1),1:6)
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
          (int((12*(stop(1) - start(1))+stop(2)-start(2))/(step))+1, start_index)
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
      if (cmd_line_entry%field(i_)%subfield(1)%name=="m" &
        .and. cmd_line_entry%field(i_)%subfield(2)%name=="m" &
        .and. ( &
        size(cmd_line_entry%field(i_)%subfield).lt.3 .or. &
        cmd_line_entry%field(i_)%subfield(3)%dataname=="" &
        ) &
        ) then
        if (size(cmd_line_entry%field(i_)%subfield).lt.3) step=1
        if (step.gt.size(model(1)%time)) step=size(model(1)%time)
        call more_dates (ceiling(size(model(1)%time)/step),start_index)
        i_aux=0
        do i = 1, size(model(1)%time), int(step)
          i_aux=i_aux+1
          date(i_aux)%date = model(1)%date(i,:)
          date(i_aux)%mjd =mjd (date(i_aux)%date)
        enddo
      else
        if (interval_unit.eq."D") step = 24. * step
        if (interval_unit.eq."m") step = step/60.
        if (interval_unit.eq."s") step = step/60./60.

        call more_dates (int((mjd(stop)-mjd(start)) / step * 24. + 1 ), start_index )
        do i = start_index , size(date)
          date(i)%mjd = mjd(start) + (i-start_index)*step/24.
          call invmjd (date(i)%mjd, date(i)%date)
        enddo
      endif
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
!! 20110612060302 --> [2011 , 6 , 12 , 6 , 3 , 2 ]
!! you can omit
!! \warning decimal seconds are not allowed
! =============================================================================
subroutine string2date (string, date)
  use mod_utilities, only: is_numeric
  use mod_cmdline, only: method
  character (*) , intent(in) :: string
  integer , dimension(6) ,intent(out):: date 
  integer :: start_char , end_char , j

  ! this allow to specify !st Jan of year simple as -Dyyyy
  date = [2000, 1, 1, 0, 0, 0]

  start_char = 1
  do j = 1, 6 
    if (j.eq.1) then
      end_char=min(len(string),start_char+3)
    else
      end_char=min(len(string),start_char+1)
    endif
    if (is_numeric(string(start_char : end_char) )) then
      read(string(start_char : end_char),*) date(j)
    else
      call print_warning ("bad date " // string)
      return
    endif
    start_char=end_char+1
    if (end_char.eq.len(trim(string))) exit
  enddo 
  ! if (method.eq."n") then
    ! write (output%unit, '(i4.4,5(x,i2.2))') date
  ! endif
end subroutine

end module mod_date
