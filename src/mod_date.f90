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
! convert date 2010-01-02 into grat format 20100102
! =============================================================================
subroutine strip_hyphen_date_iso(string)
  use mod_utilities, only: count_separator

  character(*), intent(inout) :: string
  integer                     :: i

  do i = 1, count_separator(string,'-')
    string = string(1:index(string,'-')-1)//'0'//string(index(string,'-')+1:)
  enddo

  call print_warning('iso_date not supported in parser yet', error=.true.)
end subroutine

! =============================================================================
!> Parse date given as 20110503020103  to yy mm dd hh mm ss and mjd
!!
!! \warning decimal seconds are not allowed

!! TODO remove mod_data model from use statemant and turn it to function
!! returning dates. for m option pass array of model dates in optional dummy arg
! =============================================================================
subroutine parse_date(cmd_line_entry)
  use mod_cmdline
  use mod_mjd,       only: mjd, invmjd
  use mod_utilities, only: is_numeric
  use mod_data,      only: model

  integer, dimension(6) :: start, stop, swap
  real(dp)              :: step
  integer               :: i_, i, start_index, i_aux
  character(1)          :: interval_unit
  type(cmd_line_arg)    :: cmd_line_entry
  logical               :: success

  if (allocated(date)) then
    call print_warning ("repeated")
    return
  endif

  do i_ = 1, ubound(cmd_line_entry%field,1)

    if (                                                     &
      any(cmd_line_entry%field(i_)%subfield%name.eq."m")     &
      .or.                                                   &
      any(cmd_line_entry%field(i_)%subfield%dataname.eq."~") &
      ) then

      if (.not. allocated (model)) then
        call print_warning(                                              &
          "cannot use m or ~ in data specifier (-D m ) if no model file" &
          // " with dates given", error = .true.)

      elseif (.not.allocated(model(1)%date)) then
        call print_warning(                                         &
          "cannot use m or ~ in data specifier if first model file" &
          // " does not contains dates", error = .true.)

      endif
    endif

    if (trim(cmd_line_entry%field(i_)%full).eq."") then
      call print_warning("bad date " //trim(cmd_line_entry%field(i_)%full))
      cycle
    endif

    do i_aux=1, min(ubound(cmd_line_entry%field(i_)%subfield,1),2)
      if(index(cmd_line_entry%field(i_)%subfield(i_aux)%name,'-').gt.1) then
        call strip_hyphen_date_iso(cmd_line_entry%field(i_)%subfield(i_aux)%name)
      endif
    enddo

    if (any([(cmd_line_entry%field(i_)%subfield(i_aux)%name.ne."m"         &
      .and..not.is_numeric(cmd_line_entry%field(i_)%subfield(i_aux)%name), &
      i_aux=1, size(cmd_line_entry%field(i_)%subfield))])) then

      call print_warning(                                          &
        "date not numeric "// trim(cmd_line_entry%field(i_)%full), &
        error=.true.                                               &
        )
      cycle

    endif

    if (any( [(                                                            &
      is_numeric(cmd_line_entry%field(i_)%subfield(i_aux)%name)            &
      .and.index(cmd_line_entry%field(i_)%subfield(i_aux)%name,".").ne.0 , &
      i_aux=1, size(cmd_line_entry%field(i_)%subfield))])) then

      call print_warning("decimal date not supported "// trim(cmd_line_entry%field(i_)%full))
      cycle

    endif

    interval_unit = "h"

    if (.not.log%sparse)                                           &
      write(log%unit, form%i2) trim(cmd_line_entry%field(i_)%full)

    if (cmd_line_entry%field(i_)%subfield(1)%name.eq."m") then
      if (ubound(model(1)%date,1).eq.0) then
        call print_warning("no dates in first model: -Dm is forbidden (or -D before -F)", error=.true.)
      else
        start = model(1)%date(lbound(model(1)%date, 1), 1:6)
      endif
    else
      call string2date(cmd_line_entry%field(i_)%subfield(1)%name, start, success=success)
      if (.not.success) cycle
    endif

    stop = start

    ! tilde
    if (cmd_line_entry%field(i_)%subfield(1)%dataname=="~".and. allocated(model)) then
      if ((ubound(model(1)%date,1).gt.0)) then
        swap = model(1)%date(1, :)
        do i=2, size(model(1)%date(:, :), 1)
          if (abs(mjd(model(1)%date(i, :))-mjd(start)).lt.abs(mjd(swap)-mjd(start)))  then
            swap = model(1)%date(i, :)
          endif
        enddo
        start = swap
      endif
    endif

    if (size(cmd_line_entry%field(i_)%subfield).ge.2        &
      .and. cmd_line_entry%field(i_)%subfield(2)%name.ne."" &
      ) then

      if (cmd_line_entry%field(i_)%subfield(2)%name.eq."m") then
        if (size(model(1)%date).eq.0) then
          call print_warning("no dates in first model: -Dm is forbidden (or -D before -F)", error=.true.)
        else
          stop = model(1)%date(ubound(model(1)%date, 1), 1:6)
        endif
      else if (cmd_line_entry%field(i_)%subfield(2)%dataname.ne."") then
        read (cmd_line_entry%field(i_)%subfield(2)%name,*) stop(1)

        select case (cmd_line_entry%field(i_)%subfield(2)%dataname)

        case('Y')
          stop(1)  = start(1)+stop(1)
          stop(2:) = start(2:)

        case('M')
          stop(2)  = start(2)+stop(1)
          stop(1)  = start(1)
          stop(3:) = start(3:)

          if (stop(2).gt.12) then
            stop(1) = stop(1)+int(stop(2)/12)
            stop(2) = modulo(stop(2), 12)
          else if (stop(2).lt.1) then
            stop(1) = stop(1)-int(-stop(2)/12+1)
            stop(2) = stop(2)+12*(1+int(-stop(2)/12))
          endif

        case('D')
          call invmjd (mjd(start)+stop(1), stop)

        case('H')
          call invmjd (mjd(start)+stop(1)/24._dp, stop)

        case('s')
          call invmjd (mjd(start)+stop(1)/24._dp/3600._dp, stop)

        case('')

        case default
          call print_warning ("unit not valid", error=.true.)
          cycle
        endselect

      else
        call string2date(cmd_line_entry%field(i_)%subfield(2)%name, stop)
      endif

    else
      stop = start

    endif

    if (size(cmd_line_entry%field(i_)%subfield).ge.3) then

      if(.not.is_numeric(cmd_line_entry%field(i_)%subfield(3)%name)) then
        call print_warning(                                              &
          "not numeric interval "// trim(cmd_line_entry%field(i_)%full), &
          error=.true.                                                   &
          )
      endif

      read (cmd_line_entry%field(i_)%subfield(3)%name, *) step

      select case (cmd_line_entry%field(i_)%subfield(3)%dataname)

      case("M", "D", "Y", "H", "s")
        read (cmd_line_entry%field(i_)%subfield(3)%dataname, * ) interval_unit

      case default
        call print_warning ("interval unit not valid", error=.true.)
        cycle

      endselect
    else
      step=6
    endif

    if (.not.log%sparse) then
      write (log%unit, '('//form%t3//', a, x, i4, 5(1x, i2.2))')  "start date:", start

      if (mjd(start).ne.mjd(stop)) then
        write (log%unit, '('//form%t3//', a, x, i4, 5(1x, i2.2))') "stop  date:", stop
        write (log%unit, "(" // form%t3// "a, f5.1, a)") "interval:", step, interval_unit
      endif
    endif

    ! allow that stop is previous than start and list in reverse order
    ! chage the sign of step in dates if necessery
    if(mjd(stop).lt.mjd(start) .and. step.gt.0) step = -step

    ! or if step is negative
    if(mjd(stop).gt.mjd(start) .and. step.lt.0) then
      swap  = start
      start = stop
      stop  = swap
    endif

    if (interval_unit.eq."M".or.interval_unit.eq."Y") then

      if (interval_unit.eq."Y") then
        step          = step*12
        interval_unit = "M"
      endif

      if (interval_unit.eq."M") then
        call more_dates &
          (int((12*(stop(1) - start(1))+stop(2)-start(2))/(step))+1, start_index)

        date(start_index)%date = start
        date(start_index)%mjd  = mjd(date(start_index)%date)

        do i = start_index+1, size(date)
          date(i)%date    = date(i-1)%date
          date(i)%date(2) = date(i-1)%date(2)+step

          if (date(i)%date(2).gt.12) then
            date(i)%date(1) = date(i)%date(1)+int(date(i)%date(2)/12)
            date(i)%date(2) = modulo(date(i)%date(2), 12)

          else if (date(i)%date(2).lt.1) then
            date(i)%date(1) = date(i)%date(1)-int(-date(i)%date(2)/12+1)
            date(i)%date(2) = date(i)%date(2)+12*(1+int(-date(i)%date(2)/12))

          endif

          date(i)%mjd = mjd(date(i)%date)
          call invmjd(date(i)%mjd, date(i)%date)

        enddo
      endif

    else

      if (                                                    &
        cmd_line_entry%field(i_)%subfield(1)%name     == "m"  &
        .and.                                                 &
        cmd_line_entry%field(i_)%subfield(2)%name     == "m"  &
        .and. (                                               &
        ubound(cmd_line_entry%field(i_)%subfield,1).lt.3 .or. &
        cmd_line_entry%field(i_)%subfield(3)%dataname == ""   &
        )                                                     &
        ) then

        if (ubound(cmd_line_entry%field(i_)%subfield,1).lt.3) step=1

        if (step.gt.ubound(model(1)%time,1)) step=ubound(model(1)%time,1)
        call more_dates (ceiling(size(model(1)%time)/step), start_index)
        i_aux=0

        do i = 1, ubound(model(1)%time,1), int(step)
          i_aux=i_aux+1
          date(i_aux)%date = model(1)%date(i, :)
          date(i_aux)%mjd =mjd (date(i_aux)%date)
        enddo

      else
        if (interval_unit.eq."D") step = 24. * step
        if (interval_unit.eq."m") step = step/60.
        if (interval_unit.eq."s") step = step/60./60.

        ! 1e-9 to avoid subsecond instabilities
        call more_dates (int((mjd(stop)-mjd(start)+1e-9) / step * 24._dp + 1 ), start_index)

        do i = start_index, ubound(date,1)
          date(i)%mjd = mjd(start) + (i-start_index)*step/24.
          call invmjd (date(i)%mjd, date(i)%date)
        enddo

      endif
    endif

  enddo

  if (.not.log%sparse) then
    write (log%unit, form%i3) "dates total:", size(date)
  endif
end subroutine

! =============================================================================
!> Expand the array with date input
! =============================================================================
subroutine more_dates(number, start_index)
  integer, intent(in)  :: number
  integer, intent(out) :: start_index
  type(dateandmjd), allocatable, dimension(:) :: tmpdate

  if (allocated(date)) then

    write(log%unit, form%i3) "added date(s):", number
    start_index=ubound(date,1) + 1

    call move_alloc(date, tmpdate)

    allocate(date(ubound(tmpdate,1)+number))

    date(lbound(tmpdate,1):ubound(tmpdate,1))=tmpdate

    deallocate(tmpdate)
  else
    allocate(date(number))
    start_index=1
  endif
end subroutine

! =============================================================================
!> Convert dates given as string to integer (6 elements)
!!
!! 20110612060302 --> [2011, 6, 12, 6, 3, 2 ]
!! you can omit
!! \warning decimal seconds are not allowed
! =============================================================================
subroutine string2date(string, date, success)
  use mod_utilities, only: is_numeric

  character (*), intent(in) :: string
  integer, dimension(6), intent(out):: date
  integer :: start_char, end_char, j
  logical, optional :: success

  if (present(success)) success=.true.

  ! this allow to specify 1st Jan of year simple as -Dyyyy
  date = [2000, 1, 1, 0, 0, 0]

  start_char = 1
  do j = 1, 6
    if (j.eq.1) then
      end_char=min(len(string), start_char+3)
    else
      end_char=min(len(string), start_char+1)
    endif

    if (is_numeric(string(start_char : end_char) )) then
      read(string(start_char : end_char), *) date(j)
    else
      call print_warning ("bad date " // string)
      if (present(success)) success=.false.
      return
    endif

    start_char=end_char+1
    if (end_char.eq.len(trim(string))) exit
  enddo
end subroutine


end module mod_date
