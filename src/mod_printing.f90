module mod_printing
  use iso_fortran_env, only: output_unit
  implicit none

  !----------------------------------------------------
  ! For preety printing
  !----------------------------------------------------
  character(len=255), parameter ::                                 &
    form_header     = '(72("#"))',                                 &
    form_separator  = '("#",71("-"))',                             &
    form_inheader   = '(("#"),1x,a68,1x,("#"))',                   &
    form_inheader_n = '(("#"),1x,a55,1x,i2.2,"(",i8,")",x,("#"))', &
    form_60         = "(a,100(1x,g0))",                            &
    form_61         = "(2x,a,100(1x,g0))",                         &
    form_62         = "(4x,a,100(1x,g0))",                         &
    form_63         = "(6x,100(x,g0))",                            &
    form_64         = "(8x,100(x,g0))"

  type printing_info
    character(60) :: a,                 &
      i0        = "(a,100(1x,g0))",     &
      i1        = "(2x,a,100(1x,g0))",  &
      i2        = "(4x,a,100(1x,g0))",  &
      i3        = "(6x,a,100(1x,g0))",  &
      i4        = "(8x,a,100(1x,g0))",  &
      i5        = "(10x,a,100(1x,g0))", &
      t1        = "2x",                 &
      t2        = "4x",                 &
      t3        = "6x",                 &
      separator = '("#",71("-"))'
  end type
  type(printing_info) :: form

  ! where to print
  type output_info
    integer :: unit = output_unit
    character (255) :: name
    logical :: if, header, tee, &
      noclobber = .false.,      &
      full      = .false.,      &
      sparse    = .false.,      &
      height    = .false.,      &
      level     = .false.,      &
      rho       = .false.,      &
      gp2h      = .false.,      &
      prune     = .false.,      &
      nan       = .false.
    character(10) :: form="en13.3"
  end type
  type(output_info) :: log, output

contains
! =============================================================================
! =============================================================================
subroutine print_warning (warn, unit, more, error, program_calling)
  use, intrinsic:: iso_fortran_env
  use :: mod_cmdline, only: warnings, method, quiet

  integer, dimension(8):: execution_date
  character (len=*)  :: warn
  character (len=*), optional :: more, program_calling
  integer, optional :: unit
  integer :: def_unit
  logical, intent(in), optional :: error

  if (present(error).and.error.or.warnings%if) then
    def_unit = error_unit
    if (present (unit) ) def_unit=unit

    if ((present(error).and.error)) then
      write(def_unit,'(a)', advance='no') "error: "
    else
      write(def_unit,'(a)', advance='no') "warning: "
      if (warnings%strict) write(def_unit,'(a$)') "[strict set] "
    endif

    select case(warn)

    case("args")
      write(def_unit, form%i0, advance="no") "no cmd line args! try: "// program_calling // " -h"

    case("site_file_format")
      write(def_unit, form%i0, advance="no") "Some records were rejected"
      write(def_unit, form%i0, advance="no") "you should specify for each &
        line at least 3[4] parameters in free format:"
      write(def_unit, form%i0, advance="no") "name lat lon [height] (rest will be skipped)"

    case("boundaries")
      write(def_unit, form%i0, advance="no") "something wrong with boundaries. IGNORED"

    case("site")
      write(def_unit, form%i0, advance="no") "something wrong with -S|-R specification. IGNORED"

    case ("repeated")
      write(def_unit, form%i0, advance="no") "reapeted specification"

    case ("date")
      write(def_unit, form%i0, advance="no") "something wrong with date format -D. IGNORED"

    case ("model")
      write(def_unit, form%i0, advance="no") "something wrong with -F."

    case("alias_without_date")
      write(def_unit, form%i0, advance="no") "-D is required with aliased data"

    case("green_missing")
      write(def_unit, form%i0, advance="no") "-G is required"

    case("method")
      write(def_unit, form%i0, advance="no") "-M no method was set"

    case("nc")
      write(def_unit, form%i0, advance="no") "I will not overwrite with : nc (noclobber)"

    case default
      write(def_unit, form%i0, advance="no") warn
    end select

    if (present(more)) write(def_unit, form%i0, advance="no") more

    if(.not.warnings%if) write(def_unit,*)
  endif

  if (warnings%time) then
    call date_and_time (values=execution_date)
    write(def_unit, &
      '("[",i4,2("-",i2.2), 1x,i2.2,2(":",i2.2),1x,"(",dp,SP,i3.2,"h UTC)","]")'),&
      execution_date (1:3), execution_date(5:7), execution_date(4)/60
  endif

  if(.not.warnings%time.and.warnings%if) write(def_unit,*)

  if ((present(error).and.error).or.warnings%strict) then
    call exit(1)
  endif

end subroutine

! =============================================================================
! =============================================================================
subroutine progress(j, time, cpu, every)
  use mod_constants,   only: dp
  use mod_cmdline,     only: moreverbose, quiet, quiet_step
  use iso_fortran_env, only: output_unit

  implicit none
  integer(kind=4)::j, k
  integer:: ii
  character(len=27)::bar="???% |                    |"
  real :: time, cpu
  integer, optional :: every
  integer :: every_
  integer,save :: step=0
  character(len=1) :: timeunit
  logical :: logprinted


  if (present(every)) then
    every_=every
  else
    every_=quiet_step
  endif

  step = step + 1

  if ((every_.eq.0).and.j.ne.100) then
    return
  else if (every_.eq.0.and.j.eq.100) then
  else if (                     &
    modulo(step,every_).ne.0 &
    .and.j.ne.every_         &
    .and.j.ne.100            &
    .and.step.ne.1           &
    ) then
    return
  endif

  write(unit=bar(1:3),fmt="(i3)") j
  do k=1, j/5
    bar(6+k:6+k)="*"
  enddo

  if (time.gt.60000) then
    time = time/3600
    cpu  = cpu/3600
    timeunit = "h"
  else if (time.gt.1000) then
    time = time/60
    cpu  = cpu/60
    timeunit = "m"
  else
    timeunit="s"
  endif


  if (.not.(quiet.or.output%unit.eq.output_unit)) then
    open (unit=output_unit, carriagecontrol='fortran')
    write(                                                &
      unit=output_unit,                                   &
      fmt="(a1,a1,a27,                                    &
      f5.1,a1,1x,a,f5.1,a,1x,                             &
      a,f5.1,x,                                           &
      a,f5.1,a1,                                          &
      x,a,<size(moreverbose)+1>(x,a)$)"                   &
      )                                                   &
      '+',char(13), bar,                                  &
      time, timeunit, "[eta", 100.*time/j,"]",            &
      "(proc:",cpu,                                       &
      "| %:",cpu/time*100,")",                            &
      trim(output%name),                                  &
      (                                                   &
      trim(moreverbose(ii)%name), ii=1, size(moreverbose) &
      )
  endif

  if (j.eq.100.and..not.logprinted) then
    close(output_unit)
    write(log%unit,                                 &
      '("Execution time:",1x,f5.1,a,                &
      &" (proc time:",1x,f5.1,1x,"|%", f5.1,")")') &
      time, timeunit,                               &
      cpu,                                          &
      cpu/time*100
    logprinted=.true.
  endif
  return
end subroutine progress

! =============================================================================
! =============================================================================
function basename (file)
  character(200) :: basename
  character(*) :: file

  if (log%full) then
    basename=file
  else
    basename=file(index(file,'/', back=.true.)+1:)
  endif
end function

! =============================================================================
!> Print version of program depending on program calling
!!
!! \author M. Rajner
!! \date 2013-03-06
! =============================================================================
subroutine print_version (            &
    program_calling,                  &
    version, cdate, fflags, compiler  &
    )
  character(*) :: program_calling
  character(*), optional :: version, cdate, fflags, compiler
  character(10) :: host

  call hostnm(host)

  write(log%unit, form_header)
  write(log%unit, form_inheader), trim(program_calling)
  write(log%unit, form_inheader), version
  write(log%unit, form_header)
  write(log%unit, form_inheader), &
    "compiler: "// trim(compiler)
  write(log%unit, form_inheader), "compiled on "//trim(host)//" "//cdate
  write(log%unit, form_inheader), 'FFLAGS = '//fflags
  write(log%unit, form_header)
  write(log%unit, form_inheader), 'Copyright 2013, 2014 by Marcin Rajner'
  write(log%unit, form_inheader), 'Warsaw University of Technology'
  write(log%unit, form_inheader), 'License: GPL v3 or later'
  write(log%unit, form_header)
end subroutine

end module mod_printing
