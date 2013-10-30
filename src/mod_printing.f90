module mod_printing
  use iso_fortran_env, only: output_unit

  !----------------------------------------------------
  ! For preety printing
  !----------------------------------------------------
  character(len=255), parameter ::                     & 
    form_header     = '(72("#"))' ,                    & 
    form_separator  = '("#",71("-"))' ,                & 
    form_inheader   = '(("#"),1x,a68,1x,("#"))' ,      & 
    form_inheader_n = '(("#"),1x,a55,1x,i2.2,"(",i8,")",x,("#"))' , & 
    form_60         = "(a,100(1x,g0))",                & 
    form_61         = "(2x,a,100(1x,g0))",             & 
    form_62         = "(4x,a,100(1x,g0))",             & 
    form_63         = "(6x,100(x,g0))",                & 
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
      sparse    = .false.
  end type
  type(output_info) :: log, output 

contains
! =============================================================================
! =============================================================================
subroutine print_warning (warn , unit, more, error)
  use, intrinsic:: iso_fortran_env
  character (len=*)  :: warn
  character (len=*), optional :: more
  integer , optional :: unit
  integer :: def_unit
  logical, optional :: error

  def_unit = error_unit
  if (present (unit) ) def_unit=unit

  select case(warn)
  case("site_file_format")
    write(def_unit, form%i1) "Some records were rejected"
    write(def_unit, form%i1) "you should specify for each &
      line at least 3[4] parameters in free format:"
    write(def_unit, form%i1) "name lat lon [H=0] (skipped)"
  case("boundaries")
     write(def_unit, form%i1) "something wrong with boundaries. IGNORED"
   case("site")
    write(def_unit, form%i1) "something wrong with -S|-R specification. IGNORED"
  case ("repeated") 
    write(def_unit, form%i1) "reapeted specification. IGNORED"
  case ("date") 
    write(def_unit, form%i1) "something wrong with date format -D. IGNORED"
  case ("model") 
    write(def_unit, form%i3) "something wrong with -F."
  case("alias_without_date")
    write(def_unit, form%i1) "-D is required with aliased data"
  case("green_missing")
    write(def_unit, form%i1) "-G is required"
  case("method")
    write(def_unit, form%i1) "-M no method was set"
  case default 
    write(def_unit, form%i1) "error ---"
  end select
  if (present(more)) write(def_unit, form%i2) more
  if (present(error) .and. error) call exit(1)
end subroutine

! =============================================================================
! =============================================================================
subroutine progress(j, time)
    use mod_constants, only: dp
    use mod_cmdline, only: moreverbose
    use iso_fortran_env, only: output_unit
    implicit none
    integer(kind=4)::j,k
    integer:: ii
    character(len=27)::bar="???% |                    |"
    real(dp), optional :: time

    write(unit=bar(1:3),fmt="(i3)") j
    do k=1, j/5
      bar(6+k:6+k)="*"
    enddo
    if (present(time)) then
      write(unit=output_unit,fmt="(a1,a1,a27,f6.1,a1,' [eta', i5,']', <size(moreverbose)+1>(x,a))") &
          '+',char(13), bar, &
          time, "s" , int(100.*time/j), trim(output%name) , &
          (trim(moreverbose(ii)%name),ii=1,size(moreverbose))
    else
      write(unit=output_unit,fmt="(a1,a1,a27)") '+',char(13), bar
    endif
    return
end subroutine progress
end module mod_printing
