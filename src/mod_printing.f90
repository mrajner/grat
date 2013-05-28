module mod_printing
  !----------------------------------------------------
  ! For preety printing
  !----------------------------------------------------
  character(len=255), parameter ::               & 
    form_header    = '(72("#"))' ,               & 
    form_separator = '("#",71("-"))' ,           & 
    form_inheader  = '(("#"),1x,a68,1x,("#"))' , & 
    form_60        = "(a,100(1x,g0))",           & 
    form_61        = "(2x,a,100(1x,g0))",        & 
    form_62        = "(4x,a,100(1x,g0))",        & 
    form_63        = "(6x,100(x,g0))",           & 
    form_64        = "(8x,100(x,g0))"

  type printing_info
    character(60) :: a, &
      i0      = "(a,100(1x,g0))",           & 
      i1      = "(2x,a,100(1x,g0))",        & 
      i2      = "(4x,a,100(1x,g0))",        & 
      i3      = "(6x,a,100(1x,g0))",        & 
      i4      = "(8x,a,100(1x,g0))",        & 
      t2      = "4x",        & 
      t3      = "6x",        & 
      separator = '("#",71("-"))' 
  end type
  type(printing_info) :: form

  ! where to print
  type output_info
    integer :: unit = 6
    character (255) :: name
    logical :: if, header
  end type
  type(output_info) :: log, output 

contains

! =============================================================================
! =============================================================================
subroutine print_warning (warn , unit)
  use, intrinsic:: iso_fortran_env
  character (len=*)  :: warn
  integer , optional :: unit
  integer :: def_unit

  def_unit = log%unit
  if (present (unit) ) def_unit=unit

  if (warn .eq. "site_file_format") then
    write(def_unit, form_63) "Some records were rejected"
    write(def_unit, form_63) "you should specify for each &
      line at least 3[4] parameters in free format:"
    write(def_unit, form_63) "name lat lon [H=0] (skipped)"
  else if (warn .eq. "boundaries") then
    write(def_unit, form_62) "something wrong with boundaries. IGNORED"
  else if (warn .eq. "site") then
    write(def_unit, form_62) "something wrong with -S|-R specification. IGNORED"
  else if (warn .eq. "repeated") then
    write(def_unit, form_62) "reapeted specification. IGNORED"
  else if (warn .eq. "date") then
    write(def_unit, form_62) "something wrong with date format -D. IGNORED"
  else if (warn .eq. "model") then
    write(def_unit, form%i3) "something wrong with -F."
  endif
end subroutine

end module mod_printing
