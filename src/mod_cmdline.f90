!> \file
!! \brief This module gather cmd line arguments
!!
!! it allows to specify commands with or without spaces therefore it is
!! convienient to use with auto completion of names
! =============================================================================
module mod_cmdline
  use mod_constants, only: dp

  implicit none

  !----------------------------------------------------
  ! command line entry
  !----------------------------------------------------
  type subfield_info
    character (len=100) :: name
    character (len=100) :: dataname
  end type

  type field_info
    character (len=355) :: full
    type(subfield_info), allocatable, &
      dimension(:) :: subfield
  end type

  type cmd_line_arg
    character(2) :: switch
    type (field_info), allocatable, &
      dimension(:) :: field
    character (len=455) :: full
  end type

  type(cmd_line_arg), allocatable, dimension(:) :: cmd_line

  private :: check_if_switch_or_minus

  type moreverbose_info
    character(60) :: name
    character(30):: dataname
    logical  :: sparse=.false.
    logical :: first_call = .true.
    integer :: unit
    logical :: noclobber = .false.
  end type

  type(moreverbose_info), allocatable, dimension(:) :: moreverbose

  !----------------------------------------------------
  ! info
  !----------------------------------------------------
  type range
    real(dp):: start
    real(dp):: stop
    real(dp):: step
    integer :: denser
    real(dp):: stop_3d
  end type

  type info_info
    type (range):: distance, azimuth, height
    character (1) :: interpolation
    logical :: height_progressive = .false.
  end type

  type(info_info), dimension(:), allocatable:: info

  !----------------------------------------------------
  ! general settings
  !----------------------------------------------------
  logical ::                           &
    inverted_barometer     = .true.  , &
    non_inverted_barometer = .false. , &
    ocean_conserve_mass    = .false. , &
    inverted_landsea_mask  = .false. , &
    optimize               = .false. , &
    quiet                  = .false.
  integer :: quiet_step=50

  type transfer_sp_info
    logical :: if = .false.
    ! by default with 2D method pressure is transfered
    ! on topography (@H)
    character(20) :: method="standard"
  end type
  type(transfer_sp_info) transfer_sp

  type warnings_info
    logical ::              &
      if         = .true.,  &
      strict     = .false., &
      time       = .false., &
      all        = .false., &
      file_exist = .false.
  end type

  type(warnings_info) warnings

  type model_index
    integer(2) :: sp, t, rsp, ewt, h, ls, hp, hrsp, gp, vt, vsh
  end type

  type poly_index
    integer(2) :: e, n
  end type

  type moreverbose_index
    integer(2) :: p, g, t, a, d, l, n, r, s, o, b, j, v
  end type

  type green_index
    integer(2) :: &
      gn          = 0,  & ! green newtonian   - with SP  in Pa
      ge          = 0,  & ! green elastic     - with SP  in Pa
      gegdt       = 0,  & ! green elastic     - first derivative of gravity part respect to temp (see Guo et al., 2004)
      gr          = 0,  & ! green radial      - with EWT in mm
      ghn         = 0,  & ! green horizontal  - with EWT in mm
      ghe         = 0,  & ! green horizontal  - with EWT in mm
      gg          = 0,  & ! green gravimetric - with SP  in Pa
      ! (like elastic but uses green not normalized according to Merriam)
    gndt        = 0,  & ! first derivative respect to temperature
      gndh        = 0,  & ! first derivative respect to station height
      gndz        = 0,  & ! first derivative respect to column height
      gndz2       = 0,  & ! second derivative respect to column height
      gnc         = 0,  & ! compute aggf every time
      g3d
  end type

  type index_info
    type (model_index)       :: model
    type (moreverbose_index) :: moreverbose
    type (green_index) :: green
    type (poly_index) :: polygon
  end type

  type(index_info) :: ind

  type admitance_info
    logical :: if
    real(dp), allocatable, dimension(:) :: value
  end type

  type(admitance_info) :: admitance

  logical :: method(3)

  character(9), dimension(3), parameter :: &
    method3dnames=[character(9)::"point", "potential", "cylinder"]
  logical :: method3d(3)
  logical :: method3d_compute_reference  = .false.
  real    :: method3d_refinment_distance = 0.1
  logical :: dryrun

  logical ::  &
    result_total     = .false.,  &
    result_total_all = .false.,  &
    result_component = .true. ,  &
    center_data      = .false. 
  
contains

! =============================================================================
!> This routine collect command line arguments to one matrix depending on
!! given switches and separators
!!
!! \date 2013.05.21
!! \author Marcin Rajner
! =============================================================================
subroutine collect_args (dummy)
  use mod_utilities, only: ntokens, count_separator

  character(*) :: dummy
  character(455) :: dummy_aux, dummy_aux2
  integer :: i, j, n
  integer :: indeks_space, indeks_comma, indeks_at, indeks_colon

  allocate(cmd_line(ntokens(dummy)))

  do i=1, ntokens(dummy)
    indeks_space       = index(dummy," ")
    cmd_line(i)%full   = dummy(1:indeks_space-1)
    cmd_line(i)%switch = cmd_line(i)%full(1:2)
    allocate(cmd_line(i)%field (count_separator (cmd_line(i)%full,",") + 1))

    dummy_aux = cmd_line(i)%full(3:)
    do j=1,size(cmd_line(i)%field)
      indeks_comma=index(dummy_aux,",")
      if (indeks_comma.gt.0) then
        cmd_line(i)%field(j)%full=dummy_aux(1:indeks_comma-1)
      else
        cmd_line(i)%field(j)%full=dummy_aux
      endif

      allocate(cmd_line(i)%field(j)%subfield &
        (count_separator (cmd_line(i)%field(j)%full,":") + 1))
      dummy_aux2 = cmd_line(i)%field(j)%full
      do n = 1, count_separator(cmd_line(i)%field(j)%full,":")+1
        indeks_colon=index(dummy_aux2,":")
        if (indeks_colon.gt.0) then
          cmd_line(i)%field(j)%subfield(n)%name=dummy_aux2(1:indeks_colon-1)
        else
          cmd_line(i)%field(j)%subfield(n)%name=dummy_aux2
        endif
        dummy_aux2=dummy_aux2(indeks_colon+1:)
        indeks_at=index(cmd_line(i)%field(j)%subfield(n)%name,"@")

        if (indeks_at.gt.0) then

          cmd_line(i)%field(j)%subfield(n)%dataname =              &
            cmd_line(i)%field(j)%subfield(n)%name(indeks_at+1:)

          cmd_line(i)%field(j)%subfield(n)%name =                  &
            cmd_line(i)%field(j)%subfield(n)%name(1:indeks_at-1)
        else
          cmd_line(i)%field(j)%subfield(n)%dataname = " "
        endif
      enddo
      dummy_aux=dummy_aux(indeks_comma+1:)
    enddo
    dummy= dummy(indeks_space+1:)
  enddo
end subroutine

! ==============================================================================
!> This subroutine removes unnecesary blank spaces from cmdline entry
!!
!! Marcin Rajner
!! \date 2013-05-13
!! allows specification like '-F file' and '-Ffile'
!! but  if -[0,9] it is treated as number belonging to switch (-S -2)
!! but  if -[\s,:] do not start next command line option
! ==============================================================================
subroutine get_command_cleaned(dummy)
  character(*), intent(out) :: dummy
  character(355) :: a, b, arg
  integer :: i

  dummy = ''
  arg   = ''

  do i = 1, iargc()
    call get_command_argument(i,a)
    call get_command_argument(i+1,b)

    if (check_if_switch_or_minus(a)) then
      arg = trim(a)
    else
        arg=trim(arg)//trim(a)
    endif

    if(check_if_switch_or_minus(b).or.i.eq.iargc()) then
      if(trim(dummy).eq."") then
        dummy=trim(arg)
      else
        dummy=trim(dummy)//" "//trim(arg)
      endif
    endif

  enddo
end subroutine

! ==============================================================================
!> Check if - starts new option in command line or is just a minus in command
!! line entry
!!
!! if after '-' is space or number or ',' or ':' (field separators) do not start
!! next option for command line
!! If switch return .true. otherwise return .false
!!
!! \author M. Rajner
!! \date 2013-03-19
! ==============================================================================
pure function check_if_switch_or_minus(dummy)
  use mod_utilities, only: is_numeric

  logical:: check_if_switch_or_minus
  character(*), intent(in) :: dummy

  check_if_switch_or_minus = .false.
  if (dummy(1:1).eq."-") check_if_switch_or_minus = .true.
  if (dummy(2:2).eq." ") check_if_switch_or_minus = .false.
  if (dummy(2:2).eq.",") check_if_switch_or_minus = .false.
  if (dummy(2:2).eq.":") check_if_switch_or_minus = .false.
  if (is_numeric(dummy(2:2))) check_if_switch_or_minus = .false.
end function

end module
