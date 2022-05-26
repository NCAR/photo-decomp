! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The abstract photolysis quantum yield module

!> The abstract quantum yield type and related functions
module tuvx_quantum_yield

  use musica_constants,                only : musica_dk, musica_ik

  implicit none
  private

  public :: abs_quantum_yield_t, abs_quantum_yield_ptr

  !> Photo rate quantum yield abstract type
  type, abstract :: abs_quantum_yield_t
  contains
    procedure(initial),   deferred :: initialize
    !> Calculate the photo rate quantum yield
    procedure(calculate), deferred :: calculate
    procedure                      :: addpnts
  end type abs_quantum_yield_t

  !> Pointer type for building sets of quantum yields
  type :: abs_quantum_yield_ptr
    class(abs_quantum_yield_t), pointer :: val_ => null( )
  end type abs_quantum_yield_ptr

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

interface

  !> Initialize the quantum yield object
  subroutine initial( this, config, gridWareHouse, ProfileWareHouse )
    use musica_config,    only : config_t
    use musica_constants, only : musica_dk
    use tuvx_grid_warehouse,    only : grid_warehouse_t
    use tuvx_profile_warehouse, only : Profile_warehouse_t

    import abs_quantum_yield_t

    !> Quantum yield calculator
    class(abs_quantum_yield_t), intent(inout) :: this
    type(config_t),             intent(inout) :: config
    type(grid_warehouse_t),     intent(inout) :: gridWareHouse
    type(Profile_warehouse_t),  intent(inout) :: ProfileWareHouse
  end subroutine initial

  !> Calculate the quantum yield
  function calculate( this, gridWareHouse, ProfileWareHouse ) result( quantum_yield )

    use musica_constants,       only : musica_dk
    use tuvx_grid_warehouse,    only : grid_warehouse_t
    use tuvx_profile_warehouse, only : Profile_warehouse_t

    import abs_quantum_yield_t

    !> Quantum yield calculator
    class(abs_quantum_yield_t), intent(in)   :: this
    !> quantum yield on model photo grid
    real(kind=musica_dk), allocatable        :: quantum_yield(:,:)
    type(grid_warehouse_t), intent(inout)    :: gridWareHouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse
  end function calculate

end interface

contains

  subroutine addpnts( this, config, data_lambda, data_parameter )
    use musica_config, only : config_t
    use musica_string, only : string_t
    use tuvx_util,   only : addpnt

    class(abs_quantum_yield_t)    :: this
    type(config_t), intent(inout) :: config
    real(musica_dk), allocatable, intent(inout) :: data_lambda(:)
    real(musica_dk), allocatable, intent(inout) :: data_parameter(:)

    real(musica_dk), parameter :: rZERO = 0.0_musica_dk
    real(musica_dk), parameter :: rONE  = 1.0_musica_dk
    real(musica_dk), parameter :: deltax = 1.e-5_musica_dk
    character(len=*), parameter :: Iam = 'cross_section; addpnts: '

    integer(musica_ik) :: nRows
    real(musica_dk) :: lowerLambda, upperLambda
    real(musica_dk) :: addpnt_val_
    type(string_t)  :: addpnt_type_
    logical         :: found
    character(len=:), allocatable :: number

    write(*,*) Iam,'entering'

    !> add endpoints to data arrays; first the lower bound
    nRows = size(data_lambda)
    lowerLambda = data_lambda(1) ; upperLambda = data_lambda(nRows)
    call config%get( 'lower extrapolation', addpnt_type_, Iam, found=found )
    if( .not. found ) then
      addpnt_val_ = rZERO
    elseif( addpnt_type_ == 'boundary' ) then
      addpnt_val_ = data_parameter(1)
    else
      number = addpnt_type_%to_char()
      read( number, '(g30.20)' ) addpnt_val_
    endif

    call addpnt(x=data_lambda,y=data_parameter,xnew=rZERO,ynew=addpnt_val_) 
    call addpnt(x=data_lambda,y=data_parameter,xnew=(rONE-deltax)*lowerLambda,ynew=addpnt_val_) 
    !> add endpoints to data arrays; now the upper bound
    call config%get( 'upper extrapolation', addpnt_type_, Iam, found=found )
    if( .not. found ) then
      addpnt_val_ = rZERO
    elseif( addpnt_type_ == 'boundary' ) then
      addpnt_val_ = data_parameter(nRows)
    else
      number = addpnt_type_%to_char()
      read( number, '(g30.20)' ) addpnt_val_
    endif

    call addpnt(x=data_lambda,y=data_parameter,xnew=(rONE+deltax)*upperLambda,ynew=addpnt_val_) 
    call addpnt(x=data_lambda,y=data_parameter,xnew=1.e38_musica_dk,ynew=addpnt_val_) 

    write(*,*) Iam,'exiting'

  end subroutine addpnts

end module tuvx_quantum_yield
