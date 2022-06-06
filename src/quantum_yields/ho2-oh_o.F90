! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This ho2+hv->oh+o quantum yield module

!> The ho2+hv->oh+h quantum yield type and related functions
module tuvx_quantum_yield_ho2_oh_o

  use tuvx_quantum_yield,              only : quantum_yield_t, base_constructor

  implicit none

  private
  public :: quantum_yield_ho2_oh_o_t

  !> Calculator for ho2+hv->oh+h quantum yield
  type, extends(quantum_yield_t) :: quantum_yield_ho2_oh_o_t
  contains
    !> Calculate the quantum yield
    procedure :: calculate => run
  end type quantum_yield_ho2_oh_o_t

  !> Constructor
  interface quantum_yield_ho2_oh_o_t
    module procedure constructor
  end interface quantum_yield_ho2_oh_o_t

contains

function constructor( config, grid_warehouse, profile_warehouse ) result( this )

    use musica_assert,                 only : die_msg
    use musica_config,                 only : config_t
    use musica_string,                 only : string_t
    use tuvx_grid,                     only : grid_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_netcdf_util,              only : netcdf_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t
    use tuvx_util,                     only : inter2

    class(quantum_yield_t),    pointer :: this
    !> quantum yield configuration data
    type(config_t),            intent(inout) :: config
    type(grid_warehouse_t),    intent(inout) :: grid_warehouse
    type(profile_warehouse_t), intent(inout) :: profile_warehouse

    allocate ( quantum_yield_ho2_oh_o_t :: this )

    call base_constructor( this, config, grid_warehouse, profile_warehouse )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the quantum yield
  function run( this, grid_warehouse, profile_warehouse )                     &
      result( quantum_yield )

    use musica_constants,              only : dk => musica_dk
    use musica_string,                 only : string_t
    use tuvx_grid,                     only : grid_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    class(quantum_yield_ho2_oh_o_t), intent(in) :: this
    type(grid_warehouse_t),    intent(inout)    :: grid_warehouse
    type(profile_warehouse_t), intent(inout)    :: profile_warehouse
    !> Calculated quantum yield
    real(dk), allocatable                       :: quantum_yield(:,:)

    ! Local variables
    real(dk), parameter         :: lambda0 = 193._dk
    character(len=*), parameter :: Iam = 'ho2+hv->oh+o quantum yield calculate'
    real(dk), parameter    :: rZERO = 0.0_dk
    real(dk), parameter    :: rONE  = 1.0_dk

    integer                       :: vertNdx
    class(grid_t), pointer :: lambdaGrid, zGrid
    type(string_t)                :: Handle
    real(dk), allocatable         :: wrkQuantumYield(:)

    Handle = 'Vertical Z' ; zGrid => grid_warehouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength'
    lambdaGrid => grid_warehouse%get_grid( Handle )

    allocate( wrkQuantumYield( lambdaGrid%ncells_ ) )
    allocate( quantum_yield( lambdaGrid%ncells_, zGrid%ncells_ + 1 ) )

    where( lambdaGrid%mid_ >= 248._dk )
      wrkQuantumYield = rONE
    elsewhere
      wrkQuantumYield =                                                       &
          ( rONE + 14._dk * ( lambdaGrid%mid_ - lambda0 ) / 55._dk ) / 15._dk
    endwhere
    wrkQuantumYield = max( rZERO, wrkQuantumYield )
    do vertNdx = 1, zGrid%ncells_ + 1
      quantum_yield( :, vertNdx ) = wrkQuantumYield
    enddo

    quantum_yield = transpose( quantum_yield )

  end function run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_quantum_yield_ho2_oh_o
