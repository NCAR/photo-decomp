! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This clono2+hv->clo+no2 quantum yield module

!> The clono2+hv->clo+no2 quantum yield type and related functions
module tuvx_quantum_yield_clono2_clo_no2

  use tuvx_quantum_yield,              only : quantum_yield_t

  implicit none

  private
  public :: quantum_yield_clono2_clo_no2_t

  !> Calculator for clono2+hv->clo+no2 quantum yield
  type, extends(quantum_yield_t) :: quantum_yield_clono2_clo_no2_t
  contains
    !> Calculate the quantum yield
    procedure :: calculate => run
  end type quantum_yield_clono2_clo_no2_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the photorate quantum yield for a given set of environmental
  !! conditions
  function run( this, grid_warehouse, profile_warehouse )                     &
      result( quantum_yield )

    use musica_constants,              only : dk => musica_dk
    use musica_string,                 only : string_t
    use tuvx_grid,                     only : abs_1d_grid_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    class(quantum_yield_clono2_clo_no2_t), intent(in) :: this
    type(grid_warehouse_t),      intent(inout) :: grid_warehouse
    type(profile_warehouse_t),   intent(inout) :: profile_warehouse
    !> Calculated quantum yield
    real(kind=dk), allocatable                 :: quantum_yield(:,:)

    ! Local variables
    character(len=*), parameter :: Iam = 'clono2+hv->clo+no2 calculate'
    real(dk), parameter :: rZERO = 0.0_dk
    real(dk), parameter :: rONE  = 1.0_dk

    integer     :: vertNdx, lambdaNdx
    real(dk)    :: lambda, qyield
    real(dk), allocatable :: wrkQuantumYield(:)
    class(abs_1d_grid_t), pointer :: lambdaGrid
    class(abs_1d_grid_t), pointer :: zGrid
    type(string_t) :: Handle

    !> Get model wavelength grid
    Handle = 'Vertical Z'
    zGrid => grid_warehouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength'
    lambdaGrid => grid_warehouse%get_grid( Handle )

    allocate( wrkQuantumYield( lambdaGrid%ncells_ ) )
    allocate( quantum_yield( lambdaGrid%ncells_, zGrid%ncells_ + 1 ) )

    do lambdaNdx = 1, lambdaGrid%ncells_
      lambda = lambdaGrid%mid_( lambdaNdx )
      if( lambda < 308._dk ) then
        wrkQuantumYield( lambdaNdx ) = .6_dk
      elseif( 308._dk <= lambda .and. lambda <= 364._dk ) then
        wrkQuantumYield( lambdaNdx ) = 7.143e-3_dk * lambda - 1.6_dk
      else
        wrkQuantumYield( lambdaNdx ) = rONE
      endif
    enddo

    do vertNdx = 1, zGrid%ncells_ + 1
      quantum_yield( :, vertNdx ) = rONE - wrkQuantumYield
    enddo

    quantum_yield = transpose( quantum_yield )

  end function run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_quantum_yield_clono2_clo_no2
