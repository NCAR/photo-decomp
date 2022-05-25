! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This clono2+hv->clo+no2 quantum yield module

!> The clono2+hv->clo+no2 quantum yield type and related functions
module tuvx_quantum_yield_clono2_clo_no2

  use tuvx_quantum_yield_base,    only : base_quantum_yield_t
  use musica_constants,                only : dk => musica_dk, ik => musica_ik, lk => musica_lk
  use tuvx_grid_warehouse,             only : grid_warehouse_t
  use tuvx_grid,                    only : abs_1d_grid_t
  use tuvx_profile_warehouse,          only : Profile_warehouse_t
  use musica_string,                   only : string_t

  implicit none

  private
  public :: clono2_clo_no2_quantum_yield_t

  integer(ik), parameter :: iONE  = 1_ik
  real(dk), parameter :: rZERO = 0.0_dk
  real(dk), parameter :: rONE  = 1.0_dk

  !> Calculator for clono2+hv->clo+no2 quantum yield
  type, extends(base_quantum_yield_t) :: clono2_clo_no2_quantum_yield_t
  contains
    !> Calculate the quantum yield
    procedure :: calculate => run
  end type clono2_clo_no2_quantum_yield_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the photorate quantum yield for a given set of environmental conditions
  function run( this, gridWareHouse, ProfileWareHouse ) result( quantum_yield )

    !> Arguments
    class(clono2_clo_no2_quantum_yield_t), intent(in) :: this
    !> The warehouses
    type(grid_warehouse_t),      intent(inout) :: gridWareHouse
    type(Profile_warehouse_t),   intent(inout) :: ProfileWareHouse
    !> Calculated quantum yield
    real(kind=dk), allocatable                 :: quantum_yield(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = 'clono2+hv->clo+no2 calculate: '

    integer(ik) :: vertNdx, lambdaNdx
    real(dk)    :: lambda, qyield
    real(dk), allocatable :: wrkQuantumYield(:)
    class(abs_1d_grid_t), pointer :: lambdaGrid
    class(abs_1d_grid_t), pointer :: zGrid
    type(string_t) :: Handle

    write(*,*) Iam,'entering'

    !> Get model wavelength grid
    Handle = 'Vertical Z'             ; zGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength' ; lambdaGrid => gridWareHouse%get_grid( Handle )

    allocate( wrkQuantumYield(lambdaGrid%ncells_) )
    allocate( quantum_yield(lambdaGrid%ncells_,zGrid%ncells_+iONE) )

    do lambdaNdx = iONE,lambdaGrid%ncells_
      lambda = lambdaGrid%mid_(lambdaNdx)
      if( lambda < 308._dk ) then
        wrkQuantumYield(lambdaNdx) = .6_dk
      elseif( 308._dk <= lambda .and. lambda <= 364._dk ) then
        wrkQuantumYield(lambdaNdx) = 7.143e-3_dk * lambda - 1.6_dk
      else
        wrkQuantumYield(lambdaNdx) = rONE
      endif
    enddo

    do vertNdx = iONE,zGrid%ncells_+iONE
      quantum_yield(:,vertNdx) = rONE - wrkQuantumYield
    enddo

    quantum_yield = transpose( quantum_yield )

    write(*,*) Iam,'exiting'

  end function run

end module tuvx_quantum_yield_clono2_clo_no2
