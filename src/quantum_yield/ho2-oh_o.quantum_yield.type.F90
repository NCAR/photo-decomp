! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This ho2+hv->oh+o quantum yield module

!> The ho2+hv->oh+h quantum yield type and related functions
module micm_ho2_oh_o_quantum_yield_type

  use micm_base_quantum_yield_type,    only : base_quantum_yield_t
  use musica_constants,                only : dk => musica_dk, ik => musica_ik
  use musica_string,                   only : string_t
  use micm_grid_warehouse,             only : grid_warehouse_t
  use micm_1d_grid,                    only : abs_1d_grid_t
  use micm_Profile_warehouse,          only : Profile_warehouse_t

  implicit none

  private
  public :: ho2_oh_o_quantum_yield_t

  integer(ik), parameter :: iONE  = 1_ik
  real(dk), parameter    :: rZERO = 0.0_dk
  real(dk), parameter    :: rONE  = 1.0_dk

  !> Calculator for ho2+hv->oh+h quantum yield
  type, extends(base_quantum_yield_t) :: ho2_oh_o_quantum_yield_t
  contains
    !> Calculate the quantum yield
    procedure :: calculate => run
  end type ho2_oh_o_quantum_yield_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the quantum yield
  function run( this, gridWareHouse, ProfileWareHouse ) result( quantum_yield )

    !> called quantum yield object
    class(ho2_oh_o_quantum_yield_t), intent(in)  :: this
    !> The warehouses
    type(grid_warehouse_t),    intent(inout) :: gridWareHouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse
    !> Calculated quantum yield
    real(dk), allocatable                    :: quantum_yield(:,:)

    !> Local variables
    real(dk), parameter         :: lambda0 = 193._dk
    character(len=*), parameter :: Iam = 'ho2+hv->oh+o quantum yield calculate: '

    integer(ik)                   :: vertNdx
    class(abs_1d_grid_t), pointer :: lambdaGrid, zGrid
    type(string_t)                :: Handle
    real(dk), allocatable         :: wrkQuantumYield(:)

    write(*,*) Iam,'entering'

    Handle = 'Vertical Z' ; zGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength' ; lambdaGrid => gridWareHouse%get_grid( Handle )

    allocate( wrkQuantumYield(lambdaGrid%ncells_) )
    allocate( quantum_yield(lambdaGrid%ncells_,zGrid%ncells_+1) )

    where( lambdaGrid%mid_ >= 248._dk )
      wrkQuantumYield = rONE
    elsewhere
      wrkQuantumYield = (rONE + 14._dk*(lambdaGrid%mid_ - lambda0)/55._dk)/15._dk
    endwhere
    wrkQuantumYield = max( rZERO,wrkQuantumYield )
    do vertNdx = iONE,zGrid%ncells_+iONE
      quantum_yield(:,vertNdx) = wrkQuantumYield
    enddo

    quantum_yield = transpose( quantum_yield )

    write(*,*) Iam,'exiting'

  end function run

end module micm_ho2_oh_o_quantum_yield_type
