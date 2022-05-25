! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This mvk+hv->products quantum yield module

!> The mvk+hv->prodcuts quantum yield type and related functions
module tuvx_quantum_yield_mvk

  use tuvx_quantum_yield_base,    only : base_quantum_yield_t

  implicit none

  private
  public :: mvk_quantum_yield_t

  !> Calculator for mvk+hv->oh+h quantum yield
  type, extends(base_quantum_yield_t) :: mvk_quantum_yield_t
  contains
    !> Calculate the quantum yield
    procedure :: calculate => run
  end type mvk_quantum_yield_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the photorate quantum yield for a given set of environmental conditions
  function run( this, gridWareHouse, ProfileWareHouse ) result( quantum_yield )

    use musica_constants,           only : dk => musica_dk, ik => musica_ik
    use tuvx_grid_warehouse,        only : grid_warehouse_t
    use tuvx_grid,               only : abs_1d_grid_t
    use tuvx_profile_warehouse,     only : Profile_warehouse_t
    use tuvx_profile,               only : abs_Profile_t
    use musica_string,              only : string_t

    !> Arguments
    class(mvk_quantum_yield_t), intent(in)   :: this
    !> The warehouses
    type(grid_warehouse_t), intent(inout)    :: gridWareHouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse
    !> Calculated quantum_yield
    real(kind=dk), allocatable               :: quantum_yield(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = 'mvk+hv->products calculate: '
    integer(ik), parameter :: iONE  = 1_ik
    real(dk), parameter ::    rZERO = 0.0_dk
    real(dk), parameter ::    rONE  = 1.0_dk

    integer(ik)           :: nzdim, vertNdx
    real(dk)              :: divisor
    real(dk), allocatable :: modelDens(:)
    class(abs_1d_grid_t), pointer :: zGrid
    class(abs_1d_grid_t), pointer :: lambdaGrid
    class(abs_Profile_t), pointer :: mdlDensity
    type(string_t)                :: Handle

    write(*,*) Iam,'entering'

    Handle = 'Vertical Z'             ; zGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength' ; lambdaGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Air'                    ; mdlDensity => ProfileWareHouse%get_Profile( Handle )

    nzdim = zGrid%ncells_ + iONE
    modelDens = mdlDensity%edge_val_

    allocate( quantum_yield(lambdaGrid%ncells_,nzdim) )
    quantum_yield = rZERO

    do vertNdx = iONE,nzdim
      divisor = 5.5_dk + 9.2e-19_dk*modelDens(vertNdx)
      quantum_yield(:,vertNdx) = exp( -0.055_dk*(lambdaGrid%mid_ - 308._dk)) / divisor
      quantum_yield(:,vertNdx) = min( quantum_yield(:,vertNdx),rONE )
    enddo

    quantum_yield = transpose( quantum_yield )

    write(*,*) Iam,'exiting'

  end function run

end module tuvx_quantum_yield_mvk
