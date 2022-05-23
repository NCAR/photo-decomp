! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This ch3coch2ch3+hv->ch3co+ch2ch3 quantum yield module

!> The ch3coch2ch3+hv->ch3co+ch2ch3 quantum yield type and related functions
module micm_ch3coch2ch3_quantum_yield_type

  use micm_base_quantum_yield_type,    only : base_quantum_yield_t

  implicit none

  private
  public :: ch3coch2ch3_quantum_yield_t

  !> Calculator for ch3coch2ch3+hv->ch3co+ch2ch3 quantum yield
  type, extends(base_quantum_yield_t) :: ch3coch2ch3_quantum_yield_t
  contains
    !> Calculate the quantum yield
    procedure :: calculate => run
  end type ch3coch2ch3_quantum_yield_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the photorate quantum yield for a given set of environmental conditions
  function run( this, gridWareHouse, ProfileWareHouse ) result( quantum_yield )

    use musica_constants,           only : dk => musica_dk, ik => musica_ik
    use micm_grid_warehouse,        only : grid_warehouse_t
    use micm_1d_grid,               only : abs_1d_grid_t
    use micm_Profile_warehouse,     only : Profile_warehouse_t
    use micm_Profile,               only : abs_Profile_t
    use musica_string,              only : string_t

    !> Arguments
    class(ch3coch2ch3_quantum_yield_t), intent(in) :: this
    !> The warehouses
    type(grid_warehouse_t), intent(inout)    :: gridWareHouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse
    !> Calculated quantum_yield
    real(kind=dk), allocatable               :: quantum_yield(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = 'ch3coch2ch3+hv->ch3co+ch2ch3 calculate: '
    integer(ik), parameter :: iONE = 1_ik
    real(dk), parameter ::    rZERO = 0.0_dk
    real(dk), parameter ::    rONE  = 1.0_dk

    integer(ik)                   :: nzdim, vertNdx
    real(dk)                      :: ptorr
    real(dk), allocatable         :: modelDens(:)
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
      ptorr = 760._dk*modelDens(vertNdx)/2.69e19_dk
      quantum_yield(:,vertNdx) = rONE/(0.96_dk + 2.22E-3_dk*ptorr)
      quantum_yield(:,vertNdx) = min(quantum_yield(:,vertNdx), rONE)
    enddo

    quantum_yield = transpose( quantum_yield )

    write(*,*) Iam,'exiting'

  end function run

end module micm_ch3coch2ch3_quantum_yield_type
