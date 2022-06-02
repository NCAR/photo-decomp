! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This ch3coch2ch3+hv->ch3co+ch2ch3 quantum yield module

!> The ch3coch2ch3+hv->ch3co+ch2ch3 quantum yield type and related functions
module tuvx_quantum_yield_ch3coch2ch3

  use tuvx_quantum_yield,              only : quantum_yield_t

  implicit none

  private
  public :: quantum_yield_ch3coch2ch3_t

  !> Calculator for ch3coch2ch3+hv->ch3co+ch2ch3 quantum yield
  type, extends(quantum_yield_t) :: quantum_yield_ch3coch2ch3_t
  contains
    !> Calculate the quantum yield
    procedure :: calculate => run
  end type quantum_yield_ch3coch2ch3_t

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
    use tuvx_profile,                  only : abs_profile_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    class(quantum_yield_ch3coch2ch3_t), intent(in) :: this
    type(grid_warehouse_t), intent(inout)    :: grid_warehouse
    type(profile_warehouse_t), intent(inout) :: profile_warehouse
    !> Calculated quantum_yield
    real(kind=dk), allocatable               :: quantum_yield(:,:)

    ! Local variables
    character(len=*), parameter :: Iam =                                      &
        'ch3coch2ch3+hv->ch3co+ch2ch3 calculate'
    real(dk), parameter ::    rZERO = 0.0_dk
    real(dk), parameter ::    rONE  = 1.0_dk

    integer                       :: nzdim, vertNdx
    real(dk)                      :: ptorr
    real(dk), allocatable         :: modelDens(:)
    class(abs_1d_grid_t), pointer :: zGrid
    class(abs_1d_grid_t), pointer :: lambdaGrid
    class(abs_profile_t), pointer :: mdlDensity
    type(string_t)                :: Handle

    Handle = 'Vertical Z'
    zGrid => grid_warehouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength'
    lambdaGrid => grid_warehouse%get_grid( Handle )
    Handle = 'Air'
    mdlDensity => profile_warehouse%get_profile( Handle )

    nzdim = zGrid%ncells_ + 1
    modelDens = mdlDensity%edge_val_

    allocate( quantum_yield( lambdaGrid%ncells_, nzdim ) )
    quantum_yield = rZERO

    do vertNdx = 1, nzdim
      ptorr = 760._dk * modelDens( vertNdx ) / 2.69e19_dk
      quantum_yield( :, vertNdx ) = rONE / ( 0.96_dk + 2.22E-3_dk * ptorr )
      quantum_yield( :, vertNdx ) = min( quantum_yield( :, vertNdx ), rONE )
    enddo

    quantum_yield = transpose( quantum_yield )

  end function run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_quantum_yield_ch3coch2ch3
