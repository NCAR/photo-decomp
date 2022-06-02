! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This ch2chcho+hv->products quantum yield module

!> The ch2chcho+hv->prodcuts quantum yield type and related functions
module tuvx_quantum_yield_ch2chcho

  use tuvx_quantum_yield,    only : quantum_yield_t

  implicit none

  private
  public :: quantum_yield_ch2chcho_t

  !> Calculator for ch2chcho+hv->oh+h quantum yield
  type, extends(quantum_yield_t) :: quantum_yield_ch2chcho_t
  contains
    !> Calculate the quantum yield
    procedure :: calculate => run
  end type quantum_yield_ch2chcho_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the photorate quantum yield for a given set of environmental
  !! conditions
  function run( this, grid_warehouse, profile_warehouse )                     &
      result( quantum_yield )

    use musica_constants,              only : dk => musica_dk, ik => musica_ik
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_grid,                     only : abs_1d_grid_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t
    use tuvx_profile,                  only : abs_profile_t
    use musica_string,                 only : string_t

    !> Arguments
    class(quantum_yield_ch2chcho_t), intent(in) :: this
    !> The warehouses
    type(grid_warehouse_t), intent(inout)    :: grid_warehouse
    type(profile_warehouse_t), intent(inout) :: profile_warehouse
    !> Calculated quantum_yield
    real(kind=dk), allocatable               :: quantum_yield(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = 'ch2chcho+hv->products calculate'
    real(dk), parameter :: phiL = .004_dk
    real(dk), parameter :: phiU = .086_dk
    integer    , parameter :: iONE = 1_ik
    real(dk), parameter ::    rZERO = 0.0_dk
    real(dk), parameter ::    rONE  = 1.0_dk

    integer               :: nzdim, vertNdx
    real(dk)              :: M
    real(dk), allocatable :: phi0(:)
    real(dk), allocatable :: modelDens(:)
    class(abs_1d_grid_t), pointer :: zGrid
    class(abs_1d_grid_t), pointer :: lambdaGrid
    class(abs_profile_t), pointer :: mdlDensity
    type(string_t)                :: Handle

    Handle = 'Vertical Z'
    zGrid => grid_warehouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength'
    lambdaGrid => grid_warehouse%get_grid( Handle )
    Handle = 'Air'
    mdlDensity => profile_warehouse%get_Profile( Handle )

    nzdim = zGrid%ncells_ + iONE
    modelDens = mdlDensity%edge_val_

    allocate( quantum_yield(lambdaGrid%ncells_,nzdim) )
    allocate( phi0(lambdaGrid%ncells_) )
    quantum_yield = rZERO

    do vertNdx = iONE,nzdim
      associate( M => modelDens(vertNdx) )
      if( M > 2.6e19_dk ) then
        quantum_yield(:,vertNdx) = phiL
      else
        if( M <= 8.e17_dk ) then
          phi0 = phiU + 1.613e-17_dk*8.e17_dk
        else
          phi0 = phiU + 1.613e-17_dk*M
        endif
        quantum_yield(:,vertNdx) = phiL + rONE/phi0
      endif
      end associate
    enddo

    quantum_yield = transpose( quantum_yield )

  end function run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_quantum_yield_ch2chcho
