! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This ch2chcho+hv->products quantum yield module

!> The ch2chcho+hv->prodcuts quantum yield type and related functions
module tuvx_quantum_yield_ch2chcho

  use tuvx_quantum_yield_base,    only : base_quantum_yield_t

  implicit none

  private
  public :: ch2chcho_quantum_yield_t

  !> Calculator for ch2chcho+hv->oh+h quantum yield
  type, extends(base_quantum_yield_t) :: ch2chcho_quantum_yield_t
  contains
    !> Calculate the quantum yield
    procedure :: calculate => run
  end type ch2chcho_quantum_yield_t

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
    class(ch2chcho_quantum_yield_t), intent(in) :: this
    !> The warehouses
    type(grid_warehouse_t), intent(inout)    :: gridWareHouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse
    !> Calculated quantum_yield
    real(kind=dk), allocatable               :: quantum_yield(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = 'ch2chcho+hv->products calculate: '
    real(dk), parameter :: phiL = .004_dk
    real(dk), parameter :: phiU = .086_dk
    integer(ik), parameter :: iONE = 1_ik
    real(dk), parameter ::    rZERO = 0.0_dk
    real(dk), parameter ::    rONE  = 1.0_dk

    integer(ik)           :: nzdim, vertNdx
    real(dk)              :: M
    real(dk), allocatable :: phi0(:)
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

    write(*,*) Iam,'exiting'

  end function run

end module tuvx_quantum_yield_ch2chcho
