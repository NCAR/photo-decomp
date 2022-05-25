! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This clo+hv->cl+o1d quantum yield module

!> The clo+hv->cl+o1d quantum yield type and related functions
module tuvx_quantum_yield_clo_cl_o1d

  use tuvx_quantum_yield_base,    only : base_quantum_yield_t

  implicit none

  private
  public :: clo_cl_o1d_quantum_yield_t

  !> Calculator for clo+hv->cl+o1d quantum yield
  type, extends(base_quantum_yield_t) :: clo_cl_o1d_quantum_yield_t
  contains
    !> Calculate the quantum yield
    procedure :: calculate => run
  end type clo_cl_o1d_quantum_yield_t

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
    class(clo_cl_o1d_quantum_yield_t), intent(in) :: this
    !> The warehouses
    type(grid_warehouse_t), intent(inout)    :: gridWareHouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse
    !> Calculated quantum_yield
    real(kind=dk), allocatable               :: quantum_yield(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = 'clo+hv->cl+o1d calculate: '
    integer(ik), parameter :: iONE = 1_ik
    integer(ik), parameter :: iTWO = 2_ik
    real(dk), parameter ::    rZERO = 0.0_dk
    real(dk), parameter ::    rONE  = 1.0_dk
    integer(ik)                   :: nzdim, vertNdx
    class(abs_1d_grid_t), pointer :: zGrid
    class(abs_1d_grid_t), pointer :: lambdaGrid
    type(string_t)                :: Handle

    write(*,*) Iam,'entering'

    Handle = 'Vertical Z'             ; zGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength' ; lambdaGrid => gridWareHouse%get_grid( Handle )

    nzdim = zGrid%ncells_ + iONE

    allocate( quantum_yield(lambdaGrid%ncells_,nzdim) )

    where( lambdaGrid%mid_ < 263.4_dk )
      quantum_yield(:,1) = rONE
    elsewhere
      quantum_yield(:,1) = rZERO
    endwhere
    do vertNdx = iTWO,nzdim
      quantum_yield(:,vertNdx) = quantum_yield(:,1)
    enddo

    quantum_yield = transpose( quantum_yield )

    write(*,*) Iam,'exiting'

  end function run

end module tuvx_quantum_yield_clo_cl_o1d
