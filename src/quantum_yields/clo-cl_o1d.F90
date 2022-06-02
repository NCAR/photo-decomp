! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This clo+hv->cl+o1d quantum yield module

!> The clo+hv->cl+o1d quantum yield type and related functions
module tuvx_quantum_yield_clo_cl_o1d

  use tuvx_quantum_yield,              only : quantum_yield_t

  implicit none

  private
  public :: quantum_yield_clo_cl_o1d_t

  !> Calculator for clo+hv->cl+o1d quantum yield
  type, extends(quantum_yield_t) :: quantum_yield_clo_cl_o1d_t
  contains
    !> Calculate the quantum yield
    procedure :: calculate => run
  end type quantum_yield_clo_cl_o1d_t

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

    class(quantum_yield_clo_cl_o1d_t), intent(in) :: this
    type(grid_warehouse_t), intent(inout)    :: grid_warehouse
    type(profile_warehouse_t), intent(inout) :: profile_warehouse
    !> Calculated quantum_yield
    real(kind=dk), allocatable               :: quantum_yield(:,:)

    ! Local variables
    character(len=*), parameter :: Iam = 'clo+hv->cl+o1d calculate'
    real(dk), parameter ::    rZERO = 0.0_dk
    real(dk), parameter ::    rONE  = 1.0_dk
    integer                       :: nzdim, vertNdx
    class(abs_1d_grid_t), pointer :: zGrid
    class(abs_1d_grid_t), pointer :: lambdaGrid
    type(string_t)                :: Handle

    Handle = 'Vertical Z'
    zGrid => grid_warehouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength'
    lambdaGrid => grid_warehouse%get_grid( Handle )

    nzdim = zGrid%ncells_ + 1

    allocate( quantum_yield( lambdaGrid%ncells_, nzdim ) )

    where( lambdaGrid%mid_ < 263.4_dk )
      quantum_yield(:,1) = rONE
    elsewhere
      quantum_yield(:,1) = rZERO
    endwhere
    do vertNdx = 2, nzdim
      quantum_yield( :, vertNdx ) = quantum_yield(:,1)
    enddo

    quantum_yield = transpose( quantum_yield )

  end function run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_quantum_yield_clo_cl_o1d
