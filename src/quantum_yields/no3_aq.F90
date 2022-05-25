! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This no3-aq+hv->no2(aq)+o- quantum yield module

!> The no3-aq+hv->no2(aq)+o- quantum yield type and related functions
module tuvx_quantum_yield_no3m_aq

  use tuvx_quantum_yield_base,    only : base_quantum_yield_t

  implicit none

  private
  public :: no3m_aq_quantum_yield_t

  !> Calculator for no3m(aq)+hv->no2(aq)+o- quantum yield
  type, extends(base_quantum_yield_t) :: no3m_aq_quantum_yield_t
  contains
    !> Calculate the quantum yield
    procedure :: calculate => run
  end type no3m_aq_quantum_yield_t

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
    class(no3m_aq_quantum_yield_t), intent(in) :: this
    !> The warehouses
    type(grid_warehouse_t), intent(inout)    :: gridWareHouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse
    !> Calculated quantum_yield
    real(kind=dk), allocatable               :: quantum_yield(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = 'no3-_(aq)+hv->products calculate: '
    integer(ik), parameter :: iONE  = 1_ik
    real(dk), parameter ::    rZERO = 0.0_dk
    real(dk), parameter ::    rONE  = 1.0_dk

    integer(ik)           :: nzdim, vertNdx
    real(dk), allocatable :: modelTemp(:)
    class(abs_1d_grid_t), pointer :: zGrid
    class(abs_1d_grid_t), pointer :: lambdaGrid
    class(abs_Profile_t), pointer :: mdlTemperature
    type(string_t)                :: Handle

    write(*,*) Iam,'entering'

    Handle = 'Vertical Z'             ; zGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength' ; lambdaGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Temperature'            ; mdlTemperature => ProfileWareHouse%get_Profile( Handle )

    nzdim = zGrid%ncells_ + iONE
    modelTemp = mdlTemperature%edge_val_

    allocate( quantum_yield(lambdaGrid%ncells_,nzdim) )
    quantum_yield = rZERO

    do vertNdx = iONE,nzdim
      quantum_yield(:,vertNdx) = exp( -2400._dk/modelTemp(vertNdx) + 3.6_dk )      ! Chu & Anastasio, 2003
    enddo

    quantum_yield = transpose( quantum_yield )

    write(*,*) Iam,'exiting'

  end function run

end module tuvx_quantum_yield_no3m_aq
