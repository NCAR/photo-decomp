! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This clono2+hv cross_section module

!> The clono2+hv->n2+o1d_cross_section type and related functions
module tuvx_cross_section_clono2

  use tuvx_cross_section_base,    only : base_cross_section_t

  implicit none

  private
  public :: clono2_cross_section_t

  !> Calculator for clono2_cross_section
  type, extends(base_cross_section_t) :: clono2_cross_section_t
  contains
    !> Calculate the cross section
    procedure :: calculate => run
  end type clono2_cross_section_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the photorate cross section for a given set of environmental conditions
  function run( this, gridWareHouse, ProfileWareHouse, atMidPoint ) result( cross_section )

    use musica_constants,           only : dk => musica_dk, ik => musica_ik, lk => musica_lk
    use tuvx_grid_warehouse,        only : grid_warehouse_t
    use tuvx_grid,               only : abs_1d_grid_t
    use tuvx_profile_warehouse,     only : Profile_warehouse_t
    use tuvx_profile,               only : abs_Profile_t
    use musica_string,              only : string_t

    !> Arguments
    class(clono2_cross_section_t), intent(in)       :: this
    logical(lk), optional, intent(in)               :: atMidPoint
    !> The warehouses
    type(grid_warehouse_t), intent(inout)           :: gridWareHouse
    type(Profile_warehouse_t), intent(inout)        :: ProfileWareHouse
    !> Calculated cross section
    real(kind=dk), allocatable                      :: cross_section(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = 'clono2 cross section calculate: '

    integer(ik), parameter :: iONE  = 1_ik
    real(dk), parameter :: rZERO = 0.0_dk
    real(dk), parameter :: rONE  = 1.0_dk
    real(dk), parameter :: Thold = 296._dk

    integer(ik) :: nzdim, vertNdx
    real(dk)    :: Tadj
    real(dk), allocatable  :: modelTemp(:)
    class(abs_1d_grid_t), pointer :: zGrid
    class(abs_1d_grid_t), pointer :: lambdaGrid
    class(abs_Profile_t), pointer :: mdlTemperature
    type(string_t) :: Handle

    write(*,*) Iam,'entering'

    Handle = 'Vertical Z'             ; zGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength' ; lambdaGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Temperature'            ; mdlTemperature => ProfileWareHouse%get_Profile( Handle )

    nzdim = zGrid%ncells_ + iONE
    if( present(atMidPoint) ) then
      if( atMidpoint ) then
        nzdim = nzdim - iONE
        modelTemp = mdlTemperature%mid_val_
      else
        modelTemp = mdlTemperature%edge_val_
      endif
    else
      modelTemp = mdlTemperature%edge_val_
    endif

    allocate( cross_section(lambdaGrid%ncells_,nzdim) )
    cross_section = rZERO

    associate( polyCoeff => this%cross_section_parms(1)%array )
    do vertNdx = iONE,nzdim
      Tadj = modelTemp(vertNdx) - Thold
      cross_section(:,vertNdx) = polyCoeff(:,1)*(rONE + Tadj*(polyCoeff(:,2) + Tadj*polyCoeff(:,3)))
    enddo
    end associate

    cross_section = transpose( cross_section )

    write(*,*) Iam,'exiting'

  end function run

end module tuvx_cross_section_clono2
