! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This ch3ono2->ch3o+no2 cross_section module

!> The ch3ono2->ch3o+no2_cross_section type and related functions
module tuvx_cross_section_ch3ono2_ch3o_no2

  use tuvx_cross_section_base,    only : base_cross_section_t

  implicit none

  private
  public :: ch3ono2_ch3o_no2_cross_section_t

  !> Calculator for ch3ono2-ch3o_no2 cross section
  type, extends(base_cross_section_t) :: ch3ono2_ch3o_no2_cross_section_t
  contains
    !> Calculate the cross section
    procedure :: calculate => run
  end type ch3ono2_ch3o_no2_cross_section_t

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
    class(ch3ono2_ch3o_no2_cross_section_t), intent(in) :: this
    logical(lk), optional, intent(in)        :: atMidPoint
    !> The warehouses
    type(grid_warehouse_t), intent(inout)    :: gridWareHouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse
    !> Calculated cross section
    real(dk), allocatable                    :: cross_section(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = 'ch3ono2->ch3o+no2 cross section run: '
    integer(ik), parameter :: iONE = 1_lk
    real(dk), parameter    :: rZERO = 0.0_dk
    real(dk), parameter    :: T0    = 298._dk
    real(dk)               :: Temp
    integer(ik)            :: nzdim
    integer(ik)            :: vertNdx
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

    do vertNdx = iONE,nzdim
      Temp = modelTemp(vertNdx) - T0
      cross_section(:,vertNdx) = this%cross_section_parms(1)%array(:,1)*exp( this%cross_section_parms(1)%array(:,2)*Temp )
    enddo

    cross_section = transpose( cross_section )

    write(*,*) Iam,'exiting'

  end function run

end module tuvx_cross_section_ch3ono2_ch3o_no2
