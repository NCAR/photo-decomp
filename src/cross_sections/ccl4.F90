! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This ccl4->Products cross_section module

!> The ccl4->Products_cross_section type and related functions
module tuvx_cross_section_ccl4

  use tuvx_cross_section_base,    only : base_cross_section_t

  implicit none

  private
  public :: ccl4_cross_section_t

  !> Calculator for ccl4 cross section
  type, extends(base_cross_section_t) :: ccl4_cross_section_t
  contains
    !> Calculate the cross section
    procedure :: calculate => run
  end type ccl4_cross_section_t

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
    class(ccl4_cross_section_t), intent(in)  :: this
    logical(lk), optional, intent(in)        :: atMidPoint
    !> The warehouses
    type(grid_warehouse_t), intent(inout)    :: gridWareHouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse
    !> Calculated cross section
    real(kind=dk), allocatable               :: cross_section(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = 'ccl4 cross section run: '
    integer(ik), parameter :: iONE  = 1_ik
    real(dk), parameter :: rZERO = 0.0_dk
    real(dk), parameter :: b0 = 1.0739_dk
    real(dk), parameter :: b1 = -1.6275e-2_dk
    real(dk), parameter :: b2 = 8.8141e-5_dk
    real(dk), parameter :: b3 = -1.9811e-7_dk
    real(dk), parameter :: b4 = 1.5022e-10_dk

    real(dk)    :: Temp, Wpoly, w1
    integer(ik) :: lambdaNdx, vertNdx, nzdim
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

    do vertNdx = 1,nzdim
      Temp = max( min( 300._dk,modelTemp(vertNdx) ),210._dk )
      Temp = Temp - 295._dk 
      do lambdaNdx = 1,lambdaGrid%ncells_
        w1 = lambdaGrid%mid_(lambdaNdx)
        if( w1 > 194._dk .and. w1 < 250._dk ) then
          Wpoly = b0 + w1*(b1 + w1*(b2 + w1*(b3 + b4*w1)))
          cross_section(lambdaNdx,vertNdx) = this%cross_section_parms(1)%array(lambdaNdx,1) * 10._dk**(Wpoly*Temp)
        else
          cross_section(lambdaNdx,vertNdx) = this%cross_section_parms(1)%array(lambdaNdx,1)
        endif
      enddo
    enddo

    cross_section = transpose( cross_section )

    write(*,*) Iam,'exiting'

  end function run

end module tuvx_cross_section_ccl4
