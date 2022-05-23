! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This n2o5+hv->no2_no3 cross_section module

!> The n2o5+hv->no2+no3_cross_section type and related functions
module micm_radXfer_n2o5_no2_no3_cross_section_type

  use micm_radXfer_base_cross_section_type,    only : base_cross_section_t

  implicit none

  private
  public :: n2o5_no2_no3_cross_section_t

  !> Calculator for base_cross_section
  type, extends(base_cross_section_t) :: n2o5_no2_no3_cross_section_t
  contains
    !> Calculate the cross section
    procedure :: calculate => run
  end type n2o5_no2_no3_cross_section_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the photorate cross section for a given set of environmental conditions
  function run( this, gridWareHouse, ProfileWareHouse, atMidPoint ) result( cross_section )

    use musica_constants,           only : dk => musica_dk, ik => musica_ik, lk => musica_lk
    use micm_grid_warehouse,        only : grid_warehouse_t
    use micm_1d_grid,               only : abs_1d_grid_t
    use micm_Profile_warehouse,     only : Profile_warehouse_t
    use micm_Profile,               only : abs_Profile_t
    use musica_string,              only : string_t

    !> Arguments
    class(n2o5_no2_no3_cross_section_t), intent(in) :: this
    logical(lk), optional, intent(in)               :: atMidPoint
    !> The warehouses
    type(grid_warehouse_t), intent(inout)           :: gridWareHouse
    type(Profile_warehouse_t), intent(inout)        :: ProfileWareHouse
    !> Calculated cross section
    real(kind=dk), allocatable                      :: cross_section(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = 'n2o5_no2_no3 cross section calculate: '
    integer(ik), parameter  :: iONE = 1_ik
    real(dk), parameter  :: rZERO  = 0.0_dk
    real(dk), parameter  :: rTEN   = 10.0_dk
    real(dk), parameter  :: Tfloor = 233._dk
    real(dk), parameter  :: Tceil  = 300._dk
    real(dk), parameter  :: Tsf    = 1000._dk

    integer(ik) :: lambdaNdx, nzdim, vertNdx
    real(dk)    :: Tadj, Tfac
    real(dk), allocatable  :: modelTemp(:)
    class(abs_1d_grid_t), pointer :: zGrid
    class(abs_1d_grid_t), pointer :: lambdaGrid
    class(abs_Profile_t), pointer :: mdlTemperature
    type(string_t)     :: Handle

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
      Tadj = max( Tfloor,min(modelTemp(vertNdx),Tceil) )
      do lambdaNdx = iONE,lambdaGrid%ncells_
        Tfac = Tsf*this%cross_section_parms(2)%array(lambdaNdx,1)*(Tceil - Tadj)/(Tceil*Tadj)
        cross_section(lambdaNdx,vertNdx) = this%cross_section_parms(1)%array(lambdaNdx,1) * rTEN**(Tfac)
      enddo
    enddo

    cross_section = transpose( cross_section )

    write(*,*) Iam,'exiting'

  end function run

end module micm_radXfer_n2o5_no2_no3_cross_section_type
