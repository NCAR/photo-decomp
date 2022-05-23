! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This n2o+hv->n2+o1d cross_section module

!> The n2o+hv->n2+o1d_cross_section type and related functions
module micm_radXfer_n2o_n2_o1d_cross_section_type

  use micm_radXfer_base_cross_section_type,    only : base_cross_section_t

  implicit none

  private
  public :: n2o_n2_o1d_cross_section_t

  !> Calculator for base_cross_section
  type, extends(base_cross_section_t) :: n2o_n2_o1d_cross_section_t
  contains
    !> Calculate the cross section
    procedure :: calculate => run
  end type n2o_n2_o1d_cross_section_t

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
    class(n2o_n2_o1d_cross_section_t), intent(in) :: this
    logical(lk), optional, intent(in)             :: atMidPoint
    !> The warehouses
    type(grid_warehouse_t), intent(inout)         :: gridWareHouse
    type(Profile_warehouse_t), intent(inout)      :: ProfileWareHouse
    !> Calculated cross section
    real(kind=dk), allocatable                    :: cross_section(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = 'n2o_n2+o1d cross section calculate: '

    integer(ik), parameter :: iONE = 1_ik
    real(dk), parameter :: rZERO = 0._dk
    real(dk), parameter :: A0 = 68.21023_dk
    real(dk), parameter :: A1 = -4.071805_dk
    real(dk), parameter :: A2 = 4.301146E-02_dk
    real(dk), parameter :: A3 = -1.777846E-04_dk
    real(dk), parameter :: A4 = 2.520672E-07_dk

    real(dk), parameter :: B0 = 123.4014_dk
    real(dk), parameter :: B1 = -2.116255_dk
    real(dk), parameter :: B2 = 1.111572E-02_dk
    real(dk), parameter :: B3 = -1.881058E-05_dk

    real(dk), parameter :: Tlower = 173._dk
    real(dk), parameter :: Tupper = 240._dk
    real(dk), parameter :: Tfloor = 194._dk
    real(dk), parameter :: Tceil  = 320._dk
    real(dk), parameter :: Thold  = 300._dk

    integer(ik) :: lambdaNdx, nzdim, vertNdx
    real(dk)    :: lambda, Tadj, A, B
    real(dk), allocatable  :: modelTemp(:)
    class(abs_1d_grid_t), pointer :: zGrid
    class(abs_1d_grid_t), pointer :: lambdaGrid
    class(abs_Profile_t), pointer :: mdlTemperature
    type(string_t)     :: Handle

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

    !>*** quantum yield of N(4s) and NO(2Pi) is less than 1% (Greenblatt and
    !>*** Ravishankara), so quantum yield of O(1D) is assumed to be unity
    do vertNdx = iONE,nzdim
      Tadj = max(Tfloor,min(modelTemp(vertNdx),Tceil))
      do lambdaNdx = 1,lambdaGrid%ncells_
        lambda = lambdaGrid%mid_(lambdaNdx)   
        if( lambda >= Tlower .and. lambda <= Tupper) then
          A = (((A4*lambda+A3)*lambda+A2)*lambda+A1)*lambda+A0
          B = ((B3*lambda+B2)*lambda+B1)*lambda+B0
          B = (Tadj - Thold)*exp(B)
          cross_section(lambdaNdx,vertNdx) = exp(A+B)
        endif
      enddo
    enddo

    cross_section = transpose( cross_section )

    write(*,*) Iam,'exiting'

  end function run

end module micm_radXfer_n2o_n2_o1d_cross_section_type
