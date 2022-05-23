! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This hcfc+hv->products cross_section module

!> The hcfc+hv->products_cross_section type and related functions
module micm_radXfer_hcfc_cross_section_type

  use micm_radXfer_base_cross_section_type,    only : base_cross_section_t

  implicit none

  private
  public :: hcfc_cross_section_t

  !> Calculator for acetone cross_section
  type, extends(base_cross_section_t) :: hcfc_cross_section_t
  contains
    !> Initialize the cross section
    procedure :: calculate => run
  end type hcfc_cross_section_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the photorate cross section for a given set of environmental conditions
  function run( this, gridWareHouse, ProfileWareHouse, atMidPoint ) result( cross_section )
!     qyacet - q.y. for acetone, based on Blitz et al. (2004)
! Compute acetone quantum yields according to the parameterization of:
! Blitz, M. A., D. E. Heard, M. J. Pilling, S. R. Arnold, and M. P. Chipperfield 
!       (2004), Pressure and temperature-dependent quantum yields for the 
!       photodissociation of acetone between 279 and 327.5 nm, Geophys. 
!       Res. Lett., 31, L06111, doi:10.1029/2003GL018793.

    use musica_constants,           only : dk => musica_dk, ik => musica_ik, lk => musica_lk
    use micm_grid_warehouse,        only : grid_warehouse_t
    use micm_1d_grid,               only : abs_1d_grid_t
    use micm_Profile_warehouse,     only : Profile_warehouse_t
    use micm_Profile,               only : abs_Profile_t
    use musica_string,              only : string_t

    !> Arguments
    class(hcfc_cross_section_t), intent(in)  :: this
    logical(lk), optional, intent(in)        :: atMidPoint
    !> The warehouses
    type(grid_warehouse_t), intent(inout)    :: gridWareHouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse
    !> Calculated cross section
    real(kind=dk), allocatable               :: cross_section(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = 'cf3chcl2+hv->products cross section calculate: '
    integer(ik), parameter :: iONE = 1_lk
    real(dk), parameter    :: rZERO = 0.0_dk
    real(dk), parameter    :: LBar  = 206.214_dk
    integer(ik)                   :: nzdim, vertNdx
    integer(ik)                   :: lambdaNdx, polyNdx
    real(dk)                      :: Tadj, sigma, uLambda
    real(dk), allocatable         :: modelTemp(:)
    class(abs_1d_grid_t), pointer :: zGrid
    class(abs_1d_grid_t), pointer :: lambdaGrid
    class(abs_Profile_t), pointer :: mdlTemperature
    type(string_t)                :: Handle

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

    uLambda = this%cross_section_parms(1)%temperature(2)
vert_loop: &
    do vertNdx = iONE,nzdim
      Tadj = min( 295._dk,max( 203._dk,modelTemp(vertNdx) ) ) &
             - this%cross_section_parms(1)%temperature(1)
lambda_loop: &
      do lambdaNdx = iONE,lambdaGrid%ncells_
        if( lambdaGrid%mid_(lambdaNdx) >= 190._dk &
            .and. lambdaGrid%mid_(lambdaNdx) <= uLambda ) then
          sigma = rZERO
          associate( coefficient => this%cross_section_parms(1)%array )
          do polyNdx = 1,size(coefficient,dim=1)
            sigma = sigma &
                  + (coefficient(polyNdx,1) + Tadj*(coefficient(polyNdx,2) + Tadj*coefficient(polyNdx,3))) &
                    * (lambdaGrid%mid_(lambdaNdx) - LBar)**(polyNdx-1)
          enddo
          end associate
          sigma = exp( sigma )
        else
          sigma = rZERO
        endif
        cross_section(lambdaNdx,vertNdx) = sigma
      enddo lambda_loop
    enddo vert_loop

    cross_section = transpose( cross_section )

    write(*,*) Iam,'exiting'

  end function run

end module micm_radXfer_hcfc_cross_section_type
