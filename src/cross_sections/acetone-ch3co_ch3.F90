! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This ch3coch3+hv->ch3co_ch3 cross_section module

!> The ch3coch3+hv->ch3co+ch3_cross_section type and related functions
module tuvx_cross_section_ch3coch3_ch3co_ch3

  use tuvx_cross_section, only : base_cross_section_t, base_constructor

  implicit none

  private
  public :: ch3coch3_ch3co_ch3_cross_section_t

  !> Calculator for acetone cross_section
  type, extends(base_cross_section_t) :: ch3coch3_ch3co_ch3_cross_section_t
  contains
    !> Initialize the cross section
    procedure :: calculate => run
  end type ch3coch3_ch3co_ch3_cross_section_t

  !> Constructor
  interface ch3coch3_ch3co_ch3_cross_section_t
    module procedure constructor
  end interface ch3coch3_ch3co_ch3_cross_section_t

contains

  !> Initialize the cross section
  function constructor( config, gridWareHouse, ProfileWareHouse, atMidPoint ) result ( this )
 
    use musica_config,    only : config_t
    use musica_constants, only : lk => musica_lk
    use tuvx_grid_warehouse,    only : grid_warehouse_t
    use tuvx_profile_warehouse, only : Profile_warehouse_t
 
 
    !> Cross section calculator
    logical(lk), optional, intent(in)          :: atMidPoint
    class(base_cross_section_t), pointer  :: this
    type(config_t), intent(inout)              :: config
    type(grid_warehouse_t), intent(inout)      :: gridWareHouse
    type(Profile_warehouse_t), intent(inout)   :: ProfileWareHouse

    allocate ( ch3coch3_ch3co_ch3_cross_section_t :: this )
    call base_constructor( this, config, gridWareHouse, ProfileWareHouse, atMidPoint )
  end function constructor

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
    use tuvx_grid_warehouse,        only : grid_warehouse_t
    use tuvx_grid,               only : abs_1d_grid_t
    use tuvx_profile_warehouse,     only : Profile_warehouse_t
    use tuvx_profile,               only : abs_Profile_t
    use musica_string,              only : string_t
    use musica_assert,              only : die_msg

    !> Arguments
    class(ch3coch3_ch3co_ch3_cross_section_t), intent(in) :: this
    logical(lk), optional, intent(in)        :: atMidPoint
    !> The warehouses
    type(grid_warehouse_t), intent(inout)    :: gridWareHouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse
    !> Calculated cross section
    real(kind=dk), allocatable               :: cross_section(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = 'ch3coch3+hv->ch3co+ch3 cross section calculate: '
    integer(ik), parameter :: iONE  = 1_ik
    real(dk), parameter :: rZERO    = 0.0_dk
    real(dk), parameter :: rONE     = 1.0_dk
    integer(ik)                   :: nzdim, vertNdx
    real(dk)                      :: Tadj
    real(dk), allocatable         :: modelTemp(:)
    class(abs_1d_grid_t), pointer :: zGrid
    class(abs_1d_grid_t), pointer :: lambdaGrid
    class(abs_Profile_t), pointer :: mdlTemperature
    type(string_t)                :: Handle
    character(len=:), allocatable :: msg

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

    if( size(this%cross_section_parms(1)%array,dim=2) == 4 ) then
      associate( coefficient => this%cross_section_parms(1)%array )
      do vertNdx = iONE,nzdim
        Tadj = min( 298._dk,max( 235._dk,modelTemp(vertNdx) ) )
        cross_section(:,vertNdx) = coefficient(:,1) &
                         *(rONE + Tadj*(coefficient(:,2) + Tadj*(coefficient(:,3) + Tadj*coefficient(:,4))))
      enddo
      end associate
    else
      write(msg,*) Iam//'array must have 4 parameters'
      call die_msg( 500000001, msg )
    endif

    cross_section = transpose( cross_section )

    write(*,*) Iam,'exiting'

  end function run

end module tuvx_cross_section_ch3coch3_ch3co_ch3
