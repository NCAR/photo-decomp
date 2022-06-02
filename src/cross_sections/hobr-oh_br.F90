! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This hobr-oh_br cross_section module

!> The hobr-oh_br_cross_section type and related functions
module tuvx_cross_section_hobr_oh_br

  use tuvx_cross_section, only : base_cross_section_t, base_constructor

  implicit none

  private
  public :: hobr_oh_br_cross_section_t

  !> Calculator for hobr-oh_br cross section
  type, extends(base_cross_section_t) :: hobr_oh_br_cross_section_t
  contains
    !> Calculate the cross section
    procedure :: calculate => run
  end type hobr_oh_br_cross_section_t

  !> Constructor
  interface hobr_oh_br_cross_section_t
    module procedure constructor
  end interface hobr_oh_br_cross_section_t

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

    allocate ( hobr_oh_br_cross_section_t :: this )
    call base_constructor( this, config, gridWareHouse, ProfileWareHouse, atMidPoint )
  end function

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the photorate cross section for a given set of environmental conditions
  function run( this, gridWareHouse, ProfileWareHouse, atMidPoint ) result( cross_section )

    use musica_constants,           only : dk => musica_dk, ik => musica_ik, lk => musica_lk
    use tuvx_grid_warehouse,        only : grid_warehouse_t
    use tuvx_grid,               only : abs_1d_grid_t
    use tuvx_profile_warehouse,     only : Profile_warehouse_t
    use musica_string,              only : string_t

    !> Arguments
    class(hobr_oh_br_cross_section_t), intent(in)  :: this
    logical(lk), optional, intent(in)        :: atMidPoint
    !> The warehouses
    type(grid_warehouse_t), intent(inout)    :: gridWareHouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse
    !> Calculated cross section
    real(kind=dk), allocatable               :: cross_section(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = 'hobr_oh_br cross section calculate: '
    integer(ik), parameter :: iONE = 1_ik
    real(dk), parameter :: rZERO = 0.0_dk
    real(dk), parameter :: a = -2.359E-3_dk
    real(dk), parameter :: b = 1.2478_dk
    real(dk), parameter :: c = -210.4_dk

    integer(ik)            :: nzdim
    integer(ik)            :: vertNdx
    real(dk), allocatable  :: wrkCrossSection(:)
    class(abs_1d_grid_t), pointer :: zGrid
    class(abs_1d_grid_t), pointer :: lambdaGrid
    type(string_t) :: Handle

    write(*,*) Iam,'entering'

    Handle = 'Vertical Z'             ; zGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength' ; lambdaGrid => gridWareHouse%get_grid( Handle )

    nzdim = zGrid%ncells_ + iONE
    if( present(atMidPoint) ) then
      if( atMidpoint ) then
        nzdim = nzdim - iONE
      endif
    endif

    allocate( cross_section(lambdaGrid%ncells_,nzdim) )
    allocate( wrkCrossSection(lambdaGrid%ncells_) )

    associate( wc => lambdaGrid%mid_ )
    where( wc >= 250._dk .and. wc <= 550._dk )
      wrkCrossSection = &
               24.77_dk * exp( -109.80_dk*(log(284.01_dk/wc))**2 ) &
             + 12.22_dk * exp(  -93.63_dk*(log(350.57_dk/wc))**2 ) &
             + 2.283_dk * exp(- 242.40_dk*(log(457.38_dk/wc))**2 )
      wrkCrossSection = wrkCrossSection * 1.e-20_dk
    elsewhere
      wrkCrossSection = rZERO
    endwhere
    end associate

    do vertNdx = iONE,nzdim
      cross_section(:,vertNdx) = wrkCrossSection
    enddo

    cross_section = transpose( cross_section )

    write(*,*) Iam,'exiting'

  end function run

end module tuvx_cross_section_hobr_oh_br
