! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This nitroxy_acetone cross_section module

!> The nitroxy_acetone_cross_section type and related functions
module tuvx_cross_section_nitroxy_acetone

  use tuvx_cross_section,              only : cross_section_t

  implicit none

  private
  public :: cross_section_nitroxy_acetone_t

  !> Calculator for nitroxy_acetone cross section
  type, extends(cross_section_t) :: cross_section_nitroxy_acetone_t
  contains
    !> Calculate the cross section
    procedure :: calculate => run
  end type cross_section_nitroxy_acetone_t

  !> Constructor
  interface cross_section_nitroxy_acetone_t
    module procedure constructor
  end interface cross_section_nitroxy_acetone_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize the cross section
  function constructor( config, grid_warehouse, profile_warehouse )           &
      result ( this )

    use musica_assert,                 only : assert_msg
    use musica_config,                 only : config_t
    use musica_string,                 only : string_t
    use tuvx_cross_section,            only : base_constructor
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    class(cross_section_t),    pointer       :: this
    type(config_t),            intent(inout) :: config
    type(grid_warehouse_t),    intent(inout) :: grid_warehouse
    type(profile_warehouse_t), intent(inout) :: profile_warehouse

    type(string_t) :: required_keys(1), optional_keys(1)

    required_keys(1) = "type"
    optional_keys(1) = "name"
    call assert_msg( 170826606,                                               &
                     config%validate( required_keys, optional_keys ),         &
                     "Bad configuration data format for "//                   &
                     "nitroxy_acetone cross section." )
    allocate( cross_section_nitroxy_acetone_t :: this )
    call base_constructor( this, config, grid_warehouse, profile_warehouse )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the cross section for a given set of environmental conditions
  function run( this, grid_warehouse, profile_warehouse, at_mid_point )       &
      result( cross_section )

    use musica_constants,              only : dk => musica_dk
    use musica_string,                 only : string_t
    use tuvx_grid,                     only : grid_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile,                  only : profile_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    !> Calculated cross section
    real(dk), allocatable                                 :: cross_section(:,:)
    !> Cross section calculator
    class(cross_section_nitroxy_acetone_t), intent(in)    :: this
    !> Grid warehouse
    type(grid_warehouse_t),                 intent(inout) :: grid_warehouse
    !> Profile warehouse
    type(profile_warehouse_t),              intent(inout) :: profile_warehouse
    !> Flag indicating whether cross-section data should be at mid-points on
    !! the wavelength grid.
    !!
    !! If this is false or omitted, cross-section data are calculated at
    !! interfaces on the wavelength grid.
    logical, optional,                      intent(in)    :: at_mid_point

    ! Local variables
    character(len=*), parameter :: Iam =                                      &
        'nitroxy_acetone cross section calculate'
    real(dk), parameter :: rZERO = 0.0_dk
    real(dk), parameter :: a = -1.365E-3_dk
    real(dk), parameter :: b = 0.7834_dk
    real(dk), parameter :: c = -156.8_dk

    integer            :: nzdim
    integer            :: vertNdx
    real(dk),      allocatable :: wrkCrossSection(:)
    class(grid_t), pointer     :: zGrid => null( )
    class(grid_t), pointer     :: lambdaGrid => null( )

    zGrid => grid_warehouse%get_grid( "height", "km" )
    lambdaGrid => grid_warehouse%get_grid( "wavelength", "nm" )

    nzdim = zGrid%ncells_ + 1
    if( present( at_mid_point ) ) then
      if( at_mid_point ) then
        nzdim = nzdim - 1
      endif
    endif

    allocate( cross_section( lambdaGrid%ncells_, nzdim ) )
    allocate( wrkCrossSection( lambdaGrid%ncells_ ) )

    where( lambdaGrid%mid_ >= 284._dk .and. lambdaGrid%mid_ <= 335._dk )
      wrkCrossSection =                                                       &
          exp( c + lambdaGrid%mid_ * ( b + a * lambdaGrid%mid_ ) )
    elsewhere
      wrkCrossSection = rZERO
    endwhere

    do vertNdx = 1, nzdim
      cross_section( :, vertNdx ) = wrkCrossSection
    enddo

    cross_section = transpose( cross_section )

    deallocate( zGrid )
    deallocate( lambdaGrid )

  end function run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_cross_section_nitroxy_acetone
