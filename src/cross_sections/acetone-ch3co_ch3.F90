! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This ch3coch3+hv->ch3co_ch3 cross_section module

!> The ch3coch3+hv->ch3co+ch3_cross_section type and related functions
module tuvx_cross_section_ch3coch3_ch3co_ch3

  use tuvx_cross_section,              only : cross_section_t

  implicit none

  private
  public :: cross_section_ch3coch3_ch3co_ch3_t

  !> Calculator for acetone cross_section
  type, extends(cross_section_t) :: cross_section_ch3coch3_ch3co_ch3_t
  contains
    !> Initialize the cross section
    procedure :: calculate => run
  end type cross_section_ch3coch3_ch3co_ch3_t

  !> Constructor
  interface cross_section_ch3coch3_ch3co_ch3_t
    module procedure constructor
  end interface cross_section_ch3coch3_ch3co_ch3_t

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

    type(string_t) :: required_keys(2), optional_keys(3)

    required_keys(1) = "type"
    required_keys(2) = "netcdf files"
    optional_keys(1) = "lower extrapolation"
    optional_keys(2) = "upper extrapolation"
    optional_keys(3) = "name"
    call assert_msg( 577238139,                                               &
                     config%validate( required_keys, optional_keys ),         &
                     "Bad configuration data format for "//                   &
                     "ch3coch3+hv->ch3co+ch3 cross section." )
    allocate ( cross_section_ch3coch3_ch3co_ch3_t :: this )
    call base_constructor( this, config, grid_warehouse, profile_warehouse )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the cross section for a given set of environmental conditions
  !!     qyacet - q.y. for acetone, based on Blitz et al. (2004)
  !! Compute acetone quantum yields according to the parameterization of:
  !! Blitz, M. A., D. E. Heard, M. J. Pilling, S. R. Arnold,
  !! and M. P. Chipperfield
  !!       (2004), Pressure and temperature-dependent quantum yields for the
  !!       photodissociation of acetone between 279 and 327.5 nm, Geophys.
  !!       Res. Lett., 31, L06111, doi:10.1029/2003GL018793.
  function run( this, grid_warehouse, profile_warehouse, at_mid_point )       &
      result( cross_section )

    use musica_assert,                 only : die_msg
    use musica_constants,              only : dk => musica_dk
    use musica_string,                 only : string_t
    use tuvx_grid,                     only : grid_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile,                  only : profile_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    !> Calculated cross section
    real(kind=dk), allocatable               :: cross_section(:,:)
    !> Cross section calculator
    class(cross_section_ch3coch3_ch3co_ch3_t), intent(in) :: this
    !> Grid warehouse
    type(grid_warehouse_t),    intent(inout) :: grid_warehouse
    !> Profile warehouse
    type(profile_warehouse_t), intent(inout) :: profile_warehouse
    !> Flag indicating whether cross-section data should be at mid-points on
    !! the wavelength grid.
    !!
    !! If this is false or omitted, cross-section data are calculated at
    !! interfaces on the wavelength grid.
    logical, optional,         intent(in)    :: at_mid_point

    ! Local variables
    character(len=*), parameter :: Iam =                                      &
        'ch3coch3+hv->ch3co+ch3 cross section calculate'
    real(dk), parameter :: rZERO    = 0.0_dk
    real(dk), parameter :: rONE     = 1.0_dk
    integer                   :: nzdim, vertNdx
    real(dk)                      :: Tadj
    real(dk),         allocatable :: modelTemp(:)
    class(grid_t),    pointer     :: zGrid => null( )
    class(grid_t),    pointer     :: lambdaGrid => null( )
    class(profile_t), pointer     :: mdlTemperature => null( )
    type(string_t)                :: Handle
    character(len=:), allocatable :: msg

    Handle = 'Vertical Z'
    zGrid => grid_warehouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength'
    lambdaGrid => grid_warehouse%get_grid( Handle )
    Handle = 'Temperature'
    mdlTemperature => profile_warehouse%get_Profile( Handle )

    nzdim = zGrid%ncells_ + 1
    if( present(at_mid_point) ) then
      if( at_mid_point ) then
        nzdim = nzdim - 1
        modelTemp = mdlTemperature%mid_val_
      else
        modelTemp = mdlTemperature%edge_val_
      endif
    else
      modelTemp = mdlTemperature%edge_val_
    endif

    allocate( cross_section( lambdaGrid%ncells_, nzdim ) )
    cross_section = rZERO

    if( size( this%cross_section_parms(1)%array, dim = 2 ) == 4 ) then
      associate( coefficient => this%cross_section_parms(1)%array )
      do vertNdx = 1, nzdim
        Tadj = min( 298._dk, max( 235._dk, modelTemp( vertNdx ) ) )
        cross_section(:,vertNdx) = coefficient(:,1)                           &
                         * ( rONE + Tadj * ( coefficient(:,2)                 &
                                             + Tadj*(coefficient(:,3)         &
                                             + Tadj*coefficient(:,4) ) ) )
      enddo
      end associate
    else
      write(msg,*) Iam//'array must have 4 parameters'
      call die_msg( 811958314, msg )
    endif

    cross_section = transpose( cross_section )

    deallocate( zGrid )
    deallocate( lambdaGrid )
    deallocate( mdlTemperature )

  end function run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_cross_section_ch3coch3_ch3co_ch3
