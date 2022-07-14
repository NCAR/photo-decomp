! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This n2o5+hv->no2_no3 cross_section module

!> The n2o5+hv->no2+no3_cross_section type and related functions
module tuvx_cross_section_n2o5_no2_no3

  use tuvx_cross_section,              only : cross_section_t

  implicit none

  private
  public :: cross_section_n2o5_no2_no3_t

  !> Calculator for base_cross_section
  type, extends(cross_section_t) :: cross_section_n2o5_no2_no3_t
  contains
    !> Calculate the cross section
    procedure :: calculate => run
  end type cross_section_n2o5_no2_no3_t

  !> Constructor
  interface cross_section_n2o5_no2_no3_t
    module procedure constructor
  end interface cross_section_n2o5_no2_no3_t

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
    call assert_msg( 456660628,                                               &
                     config%validate( required_keys, optional_keys ),         &
                     "Bad configuration data format for "//                   &
                     "n2o5_no2_no3 cross section." )
    allocate( cross_section_n2o5_no2_no3_t :: this )
    call base_constructor( this, config, grid_warehouse, profile_warehouse )

    ! make sure all NetCDF files have been included
    call assert_msg( 590380798, size( this%cross_section_parms ) >= 2,        &
                     "n2o5_no2_no3 cross section type requires 2 NetCDF "//   &
                     "files." )

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
    real(kind=dk), allocatable                         :: cross_section(:,:)
    !> Cross section calculator
    class(cross_section_n2o5_no2_no3_t), intent(in)    :: this
    !> Grid warehouse
    type(grid_warehouse_t),              intent(inout) :: grid_warehouse
    !> Profile warehouse
    type(profile_warehouse_t),           intent(inout) :: profile_warehouse
    !> Flag indicating whether cross-section data should be at mid-points on
    !! the wavelength grid.
    !!
    !! If this is false or omitted, cross-section data are calculated at
    !! interfaces on the wavelength grid.
    logical, optional,                   intent(in)    :: at_mid_point

    ! Local variables
    character(len=*), parameter :: Iam =                                      &
        'n2o5_no2_no3 cross section calculate'
    real(dk), parameter  :: rZERO  = 0.0_dk
    real(dk), parameter  :: rTEN   = 10.0_dk
    real(dk), parameter  :: Tfloor = 233._dk
    real(dk), parameter  :: Tceil  = 300._dk
    real(dk), parameter  :: Tsf    = 1000._dk

    integer :: lambdaNdx, nzdim, vertNdx
    real(dk)    :: Tadj, Tfac
    real(dk),         allocatable :: modelTemp(:)
    class(grid_t),    pointer     :: zGrid => null( )
    class(grid_t),    pointer     :: lambdaGrid => null( )
    class(profile_t), pointer     :: mdlTemperature => null( )
    type(string_t)                :: Handle

    Handle = 'Vertical Z'
    zGrid => grid_warehouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength'
    lambdaGrid => grid_warehouse%get_grid( Handle )
    Handle = 'Temperature'
    mdlTemperature => profile_warehouse%get_Profile( Handle )

    nzdim = zGrid%ncells_ + 1
    if( present( at_mid_point ) ) then
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

    do vertNdx = 1, nzdim
      Tadj = max( Tfloor, min( modelTemp( vertNdx ), Tceil ) )
      do lambdaNdx = 1, lambdaGrid%ncells_
        Tfac = Tsf * this%cross_section_parms(2)%array( lambdaNdx, 1 )        &
                   * ( Tceil - Tadj ) / ( Tceil * Tadj )
        cross_section( lambdaNdx, vertNdx ) =                                 &
            this%cross_section_parms(1)%array( lambdaNdx, 1 ) * rTEN**( Tfac )
      enddo
    enddo

    cross_section = transpose( cross_section )

    deallocate( zGrid )
    deallocate( lambdaGrid )
    deallocate( mdlTemperature )

  end function run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_cross_section_n2o5_no2_no3
