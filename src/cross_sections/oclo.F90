! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This oclo_cross_section module

!> The oclo_cross_section type and related functions
module tuvx_cross_section_oclo

  use tuvx_cross_section,              only : cross_section_t

  implicit none

  private
  public :: cross_section_oclo_t

  !> Calculator for oclo_cross_section
  type, extends(cross_section_t) :: cross_section_oclo_t
    !> The cross section array
  contains
    !> Calculate the cross section
    procedure :: calculate => run
  end type cross_section_oclo_t

  !> Constructor
  interface cross_section_oclo_t
    module procedure constructor
  end interface cross_section_oclo_t

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
    call assert_msg( 473027795,                                               &
                     config%validate( required_keys, optional_keys ),         &
                     "Bad configuration data format for "//                   &
                     "oclo cross section." )
    allocate( cross_section_oclo_t :: this )
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
    real(kind=dk), allocatable                 :: cross_section(:,:)
    !> Cross section calculator
    class(cross_section_oclo_t), intent(in)    :: this
    !> Grid warehouse
    type(grid_warehouse_t),      intent(inout) :: grid_warehouse
    !> Profile warehouse
    type(profile_warehouse_t),   intent(inout) :: profile_warehouse
    !> Flag indicating whether cross-section data should be at mid-points on
    !! the wavelength grid.
    !!
    !! If this is false or omitted, cross-section data are calculated at
    !! interfaces on the wavelength grid.
    logical, optional,           intent(in)    :: at_mid_point

    ! Local variables
    character(len=*), parameter :: Iam = 'oclo cross section calculate'
    real(dk), parameter    :: rZERO = 0.0_dk
    integer :: ndx, nParms
    integer :: lambdaNdx, vertNdx, nzdim
    real(dk)    :: Tfac
    real(dk), allocatable  :: wrkCrossSection(:)
    real(dk), allocatable  :: modelTemp(:)
    class(grid_t), pointer :: zGrid
    class(grid_t), pointer :: lambdaGrid
    class(profile_t), pointer :: mdlTemperature
    type(string_t) :: Handle

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
    allocate( wrkCrossSection( lambdaGrid%ncells_ ) )
    cross_section = rZERO

    associate( Temp => modelTemp, Xsection => this%cross_section_parms )
    nParms = size( Xsection )
    do vertNdx = 1, nzdim
      if( Temp( vertNdx ) <= Xsection(1)%temperature(1) ) then
        wrkCrossSection = Xsection(1)%array(:,1)
      elseif( Temp( vertNdx ) >= Xsection( nParms )%temperature(1) ) then
        wrkCrossSection = Xsection( nParms )%array(:,1)
      else
        do ndx = 2, nParms
          if( Xsection( ndx )%temperature(1) > Temp( vertNdx ) ) then
            exit
          endif
        enddo
        ndx = ndx - 1
        Tfac = ( Temp( vertNdx ) - Xsection( ndx )%temperature(1) )           &
               / ( Xsection( ndx + 1 )%temperature(1)                         &
                   - Xsection( ndx )%temperature(1) )
        wrkCrossSection = Xsection( ndx )%array(:,1)                          &
                        + Tfac * ( Xsection( ndx + 1 )%array(:,1)             &
                                   - Xsection( ndx )%array(:,1) )
      endif
      cross_section( :, vertNdx ) = wrkCrossSection
    enddo
    end associate

    cross_section = transpose( cross_section )

  end function run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_cross_section_oclo
