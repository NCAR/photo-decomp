! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This hcfc+hv->products cross_section module

!> The hcfc+hv->products_cross_section type and related functions
module tuvx_cross_section_hcfc

  use tuvx_cross_section,              only : cross_section_t

  implicit none

  private
  public :: cross_section_hcfc_t

  !> Calculator for acetone cross_section
  type, extends(cross_section_t) :: cross_section_hcfc_t
  contains
    !> Initialize the cross section
    procedure :: calculate => run
  end type cross_section_hcfc_t

  !> Constructor
  interface cross_section_hcfc_t
    module procedure constructor
  end interface cross_section_hcfc_t

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

    type(string_t) :: required_keys(2), optional_keys(1)

    required_keys(1) = "type"
    required_keys(2) = "netcdf files"
    optional_keys(1) = "name"
    call assert_msg( 369759228,                                               &
                     config%validate( required_keys, optional_keys ),         &
                     "Bad configuration data format for "//                   &
                     "cf3chcl2+hv->products cross section." )
    allocate( cross_section_hcfc_t :: this )
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

    use musica_constants,              only : dk => musica_dk
    use musica_string,                 only : string_t
    use tuvx_grid,                     only : grid_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile,                  only : profile_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    !> Calculated cross section
    real(kind=dk), allocatable                 :: cross_section(:,:)
    !> Cross section calculator
    class(cross_section_hcfc_t), intent(in)    :: this
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
    character(len=*), parameter :: Iam =                                      &
        'cf3chcl2+hv->products cross section calculate'
    real(dk), parameter    :: rZERO = 0.0_dk
    real(dk), parameter    :: LBar  = 206.214_dk
    integer                   :: nzdim, vertNdx
    integer                   :: lambdaNdx, polyNdx
    real(dk)                      :: Tadj, sigma, uLambda
    real(dk),         allocatable :: modelTemp(:)
    class(grid_t),    pointer     :: zGrid => null( )
    class(grid_t),    pointer     :: lambdaGrid => null( )
    class(profile_t), pointer     :: mdlTemperature => null( )

    zGrid => grid_warehouse%get_grid( "height", "km" )
    lambdaGrid => grid_warehouse%get_grid( "wavelength", "nm" )
    mdlTemperature => profile_warehouse%get_profile( "temperature", "K" )

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

    uLambda = this%cross_section_parms(1)%temperature(2)
vert_loop:                                                                    &
    do vertNdx = 1, nzdim
      Tadj = min( 295._dk, max( 203._dk,modelTemp( vertNdx ) ) )              &
             - this%cross_section_parms(1)%temperature(1)
lambda_loop:                                                                  &
      do lambdaNdx = 1, lambdaGrid%ncells_
        if( lambdaGrid%mid_( lambdaNdx ) >= 190._dk                           &
            .and. lambdaGrid%mid_( lambdaNdx ) <= uLambda ) then
          sigma = rZERO
          associate( coefficient => this%cross_section_parms(1)%array )
          do polyNdx = 1, size( coefficient, dim = 1 )
            sigma = sigma                                                     &
                  + ( coefficient( polyNdx, 1 )                               &
                      + Tadj * ( coefficient( polyNdx, 2 )                    &
                                 + Tadj * coefficient( polyNdx, 3 ) ) )       &
                    * ( lambdaGrid%mid_( lambdaNdx ) - LBar )**( polyNdx - 1 )
          enddo
          end associate
          sigma = exp( sigma )
        else
          sigma = rZERO
        endif
        cross_section( lambdaNdx, vertNdx ) = sigma
      enddo lambda_loop
    enddo vert_loop

    cross_section = transpose( cross_section )

    deallocate( zGrid )
    deallocate( lambdaGrid )
    deallocate( mdlTemperature )

  end function run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_cross_section_hcfc
