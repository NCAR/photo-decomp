! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This h2o2_oh_oh_cross_section module

!> The h2o2+hv->oh_oh cross_section type and related functions
module tuvx_cross_section_h2o2_oh_oh

  use tuvx_cross_section,              only : cross_section_t

  implicit none

  private
  public :: cross_section_h2o2_oh_oh_t

  !> Calculator for base_cross_section
  type, extends(cross_section_t) :: cross_section_h2o2_oh_oh_t
  contains
    !> Calculate the cross section
    procedure :: calculate => run
  end type cross_section_h2o2_oh_oh_t

  !> Constructor
  interface cross_section_h2o2_oh_oh_t
    module procedure constructor
  end interface cross_section_h2o2_oh_oh_t

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
    call assert_msg( 969725098,                                               &
                     config%validate( required_keys, optional_keys ),         &
                     "Bad configuration data format for "//                   &
                     "h2o2+hv->oh+oh cross section." )
    allocate( cross_section_h2o2_oh_oh_t :: this )
    call base_constructor( this, config, grid_warehouse, profile_warehouse )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the cross section for a given set of environmental conditions
  !!
  !! \todo provide reference and description
  function run( this, grid_warehouse, profile_warehouse, at_mid_point )       &
      result( cross_section )

    use musica_constants,              only : dk => musica_dk
    use musica_string,                 only : string_t
    use tuvx_grid,                     only : grid_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile,                  only : profile_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    !> Calculated cross section
    real(kind=dk), allocatable                       :: cross_section(:,:)
    !> Cross section calculator
    class(cross_section_h2o2_oh_oh_t), intent(in)    :: this
    !> Grid warehouse
    type(grid_warehouse_t),            intent(inout) :: grid_warehouse
    !> Profile warehouse
    type(profile_warehouse_t),         intent(inout) :: profile_warehouse
    !> Flag indicating whether cross-section data should be at mid-points on
    !! the wavelength grid.
    !!
    !! If this is false or omitted, cross-section data are calculated at
    !! interfaces on the wavelength grid.
    logical, optional,                 intent(in)    :: at_mid_point

    ! local variables
    real(dk), parameter ::  rONE = 1.0_dk
    real(dk), parameter ::  A0 = 6.4761E+04_dk
    real(dk), parameter ::  A1 = -9.2170972E+02_dk
    real(dk), parameter ::  A2 = 4.535649_dk
    real(dk), parameter ::  A3 = -4.4589016E-03_dk
    real(dk), parameter ::  A4 = -4.035101E-05_dk
    real(dk), parameter ::  A5 = 1.6878206E-07_dk
    real(dk), parameter ::  A6 = -2.652014E-10_dk
    real(dk), parameter ::  A7 = 1.5534675E-13_dk

    real(dk), parameter ::  B0 = 6.8123E+03_dk
    real(dk), parameter ::  B1 = -5.1351E+01_dk
    real(dk), parameter ::  B2 = 1.1522E-01_dk
    real(dk), parameter ::  B3 = -3.0493E-05_dk
    real(dk), parameter ::  B4 = -1.0924E-07_dk

    character(len=*), parameter :: Iam =                                      &
        'h2o2+hv->oh+oh cross section calculate'
    integer    :: vertNdx, wNdx
    real(dk)       :: lambda, sumA, sumB, t, chi, xs
    type(string_t) :: Handle
    class(grid_t),    pointer :: zGrid => null( )
    class(grid_t),    pointer :: lambdaGrid => null( )
    class(profile_t), pointer :: temperature => null( )

    Handle = 'Vertical Z'
    zGrid => grid_warehouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength'
    lambdaGrid => grid_warehouse%get_grid( Handle )
    Handle = 'Temperature'
    temperature => profile_warehouse%get_Profile( Handle )

    allocate( cross_section( lambdaGrid%ncells_, zGrid%ncells_ + 1 ) )

    associate( wl => lambdaGrid%edge_, wc => lambdaGrid%mid_ )
    do vertNdx = 1, zGrid%ncells_ + 1
      do wNdx = 1, lambdaGrid%ncells_
        ! Parameterization (JPL94)
        ! Range 260-350 nm; 200-400 K
        if( wl( wNdx ) >= 260._dk .and. wl( wNdx ) < 350._dk ) then
           lambda = wc( wNdx )
           sumA = ( ( ( ( ( ( A7 * lambda + A6 ) * lambda + A5 ) * lambda     &
                  + A4 ) * lambda + A3 ) * lambda + A2 ) * lambda             &
                  + A1 ) * lambda + A0
           sumB = ( ( ( B4 * lambda + B3 ) * lambda + B2 ) * lambda + B1 )    &
                  * lambda + B0
           t = min( max( temperature%edge_val_( vertNdx ), 200._dk ), 400._dk )
           chi = rONE / ( rONE + exp( -1265._dk / t ) )
           cross_section( wNdx, vertNdx ) =                                   &
               ( chi * sumA + ( rONE - chi ) * sumB ) * 1.E-21_dk
         else
           cross_section( wNdx, vertNdx ) =                                   &
               this%cross_section_parms(1)%array( wNdx, 1 )
         endif
      enddo
    enddo
    end associate

    cross_section = transpose( cross_section )

    deallocate( zGrid )
    deallocate( lambdaGrid )
    deallocate( temperature )

  end function run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_cross_section_h2o2_oh_oh
