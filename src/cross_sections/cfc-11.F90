! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This cfc-11->Products cross_section module

!> The cfc-11->Products type and related functions
module tuvx_cross_section_cfc11

  use tuvx_cross_section,              only : cross_section_t

  implicit none

  private
  public :: cross_section_cfc11_t

  !> Calculator for cfc-11->Products cross section
  type, extends(cross_section_t) :: cross_section_cfc11_t
  contains
    !> Calculate the cross section
    procedure :: calculate => run
  end type cross_section_cfc11_t

  !> Constructor
  interface cross_section_cfc11_t
    module procedure constructor
  end interface cross_section_cfc11_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize the cross section
  function constructor( config, grid_warehouse, profile_warehouse )           &
      result ( this )

    use musica_config,                 only : config_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t
    use tuvx_cross_section,            only : base_constructor

    class(cross_section_t),    pointer       :: this
    type(config_t),            intent(inout) :: config
    type(grid_warehouse_t),    intent(inout) :: grid_warehouse
    type(profile_warehouse_t), intent(inout) :: profile_warehouse

    allocate (cross_section_cfc11_t :: this )
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
    real(kind=dk), allocatable                  :: cross_section(:,:)
    !> Cross section calculator
    class(cross_section_cfc11_t), intent(in)    :: this
    !> Grid warehouse
    type(grid_warehouse_t),       intent(inout) :: grid_warehouse
    !> Profile warehouse
    type(profile_warehouse_t),    intent(inout) :: profile_warehouse
    !> Flag indicating whether cross-section data should be at mid-points on
    !! the wavelength grid.
    !!
    !! If this is false or omitted, cross-section data are calculated at
    !! interfaces on the wavelength grid.
    logical, optional,            intent(in)    :: at_mid_point

    ! Local variables
    character(len=*), parameter :: Iam = 'cfc-11->Products cross section run: '
    real(dk), parameter :: rZERO    = 0.0_dk
    real(dk), parameter :: rONE     = 1.0_dk
    integer                   :: nzdim, vertNdx
    real(dk)                      :: Tadj
    real(dk), allocatable         :: modelTemp(:)
    class(grid_t), pointer :: zGrid
    class(grid_t), pointer :: lambdaGrid
    class(profile_t), pointer :: mdlTemperature
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
      Tadj = 1.e-4_dk * ( modelTemp( vertNdx ) - 298._dk )
      cross_section( :, vertNdx ) =                                           &
          this%cross_section_parms(1)%array(:,1)                              &
          * exp( ( lambdaGrid%mid_ - 184.9_dk ) * Tadj )
    enddo

    cross_section = transpose( cross_section )

  end function run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_cross_section_cfc11
