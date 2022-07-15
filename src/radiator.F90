! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The base_tuvx_radiator module

!> The radiator_t type and related functions
module tuvx_radiator

  use musica_constants,                only : dk => musica_dk
  use musica_string,                   only : string_t

  implicit none

  private
  public :: radiator_t, radiator_ptr, radiator_state_t, base_constructor

  !> Optical properties for a radiator
  type :: radiator_state_t
    !> layer optical depth
    real(kind=dk), allocatable :: layer_OD_(:,:)
    !> layer single scattering albedo
    real(kind=dk), allocatable :: layer_SSA_(:,:)
    !> layer asymmetry factor
    real(kind=dk), allocatable :: layer_G_(:,:)
  contains
    final :: finalize
  end type radiator_state_t

  !> Optically active species
  type :: radiator_t
    type(string_t)         :: handle_
    !> Name of the vertical profile to use
    type(string_t)         :: vertical_profile_name_
    !> Units for the vertical profile
    type(string_t)         :: vertical_profile_units_
    !> Name of the absorption cross-section to use
    type(string_t)         :: cross_section_name_
    !> Optical properties
    type(radiator_state_t) :: state_
  contains
    !> Update radiator for new environmental conditions
    procedure :: update_state
  end type radiator_t

  interface radiator_t
    module procedure :: constructor
  end interface

  !> Pointer type for building sets of radiator objects
  type :: radiator_ptr
    class(radiator_t), pointer :: val_ => null( )
  end type radiator_ptr

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructs a base_radiator_t object
  function constructor( config, grid_warehouse ) result( new_radiator )

    use musica_config,                 only : config_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t

    !> New radiator object
    class(radiator_t),      pointer       :: new_radiator
    !> Radiator configuration
    type(config_t),         intent(inout) :: config
    !> Grid warehouse
    type(grid_warehouse_t), intent(inout) :: grid_warehouse

    allocate( new_radiator )
    call base_constructor( new_radiator, config, grid_warehouse )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initializes a radiator_t object
  !!
  !! This should only be called by subclasses of radiator_t
  subroutine base_constructor( this, config, grid_warehouse )

    use musica_assert,                 only : assert_msg
    use musica_config,                 only : config_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_grid,                     only : grid_t

    !> Radiator object
    class(radiator_t),      intent(inout) :: this
    !> Radiator configuration
    type(config_t),         intent(inout) :: config
    !> Grid warehouse
    type(grid_warehouse_t), intent(inout) :: grid_warehouse

    ! local variables
    character(len=*), parameter   :: Iam = "Base radiator constructor"
    class(grid_t),    pointer     :: z_grid, lambda_grid
    type(string_t)                :: required_keys(5), optional_keys(0)

    required_keys(1) = "name"
    required_keys(2) = "type"
    required_keys(3) = "cross section"
    required_keys(4) = "vertical profile"
    required_keys(5) = "vertical profile units"
    call assert_msg( 691711954,                                               &
                     config%validate( required_keys, optional_keys ),         &
                     "Bad configuration data format for "//                   &
                     "base radiator." )

    z_grid => grid_warehouse%get_grid( "height", "km" )
    lambda_grid => grid_warehouse%get_grid( "wavelength", "nm" )

    call config%get( 'name',             this%handle_,                Iam )
    call config%get( 'vertical profile', this%vertical_profile_name_, Iam )
    call config%get( 'vertical profile units', this%vertical_profile_units_,  &
                     Iam )
    call config%get( 'cross section',    this%cross_section_name_,    Iam )

    ! allocate radiator state variables
    allocate( this%state_%layer_OD_(  z_grid%ncells_, lambda_grid%ncells_ ) )
    allocate( this%state_%layer_SSA_( z_grid%ncells_, lambda_grid%ncells_ ) )
    allocate( this%state_%layer_G_(   z_grid%ncells_, lambda_grid%ncells_ ) )

    deallocate( z_grid      )
    deallocate( lambda_grid )

  end subroutine base_constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Update radiator state
  subroutine update_state( this, grid_warehouse, profile_warehouse,           &
      cross_section_warehouse )

    use musica_assert,                 only : assert_msg
    use tuvx_cross_section,            only : cross_section_t
    use tuvx_cross_section_warehouse,  only : cross_section_warehouse_t
    use tuvx_diagnostic_util,          only : diagout
    use tuvx_grid,                     only : grid_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile,                  only : profile_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    !> Radiator
    class(radiator_t),               intent(inout) :: this
    !> Grid warehouse
    type(grid_warehouse_t),          intent(inout) :: grid_warehouse
    !> Profile warehouse
    type(profile_warehouse_t),       intent(inout) :: profile_warehouse
    !> Cross section warehouse
    type(cross_section_warehouse_t), intent(inout) :: cross_section_warehouse

    ! Local variables
    character(len=*), parameter     :: Iam = 'Base radiator update state'
    real(dk) ,        parameter     :: km2cm = 1.e5_dk
    integer                         :: w_index
    real(dk),         allocatable   :: cross_section(:,:)
    class(grid_t),          pointer :: z_grid
    class(grid_t),          pointer :: lambda_grid
    class(profile_t),       pointer :: radiator_profile
    class(cross_section_t), pointer :: radiator_cross_section

    ! get specific grids and profiles
    z_grid => grid_warehouse%get_grid( "height", "km" )
    lambda_grid => grid_warehouse%get_grid( "wavelength", "nm" )

    radiator_profile =>                                                       &
      profile_warehouse%get_profile( this%vertical_profile_name_,             &
                                     this%vertical_profile_units_ )

    radiator_cross_section =>                                                 &
      cross_section_warehouse%get( this%cross_section_name_ )

    ! check radiator state type allocation
    call assert_msg( 345645215, allocated( this%state_%layer_OD_ ),           &
                     "Radiator state not allocated" )

    ! set radiator state members
    cross_section = radiator_cross_section%calculate( grid_warehouse,         &
                                                      profile_warehouse,      &
                                                      at_mid_point = .true. )
    call diagout( 'o2xs.new',cross_section )
    do w_index = 1,lambda_grid%ncells_
      this%state_%layer_OD_(:,w_index) = radiator_profile%layer_dens_         &
                                         * cross_section(:,w_index)
    enddo

    ! Settings for a gas phase radiator
    if( this%handle_ == 'air' ) then
      this%state_%layer_SSA_ = 1._dk
      this%state_%layer_G_   = 0._dk
    else
      this%state_%layer_SSA_ = 0._dk
      this%state_%layer_G_   = 0._dk
    endif

    deallocate( z_grid )
    deallocate( lambda_grid )
    deallocate( radiator_profile )
    deallocate( radiator_cross_section )

  end subroutine update_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalizes a radiator state object
  subroutine finalize( this )

    !> Radiator optical properties
    type(radiator_state_t) :: this

    if( allocated( this%layer_OD_ ) ) then
      deallocate( this%layer_OD_ )
    endif
    if( allocated( this%layer_SSA_ ) ) then
      deallocate( this%layer_SSA_ )
    endif
    if( allocated( this%layer_G_ ) ) then
      deallocate( this%layer_G_ )
    endif

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_radiator
