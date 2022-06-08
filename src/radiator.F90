! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The base_tuvx_radiator module

!> The base_radiator_t type and related functions
!!
module tuvx_radiator

  use musica_constants,       only : dk => musica_dk, ik => musica_ik, lk => musica_lk
  use musica_string,          only : string_t

  implicit none

  private
  public :: base_radiator_t, base_constructor, radiator_ptr, radiator_state_t

  !> radiator state type
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

  !> base radiator type
  type :: base_radiator_t
    type(string_t)         :: handle_
    type(radiator_state_t) :: state_
    !> Name of the vertical profile to use
    type(string_t) :: vertical_profile_name_
    !> Name of the absorption cross-section to use
    type(string_t) :: cross_section_name_
  contains
    !> Update radiator for new environmental conditions
    procedure :: upDateState
  end type base_radiator_t

  !> Pointer type for building sets of radiator objects
  type :: radiator_ptr
    class(base_radiator_t), pointer :: val_ => null( )
  end type radiator_ptr

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize radiator_t object
  subroutine base_constructor( this, radiator_config, gridWareHouse )

    use musica_assert,                 only : assert_msg
    use musica_config,        only : config_t
    use tuvx_grid_warehouse,  only : grid_warehouse_t
    use tuvx_grid,         only : grid_t

    !> radiator object
    class(base_radiator_t), intent(inout) :: this
    !> radiator configuration object
    type(config_t), intent(inout)         :: radiator_config
    !> grid warehouse
    type(grid_warehouse_t), intent(inout) :: gridWareHouse

    !> local variables
    character(len=*), parameter   :: Iam = "Radiator initialize: "
    type(string_t)                :: Handle
    class(grid_t), pointer :: zGrid, lambdaGrid
    type(string_t) :: required_keys(4), optional_keys(0)

    required_keys(1) = "name"
    required_keys(2) = "type"
    required_keys(3) = "cross section"
    required_keys(4) = "vertical profile"
    call assert_msg( 691711954,                                               &
                     radiator_config%validate( required_keys, optional_keys ),&
                     "Bad configuration data format for "//                   &
                     "base radiator." )

    write(*,*) ' '
    write(*,*) Iam,'entering'

    Handle = 'Vertical Z' ; zGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength' ; lambdaGrid => gridWareHouse%get_grid( Handle )

!-----------------------------------------------------------------------------
!> Get radiator "Handle"
!-----------------------------------------------------------------------------
    call radiator_config%get( 'name', this%handle_, Iam )
    write(*,*) Iam // 'handle = ',this%handle_%to_char()

    call radiator_config%get( 'vertical profile', this%vertical_profile_name_, &
                              Iam )
    call radiator_config%get( 'cross section', this%cross_section_name_, Iam )

!> allocate radiator state_ variables
    allocate( this%state_%layer_OD_(zGrid%ncells_,lambdaGrid%ncells_) )
    allocate( this%state_%layer_SSA_(zGrid%ncells_,lambdaGrid%ncells_) )
    allocate( this%state_%layer_G_(zGrid%ncells_,lambdaGrid%ncells_) )
    write(*,*) Iam // 'state_%layer_OD_ is allocated = ',allocated(this%state_%layer_OD_)
    write(*,*) Iam // 'state_%layer_SSA_ is allocated = ',allocated(this%state_%layer_SSA_)
    write(*,*) Iam // 'state_%layer_G_ is allocated = ',allocated(this%state_%layer_G_)
    write(*,*) Iam // 'state_%layer_OD_ is (',size(this%state_%layer_OD_,dim=1),' x ',size(this%state_%layer_OD_,dim=2),')'

    write(*,*) ' '
    write(*,*) Iam,'exiting'

  end subroutine base_constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Update radiator state
  subroutine upDateState( this, gridWareHouse, ProfileWareHouse, radXferXsectWareHouse )

    use musica_assert,                 only : die_msg
    use tuvx_profile_warehouse,        only : Profile_warehouse_t
    use tuvx_profile,                  only : profile_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_grid,                  only : grid_t
    use tuvx_cross_section, only : cross_section_t
    use tuvx_diagnostic_util,                         only : diagout
    use tuvx_cross_section_warehouse,  only : cross_section_warehouse_t

    !> Arguments
    !> radiator obj
    class(base_radiator_t), intent(inout) :: this
    !> Grid warehouse
    type(grid_warehouse_t), intent(inout) :: gridWareHouse
    !> Profile warehouse
    type(Profile_warehouse_t), intent(inout)       :: ProfileWareHouse
    type(cross_section_warehouse_t), intent(inout) :: radXferXsectWareHouse
    !> Radiator state

    !> Local variables
    real(dk) , parameter  :: km2cm = 1.e5_dk

    integer(ik) :: wNdx
    real(dk), allocatable :: CrossSection(:,:)
    character(len=*), parameter :: Iam = 'base radiator upDateState: '
    type(string_t)      :: Handle
    class(grid_t), pointer :: zGrid
    class(grid_t), pointer :: lambdaGrid
    class(profile_t), pointer  :: radiatorProfile
    class(cross_section_t), pointer :: radiatorCrossSection

    write(*,*) ' '
    write(*,*) Iam,'entering'

    write(*,*) Iam // 'handle = ',this%handle_%to_char()
!-----------------------------------------------------------------------------
!> get specific grids and profiles
!-----------------------------------------------------------------------------
    Handle = 'Vertical Z' ; zGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength' ; lambdaGrid => gridWareHouse%get_grid( Handle )
    write(*,*) Iam // 'nlyr,nbins = ',zGrid%ncells_,lambdaGrid%ncells_

    radiatorProfile => ProfileWareHouse%get_Profile( this%vertical_profile_name_ )

    radiatorCrossSection =>                                                   &
      radXferXsectWareHouse%get( this%cross_section_name_ )

    !> check radiator state type allocation
    if( .not. allocated( this%state_%layer_OD_ ) ) then
      call die_msg( 2222222,"In radiator%upDateState radiator state not allocate" )
    else
      write(*,*) Iam // 'radiator state is allocated'
    endif
    write(*,*) Iam // 'size OD = ',size(this%state_%layer_OD_,dim=1),' x ', &
                                  size(this%state_%layer_OD_,dim=2)

    !> set radiator state members
    CrossSection = radiatorCrossSection%calculate( gridWareHouse, ProfileWareHouse, at_mid_point=.true._lk )
    call diagout( 'o2xs.new',CrossSection )
    do wNdx = 1,lambdaGrid%ncells_
      this%state_%layer_OD_(:,wNdx) = radiatorProfile%layer_dens_ * CrossSection(:,wNdx)
    enddo

    !> Settings for a gas phase radiator
    if( this%handle_ == 'air' ) then
      this%state_%layer_SSA_ = 1._dk
      this%state_%layer_G_   = 0._dk
    else
      this%state_%layer_SSA_ = 0._dk
      this%state_%layer_G_   = 0._dk
    endif

    write(*,*) ' '
    write(*,*) Iam,'exiting'

  end subroutine upDateState

  !> Finalize the radiator state obj
  subroutine finalize( this )

    !> object declaration
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

end module tuvx_radiator
