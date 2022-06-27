! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The aerosol_tuvx_radiator module

!> The radiator_aerosol_t type and related functions
module tuvx_radiator_aerosol

  use musica_constants,                only : dk => musica_dk
  use musica_string,                   only : string_t
  use tuvx_radiator,                   only : radiator_t

  implicit none

  private
  public :: radiator_aerosol_t

  !> aerosol radiator type
  type, extends(radiator_t) :: radiator_aerosol_t
  contains
    !> Initialize radiator
    !> Update radiator for new environmental conditions
    procedure :: update_state
  end type radiator_aerosol_t

  !> Constructor
  interface radiator_aerosol_t
    module procedure constructor
  end interface radiator_aerosol_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize radiator_t object
  function constructor( config, grid_warehouse ) result( this )

    use musica_assert,                 only : assert_msg
    use musica_config,                 only : config_t
    use tuvx_constants,                only : nzero, pzero
    use tuvx_diagnostic_util,          only : diagout
    use tuvx_interpolate,              only : abs_interpolator_t, interp3_t
    use tuvx_grid,                     only : grid_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t

    !> Radiator object
    type(radiator_aerosol_t), pointer     :: this
    !> Radiator configuration object
    type(config_t), intent(inout)         :: config
    !> Grid warehouse
    type(grid_warehouse_t), intent(inout) :: grid_warehouse

    ! Local variables
    character(len=*), parameter   :: Iam = "Aerosol radiator constructor"
    real(dk), parameter           :: scaling_factor = 550._dk/340._dk

    integer                       :: k, nInputBins, binNdx
    real(dk)                      :: tau550, alpha, wscaling, ODscaling
    real(dk)                      :: coldens
    real(dk), allocatable         :: input_OD(:), rad_OD(:)
    real(dk)                      :: input_SSA
    real(dk)                      :: input_G
    real(dk), allocatable         :: input_zgrid(:)
    real(dk), allocatable         :: winput_SSA(:), winput_G(:)
    type(string_t)                :: required_keys(5), optional_keys(1)
    type(string_t)                :: Handle
    type(config_t)                :: Aerosol_config
    class(grid_t),             pointer :: zGrid, lambdaGrid
    class(abs_interpolator_t), pointer :: theInterpolator

    required_keys(1) = "type"
    required_keys(2) = "optical depths"
    required_keys(3) = "single scattering albedo"
    required_keys(4) = "asymmetry factor"
    required_keys(5) = "name"
    optional_keys(1) = "550 nm optical depth"
    call assert_msg( 584205621,                                               &
                     config%validate( required_keys, optional_keys ),&
                     "Bad configuration data format for "//                   &
                     "aerosol radiator." )

    allocate( this )

    Handle = 'Vertical Z'
    zGrid => grid_warehouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength'
    lambdaGrid => grid_warehouse%get_grid( Handle )

    ! Get radiator "Handle"
    call config%get( 'name', this%handle_, Iam )

    ! allocate radiator state variables
    allocate( this%state_%layer_OD_(  zGrid%ncells_, lambdaGrid%ncells_ ) )
    allocate( this%state_%layer_SSA_( zGrid%ncells_, lambdaGrid%ncells_ ) )
    allocate( this%state_%layer_G_(   zGrid%ncells_, lambdaGrid%ncells_ ) )

    ! set up the interpolator
    allocate( interp3_t :: theInterpolator )

    ! read json config
    call config%get( "optical depths", input_OD, Iam )
    nInputBins = size( input_OD )
    if( nInputBins > 1 ) then

      ! interpolate input OD to state variable
      call diagout( 'rawOD.new',input_OD )
      input_OD( : nInputBins - 1 ) =                                          &
          .5_dk * ( input_OD( : nInputBins - 1 ) + input_OD( 2 : ) )
      call diagout( 'inpaerOD.new', input_OD( : nInputBins - 1 ) )

      input_zgrid = (/ (real( k, dk ), k = 0, nInputBins - 1 ) /)
      rad_OD =                                                                &
          theInterpolator%interpolate( zGrid%edge_, input_zgrid, input_OD, 1 )
      call diagout( 'cz.aer.new', rad_OD )
      do binNdx = 1, lambdaGrid%ncells_
        this%state_%layer_OD_( :, binNdx ) = rad_OD
      enddo
    else
      this%state_%layer_OD_ = input_OD(1)
    endif

    ! interpolate input SSA to state variable
    call config%get( "single scattering albedo", input_SSA, Iam )
    winput_SSA = input_OD( : nInputBins - 1 ) * input_SSA
    this%state_%layer_SSA_( :, 1 ) =                                          &
        theInterpolator%interpolate( zGrid%edge_, input_zgrid,winput_SSA, 1 )
    call diagout( 'omz.aer.new', this%state_%layer_SSA_( :, 1 ) )
    do binNdx = 2, lambdaGrid%ncells_
      this%state_%layer_SSA_(:,binNdx) = this%state_%layer_SSA_(:,1)
    enddo

    ! interpolate input G to state variable
    call config%get( "asymmetry factor", input_G, Iam )
    winput_G = input_OD( : nInputBins - 1 ) * input_G
    this%state_%layer_G_( :, 1 ) =                                            &
        theInterpolator%interpolate( zGrid%edge_, input_zgrid, winput_G, 1 )
    call diagout( 'gz.aer.new', this%state_%layer_G_( :, 1 ) )
    do binNdx = 2, lambdaGrid%ncells_
      this%state_%layer_G_( :, binNdx ) = this%state_%layer_G_( :, 1 )
    enddo

    call config%get( "550 nm optical depth", tau550, Iam, default = 0._dk )
    call config%get( "Alpha", alpha, Iam, default = 1._dk )

    if( tau550 > nzero ) then
      coldens = max( sum( this%state_%layer_OD_( :, 1 ) ), pzero )
      ODscaling = ( tau550/coldens ) * scaling_factor**alpha
      do binNdx = 1, lambdaGrid%ncells_
        this%state_%layer_OD_( :, binNdx ) =                                  &
            this%state_%layer_OD_( :, binNdx ) * ODscaling
      enddo
    endif

    do binNdx = 1, lambdaGrid%ncells_
      wscaling = ( 340._dk / lambdaGrid%mid_( binNdx ) )**alpha
      this%state_%layer_OD_( :, binNdx ) =                                    &
          this%state_%layer_OD_( :, binNdx ) * wscaling
      where( rad_OD > 0._dk )
        this%state_%layer_SSA_( :, binNdx ) =                                 &
            this%state_%layer_SSA_( :, binNdx ) / rad_OD
        this%state_%layer_G_( :, binNdx )   =                                 &
            this%state_%layer_G_( :, binNdx ) / rad_OD
      elsewhere
        this%state_%layer_SSA_( :, binNdx ) = 1._dk
        this%state_%layer_G_( :, binNdx )   = 0._dk
      endwhere
    enddo

    deallocate( zGrid )
    deallocate( lambdaGrid )
    deallocate( theInterpolator )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Update radiator state
  subroutine update_state( this, grid_warehouse, profile_warehouse,           &
      cross_section_warehouse )

    use musica_assert,                 only : assert_msg
    use tuvx_cross_section_warehouse,  only : cross_section_warehouse_t
    use tuvx_grid,                     only : grid_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile,                  only : profile_t
    use tuvx_profile_warehouse,        only : Profile_warehouse_t

    !> Radiator
    class(radiator_aerosol_t),       intent(inout) :: this
    !> Grid warehouse
    type(grid_warehouse_t),          intent(inout) :: grid_warehouse
    !> Profile warehouse
    type(Profile_warehouse_t),       intent(inout) :: profile_warehouse
    !> Cross section warehouse
    type(cross_section_warehouse_t), intent(inout) :: cross_section_warehouse

    ! check that radiator state is allocated
    ! (no updates to the aerosol state are required)
    call assert_msg( 979324400, allocated( this%state_%layer_OD_ ),           &
                     "Radiator state not allocated." )

  end subroutine update_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_radiator_aerosol
