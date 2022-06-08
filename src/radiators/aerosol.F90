! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The aerosol_tuvx_radiator module

!> The aerosol_radiator_t type and related functions
!!
module tuvx_radiator_aerosol

  use musica_constants,       only : dk => musica_dk, ik => musica_ik
  use musica_string,          only : string_t
  use tuvx_radiator, only : base_radiator_t

  implicit none

  private
  public :: aerosol_radiator_t

  !> aerosol radiator type
  type, extends(base_radiator_t) :: aerosol_radiator_t
  contains
    !> Initialize radiator
    !> Update radiator for new environmental conditions
    procedure :: upDateState
  end type aerosol_radiator_t

  !> Constructor
  interface aerosol_radiator_t
    module procedure constructor
  end interface aerosol_radiator_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize radiator_t object
  function constructor( radiator_config, gridWareHouse ) result( this )

    use musica_assert,        only : assert_msg, die_msg
    use musica_config,        only : config_t
    use tuvx_grid_warehouse,  only : grid_warehouse_t
    use tuvx_grid,         only : grid_t
    use tuvx_interpolate
    use tuvx_constants,           only : nzero, pzero
    use tuvx_diagnostic_util,                only : diagout

    !> Arguments
    !> Radiator object
    type(aerosol_radiator_t), pointer :: this
    !> Radiator configuration object
    type(config_t), intent(inout)         :: radiator_config
    !> Grid warehouse
    type(grid_warehouse_t), intent(inout) :: gridWareHouse

    !> Local variables
    character(len=*), parameter   :: Iam = "Aerosol radiator initialize: "
    real(dk), parameter           :: scaling_factor = 550._dk/340._dk

    integer                       :: k, nInputBins, binNdx
    real(dk)                      :: tau550, alpha, wscaling, ODscaling
    real(dk)                      :: coldens
    real(dk), allocatable         :: input_OD(:), rad_OD(:)
    real(dk)                      :: input_SSA
    real(dk)                      :: input_G
    real(dk), allocatable         :: input_zgrid(:)
    real(dk), allocatable         :: winput_SSA(:), winput_G(:)
    type(string_t)                :: Handle
    type(config_t)                :: Aerosol_config
    class(grid_t), pointer :: zGrid, lambdaGrid
    class(abs_interpolator_t), pointer :: theInterpolator
    type(string_t) :: required_keys(5), optional_keys(1)

    required_keys(1) = "type"
    required_keys(2) = "optical depths"
    required_keys(3) = "single scattering albedo"
    required_keys(4) = "asymmetry factor"
    required_keys(5) = "name"
    optional_keys(1) = "550 nm optical depth"
    call assert_msg( 584205621,                                               &
                     radiator_config%validate( required_keys, optional_keys ),&
                     "Bad configuration data format for "//                   &
                     "aerosol radiator." )

    write(*,*) ' '
    write(*,*) Iam,'entering'

    allocate( this )

    Handle = 'Vertical Z' ; zGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength' ; lambdaGrid => gridWareHouse%get_grid( Handle )

!-----------------------------------------------------------------------------
!> Get radiator "Handle"
!-----------------------------------------------------------------------------
    call radiator_config%get( 'name', this%handle_, Iam )
    write(*,*) Iam // 'handle = ',this%handle_%to_char()

!> allocate radiator state variables
    allocate( this%state_%layer_OD_(zGrid%ncells_,lambdaGrid%ncells_) )
    allocate( this%state_%layer_SSA_(zGrid%ncells_,lambdaGrid%ncells_) )
    allocate( this%state_%layer_G_(zGrid%ncells_,lambdaGrid%ncells_) )

!> read json config
    call radiator_config%get( "optical depths", input_OD, Iam )
    nInputBins = size(input_OD)
    if( nInputBins > 1 ) then
!> interpolate input OD to state variable
      write(*,*) Iam // 'OD from config'
      write(*,*) Iam // 'size input_OD = ',nInputBins
      write(*,*) Iam // 'input_OD'
      write(*,'(1p10g15.7)') input_OD
      call diagout( 'rawOD.new',input_OD )
      input_OD(:nInputBins-1) = .5_dk*(input_OD(:nInputBins-1)+input_OD(2:))
      write(*,'(1p10g15.7)') input_OD(:nInputBins-1)
      call diagout( 'inpaerOD.new',input_OD(:nInputBins-1) )

      allocate( interp3_t :: theInterpolator )
      input_zgrid = (/ (real(k,dk),k=0,nInputBins-1) /)
      write(*,*) Iam // 'input zgrid'
      write(*,'(1p10g15.7)') input_zgrid
      rad_OD = theInterpolator%interpolate( zGrid%edge_, input_zgrid,input_OD, 1 )
      call diagout( 'cz.aer.new',rad_OD )
      write(*,*) 'size interpolated_OD = ',size(rad_OD)
      write(*,*) 'size interpolated_OD = ',sizeof(rad_OD)
      write(*,*) Iam // 'interpolated OD'
      write(*,'(1p10g15.7)') rad_OD
      do binNdx = 1,lambdaGrid%ncells_
        this%state_%layer_OD_(:,binNdx) = rad_OD
      enddo
    else
      this%state_%layer_OD_ = input_OD(1)
    endif
    
    call radiator_config%get( "single scattering albedo", input_SSA, Iam )
!> interpolate input SSA to state variable
    winput_SSA = input_OD(:nInputBins-1) * input_SSA
    this%state_%layer_SSA_(:,1) = theInterpolator%interpolate( zGrid%edge_, input_zgrid,winput_SSA, 1 )
    call diagout( 'omz.aer.new',this%state_%layer_SSA_(:,1) )
    do binNdx = 2,lambdaGrid%ncells_
      this%state_%layer_SSA_(:,binNdx) = this%state_%layer_SSA_(:,1) 
    enddo
    write(*,*) Iam // 'SSA from config'
    write(*,*) input_SSA

    call radiator_config%get( "asymmetry factor", input_G, Iam )
!> interpolate input G to state variable
    winput_G = input_OD(:nInputBins-1) * input_G
    this%state_%layer_G_(:,1) = theInterpolator%interpolate( zGrid%edge_, input_zgrid,winput_G, 1 )
    call diagout( 'gz.aer.new',this%state_%layer_G_(:,1) )
    do binNdx = 2,lambdaGrid%ncells_
      this%state_%layer_G_(:,binNdx) = this%state_%layer_G_(:,1) 
    enddo
    write(*,*) Iam // 'G from config'
    write(*,*) input_G

    call radiator_config%get( "550 nm optical depth", tau550, Iam, default=0._dk )
    call radiator_config%get( "Alpha", alpha, Iam, default=1._dk )
    write(*,*) Iam // 'tau550, alpha from config'
    write(*,*) tau550, alpha

    if( tau550 > nzero ) then
      coldens = max( sum( this%state_%layer_OD_(:,1) ),pzero )
      ODscaling = (tau550/coldens) * scaling_factor**alpha
      do binNdx = 1,lambdaGrid%ncells_
        this%state_%layer_OD_(:,binNdx) = this%state_%layer_OD_(:,binNdx) * ODscaling
      enddo
    endif

    do binNdx = 1,lambdaGrid%ncells_
      wscaling = (340._dk/lambdaGrid%mid_(binNdx))**alpha
      this%state_%layer_OD_(:,binNdx) = this%state_%layer_OD_(:,binNdx) * wscaling
      where( rad_OD > 0._dk )
        this%state_%layer_SSA_(:,binNdx) = this%state_%layer_SSA_(:,binNdx)/rad_OD
        this%state_%layer_G_(:,binNdx)   = this%state_%layer_G_(:,binNdx)/rad_OD
      elsewhere
        this%state_%layer_SSA_(:,binNdx) = 1._dk
        this%state_%layer_G_(:,binNdx)   = 0._dk
      endwhere
    enddo

    write(*,*) Iam // 'layer OD @ lambda = ',lambdaGrid%mid_(1)
    write(*,'(1p10g15.7)') this%state_%layer_OD_(:,1)
    write(*,*) Iam // 'layer OD @ lambda = ',lambdaGrid%mid_(lambdaGrid%ncells_)
    write(*,'(1p10g15.7)') this%state_%layer_OD_(:,lambdaGrid%ncells_)
    write(*,*) ' '
    write(*,*) Iam // 'layer SSA @ lambda = ',lambdaGrid%mid_(1)
    write(*,'(1p10g15.7)') this%state_%layer_SSA_(:,1)
    write(*,*) Iam // 'layer SSA @ lambda = ',lambdaGrid%mid_(lambdaGrid%ncells_)
    write(*,'(1p10g15.7)') this%state_%layer_SSA_(:,lambdaGrid%ncells_)
    write(*,*) ' '
    write(*,*) Iam // 'layer G @ lambda = ',lambdaGrid%mid_(1)
    write(*,'(1p10g15.7)') this%state_%layer_G_(:,1)
    write(*,*) Iam // 'layer G @ lambda = ',lambdaGrid%mid_(lambdaGrid%ncells_)
    write(*,'(1p10g15.7)') this%state_%layer_G_(:,lambdaGrid%ncells_)

    write(*,*) ' '
    write(*,*) Iam,'exiting'

!   stop 'Debugging'

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Update radiator state
  subroutine upDateState( this, gridWareHouse, ProfileWareHouse, radXferXsectWareHouse )

    use musica_assert,                 only : die_msg
    use tuvx_profile_warehouse,        only : Profile_warehouse_t
    use tuvx_profile,                  only : profile_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_grid,                  only : grid_t
    use tuvx_cross_section_warehouse,  only : cross_section_warehouse_t

    !> Arguments
    !> radiator obj
    class(aerosol_radiator_t), intent(inout) :: this
    !> Grid warehouse
    type(grid_warehouse_t), intent(inout)    :: gridWareHouse
    !> Profile warehouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse
    !> RadXfer cross section warehouse
    type(cross_section_warehouse_t), intent(inout) :: radXferXsectWareHouse

    !> Local variables
    integer(ik) :: wNdx
    character(len=*), parameter :: Iam = 'Aerosol radiator upDateState: '
    type(string_t)                :: Handle
    class(grid_t), pointer :: zGrid
    class(grid_t), pointer :: lambdaGrid

    write(*,*) ' '
    write(*,*) Iam,'entering'

    write(*,*) Iam // 'handle = ',this%handle_%to_char()
!-----------------------------------------------------------------------------
!> get specific grids and profiles
!-----------------------------------------------------------------------------
    Handle = 'Vertical Z' ; zGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength' ; lambdaGrid => gridWareHouse%get_grid( Handle )
    write(*,*) Iam // 'nlyr,nbins = ',zGrid%ncells_,lambdaGrid%ncells_

    !> check that radiator state is allocated
    if( .not. allocated( this%state_%layer_OD_ ) ) then
      call die_msg( 2222222,"In radiator%upDateState radiator state not allocate" )
    endif

    write(*,*) ' '
    write(*,*) Iam,'exiting'

  end subroutine upDateState

end module tuvx_radiator_aerosol
