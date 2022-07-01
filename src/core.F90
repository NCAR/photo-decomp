! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The photolysis_core_t and related procedures
module tuvx_core

  use musica_config,            only : config_t
  use musica_string,            only : string_t
  use musica_assert,            only : assert
  use musica_constants,         only : ik => musica_ik, dk => musica_dk, lk => musica_lk
  use tuvx_grid_warehouse,      only : grid_warehouse_t
  use tuvx_grid,             only : grid_t
  use tuvx_profile_warehouse,   only : Profile_warehouse_t
  use tuvx_spherical_geometry,      only : spherical_geom_t
  use tuvx_la_sr_bands,              only : la_srb_t
  use tuvx_radiative_transfer,   only : radXfer_component_core_t
  use tuvx_photolysis_rates,           only : photolysis_rates_t

  implicit none

  private
  public :: photolysis_core_t

  type :: photolysis_core_t
    type(grid_warehouse_t), pointer     :: GridWareHouse_ => null()
    type(Profile_warehouse_t), pointer  :: ProfileWareHouse_ => null()
    type(spherical_geom_t), pointer     :: sphericalGeom_ => null()
    type(la_srb_t), pointer             :: la_srb_ => null()
    type(string_t), allocatable         :: diagnostics_(:)
    type(radXfer_component_core_t), pointer    :: radXfer_component_ => null()
    type(photolysis_rates_t), pointer :: photorates_component_ => null()
  contains
    procedure :: run
    final     :: finalize
  end type photolysis_core_t

  interface photolysis_core_t
    module procedure constructor
  end interface photolysis_core_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function constructor( config_flsp ) result( photolysis_core_obj )

    use musica_assert,                 only : assert_msg
    use musica_iterator,               only : iterator_t
    use musica_string,                 only : string_t
    use tuvx_diagnostic_util,          only : diagout
    use tuvx_profile,                  only : profile_t

    !> Arguments
    type(string_t), intent(in) :: config_flsp

    class(photolysis_core_t), pointer :: photolysis_core_obj

    !> Local variables
    character(len=*), parameter :: Iam = 'Photolysis core constructor: '

    character(len=32)           :: keyChar
    logical(lk)                 :: found
    type(string_t)              :: keyString
    type(config_t)              :: core_config, child_config
    type(string_t)              :: Handle
    class(iterator_t), pointer  :: iter
    class(profile_t),  pointer  :: aProfile
    type(string_t)              :: required_keys(3), optional_keys(1)

    call core_config%from_file( config_flsp%to_char() )

    ! Check json configuration file for basic structure, integrity
    required_keys(1) = "radiative transfer"
    required_keys(2) = "grids"
    required_keys(3) = "profiles"
    optional_keys(1) = "photolysis reactions"
    call assert_msg( 255400232,                                               &
                     core_config%validate( required_keys, optional_keys ),    &
                     "Bad configuration data format for tuv-x core." )

    !> Instantiate photolysis core
    allocate( photolysis_core_obj )

    ! Instantiate and initialize grid warehouse
    call core_config%get( "grids", child_config, Iam )
    photolysis_core_obj%GridWarehouse_ => grid_warehouse_t( child_config )

    !> Instantiate and initialize profile warehouse
    call core_config%get( "profiles", child_config, Iam )
    photolysis_core_obj%ProfileWarehouse_ =>                                  &
        profile_warehouse_t( child_config, photolysis_core_obj%GridWareHouse_ )

    !> Diagnostics for testing
    Handle = 'Temperature' ; aProfile => photolysis_core_obj%ProfileWareHouse_%get_Profile( Handle )
    call diagout( 'vptmp.new', aProfile%edge_val_ )
    deallocate( aProfile )

    Handle = 'Air' ; aProfile => photolysis_core_obj%ProfileWareHouse_%get_Profile( Handle )
    call diagout( 'vpair.new', aProfile%edge_val_ )
    deallocate( aProfile )

    Handle = 'O3' ; aProfile => photolysis_core_obj%ProfileWareHouse_%get_Profile( Handle )
    call diagout( 'vpco3.new', aProfile%layer_dens_ )
    deallocate( aProfile )

    ! Set up radiative transfer calculator
    call core_config%get( "radiative transfer", child_config, Iam )
    photolysis_core_obj%radXfer_component_ => &
        radXfer_component_core_t( child_config,                               &
                                  photolysis_core_obj%GridWareHouse_,         &
                                  photolysis_core_obj%ProfileWareHouse_ )

    ! get optical depth diagnostics to output
    !> \todo this should be moved out of the radiative transfer config if it
    !!       is owned by the core
    call child_config%get( "Diagnostics", photolysis_core_obj%diagnostics_,   &
                           Iam, found = found )
    if( .not. found ) allocate( photolysis_core_obj%diagnostics_( 0 ) )

    ! photo and dose rate components
    call core_config%get( "photolysis reactions", child_config, Iam,          &
                          found = found )
    if( found ) then
      photolysis_core_obj%photorates_component_ => &
          photolysis_rates_t( child_config,                                   &
                              photolysis_core_obj%GridWareHouse_,             &
                              photolysis_core_obj%ProfileWareHouse_ )
    end if

    !> instantiate and initialize spherical geometry type
    photolysis_core_obj%sphericalGeom_ => spherical_geom_t( photolysis_core_obj%GridWareHouse_ )
    !> instantiate and initialize lyman alpha, srb type
    photolysis_core_obj%la_srb_ => la_srb_t( photolysis_core_obj%GridWareHouse_ )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine run( this )

  use tuvx_profile,                 only : profile_t
  use tuvx_radiator_warehouse,      only : radiator_warehouse_t
  use tuvx_radiator,                only : radiator_t
  use tuvx_radiative_transfer_solver,        only : radField_t
  use tuvx_diagnostic_util,                        only : diagout

  !> Arguments
  class(photolysis_core_t), intent(inout)  :: this

  !> Local variables
  character(len=*), parameter :: Iam = 'Photolysis core run: '

  integer(ik)                     :: i_ndx, i_diag
  real(dk), allocatable           :: photoRates(:,:)
  character(len=2)                :: number
  class(profile_t),       pointer :: SZAngles => null( )
  class(radiator_t),      pointer :: aRadiator => null()
  class(radField_t),      pointer :: radiationFld => null( )
  type(string_t)                  :: Handle

  write(*,*) ' '
  write(*,*) Iam // 'entering'

  ! get the solar zenith angles
  Handle = 'Sza' ; SZAngles => this%ProfileWareHouse_%get_Profile( Handle )

  ! calculate the radiation field
sza_loop: &
  do i_ndx = 1,size(SZAngles%edge_val_)
    write(*,*) Iam // 'calculating rad field @ i_ndx,sza = ',i_ndx,SZAngles%edge_val_(i_ndx)
    if( associated( this%sphericalGeom_ ) ) then
      call this%sphericalGeom_%setSphericalParams( SZAngles%edge_val_(i_ndx), this%GridWareHouse_ )
    endif
    call this%RadXfer_component_%upDate( this%la_srb_, this%sphericalGeom_, this%GridWareHouse_, &
                                         this%ProfileWareHouse_, radiationFld )
    write(number,'(i2.2)') i_ndx
    call diagout( 'radField.' // number // '.new',radiationFld%fdr_+radiationFld%fup_+radiationFld%fdn_ )
    if( associated(this%photorates_component_) ) then
      if( allocated(photoRates) ) then
        deallocate(photoRates)
      endif
      call this%photorates_component_%get( this%la_srb_, this%sphericalGeom_, &
                                              this%GridWareHouse_, this%ProfileWareHouse_, &
                                              radiationFld, photoRates, number )
    endif
    deallocate( radiationFld )
  enddo sza_loop

  ! diagnostic output
  do i_diag = 1, size( this%diagnostics_ )
    associate( diagnostic => this%diagnostics_( i_diag ) )
      aRadiator => this%RadXfer_component_%RadiatorWareHouse_%get_radiator( diagnostic )
        ! Diagnostics for testing
      if( diagnostic == 'air' ) then
        call diagout( 'dtrl.new', aRadiator%state_%layer_OD_ )
      elseif( diagnostic == 'Aerosols' ) then
        call diagout( 'dtaer.new', aRadiator%state_%layer_OD_ )
      elseif( diagnostic == 'O3' ) then
        call diagout( 'dto3.new', aRadiator%state_%layer_OD_ )
      elseif( diagnostic == 'O2' ) then
        call diagout( 'dto2.new', aRadiator%state_%layer_OD_ )
      endif
    end associate
  end do

  deallocate( SZAngles )

  write(*,*) ' '
  write(*,*) Iam // 'exiting'

  end subroutine run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalize the photolysis core
  subroutine finalize( this )

    !> radXfer core
    type(photolysis_core_t), intent(inout) :: this

    if( associated( this%GridWareHouse_ ) ) then
      deallocate( this%GridWareHouse_ )
    end if

    if( associated( this%ProfileWareHouse_ ) ) then
      deallocate( this%ProfileWareHouse_ )
    end if

    if( associated( this%sphericalGeom_ ) ) then
      deallocate( this%sphericalGeom_ )
    end if

    if( associated( this%la_srb_ ) ) then
      deallocate( this%la_srb_ )
    end if

    if( associated( this%radXfer_component_ ) ) then
      deallocate( this%radXfer_component_ )
    end if

    if( associated( this%photorates_component_ ) ) then
      deallocate( this%photorates_component_ )
    end if

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_core
