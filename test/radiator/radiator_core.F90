! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests the photo_decomp radiative transfer module
module radiator_core

  use musica_config,    only : config_t
  use musica_string,    only : string_t
  use musica_assert,    only : assert
  use musica_constants, only : ik => musica_ik, dk => musica_dk
  use tuvx_grid_warehouse, only : grid_warehouse_t
  use tuvx_grid,        only : grid_t
  use tuvx_profile_warehouse, only : Profile_warehouse_t
  use tuvx_profile,           only : profile_t
  use tuvx_cross_section_warehouse,        only : cross_section_warehouse_t
  use tuvx_cross_section, only : cross_section_t
  use tuvx_radiator_warehouse,    only : radiator_warehouse_t
  use tuvx_radiator,     only : base_radiator_t
  use tuvx_radiator,     only : radiator_state_t

  implicit none

  private
  public :: radiator_core_t

  type :: radiator_core_t
    type(grid_warehouse_t), pointer          :: theGridWarehouse_
    type(Profile_warehouse_t), pointer       :: theProfileWarehouse_
    type(cross_section_warehouse_t), pointer :: theradXferXsectWarehouse_
    type(radiator_warehouse_t), pointer      :: theRadiatorWarehouse_
  contains
    procedure :: test => run
    final     :: finalize
  end type radiator_core_t

  interface radiator_core_t
    module procedure constructor
  end interface radiator_core_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function constructor( config_flsp ) result( radiator_core_obj )

    !> Arguments
    type(string_t), intent(in)         :: config_flsp

    class(radiator_core_t), pointer :: radiator_core_obj

    !> Local variables
    character(len=*), parameter :: Iam = 'radiator core constructor: '
    type(config_t)              :: tst_config

    write(*,*) Iam // 'entering'

    !> master configuration -> config type
    call tst_config%from_file( config_flsp%to_char() )

    !> Instantiate object
    allocate( radiator_core_t :: radiator_core_obj )

    !> Initialize grid warehouse
    radiator_core_obj%theGridWarehouse_ => grid_warehouse_t( tst_config )

    !> Initialize profile warehouse
    radiator_core_obj%theProfileWarehouse_ => Profile_warehouse_t( tst_config, radiator_core_obj%theGridWareHouse_ )

    !> Initialize radXfer xsect warehouse
    radiator_core_obj%theradXferXsectWarehouse_ => cross_section_warehouse_t( &
                                                  tst_config, &
                                                  radiator_core_obj%theGridWareHouse_, &
                                                  radiator_core_obj%theProfileWarehouse_ )

    !> Initialize radiator warehouse
    radiator_core_obj%theRadiatorWarehouse_ => radiator_warehouse_t( tst_config, radiator_core_obj%theGridWareHouse_ )

    write(*,*) Iam // 'exiting'

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine run( this )

  use tuvx_radiator_warehouse, only : warehouse_iterator_t

  !> Arguments
  class(radiator_core_t), intent(inout)  :: this

  !> Local variables
  character(len=*), parameter :: Iam = 'radiator core run: '

  real(dk)                    :: tstCrossSection
  real(dk), allocatable       :: aCrossSection(:,:)

  class(grid_t), pointer       :: zGrid, lambdaGrid
  class(profile_t), pointer       :: AirProfile, TemperatureProfile
  class(cross_section_t), pointer :: RaylieghCrossSection
  class(base_radiator_t), allocatable  :: RaylieghRadiator
  class(base_radiator_t), allocatable  :: aRadiator
  type(warehouse_iterator_t), pointer :: iter
  type(string_t)                      :: Handle
  type(radiator_state_t), allocatable :: RadiatorState

    write(*,*) Iam // 'entering'

    !> Get copy of grid
    Handle = 'Vertical Z'
    zGrid => this%theGridWarehouse_%get_grid( Handle )
    call assert( 412238768, zGrid%ncells_ .eq. 120_ik )
    call assert( 412238769, all( zGrid%delta_ .eq. 1._dk ) )

    !> Get copy of wavelength grid
    Handle = 'Photolysis, wavelength'
    lambdaGrid => this%theGridWarehouse_%get_grid( Handle )
    call assert( 412238766, all( lambdaGrid%edge_ > 0._dk ) )
    call assert( 412238767, all( lambdaGrid%delta_ > 0._dk ) )

    !> Get copy of the Air Profile
    Handle = 'Air'
    AirProfile => this%theProfileWarehouse_%get_Profile( Handle )
    call assert( 412238771, all( AirProfile%delta_val_ < 0._dk ) )
    call assert( 412238771, all( AirProfile%layer_dens_ > 0._dk ) )
    write(*,*) ' '
    write(*,*) Iam // 'Air layer density'
    write(*,'(1p10g15.7)') AirProfile%layer_dens_

    write(*,*) ' '
    write(*,*) Iam // 'Air burden density'
    write(*,'(1p10g15.7)') AirProfile%burden_dens_

    !> Get copy of the temperature Profile
    Handle = 'Temperature'
    TemperatureProfile => this%theProfileWarehouse_%get_Profile( Handle )
    call assert( 412238772, all( TemperatureProfile%edge_val_ < 400._dk ) )
    call assert( 412238772, all( TemperatureProfile%edge_val_ > 150._dk ) )
    call assert( 412238773, all( abs(TemperatureProfile%delta_val_) < 20._dk ) )

    !> Get copy of the rayliegh cross section
    Handle = 'air'
    RaylieghCrossSection => this%theradXferXsectWareHouse_%get( Handle )
    aCrossSection = RaylieghCrossSection%calculate( this%theGridWareHouse_, this%theProfileWareHouse_ )
    call assert( 412238776, all( aCrossSection >= 0._dk ) )
    call assert( 412238776, all( aCrossSection < 1._dk ) )

    write(*,*) ' '
    write(*,*) Iam // 'aCrossSection is (',size(aCrossSection,dim=1),' x ',size(aCrossSection,dim=2),')'

    tstCrossSection = aCrossSection(1,1)
    call assert( 412238774, all( aCrossSection(:,1) == tstCrossSection ) )

    write(*,*) ' '
    write(*,*) Iam // 'Rayliegh cross section'
    write(*,'(1p10g15.7)') aCrossSection(1,:)
    call assert( 412238775, all( aCrossSection(1,:) == aCrossSection(zGrid%ncells_,:) ) )

    !> Get copy of the rayliegh radiator
    Handle = 'air'
    RaylieghRadiator = this%theRadiatorWarehouse_%get_radiator( Handle )
    call RaylieghRadiator%upDateState( this%theGridWareHouse_, this%theProfileWareHouse_, &
                                       this%theradXferXsectWareHouse_ )
    call assert( 312238775, all( RaylieghRadiator%state_%layer_OD_ >= 0._dk ) )
    write(*,*) Iam // 'layer_OD_ is (',size(RaylieghRadiator%state_%layer_OD_,dim=1),' x ', &
                      size(RaylieghRadiator%state_%layer_OD_,dim=2),')'
    call assert( 312238776, all( RaylieghRadiator%state_%layer_SSA_ >= 0._dk ) )
    write(*,*) Iam // 'layer_SSA_ is (',size(RaylieghRadiator%state_%layer_SSA_,dim=1),' x ', &
                      size(RaylieghRadiator%state_%layer_SSA_,dim=2),')'
    call assert( 312238777, all( RaylieghRadiator%state_%layer_G_ >= 0._dk ) )
    call assert( 312238778, all( RaylieghRadiator%state_%layer_SSA_ == 1._dk ) )
    write(*,*) Iam // 'layer_G_ is (',size(RaylieghRadiator%state_%layer_G_,dim=1),' x ', &
                      size(RaylieghRadiator%state_%layer_G_,dim=2),')'
    write(*,*) ' '
    write(*,*) Iam // 'Air radiator OD @ top of model'
    write(*,'(1p10g15.7)') RaylieghRadiator%state_%layer_OD_(zGrid%ncells_,:)
    write(*,*) ' '
    write(*,*) Iam // 'Air radiator OD @ ground'
    write(*,'(1p10g15.7)') RaylieghRadiator%state_%layer_OD_(1,:)

    write(*,*) Iam // 'Before radiator iterator test'
    !> Test warehouse iterator
    iter => this%theRadiatorWareHouse_%get_iterator()
    do while( iter%next() )
      aRadiator = this%theRadiatorWareHouse_%get_radiator( iter )
      write(*,*) Iam // 'radiator = ',aRadiator%handle_%to_char()
    enddo

    write(*,*) Iam // 'exiting'

  end subroutine run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalize the radXfer core
  subroutine finalize( this )

    !> radXfer core
    type(radiator_core_t), intent(inout) :: this

  !> Local variables
    character(len=*), parameter :: Iam = 'radXfer core finalize: '

    if( associated( this%theGridWareHouse_ ) ) then
      deallocate( this%theGridWareHouse_ )
    end if

    if( associated( this%theProfileWareHouse_ ) ) then
      deallocate( this%theProfileWareHouse_ )
    end if

    if( associated( this%theradXferXsectWareHouse_ ) ) then
      deallocate( this%theradXferXsectWareHouse_ )
    end if

    if( associated( this%theRadiatorWareHouse_ ) ) then
      deallocate( this%theradiatorWareHouse_ )
    end if

  end subroutine finalize

end module radiator_core
