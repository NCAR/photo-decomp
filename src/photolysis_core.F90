!Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The photolysis_core_t and related procedures

module photolysis_core

  use musica_config,        only : config_t
  use musica_string,        only : string_t
  use musica_assert,        only : assert
  use musica_constants,         only : ik => musica_ik, dk => musica_dk, lk => musica_lk
  use micm_grid_warehouse,      only : grid_warehouse_t
  use micm_1d_grid,             only : abs_1d_grid_t
  use micm_Profile_warehouse,   only : Profile_warehouse_t
  use spherical_geom_type,      only : spherical_geom_t
  use la_srb_type,              only : la_srb_t
  use radXfer_component_core,   only : radXfer_component_core_t

  implicit none

  private
  public :: photolysis_core_t

  type :: photolysis_core_t
    type(grid_warehouse_t), pointer     :: GridWareHouse_ => null()
    type(Profile_warehouse_t), pointer  :: ProfileWareHouse_ => null()
    type(spherical_geom_t), pointer     :: sphericalGeom_ => null()
    type(la_srb_t), pointer             :: la_srb_ => null()
    type(string_t), allocatable         :: diagnostics_(:)
    type(radXfer_component_core_t), pointer :: radXfer_component_ => null()
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

    use musica_iterator,              only : iterator_t
    use musica_string,                only : string_t
    use micm_Profile,                 only : abs_Profile_t
    use debug,                        only : diagout

    !> Arguments
    type(string_t), intent(in) :: config_flsp

    class(photolysis_core_t), pointer :: photolysis_core_obj

    !> Local variables
    character(len=*), parameter :: Iam = 'Photolysis core constructor: '

    character(len=32)           :: keyChar
    logical(lk)                 :: found
    type(string_t)              :: keyString
    type(config_t)              :: master_config, components_config, radXfer_config
    type(config_t)              :: radXfer_cross_sections_config, Radiators_config
    type(config_t)              :: component_config
    type(string_t)              :: Handle
    class(iterator_t), pointer  :: iter
    class(abs_Profile_t), pointer :: aProfile

    write(*,*) Iam // 'entering'

    !> Check json configuration file for basic structure, integrity
    !> master configuration -> config type
    call master_config%from_file( config_flsp%to_char() )
    !> components; radiative transfer
    call master_config%get( "Components", components_config, Iam )
    !> Radiative transfer key must be in config
    call components_config%get( "Radiative transfer", radXfer_config, Iam )
    !> Radiative transfer cross sections are optional
    call radXfer_config%get( "Radiative xfer cross sections", radXfer_cross_sections_config, Iam, found=found )
    !> Radiators keys must be in config
    call radXfer_config%get( "Radiators", Radiators_config, Iam )


    !> Instantiate photolysis core
    allocate( photolysis_core_obj )

    ! get optical depth diagnostics to output
    call radXfer_config%get( "Diagnostics", photolysis_core_obj%diagnostics_, Iam, found = found )
    if( .not. found ) allocate( photolysis_core_obj%diagnostics_( 0 ) )

    !> Instantiate and initialize grid warehouse
    photolysis_core_obj%GridWarehouse_ => grid_warehouse_t( master_config )

    !> Instantiate and initialize profile warehouse
    photolysis_core_obj%ProfileWarehouse_ => Profile_warehouse_t( master_config, photolysis_core_obj%GridWareHouse_ )

    !> Diagnostics for testing
    Handle = 'Temperature' ; aProfile => photolysis_core_obj%ProfileWareHouse_%get_Profile( Handle )
    call diagout( 'vptmp.new', aProfile%edge_val_ )

    Handle = 'Air' ; aProfile => photolysis_core_obj%ProfileWareHouse_%get_Profile( Handle )
    call diagout( 'vpair.new', aProfile%edge_val_ )

    Handle = 'O3' ; aProfile => photolysis_core_obj%ProfileWareHouse_%get_Profile( Handle )
    call diagout( 'vpco3.new', aProfile%layer_dens_ )

    !> set up the components
    iter => components_config%get_iterator()
    do while( iter%next() )
      keyChar = components_config%key(iter)
      write(*,*) ' '
      write(*,*) Iam,'key = ',trim(keyChar)
      keyString = keyChar
      call components_config%get( iter, component_config, Iam )
      call component_config%add( 'type', keyString, Iam )
      if( keyChar == 'Radiative transfer' ) then
        photolysis_core_obj%radXfer_component_ => &
               radXfer_component_core_t( component_config, photolysis_core_obj%GridWareHouse_, &
                                         photolysis_core_obj%ProfileWareHouse_ )
      endif
    enddo
    deallocate( iter )

    !> instantiate and initialize spherical geometry type
    allocate( photolysis_core_obj%sphericalGeom_ )
    call photolysis_core_obj%sphericalGeom_%initialize( photolysis_core_obj%GridWareHouse_ )
    !> instantiate and initialize lyman alpha, srb type
    allocate( photolysis_core_obj%la_srb_ )
    call photolysis_core_obj%la_srb_%initialize( photolysis_core_obj%GridWareHouse_ )

    write(*,*) Iam // 'exiting'

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine run( this )

  use micm_Profile,                 only : abs_Profile_t
  use micm_radiator_warehouse,      only : radiator_warehouse_t
  use photolysis_radiator,          only : radiator_t
  use abstract_radXfer_type,        only : radField_t
  use debug,                        only : diagout

  !> Arguments
  class(photolysis_core_t), intent(inout)  :: this

  !> Local variables
  character(len=*), parameter :: Iam = 'Photolysis core run: '

  integer(ik)                 :: i_ndx, i_diag
  character(len=2)            :: number
  class(abs_Profile_t), pointer  :: SZAngles
  class(radiator_t), pointer     :: aRadiator => null()
  class(radField_t), allocatable :: radiationFld
  type(string_t)                 :: Handle

  write(*,*) ' '
  write(*,*) Iam // 'entering'

  ! get the solar zenith angles
  Handle = 'Sza' ; SZAngles => this%ProfileWareHouse_%get_Profile( Handle )

  ! calculate the radiation field
  do i_ndx = 1,size(SZAngles%edge_val_)
    write(*,*) Iam // 'calculating rad field @ i_ndx,sza = ',i_ndx,SZAngles%edge_val_(i_ndx)
    if( associated( this%sphericalGeom_ ) ) then
      call this%sphericalGeom_%setSphericalParams( SZAngles%edge_val_(i_ndx), this%GridWareHouse_ )
    endif
    call this%RadXfer_component_%upDate( this%la_srb_, this%sphericalGeom_, this%GridWareHouse_, &
                                        this%ProfileWareHouse_, radiationFld )
    write(number,'(i2.2)') i_ndx
    call diagout( 'radField.' // number // '.new',radiationFld%fdr_+radiationFld%fup_+radiationFld%fdn_ )
  enddo

  ! diagnostic output
  do i_diag = 1, size( this%diagnostics_ )
    associate( diagnostic => this%diagnostics_( i_diag ) )
      aRadiator => this%RadXfer_component_%RadiatorWareHouse_%get_radiator( diagnostic )
        ! Diagnostics for testing
      if( diagnostic == 'Air' ) then
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

  end subroutine finalize

end module photolysis_core
