! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0

module tuvx_radiative_transfer
  ! Radiative Transfer
  ! ^^^^^^^^^^^^^^^^^^

  use musica_config,                   only : config_t
  use musica_constants,                only : dk => musica_dk, ik => musica_ik
  use musica_string,                   only : string_t
  use tuvx_grid_warehouse,             only : grid_warehouse_t
  use tuvx_profile_warehouse,          only : Profile_warehouse_t
  use tuvx_cross_section_warehouse,    only : cross_section_warehouse_t
  use tuvx_radiator_warehouse,         only : radiator_warehouse_t
  use tuvx_radiative_transfer_solver,           only : abstract_radXfer_t
  use tuvx_delta_eddington,            only : delta_eddington_t
  use tuvx_la_sr_bands,                     only : la_srb_t

  implicit none
  private

  public :: radXfer_component_core_t

  type radXfer_component_core_t
    ! radXfer component core
    !
    ! Calculates the atmospheric radiation field

    private
    integer(ik)                              :: nStreams_ = 0_ik
    class(abstract_radXfer_t), pointer       :: radXferSolver_ => null()
    !> Copy of the original radXfer component configuration
    type(config_t)                           :: config_
    !> Copy of original radXfer cross section warehouse
    type(cross_section_warehouse_t), pointer :: radXferXsectWareHouse_ => null()
    !> Copy of original radiator warehouse
    type(radiator_warehouse_t), public, pointer :: RadiatorWareHouse_ => null()
  contains
    !> Returns the name of the component
    procedure :: name => component_name
    !> Returns a description of the component purpose
    procedure :: description
    !> Calculate the radiation field at given solar zenith angle
    procedure :: upDate
    !> Finalize the radXfer core
    final :: finalize
  end type radXfer_component_core_t

  interface radXfer_component_core_t
    ! Constructor
    module procedure constructor
  end interface radXfer_component_core_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function constructor( config, gridWareHouse, ProfileWareHouse ) result( radXfer_component )

    use musica_assert,                 only : die_msg
    ! radXfer component core constructor
    !
    ! Sets up radXfer solver objects

    !> New radXfer component core
    type(radXfer_component_core_t), pointer :: radXfer_component
    !> radXfer configuration data
    type(config_t), intent(inout) :: config
    !> grid warehouse
    type(grid_warehouse_t), intent(inout)    :: gridWareHouse
    !> Profile warehouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse

    character(len=*), parameter :: Iam = 'radXfer component constructor: '
    type(string_t)       :: solver
    type(config_t) :: child_config

    allocate( radXfer_component )

    ! instantiate and initialize the radXfer cross section warehouse
    call config%get( "cross sections", child_config, Iam )
    radXfer_component%radXferXsectWareHouse_ =>                               &
        cross_section_warehouse_t( child_config, gridWareHouse,               &
                                   ProfileWareHouse )

    ! instantiate and initialize the radiator warehouse
    call config%get( "radiators", child_config, Iam )
    radXfer_component%radiatorWareHouse_ =>                                   &
        radiator_warehouse_t( child_config, gridWareHouse )

    ! save the configuration (used for preprocessing input data only)
    radXfer_component%config_ = config

    ! get the radiative transfer solver
    call config%get( "Solver", solver, Iam, default="Delta Eddington" )
    if( solver == 'Discrete Ordinants' ) then
      call config%get( "nStreams", radXfer_component%nStreams_, Iam, default=4_ik )
      call die_msg( 900569062, "Discrete Ordinants method is not currently available" )
    else
      allocate( delta_eddington_t :: radXfer_component%radXferSolver_ )
    endif

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  type(string_t) function component_name( this )
    ! Model component name

    !> Arguments
    class(radXfer_component_core_t), intent(in) :: this

    component_name = "radXfer component"

  end function component_name

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  type(string_t) function description( this )
    ! Model component description

    !> Arguments
    class(radXfer_component_core_t), intent(in) :: this

    description = "calculate the radiation field"

  end function description

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine upDate( this, la_srb, SphericalGeom, GridWareHouse, ProfileWareHouse, radiationFld )
    ! Calculate the radiation field

    use musica_assert,                 only : die_msg
    use musica_string,                 only : to_char
    use tuvx_grid,                  only : grid_t
    use tuvx_radiator_warehouse,       only : warehouse_iterator_t
    use tuvx_radiator,        only : radiator_t
    use tuvx_radiator,        only : radiator_state_t
    use tuvx_profile,                  only : profile_t
    use tuvx_spherical_geometry,           only : spherical_geom_t
    use tuvx_la_sr_bands,                   only : la_srb_t
    use tuvx_radiative_transfer_solver,         only : radField_t

    !> Arguments
    !> radXfer component radXfer component
    class(radXfer_component_core_t), intent(inout) :: this
    !> grid warehouse
    type(grid_warehouse_t), intent(inout)          :: GridWareHouse
    !> Profile warehouse
    type(Profile_warehouse_t), intent(inout)       :: ProfileWareHouse
    !> Spherical geometry
    type(spherical_geom_t), intent(inout)          :: SphericalGeom
    !> Lyman Alpha, SRB
    type(la_srb_t), intent(inout)                  :: la_srb

    class(radField_t), pointer, intent(out)        :: radiationFld

    !> Local variables
    character(len=*), parameter          :: Iam = 'radXfer component upDate: '

    integer(ik)                          :: nlyr
    integer(ik)                          :: radNdx
    real(dk)                             :: zenithAngle
    real(dk), allocatable                :: airVcol(:), airScol(:)
    type(string_t)                       :: Handle
    type(warehouse_iterator_t), pointer  :: iter => null( )
    class(radiator_t),          pointer  :: aRadiator => null()
    class(profile_t),           pointer  :: airProfile => null()

    write(*,*) ' '
    write(*,*) Iam // 'entering'

    !> iterate over radiators
    iter => this%RadiatorWareHouse_%get_iterator()
    do while( iter%next() )
      aRadiator => this%RadiatorWareHouse_%get_radiator( iter )
      write(*,*) Iam // 'radiator handle = ',aRadiator%handle_%to_char()
      call aRadiator%update_state( GridWareHouse, ProfileWareHouse, this%radXferXsectWareHouse_ )
    enddo
    deallocate( iter )

    !> look for O2 radiator; Lyman Alpha and SR bands
    Handle = 'O2'
    radNdx = this%RadiatorWareHouse_%get_radiator_ndx_from_handle( Handle )
    if( radNdx > 0 ) then
      aRadiator => this%RadiatorWareHouse_%get_radiator( Handle )
      airProfile => ProfileWareHouse%get_Profile( "air", "molecule cm-3" )
      allocate( airVcol(airProfile%ncells_),airScol(airProfile%ncells_+1_ik) )
      call SphericalGeom%airmas( airProfile%exo_layer_dens_, airVcol, airScol )
      call la_srb%calculate_OD( gridWareHouse, ProfileWareHouse, airVcol, airScol, aRadiator%state_%layer_OD_ )
      deallocate( airVcol,airScol )
      deallocate( airProfile )
    endif

    nlyr = size( aRadiator%state_%layer_OD_,dim=1 )

    zenithAngle = SphericalGeom%SolarZenithAngle_ 
    associate( theSolver => this%radXferSolver_ )
    radiationFld => theSolver%upDateRadField( &
                     zenithAngle, this%nStreams_, nlyr, SphericalGeom, &
                     GridWareHouse, ProfileWareHouse, this%RadiatorWareHouse_ )
    end associate

    write(*,*) ' '
    write(*,*) Iam // 'exiting'

  end subroutine upDate

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine finalize( this )
    ! Finalize the radXfer component core

    !>radXfer component
    type(radXfer_component_core_t), intent(inout) :: this

    if( associated( this%radXferXsectWareHouse_ ) ) deallocate( this%radXferXsectWareHouse_ )
    if( associated( this%RadiatorWareHouse_ ) ) deallocate( this%RadiatorWareHouse_ )
    if( associated( this%radXferSolver_ ) ) deallocate( this%radXferSolver_ )

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_radiative_transfer
