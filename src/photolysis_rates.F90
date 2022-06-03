! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The tuvx_photolysis_rates module

!> The photolysis_rates_t type and related functions
module tuvx_photolysis_rates

  use musica_assert,                   only : die_msg
  use musica_constants,                only : dk => musica_dk
  use musica_string,                   only : string_t
  use tuvx_cross_section,              only : base_cross_section_ptr
  use tuvx_grid,                       only : abs_1d_grid_t
  use tuvx_grid_warehouse,             only : grid_warehouse_t
  use tuvx_profile,                    only : abs_profile_t
  use tuvx_profile_warehouse,          only : profile_warehouse_t
  use tuvx_quantum_yield,              only : quantum_yield_ptr

  implicit none

  private
  public :: photolysis_rates_t

  !> Photolysis rate constant calculator
  type :: photolysis_rates_t
    !> Absorption cross-sections
    type(base_cross_section_ptr), allocatable :: cross_sections_(:)
    !> Quantum yields
    type(quantum_yield_ptr), allocatable :: quantum_yields_(:)
    !> Scaling factor for final rate constant
    real(dk),                    allocatable :: scaling_factors_(:)
    !> User-provided label for the photolysis rate constant
    type(string_t), allocatable              :: handles_(:)
  contains
    !> Returns the photolysis rate constants for a given set of conditions
    procedure :: get
    !> Finalize the object
    final :: finalize
  end type photolysis_rates_t

  !> photolysis_rates_t constructor
  interface photolysis_rates_t
    module procedure :: constructor
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructor of photolysis_rates_t objects
  function constructor( reaction_set, grid_warehouse, profile_warehouse )     &
      result( photolysis_rates )

    use musica_config,                 only : config_t
    use musica_iterator,               only : iterator_t
    use tuvx_cross_section_factory,    only : cross_section_builder
    use tuvx_quantum_yield_factory,    only : quantum_yield_builder

    !> photorates rates
    !> Arguments
    type(config_t),            intent(inout) :: reaction_set
    !> grid warehouse
    type(grid_warehouse_t),    intent(inout) :: grid_warehouse
    !> profile warehouse
    type(profile_warehouse_t), intent(inout) :: profile_warehouse
    !> New photorates rates
    class(photolysis_rates_t),  pointer      :: photolysis_rates

    !> Local variables
    character(len=*), parameter :: Iam = "photolysis_rates_t constructor"

    integer        :: nRates
    real(dk)       :: rate_aliasing_factor
    type(config_t) :: reaction_config
    type(config_t) :: cross_section_config, quantum_yield_config
    class(iterator_t), pointer :: iter
    type(base_cross_section_ptr) :: cross_section_ptr
    type(quantum_yield_ptr) :: quantum_yield_ptr
    character(len=64)           :: keychar
    type(string_t)              :: netcdfFile, Object
    type(string_t)              :: reaction_key
    type(string_t)              :: Handle
    type(string_t), allocatable :: netcdfFiles(:)

    allocate( photolysis_rates )

    associate( rates => photolysis_rates )

    allocate( string_t :: rates%handles_(0) )
    allocate( rates%cross_sections_(0) )
    allocate( rates%quantum_yields_(0) )
    allocate( rates%scaling_factors_(0) )

    ! iterate over photo reactions
    iter => reaction_set%get_iterator( )
    do while( iter%next( ) )
      keychar = reaction_set%key( iter )
      reaction_key = keychar
      rates%handles_ = [ rates%handles_, reaction_key ]
      call reaction_set%get( iter, reaction_config, Iam )

      ! get cross section first
      call reaction_config%get( "cross section", cross_section_config, Iam )
      cross_section_ptr%val_ => cross_section_builder( cross_section_config,  &
                                            grid_warehouse, profile_warehouse )
      rates%cross_sections_ = [ rates%cross_sections_, cross_section_ptr ]

      ! now get quantum yield
      call reaction_config%get( "quantum yield", quantum_yield_config, Iam )
      quantum_yield_ptr%val_ => quantum_yield_builder( quantum_yield_config,  &
                                            grid_warehouse, profile_warehouse )
      rates%quantum_yields_ = [ rates%quantum_yields_, quantum_yield_ptr ]

      ! finally get scaling factor factor
      call reaction_config%get( "rate constant alias factor",                 &
                                 rate_aliasing_factor, Iam, default = 1.0_dk )
      rates%scaling_factors_ = [ rates%scaling_factors_, rate_aliasing_factor ]
    end do
    deallocate( iter )

    if( size( rates%cross_sections_ ) /= size( rates%quantum_yields_ ) ) then
      call die_msg( 131408672, Iam//': # cross sections != # quantum yields' )
    endif

    end associate

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> calculate photolysis rate constants
  subroutine get( this, la_srb, spherical_geometry, grid_warehouse,           &
      profile_warehouse, radiation_field, photolysis_rates, file_tag )

    use musica_assert,                 only : die_msg
    use tuvx_constants,                only : hc
    use tuvx_diagnostic_util,          only : diagout
    use tuvx_la_sr_bands,              only : la_srb_t
    use tuvx_radiative_transfer_solver,only : radField_t
    use tuvx_spherical_geometry,       only : spherical_geom_t

    !> Photolysis rate constant calculator
    class(photolysis_rates_t), intent(inout) :: this
    !> Spherical geometry
    type(spherical_geom_t),    intent(inout) :: spherical_geometry
    !> Lyman Alpha, Schumann-Runge bands
    type(la_srb_t),            intent(inout) :: la_srb
    !> Grid warehouse
    type(grid_warehouse_t),    intent(inout) :: grid_warehouse
    !> Profile warehouse
    type(profile_warehouse_t), intent(inout) :: profile_warehouse
    !> Actinic flux
    type(radField_t),          intent(in)    :: radiation_field
    !> Tag used in file name of output data
    character(len=*),          intent(in)    :: file_tag
    !> Calculated photolysis rate constants
    real(dk), allocatable,     intent(inout) :: photolysis_rates(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = "photolysis rates calculator"
    integer               :: vertNdx, rateNdx, nRates
    real(dk), allocatable :: airVcol(:), airScol(:)
    real(dk), allocatable :: bin_factor(:)
    real(dk), allocatable :: xsqyWrk(:)
    real(dk), allocatable :: cross_section(:,:)
    real(dk), allocatable :: quantum_yield(:,:)
    real(dk), allocatable :: xsqy(:,:)
    real(dk), allocatable :: actinicFlux(:,:)
    type(string_t)        :: Handle, annotatedRate
    character(len=64)     :: jlabel
    character(len=64), allocatable :: annotatedjlabel(:)
    class(abs_1d_grid_t), pointer :: zGrid => null()
    class(abs_1d_grid_t), pointer :: lambdaGrid => null()
    class(abs_profile_t), pointer :: airProfile => null()
    class(abs_profile_t), pointer :: etfl => null()

    Handle = 'Vertical Z'
    zGrid => grid_warehouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength'
    lambdaGrid => grid_warehouse%get_grid( Handle )
    Handle = 'Etfl'
    etfl  => profile_warehouse%get_profile( Handle )

    nRates = size( this%cross_sections_ )
    if( .not. allocated( photolysis_rates ) ) then
      allocate( photolysis_rates( zGrid%ncells_ + 1, nRates ) )
    endif

    bin_factor = ( etfl%mid_val_ * 1.e-13_dk * lambdaGrid%mid_ *              &
                   lambdaGrid%delta_ ) / hc
    actinicFlux = transpose( radiation_field%fdr_ + radiation_field%fup_ +    &
                             radiation_field%fdn_ )
    do vertNdx = 1, zGrid%ncells_ + 1
      actinicFlux( :, vertNdx ) = actinicFlux( :, vertNdx ) * bin_factor
    enddo

    allocate( annotatedjlabel( nRates ) )
    allocate( xsqyWrk(0) )

rate_loop:                                                                    &
    do rateNdx = 1, nRates
      associate( calc_ftn => this%cross_sections_( rateNdx )%val_ )
        cross_section = calc_ftn%calculate( grid_warehouse, profile_warehouse )
      end associate
      associate( calc_ftn => this%quantum_yields_( rateNdx )%val_ )
        quantum_yield = calc_ftn%calculate( grid_warehouse, profile_warehouse )
        if( this%handles_( rateNdx ) == 'HNO4+hv->HO2+NO2' .or.               &
            this%handles_( rateNdx ) == 'NOCl+hv->NO+Cl' ) then
          if( any( quantum_yield /= 1._dk ) ) then
            call die_msg( 54321, 'HNO4 quantum yield != 1.0' )
          endif
        endif
      end associate

      ! O2 photolysis can have special la & srb band handling
      if( trim( this%handles_( rateNdx )%to_char( ) ) == 'O2+hv->O+O' ) then
        Handle = 'Air' ; airProfile => profile_warehouse%get_profile( Handle )
        allocate( airVcol( airProfile%ncells_ ),                              &
                  airScol( airProfile%ncells_ + 1 ) )
        call spherical_geometry%airmas( airProfile%exo_layer_dens_, airVcol,  &
                                        airScol )
        call la_srb%calculate_xs( grid_warehouse, profile_warehouse, airVcol, &
                                  airScol, cross_section )
        deallocate( airVcol, airScol )
      endif

      xsqyWrk = [ xsqyWrk, reshape( cross_section * quantum_yield,            &
                                    (/ size( cross_section ) /) ) ]

      annotatedRate = this%handles_( rateNdx )//'.xsect.new'
      call diagout( trim( annotatedRate%to_char( ) ), cross_section )
      annotatedRate = this%handles_( rateNdx )//'.qyld.new'
      call diagout( trim( annotatedRate%to_char( ) ), quantum_yield )
      annotatedRate = this%handles_( rateNdx )//'.xsqy.new'
      call diagout( trim( annotatedRate%to_char( ) ),                         &
                    cross_section * quantum_yield )

      xsqy = transpose( cross_section * quantum_yield )
      do vertNdx = 1, zGrid%ncells_ + 1
        photolysis_rates( vertNdx, rateNdx ) =                                &
            dot_product( actinicFlux( :, vertNdx ), xsqy( :, vertNdx ) )
      enddo
      if( allocated( cross_section ) ) deallocate( cross_section )
      if( allocated( quantum_yield ) ) deallocate( quantum_yield )
    end do rate_loop

    open( unit = 33, file = 'OUTPUTS/annotatedjlabels.new' )
    do rateNdx = 1, nRates
      jlabel = this%handles_( rateNdx )%to_char( )
      write(33,*) this%handles_( rateNdx )%to_char( )
    enddo
    close( unit = 33 )
    open( unit = 33, file = 'OUTPUTS/xsqy.'//file_tag//'.new',                &
          form = 'unformatted' )
      write(33) xsqyWrk
    close( unit = 33 )

  end subroutine get

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalize the photorates rates
  subroutine finalize( this )

    !> Photolysis rate calculator
    type(photolysis_rates_t), intent(inout) :: this

    integer :: ndx

    if( allocated( this%cross_sections_ ) ) then
      do ndx = 1,size( this%cross_sections_ )
        if( associated( this%cross_sections_( ndx )%val_ ) ) then
          deallocate( this%cross_sections_( ndx )%val_ )
        endif
      enddo
      deallocate( this%cross_sections_ )
    end if

    if( allocated( this%quantum_yields_ ) ) then
      do ndx = 1,size( this%quantum_yields_ )
        if( associated( this%quantum_yields_( ndx )%val_ ) ) then
          deallocate( this%quantum_yields_( ndx )%val_ )
        endif
      enddo
      deallocate( this%quantum_yields_ )
    end if

    if( allocated( this%scaling_factors_ ) ) then
      deallocate( this%scaling_factors_ )
    end if
    if( allocated( this%handles_ ) ) then
      deallocate( this%handles_ )
    end if

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_photolysis_rates
