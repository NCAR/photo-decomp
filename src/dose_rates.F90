! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The tuvx_dose_rates module

!> The dose_rates_t type and related functions
module tuvx_dose_rates

  use musica_constants,                only : dk => musica_dk
  use musica_string,                   only : string_t
  use tuvx_spectral_wght,              only : spectral_wght_ptr
  use tuvx_grid_warehouse,             only : grid_warehouse_t
  use tuvx_grid,                       only : grid_t
  use tuvx_profile_warehouse,          only : profile_warehouse_t
  use tuvx_profile,                    only : profile_t

  implicit none

  private
  public :: dose_rates_t

  !> Photolysis rate constant calculator
  type :: dose_rates_t
    !> Spectral weights
    type(spectral_wght_ptr), allocatable :: spectral_wghts_(:)
    !> Configuration label for the dose rate
    type(string_t), allocatable          :: handles_(:)
  contains
    !> Returns the dose rates for a given set of conditions
    procedure :: get
    !> Finalize the object
    final :: finalize
  end type dose_rates_t

  !> dose_rates_t constructor
  interface dose_rates_t
    module procedure :: constructor
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructor of dose_rates_t objects
  function constructor( config, grid_warehouse, profile_warehouse )     &
      result( dose_rates )

    use musica_config,                 only : config_t
    use musica_iterator,               only : iterator_t
    use tuvx_spectral_wght_factory,    only : spectral_wght_builder

    !> Arguments
    type(config_t),            intent(inout) :: config
    !> grid warehouse
    type(grid_warehouse_t),    intent(inout) :: grid_warehouse
    !> profile warehouse
    type(profile_warehouse_t), intent(inout) :: profile_warehouse
    !> New dose rates
    class(dose_rates_t),        pointer      :: dose_rates

    !> Local variables
    character(len=*), parameter :: Iam = "dose_rates_t constructor"

    integer        :: nRates
    type(config_t) :: wght_config, spectral_wght_config
    class(iterator_t), pointer  :: iter
    type(spectral_wght_ptr)     :: spectral_wght_ptr
    character(len=64)           :: keychar
    type(string_t)              :: netcdfFile, Object
    type(string_t)              :: wght_key
    type(string_t), allocatable :: netcdfFiles(:)

    allocate( dose_rates )

    associate( rates => dose_rates )

    allocate( string_t :: rates%handles_(0) )
    allocate( rates%spectral_wghts_(0) )

    ! iterate over dose rates
    iter => config%get_iterator( )
    do while( iter%next( ) )
      keychar  = config%key( iter )
      wght_key = keychar
      rates%handles_ = [ rates%handles_, wght_key ]
      call config%get( iter, wght_config, Iam )

      ! get spectral wght
      call wght_config%get( "weights", spectral_wght_config, Iam )
      spectral_wght_ptr%val_ => &
         spectral_wght_builder( spectral_wght_config, grid_warehouse, profile_warehouse )
      rates%spectral_wghts_ = [ rates%spectral_wghts_, spectral_wght_ptr ]
    end do
    deallocate( iter )

    end associate

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> calculate dose rate constants
  subroutine get( this, grid_warehouse, profile_warehouse, &
                  radiation_field, dose_rates, file_tag )

    use musica_assert,                 only : die_msg
    use tuvx_constants,                only : hc
    use tuvx_diagnostic_util,          only : diagout
    use tuvx_radiative_transfer_solver,only : radField_t

    !> Dose rate constant calculator
    !> Arguments
    class(dose_rates_t),       intent(inout) :: this
    !> Warehouses
    type(grid_warehouse_t),    intent(inout) :: grid_warehouse
    type(profile_warehouse_t), intent(inout) :: profile_warehouse
    !> Actinic flux
    type(radField_t),          intent(in)    :: radiation_field
    !> Tag used in file name of output data
    character(len=*),          intent(in)    :: file_tag
    !> Calculated dose rate constants
    real(dk), allocatable,     intent(inout) :: dose_rates(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = "dose rates calculator:"
    integer               :: wavNdx, rateNdx, nRates
    real(dk), allocatable :: spectral_wght(:)
    real(dk), allocatable :: tmp_spectral_wght(:)
    real(dk), allocatable :: sirrad(:,:)
    character(len=64), allocatable :: annotatedslabel(:)
    class(grid_t),    pointer :: zGrid => null()
    class(grid_t),    pointer :: lambdaGrid => null()
    class(profile_t), pointer :: etfl => null()

    zGrid => grid_warehouse%get_grid( "height", "km" )
    lambdaGrid => grid_warehouse%get_grid( "wavelength", "nm" )
    etfl => profile_warehouse%get_profile( "extraterrestrial flux",           &
                                           "photon cm-2 s-1" )

    allocate( tmp_spectral_wght(0) )

    nRates = size( this%spectral_wghts_ )
    if( .not. allocated( dose_rates ) ) then
      allocate( dose_rates( zGrid%ncells_ + 1, nRates ) )
    endif

    !> spectral irradiance
    sirrad = radiation_field%edr_ + radiation_field%eup_ + radiation_field%edn_
    do wavNdx = 1, lambdaGrid%ncells_
      sirrad( :, wavNdx ) = sirrad( :, wavNdx ) * etfl%mid_val_( wavNdx )
    enddo

    allocate( annotatedslabel( nRates ) )

rate_loop:                                                                    &
    do rateNdx = 1, nRates
      associate( calc_ftn => this%spectral_wghts_( rateNdx )%val_ )
        spectral_wght = calc_ftn%calculate( grid_warehouse, profile_warehouse )
      end associate

      tmp_spectral_wght = [ tmp_spectral_wght, spectral_wght ]
      dose_rates( :, rateNdx ) = matmul( sirrad, spectral_wght )

      if( allocated( spectral_wght ) ) deallocate( spectral_wght )
    end do rate_loop

    open( unit = 33, file = 'OUTPUTS/annotatedslabels.new' )
    do rateNdx = 1, nRates
      write(33,'(a)') this%handles_( rateNdx )%to_char( )
    enddo
    close( unit = 33 )
    open( unit = 33, file = 'OUTPUTS/sw.'//file_tag//'.new',                &
          form = 'unformatted' )
    write(33) tmp_spectral_wght
    close( unit = 33 )

    if( associated( zGrid ) ) deallocate( zGrid )
    if( associated( lambdaGrid ) ) deallocate( lambdaGrid )
    if( associated( etfl ) ) deallocate( etfl )

  end subroutine get

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalize the dose rates object
  subroutine finalize( this )

    !> Dose rate object
    type(dose_rates_t), intent(inout) :: this

    integer :: ndx

    if( allocated( this%spectral_wghts_ ) ) then
      do ndx = 1,size( this%spectral_wghts_ )
        if( associated( this%spectral_wghts_( ndx )%val_ ) ) then
          deallocate( this%spectral_wghts_( ndx )%val_ )
        endif
      enddo
      deallocate( this%spectral_wghts_ )
    end if

    if( allocated( this%handles_ ) ) deallocate( this%handles_ )

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_dose_rates
