! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This standard_human_erythema_spectral_wght module

!> The standard human erythema type and related functions
module tuvx_spectral_wght_standard_human_erythema

  use tuvx_spectral_wght,    only : spectral_wght_t
  use musica_constants,      only : dk => musica_dk

  implicit none

  private
  public :: spectral_wght_standard_human_erythema_t

  !> Calculator for standard_human_erythema_spectral_wght
  type, extends(spectral_wght_t) :: spectral_wght_standard_human_erythema_t
  contains
    !> Calculate the spectral wght
    procedure :: calculate => run
  end type spectral_wght_standard_human_erythema_t

  !> Constructor
  interface spectral_wght_standard_human_erythema_t
    module procedure constructor
  end interface spectral_wght_standard_human_erythema_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize the spectral wght
  function constructor( config, grid_warehouse, profile_warehouse )           &
      result ( this )

    use musica_assert,                 only : assert_msg
    use musica_config,                 only : config_t
    use musica_string,                 only : string_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    !> Arguments
    class(spectral_wght_t),    pointer       :: this
    type(config_t),            intent(inout) :: config
    type(grid_warehouse_t),    intent(inout) :: grid_warehouse
    type(profile_warehouse_t), intent(inout) :: profile_warehouse

    !> Local variables
    character(len=*), parameter :: Iam = 'standard human erythema constructor: '
    type(string_t) :: required_keys(1), optional_keys(1)

    required_keys(1) = "type"
    optional_keys(1) = "name"
    call assert_msg( 399197234,                                               &
                     config%validate( required_keys, optional_keys ),         &
                     "Bad configuration data format for "//                   &
                     "standard human erythema spectral wght." )

    allocate (spectral_wght_standard_human_erythema_t :: this )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the spectral weight
  function run( this, grid_warehouse, profile_warehouse ) result( spectral_wght )

    use musica_string,          only  :  string_t
    use tuvx_grid_warehouse,    only  :  grid_warehouse_t
    use tuvx_profile_warehouse, only  :  profile_warehouse_t
    use tuvx_grid,              only  :  grid_t

    !> Arguments
    class(spectral_wght_standard_human_erythema_t), intent(in) :: this
    !> Warehouses
    type(grid_warehouse_t), intent(inout)              :: grid_warehouse
    type(profile_warehouse_t), intent(inout)           :: profile_warehouse
    !> Calculated spectral wght
    real(kind=dk), allocatable                         :: spectral_wght(:)

    !> Local variables
    character(len=*), parameter :: Iam = 'standard human erythema calculate: '

    class(grid_t), pointer      :: lambdaGrid => null()

    lambdaGrid => grid_warehouse%get_grid( "wavelength", "nm" )

    spectral_wght = sw_fery( lambdaGrid%mid_ )

    if( associated( lambdaGrid ) ) deallocate( lambdaGrid )

  end function run

  function sw_fery(w) result( fery )
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Calculate the action spectrum value for erythema at a given wavelength   =*
!* Webb, A.R., H. Slaper, P. Koepke, and A. W. Schmalwieser, 
!* Know your standard: Clarifying the CIE erythema action spectrum,
!* Photochem. Photobiol. 87, 483-486, 2011.
!=  Value at 300 nm = 0.6486                                                 =*
!-----------------------------------------------------------------------------*
!=  PARAMETERS:                                                              =*
!=  W - REAL, wavelength (nm)                                             (I)=*
!-----------------------------------------------------------------------------*

      real(dk), intent(in)  :: w(:)
      real(dk), allocatable :: fery(:)

      allocate( fery(size(w)) )

      where( w <= 298._dk )
        fery = 1._dk
      elsewhere( w > 298._dk .and. w <= 328._dk )
        fery = 10._dk**(.094_dk*(298._dk - w))
      elsewhere( w > 328._dk .and. w <= 400._dk )
        fery = 10._dk**(.015_dk*(140._dk - w))
      elsewhere
        fery = 1.e-36_dk
      endwhere

  end function sw_fery

end module tuvx_spectral_wght_standard_human_erythema
