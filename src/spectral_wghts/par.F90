! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This par_spectral_wght module

!> The par type and related functions
module tuvx_spectral_wght_par

  use tuvx_spectral_wght,    only : spectral_wght_t
  use musica_constants,      only : dk => musica_dk

  implicit none

  private
  public :: spectral_wght_par_t

  !> Calculator for par_spectral_wght
  type, extends(spectral_wght_t) :: spectral_wght_par_t
  contains
    !> Calculate the spectral wght
    procedure :: calculate => run
  end type spectral_wght_par_t

  !> Constructor
  interface spectral_wght_par_t
    module procedure constructor
  end interface spectral_wght_par_t

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
    character(len=*), parameter :: Iam = 'par constructor: '
    type(string_t) :: required_keys(1), optional_keys(1)

    required_keys(1) = "type"
    optional_keys(1) = "name"
    call assert_msg( 399197564,                                               &
                     config%validate( required_keys, optional_keys ),         &
                     "Bad configuration data format for "//                   &
                     "par spectral wght." )

    allocate (spectral_wght_par_t :: this )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the spectral weight
  function run( this, grid_warehouse, profile_warehouse ) result( spectral_wght )

    use musica_string,          only  :  string_t
    use tuvx_grid_warehouse,    only  :  grid_warehouse_t
    use tuvx_profile_warehouse, only  :  profile_warehouse_t
    use tuvx_grid,              only  :  grid_t

    !> Arguments
    class(spectral_wght_par_t), intent(in) :: this
    !> Warehouses
    type(grid_warehouse_t), intent(inout)              :: grid_warehouse
    type(profile_warehouse_t), intent(inout)           :: profile_warehouse
    !> Calculated spectral wght
    real(kind=dk), allocatable                         :: spectral_wght(:)

    !> Local variables
    real(dk), parameter         :: rZERO = 0.0_dk
    real(dk), parameter         :: rONE  = 1.0_dk
    character(len=*), parameter :: Iam = 'par calculate: '

    class(grid_t), pointer      :: lambdaGrid => null()

    lambdaGrid => grid_warehouse%get_grid( "wavelength", "nm" )

    allocate( spectral_wght(lambdaGrid%ncells_) )

    where( 400._dk < lambdaGrid%mid_ .and. &
                     lambdaGrid%mid_ < 700._dk )
      spectral_wght = 8.36e-3_dk*lambdaGrid%mid_
    elsewhere
      spectral_wght = rZERO
    endwhere

    if( associated( lambdaGrid ) ) deallocate( lambdaGrid )

  end function run

end module tuvx_spectral_wght_par
