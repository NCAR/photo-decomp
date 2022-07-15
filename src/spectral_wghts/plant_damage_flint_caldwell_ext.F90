! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This plant_damage_flint_caldwell_ext_spectral_wght module

!> The plant damage flint caldwell ext type and related functions
module tuvx_spectral_wght_plant_damage_flint_caldwell_ext

  use tuvx_spectral_wght,    only : spectral_wght_t
  use musica_constants,      only : dk => musica_dk

  implicit none

  private
  public :: spectral_wght_plant_damage_flint_caldwell_ext_t

  !> Calculator for plant damage flint caldwell ext spectral_wght
  type, extends(spectral_wght_t) :: spectral_wght_plant_damage_flint_caldwell_ext_t
  contains
    !> Calculate the spectral wght
    procedure :: calculate => run
  end type spectral_wght_plant_damage_flint_caldwell_ext_t

  !> Constructor
  interface spectral_wght_plant_damage_flint_caldwell_ext_t
    module procedure constructor
  end interface spectral_wght_plant_damage_flint_caldwell_ext_t

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
    character(len=*), parameter :: Iam = 'plant damage flint caldwell ext constructor: '
    type(string_t) :: required_keys(1), optional_keys(1)

    required_keys(1) = "type"
    optional_keys(1) = "name"
    call assert_msg( 399197234,                                               &
                     config%validate( required_keys, optional_keys ),         &
                     "Bad configuration data format for "//                   &
                     "plant damage flint caldwell ext spectral wght." )

    allocate (spectral_wght_plant_damage_flint_caldwell_ext_t :: this )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the spectral weight
  function run( this, grid_warehouse, profile_warehouse ) result( spectral_wght )

    use musica_string,          only  :  string_t
    use tuvx_grid_warehouse,    only  :  grid_warehouse_t
    use tuvx_profile_warehouse, only  :  profile_warehouse_t
    use tuvx_grid,              only  :  grid_t

    !> Arguments
    class(spectral_wght_plant_damage_flint_caldwell_ext_t), intent(in) :: this
    !> Warehouses
    type(grid_warehouse_t), intent(inout)              :: grid_warehouse
    type(profile_warehouse_t), intent(inout)           :: profile_warehouse
    !> Calculated spectral wght
    real(kind=dk), allocatable                         :: spectral_wght(:)

    !> Local variables
    character(len=*), parameter :: Iam = 'plant damage flint caldwell ext calculate: '
    real(dk), parameter  :: a0 = 4.688272_dk
    real(dk), parameter  :: a1 = .1703411_dk
    real(dk), parameter  :: w1 = 307.867_dk
    real(dk), parameter  :: w2 = 390._dk

    class(grid_t), pointer      :: lambdaGrid => null()

    lambdaGrid => grid_warehouse%get_grid( "wavelength", "nm" )

    spectral_wght = exp( a0*exp( -exp(a1*(lambdaGrid%mid_ - w1)/1.15_dk) ) &
                         + ((w2 - lambdaGrid%mid_)/121.7557_dk - 4.183832_dk) )
    spectral_wght = spectral_wght * lambdaGrid%mid_ / 300._dk
    where( spectral_wght < 0.0_dk .or. lambdaGrid%mid_ > 390._dk )
      spectral_wght = 0.0_dk
    endwhere

    if( associated( lambdaGrid ) ) deallocate( lambdaGrid )

  end function run

end module tuvx_spectral_wght_plant_damage_flint_caldwell_ext
