! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This phytoplankton_boucher_spectral_wght module

!> The phytoplankton boucher type and related functions
module tuvx_spectral_wght_phytoplankton_boucher

  use tuvx_spectral_wght,    only : spectral_wght_t
  use musica_constants,      only : dk => musica_dk

  implicit none

  private
  public :: spectral_wght_phytoplankton_boucher_t

  !> Calculator for phytoplankton boucher spectral_wght
  type, extends(spectral_wght_t) :: spectral_wght_phytoplankton_boucher_t
  contains
    !> Calculate the spectral wght
    procedure :: calculate => run
  end type spectral_wght_phytoplankton_boucher_t

  !> Constructor
  interface spectral_wght_phytoplankton_boucher_t
    module procedure constructor
  end interface spectral_wght_phytoplankton_boucher_t

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
    character(len=*), parameter :: Iam = 'phytoplankton boucher constructor: '
    type(string_t) :: required_keys(1), optional_keys(1)

    required_keys(1) = "type"
    optional_keys(1) = "name"
    call assert_msg( 399197234,                                               &
                     config%validate( required_keys, optional_keys ),         &
                     "Bad configuration data format for "//                   &
                     "phytoplankton boucher spectral wght." )

    allocate (spectral_wght_phytoplankton_boucher_t :: this )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the spectral weight
  function run( this, grid_warehouse, profile_warehouse ) result( spectral_wght )

    use musica_string,          only  :  string_t
    use tuvx_grid_warehouse,    only  :  grid_warehouse_t
    use tuvx_profile_warehouse, only  :  profile_warehouse_t
    use tuvx_grid,              only  :  grid_t

    !> Arguments
    class(spectral_wght_phytoplankton_boucher_t), intent(in) :: this
    !> Warehouses
    type(grid_warehouse_t), intent(inout)              :: grid_warehouse
    type(profile_warehouse_t), intent(inout)           :: profile_warehouse
    !> Calculated spectral wght
    real(kind=dk), allocatable                         :: spectral_wght(:)

    !> Local variables
    character(len=*), parameter :: Iam = 'phytoplankton boucher calculate: '
    real(dk), parameter  :: em = -3.17e-6_dk
    real(dk), parameter  :: a  = 112.5_dk
    real(dk), parameter  :: b  = -.6223_dk
    real(dk), parameter  :: c  = 7.67e-4_dk

    class(grid_t), pointer      :: lambdaGrid => null()

    lambdaGrid => grid_warehouse%get_grid( "wavelength", "nm" )

    allocate( spectral_wght(lambdaGrid%ncells_) )

    where( lambdaGrid%mid_ > 290._dk .and. lambdaGrid%mid_ < 400._dk )
      spectral_wght = em + exp( a + lambdaGrid%mid_*(b + lambdaGrid%mid_*c) )
    elsewhere
      spectral_wght = 0.0_dk
    endwhere
    spectral_wght = max( 0.0_dk,spectral_wght )

    if( associated( lambdaGrid ) ) deallocate( lambdaGrid )

  end function run

end module tuvx_spectral_wght_phytoplankton_boucher
