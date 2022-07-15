! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This gaussian_spectral_wght module

!> The gaussian type and related functions
module tuvx_spectral_wght_gaussian

  use tuvx_spectral_wght,    only : spectral_wght_t
  use musica_constants,      only : dk => musica_dk

  implicit none

  private
  public :: spectral_wght_gaussian_t

  !> Calculator for gaussian_spectral_wght
  type, extends(spectral_wght_t) :: spectral_wght_gaussian_t
    !> The gaussian centroid
    real(dk) :: centroid
  contains
    !> Calculate the spectral wght
    procedure :: calculate => run
  end type spectral_wght_gaussian_t

  !> Constructor
  interface spectral_wght_gaussian_t
    module procedure constructor
  end interface spectral_wght_gaussian_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize the spectral wght
  function constructor( config, grid_warehouse, profile_warehouse )           &
      result ( this )

    use musica_assert,                 only : assert_msg
    use musica_config,                 only : config_t
    use musica_string,                 only : string_t
    use tuvx_spectral_wght,            only : base_constructor
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    !> Arguments
    class(spectral_wght_t),    pointer       :: this
    type(config_t),            intent(inout) :: config
    type(grid_warehouse_t),    intent(inout) :: grid_warehouse
    type(profile_warehouse_t), intent(inout) :: profile_warehouse

    !> Local variables
    character(len=*), parameter :: Iam = 'gaussian constructor: '
    type(string_t) :: required_keys(2), optional_keys(1)

    required_keys(1) = "type"
    required_keys(2) = "centroid"
    optional_keys(1) = "name"
    call assert_msg( 399197574,                                               &
                     config%validate( required_keys, optional_keys ),         &
                     "Bad configuration data format for "//                   &
                     "gaussian spectral wght." )

    allocate (spectral_wght_gaussian_t :: this )
    select type( this )
      class is( spectral_wght_gaussian_t )
        call config%get( 'centroid', this%centroid, Iam )
    end select

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the spectral weight
  function run( this, grid_warehouse, profile_warehouse ) result( spectral_wght )

    use musica_string,          only  :  string_t
    use tuvx_grid_warehouse,    only  :  grid_warehouse_t
    use tuvx_profile_warehouse, only  :  profile_warehouse_t
    use tuvx_grid,              only  :  grid_t

    !> Arguments
    class(spectral_wght_gaussian_t), intent(in) :: this
    !> Warehouses
    type(grid_warehouse_t), intent(inout)              :: grid_warehouse
    type(profile_warehouse_t), intent(inout)           :: profile_warehouse
    !> Calculated spectral wght
    real(kind=dk), allocatable                         :: spectral_wght(:)

    !> Local variables
    real(dk), parameter         :: rTWO = 2.0_dk
    character(len=*), parameter :: Iam = 'gaussian calculate: '
    real(dk)                    :: accum

    class(grid_t), pointer      :: lambdaGrid => null()

    lambdaGrid => grid_warehouse%get_grid( "wavelength", "nm" )

    spectral_wght = exp( -(log(rTWO)*.04_dk*(lambdaGrid%mid_ - this%centroid)**2) )
    accum = sum( spectral_wght )
    spectral_wght = spectral_wght/accum

    if( associated( lambdaGrid ) ) deallocate( lambdaGrid )

  end function run

end module tuvx_spectral_wght_gaussian
