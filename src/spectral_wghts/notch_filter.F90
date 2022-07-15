! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This notch_filter_spectral_wght module

!> The notch_filter type and related functions
module tuvx_spectral_wght_notch_filter

  use tuvx_spectral_wght,    only : spectral_wght_t
  use musica_constants,      only : dk => musica_dk

  implicit none

  private
  public :: spectral_wght_notch_filter_t

  !> Calculator for notch_filter_spectral_wght
  type, extends(spectral_wght_t) :: spectral_wght_notch_filter_t
    !> The notch filter endpoints
    real(dk) :: notch_filter_begin
    real(dk) :: notch_filter_end
  contains
    !> Calculate the spectral wght
    procedure :: calculate => run
  end type spectral_wght_notch_filter_t

  !> Constructor
  interface spectral_wght_notch_filter_t
    module procedure constructor
  end interface spectral_wght_notch_filter_t

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
    character(len=*), parameter :: Iam = 'notch_filter constructor: '
    type(string_t) :: required_keys(3), optional_keys(1)

    required_keys(1) = "type"
    required_keys(2) = "notch filter begin"
    required_keys(3) = "notch filter end"
    optional_keys(1) = "name"
    call assert_msg( 399197574,                                               &
                     config%validate( required_keys, optional_keys ),         &
                     "Bad configuration data format for "//                   &
                     "notch_filter spectral wght." )

    allocate (spectral_wght_notch_filter_t :: this )
    select type( this )
      class is( spectral_wght_notch_filter_t )
        call config%get( 'notch filter begin', this%notch_filter_begin, Iam )
        call config%get( 'notch filter end', this%notch_filter_end, Iam )
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
    class(spectral_wght_notch_filter_t), intent(in) :: this
    !> Warehouses
    type(grid_warehouse_t), intent(inout)              :: grid_warehouse
    type(profile_warehouse_t), intent(inout)           :: profile_warehouse
    !> Calculated spectral wght
    real(kind=dk), allocatable                         :: spectral_wght(:)

    !> Local variables
    real(dk), parameter         :: rZERO = 0.0_dk
    real(dk), parameter         :: rONE  = 1.0_dk
    character(len=*), parameter :: Iam = 'notch_filter calculate: '

    class(grid_t), pointer      :: lambdaGrid => null()

    lambdaGrid => grid_warehouse%get_grid( "wavelength", "nm" )

    allocate( spectral_wght(lambdaGrid%ncells_) )

    where( this%notch_filter_begin < lambdaGrid%mid_ .and. &
           lambdaGrid%mid_ < this%notch_filter_end )
      spectral_wght = rONE
    elsewhere
      spectral_wght = rZERO
    endwhere

    if( associated( lambdaGrid ) ) deallocate( lambdaGrid )

  end function run

end module tuvx_spectral_wght_notch_filter
