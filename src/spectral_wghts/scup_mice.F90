! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This scup_mice_spectral_wght module

!> The scup mice type and related functions
module tuvx_spectral_wght_scup_mice

  use tuvx_spectral_wght,    only : spectral_wght_t
  use musica_constants,      only : dk => musica_dk

  implicit none

  private
  public :: spectral_wght_scup_mice_t

  !> Calculator for scup_mice_spectral_wght
  type, extends(spectral_wght_t) :: spectral_wght_scup_mice_t
  contains
    !> Calculate the spectral wght
    procedure :: calculate => run
  end type spectral_wght_scup_mice_t

  !> Constructor
  interface spectral_wght_scup_mice_t
    module procedure constructor
  end interface spectral_wght_scup_mice_t

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
    character(len=*), parameter :: Iam = 'scup mice constructor: '
    type(string_t) :: required_keys(1), optional_keys(1)

    required_keys(1) = "type"
    optional_keys(1) = "name"
    call assert_msg( 399197234,                                               &
                     config%validate( required_keys, optional_keys ),         &
                     "Bad configuration data format for "//                   &
                     "scup mice spectral wght." )

    allocate (spectral_wght_scup_mice_t :: this )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the spectral weight
  function run( this, grid_warehouse, profile_warehouse ) result( spectral_wght )

    use musica_string,          only  :  string_t
    use tuvx_grid_warehouse,    only  :  grid_warehouse_t
    use tuvx_profile_warehouse, only  :  profile_warehouse_t
    use tuvx_grid,              only  :  grid_t

    !> Arguments
    class(spectral_wght_scup_mice_t), intent(in) :: this
    !> Warehouses
    type(grid_warehouse_t), intent(inout)              :: grid_warehouse
    type(profile_warehouse_t), intent(inout)           :: profile_warehouse
    !> Calculated spectral wght
    real(kind=dk), allocatable                         :: spectral_wght(:)

    !> Local variables
    character(len=*), parameter :: Iam = 'scup mice calculate: '
    real(dk), allocatable       :: factor(:)

    class(grid_t), pointer      :: lambdaGrid => null()

    lambdaGrid => grid_warehouse%get_grid( "wavelength", "nm" )

!   allocate( spectral_wght(lambdaGrid%ncells_) )

    factor = 1._dk/sw_futr( (/300._dk/) )
    spectral_wght = sw_futr( lambdaGrid%mid_ ) * factor(1)

    if( associated( lambdaGrid ) ) deallocate( lambdaGrid )

  end function run

  function sw_futr(w) result( futr )
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Calculate the action spectrum value for skin cancer of albino hairless   =*
!=  mice at a given wavelength according to:  deGRuijl, F.R., H.J.C.M.Steren-=*
!=  borg, P.D.Forbes, R.E.Davies, C.Colse, G.Kelfkens, H.vanWeelden,         =*
!=  and J.C.van der Leun, Wavelength dependence of skin cancer induction by  =*
!=  ultraviolet irradiation of albino hairless mice, Cancer Research, vol 53,=*
!=  pp. 53-60, 1993                                                          =*
!=  (Action spectrum for carcinomas)                                         =*
!-----------------------------------------------------------------------------*
!=  PARAMETERS:                                                              =*
!=  W  - real, wavelength (nm)                                            (I)=*
!-----------------------------------------------------------------------------*

      !> Arguments
      real(dk), intent(in)  :: w(:)
      real(dk), allocatable :: futr(:)

      real(dk), allocatable :: t1(:), t2(:), t3(:), t4(:), t5(:)
      real(dk), allocatable :: p(:)

      real(dk), parameter :: a1 = -10.91_dk
      real(dk), parameter :: a2 = - 0.86_dk
      real(dk), parameter :: a3 = - 8.60_dk
      real(dk), parameter :: a4 = - 9.36_dk
      real(dk), parameter :: a5 = -13.15_dk

      real(dk), parameter :: x1 = 270._dk
      real(dk), parameter :: x2 = 302._dk
      real(dk), parameter :: x3 = 334._dk
      real(dk), parameter :: x4 = 367._dk
      real(dk), parameter :: x5 = 400._dk

      real(dk), parameter :: b1 = (x1-x2)*(x1-x3)*(x1-x4)*(x1-x5)
      real(dk), parameter :: b2 = (x2-x1)*(x2-x3)*(x2-x4)*(x2-x5)
      real(dk), parameter :: b3 = (x3-x1)*(x3-x2)*(x3-x4)*(x3-x5)
      real(dk), parameter :: b4 = (x4-x1)*(x4-x2)*(x4-x3)*(x4-x5)
      real(dk), parameter :: b5 = (x5-x1)*(x5-x2)*(x5-x3)*(x5-x4)

      real(dk), parameter :: w1 = a1/b1
      real(dk), parameter :: w2 = a2/b2
      real(dk), parameter :: w3 = a3/b3
      real(dk), parameter :: w4 = a4/b4
      real(dk), parameter :: w5 = a5/b5

      t1 = (w-x2)*(w-x3)*(w-x4)*(w-x5)
      t2 = (w-x1)*(w-x3)*(w-x4)*(w-x5)
      t3 = (w-x1)*(w-x2)*(w-x4)*(w-x5)
      t4 = (w-x1)*(w-x2)*(w-x3)*(w-x5)
      t5 = (w-x1)*(w-x2)*(w-x3)*(w-x4)

      p = w1*t1 + w2*t2 + w3*t3 + w4*t4 + w5*t5

      futr  = exp(p)

      end function sw_futr

end module tuvx_spectral_wght_scup_mice
