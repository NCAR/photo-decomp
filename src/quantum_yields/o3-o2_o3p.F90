! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This o3+hv->o2+o3p quantum yield module

!> The o3+hv->o2+o3p quantum yield type and related functions
module tuvx_quantum_yield_o3_o2_o3p

  use tuvx_quantum_yield,              only : quantum_yield_t, base_constructor

  implicit none

  private
  public :: quantum_yield_o3_o2_o3p_t

  !> Calculator for o3+hv->o2+o3p quantum yield
  type, extends(quantum_yield_t) :: quantum_yield_o3_o2_o3p_t
  contains
    !> Calculate the quantum yield
    procedure :: calculate => run
  end type quantum_yield_o3_o2_o3p_t

  !> Constructor
  interface quantum_yield_o3_o2_o3p_t
    module procedure constructor
  end interface quantum_yield_o3_o2_o3p_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function constructor( config, grid_warehouse, profile_warehouse )           &
      result( this )

    use musica_assert,                 only : die_msg
    use musica_config,                 only : config_t
    use musica_string,                 only : string_t
    use tuvx_grid,                     only : grid_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_netcdf_util,              only : netcdf_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t
    use tuvx_util,                     only : inter2

    class(quantum_yield_t),    pointer :: this
    !> quantum yield configuration data
    type(config_t),            intent(inout) :: config
    type(grid_warehouse_t),    intent(inout) :: grid_warehouse
    type(profile_warehouse_t), intent(inout) :: profile_warehouse

    allocate ( quantum_yield_o3_o2_o3p_t :: this )

    call base_constructor( this, config, grid_warehouse, profile_warehouse )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the quantum yield for a given set of environmental conditions
  !!
  !! Function to calculate the quantum yield O3 + hv -> O(1D) + O2,
  !! according to:
  !! Matsumi, Y., F. J. Comes, G. Hancock, A. Hofzumanhays, A. J. Hynes,
  !! M. Kawasaki, and A. R. Ravishankara, QUantum yields for production of O(1D)
  !! in the ultraviolet photolysis of ozone:  Recommendation based on evaluation
  !! of laboratory data, J. Geophys. Res., 107, 10.1029/2001JD000510, 2002.
  function run( this, grid_warehouse, profile_warehouse )                     &
      result( quantum_yield )

    use musica_constants,              only : dk => musica_dk
    use musica_string,                 only : string_t
    use tuvx_grid,                     only : grid_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile,                  only : profile_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    class(quantum_yield_o3_o2_o3p_t), intent(in) :: this
    type(grid_warehouse_t), intent(inout)        :: grid_warehouse
    type(profile_warehouse_t), intent(inout)     :: profile_warehouse
    !> Calculated quantum yield
    real(kind=dk), allocatable                   :: quantum_yield(:,:)

    ! Local variables
    real(dk), parameter :: a(3)  = (/ 0.8036_dk, 8.9061_dk, 0.1192_dk /)
    real(dk), parameter :: x(3)  = (/ 304.225_dk, 314.957_dk, 310.737_dk /)
    real(dk), parameter :: om(3) = (/ 5.576_dk, 6.601_dk, 2.187_dk /)
    real(dk), parameter ::   rZERO = 0.0_dk
    real(dk), parameter ::   rONE  = 1.0_dk

    character(len=*), parameter :: Iam = 'o3+hv->o2+o3p quantum yield calculate'

    integer     :: wNdx, vertNdx
    real(dk)    :: kt, q1, q2, T300, lambda
    real(dk)    :: qfac1, qfac2
    type(string_t)            :: Handle
    class(grid_t),    pointer :: lambdaGrid => null( )
    class(grid_t),    pointer :: zGrid => null( )
    class(profile_t), pointer :: Temperature => null( )

    Handle = 'Photolysis, wavelength'
    lambdaGrid => grid_warehouse%get_grid( Handle )
    Handle = 'Vertical Z'
    zGrid => grid_warehouse%get_grid( Handle )
    Handle = 'Temperature'
    Temperature => profile_warehouse%get_profile( Handle )

    allocate( quantum_yield( lambdaGrid%ncells_, zGrid%ncells_ + 1 ) )
    quantum_yield = rZERO

    associate( w => lambdaGrid%mid_, Temp => Temperature%edge_val_ )

    do vertNdx = 1, zGrid%ncells_ + 1
      kt = 0.695_dk * Temp( vertNdx )
      q1 = rONE
      q2 = exp( -825.518_dk / kt )
      qfac1 = q1 / (q1 + q2)
      qfac2 = q2 / (q1 + q2)
      T300 = Temp( vertNdx ) / 300._dk

      where( w(:) <= 305._dk )
        quantum_yield( :, vertNdx ) = 0.90_dk
      elsewhere( w(:) > 328._dk .and. w(:) <= 340._dk )
        quantum_yield( :, vertNdx ) = 0.08_dk
      endwhere
      do wNdx = 1, size(w)
        lambda = w( wNdx )
        if( lambda > 305._dk .and. lambda <= 328._dk ) then
          quantum_yield( wNdx, vertNdx ) = 0.0765_dk                           &
            + a(1) * qfac1 * EXP( -( ( x(1) - lambda ) / om(1) )**4 )          &
            + a(2) * T300 * T300 * qfac2 *                                     &
                                       EXP( -( (x(2) - lambda ) / om(2) )**2 ) &
            + a(3) * T300**1.5_dk * EXP( -( (x(3) - lambda ) / om(3) )**2 )
        endif
      enddo
    enddo

    end associate

    quantum_yield = transpose( rONE - quantum_yield )

    deallocate( zGrid )
    deallocate( lambdaGrid )
    deallocate( Temperature )

  end function run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_quantum_yield_o3_o2_o3p
