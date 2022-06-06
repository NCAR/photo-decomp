! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This ch3cocho+hv->ch3co+hco quantum yield module

!> The ch3cocho+hv->ch3co+hco quantum yield type and related functions
module tuvx_quantum_yield_ch3cocho_ch3co_hco

  use tuvx_quantum_yield,              only : quantum_yield_t, base_constructor

  implicit none

  private
  public :: quantum_yield_ch3cocho_ch3co_hco_t

  !> Calculator for ch3cocho+hv->ch3co+hco quantum yield
  type, extends(quantum_yield_t) :: quantum_yield_ch3cocho_ch3co_hco_t
  contains
    !> Calculate the quantum yield
    procedure :: calculate => run
  end type quantum_yield_ch3cocho_ch3co_hco_t

  !> Constructor
  interface quantum_yield_ch3cocho_ch3co_hco_t
    module procedure constructor
  end interface quantum_yield_ch3cocho_ch3co_hco_t

contains

function constructor( config, grid_warehouse, profile_warehouse ) result( this )

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

    allocate ( quantum_yield_ch3cocho_ch3co_hco_t :: this )

    call base_constructor( this, config, grid_warehouse, profile_warehouse )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the photorate quantum yield for a given set of environmental
  !! conditions
  function run( this, grid_warehouse, profile_warehouse )                     &
      result( quantum_yield )

    use musica_constants,              only : dk => musica_dk
    use musica_string,                 only : string_t
    use tuvx_grid,                     only : grid_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile,                  only : profile_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    class(quantum_yield_ch3cocho_ch3co_hco_t), intent(in) :: this
    type(grid_warehouse_t), intent(inout)    :: grid_warehouse
    type(profile_warehouse_t), intent(inout) :: profile_warehouse
    !> Calculated quantum_yield
    real(kind=dk), allocatable               :: quantum_yield(:,:)

    ! Local variables
    character(len=*), parameter :: Iam =                                      &
        'ch3cocho+hv->ch3co_hco quantum yield calculate'
    real(dk), parameter ::    rZERO = 0.0_dk
    real(dk), parameter ::    rONE  = 1.0_dk
    real(dk), parameter ::  lambdaL = 380._dk
    real(dk), parameter ::  lambdaU = 440._dk

    integer               :: nzdim, vertNdx
    integer               :: lambdaNdx
    real(dk)              :: phi0, kq, lambda, airfac, qy
    real(dk), allocatable :: modelDens(:)
    class(grid_t), pointer :: zGrid
    class(grid_t), pointer :: lambdaGrid
    class(profile_t), pointer :: mdlDensity
    type(string_t)                :: Handle

    Handle = 'Vertical Z'
    zGrid => grid_warehouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength'
    lambdaGrid => grid_warehouse%get_grid( Handle )
    Handle = 'Air'
    mdlDensity => profile_warehouse%get_profile( Handle )

    nzdim = zGrid%ncells_ + 1
    modelDens = mdlDensity%edge_val_

    allocate( quantum_yield( lambdaGrid%ncells_, nzdim ) )
    quantum_yield = rZERO

    ! zero pressure yield:
    ! 1.0 for wc < 380 nm
    ! 0.0 for wc > 440 nm
    ! linear in between:

    ! Pressure correction: quenching coefficient, torr-1
    ! in air, Koch and Moortgat:
    do vertNdx = 1, nzdim
      airfac = modelDens( vertNdx ) * 760._dk / 2.456E19_dk
      do lambdaNdx = 1, lambdaGrid%ncells_
        lambda = lambdaGrid%mid_( lambdaNdx )
        phi0 = rONE - ( lambda - 380._dk ) / 60._dk
        phi0 = max( min( phi0, rONE ), rZERO )
        kq = 1.36e8_dk * exp( -8793._dk / lambda )
        if( phi0 > rZERO ) then
          if( lambda >= lambdaL .and. lambda <= lambdaU ) then
            qy = phi0 / ( phi0 + kq * airfac )
          else
            qy = phi0
          endif
        else
          qy = rZERO
        endif
        quantum_yield( lambdaNdx, vertNdx ) = qy
      enddo
    enddo

    quantum_yield = transpose( quantum_yield )

  end function run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_quantum_yield_ch3cocho_ch3co_hco
