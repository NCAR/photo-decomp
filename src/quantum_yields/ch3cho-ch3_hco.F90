! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This ch3cho+hv->ch3+hco quantum yield module

!> The ch3cho+hv->ch3+hco quantum yield type and related functions
module tuvx_quantum_yield_ch3cho_ch3_hco

  use tuvx_quantum_yield,              only : quantum_yield_t, base_constructor

  implicit none

  private
  public :: quantum_yield_ch3cho_ch3_hco_t

  !> Calculator for ch3cho+hv->ch3+hco quantum yield
  type, extends(quantum_yield_t) :: quantum_yield_ch3cho_ch3_hco_t
  contains
    !> Calculate the quantum yield
    procedure :: calculate => run
  end type quantum_yield_ch3cho_ch3_hco_t

  !> Constructor
  interface quantum_yield_ch3cho_ch3_hco_t
    module procedure constructor
  end interface quantum_yield_ch3cho_ch3_hco_t

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

    allocate ( quantum_yield_ch3cho_ch3_hco_t :: this )

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

    class(quantum_yield_ch3cho_ch3_hco_t), intent(in) :: this
    type(grid_warehouse_t), intent(inout)    :: grid_warehouse
    type(profile_warehouse_t), intent(inout) :: profile_warehouse
    !> Calculated quantum_yield
    real(kind=dk), allocatable               :: quantum_yield(:,:)

    ! Local variables
    character(len=*), parameter :: Iam =                                      &
        'ch3cho+hv->ch3_hco quantum yield calculate'
    real(dk), parameter ::    rZERO = 0.0_dk
    real(dk), parameter ::    rONE  = 1.0_dk
    integer               :: nzdim, vertNdx
    real(dk)              :: air_dens_factor
    real(dk), allocatable :: quantum_yield_chnl1(:)
    real(dk), allocatable :: quantum_yield_chnl2(:)
    real(dk), allocatable :: quantum_yield_wrk(:)
    real(dk), allocatable         :: modelDens(:)
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
    allocate( quantum_yield_wrk( lambdaGrid%ncells_ ) )
    quantum_yield = rZERO

    quantum_yield_chnl1 = this%quantum_yield_parms(1)%array(:,2)
    quantum_yield_chnl2 = rONE - this%quantum_yield_parms(1)%array(:,1)
    where( quantum_yield_chnl1 > rZERO )
      quantum_yield_wrk = quantum_yield_chnl2 / quantum_yield_chnl1 - rONE
    elsewhere
      quantum_yield_wrk = rZERO
    endwhere
    do vertNdx = 1,nzdim
      air_dens_factor = modelDens( vertNdx ) / 2.465e19_dk
      quantum_yield( :, vertNdx ) =                                           &
                      quantum_yield_chnl1 * ( rONE + quantum_yield_wrk )      &
                      / ( rONE + quantum_yield_wrk * air_dens_factor )
      quantum_yield( :, vertNdx ) =                                           &
          min( rONE, max( rZERO, quantum_yield( :, vertNdx ) ) )
    enddo

    quantum_yield = transpose( quantum_yield )

  end function run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_quantum_yield_ch3cho_ch3_hco
