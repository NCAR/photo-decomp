! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This ch2o+hv->h2+co quantum yield module

!> The ch2o+hv->h2+co quantum yield type and related functions
module tuvx_quantum_yield_ch2o_h2_co

  use tuvx_quantum_yield,              only : quantum_yield_t, base_constructor

  implicit none

  private
  public :: quantum_yield_ch2o_h2_co_t


  !> Calculator for ch2o+hv->h2+co quantum yield
  type, extends(quantum_yield_t) :: quantum_yield_ch2o_h2_co_t
  contains
    !> Calculate the quantum yield
    procedure :: calculate => run
  end type quantum_yield_ch2o_h2_co_t

  !> Constructor
  interface quantum_yield_ch2o_h2_co_t
    module procedure constructor
  end interface quantum_yield_ch2o_h2_co_t

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

    allocate ( quantum_yield_ch2o_h2_co_t :: this )

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

    class(quantum_yield_ch2o_h2_co_t), intent(in) :: this
    type(grid_warehouse_t), intent(inout)    :: grid_warehouse
    type(profile_warehouse_t), intent(inout) :: profile_warehouse
    !> Calculated quantum_yield
    real(kind=dk), allocatable               :: quantum_yield(:,:)

    ! Local variables
    character(len=*), parameter :: Iam =                                      &
        'ch2o+hv->h2_co quantum yield calculate'
    real(dk), parameter ::    rZERO = 0.0_dk
    real(dk), parameter ::    rONE  = 1.0_dk
    real(dk), parameter  :: lambdaL = 330._dk
    real(dk), parameter  :: lambdaU = 360._dk

    integer                       :: nzdim, vertNdx
    real(dk)                      :: air_den_factor, Tfactor
    real(dk),         allocatable :: quantum_yield_tmp(:)
    real(dk),         allocatable :: quantum_yield_wrk(:)
    real(dk),         allocatable :: modelTemp(:), modelDens(:)
    class(grid_t),    pointer     :: zGrid => null( )
    class(grid_t),    pointer     :: lambdaGrid => null( )
    class(profile_t), pointer     :: mdlTemperature => null( )
    class(profile_t), pointer     :: mdlDensity => null( )

    zGrid => grid_warehouse%get_grid( "height", "km" )
    lambdaGrid => grid_warehouse%get_grid( "wavelength", "nm" )
    mdlTemperature => profile_warehouse%get_profile( "temperature", "K" )
    mdlDensity => profile_warehouse%get_profile( "air", "molecule cm-3" )

    nzdim = zGrid%ncells_ + 1
    modelTemp = mdlTemperature%edge_val_
    modelDens = mdlDensity%edge_val_

    allocate( quantum_yield( lambdaGrid%ncells_, nzdim ) )
    quantum_yield = rZERO

    associate( quantum_yield_chnl1 => this%quantum_yield_parms(1)%array(:,1), &
               quantum_yield_chnl2 => this%quantum_yield_parms(1)%array(:,2) )
    quantum_yield_tmp   = rONE - quantum_yield_chnl1
    allocate( quantum_yield_wrk( lambdaGrid%ncells_ ) )
    do vertNdx = 1, nzdim
      Tfactor = ( 300._dk - modelTemp( vertNdx ) ) / 80._dk
      where( lambdaGrid%mid_ >= lambdaL .and. lambdaGrid%mid_ < lambdaU &
                                        .and. quantum_yield_chnl2 > rZERO )
        quantum_yield_wrk = ( rONE -                                           &
                               ( quantum_yield_chnl1 + quantum_yield_chnl2 ) ) &
                      / ( 2.45e19_dk * quantum_yield_chnl2 * quantum_yield_tmp )
        quantum_yield_wrk = quantum_yield_wrk * ( rONE                         &
                          + .05_dk * ( lambdaGrid%mid_ - 329._dk ) * Tfactor )
        quantum_yield(:,vertNdx) = rONE / ( rONE / quantum_yield_tmp +         &
                                   quantum_yield_wrk * modelDens( vertNdx ) )
      elsewhere
        quantum_yield( :, vertNdx ) = quantum_yield_chnl2
      endwhere
    enddo
    end associate

    quantum_yield = transpose( quantum_yield )

    deallocate( zGrid )
    deallocate( lambdaGrid )
    deallocate( mdlTemperature )
    deallocate( mdlDensity )

  end function run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_quantum_yield_ch2o_h2_co
