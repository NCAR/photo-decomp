! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This c2h5cho+hv->c2h5+hco quantum yield module

!> The c2h5cho+hv->c2h5+hco quantum yield type and related functions
module tuvx_quantum_yield_c2h5cho_c2h5_hco

  use tuvx_quantum_yield_base,    only : base_quantum_yield_t

  implicit none

  private
  public :: c2h5cho_c2h5_hco_quantum_yield_t

  !> Calculator for c2h5cho+hv->c2h5+hco quantum yield
  type, extends(base_quantum_yield_t) :: c2h5cho_c2h5_hco_quantum_yield_t
  contains
    !> Calculate the quantum yield
    procedure :: calculate => run
  end type c2h5cho_c2h5_hco_quantum_yield_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the photorate quantum yield for a given set of environmental conditions
  function run( this, gridWareHouse, ProfileWareHouse ) result( quantum_yield )

    use musica_constants,           only : dk => musica_dk, ik => musica_ik
    use tuvx_grid_warehouse,        only : grid_warehouse_t
    use tuvx_grid,               only : abs_1d_grid_t
    use tuvx_profile_warehouse,     only : Profile_warehouse_t
    use tuvx_profile,               only : abs_Profile_t
    use musica_string,              only : string_t

    !> Arguments
    class(c2h5cho_c2h5_hco_quantum_yield_t), intent(in) :: this
    !> The warehouses
    type(grid_warehouse_t), intent(inout)    :: gridWareHouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse
    !> Calculated quantum_yield
    real(kind=dk), allocatable               :: quantum_yield(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = 'c2h5cho+hv->c2h5+hco calculate: '
    integer(ik), parameter :: iONE = 1_ik
    real(dk), parameter ::    rZERO = 0.0_dk
    real(dk), parameter ::    rONE  = 1.0_dk
    real(dk), parameter ::    largest=1.E+36_dk
    real(dk), parameter ::    pzero = 10._dk/largest

    integer(ik)           :: nzdim, vertNdx
    real(dk)              :: air_dens_fac
    real(dk), allocatable :: quantum_yield_wrk(:)
    real(dk), allocatable         :: modelDens(:)
    class(abs_1d_grid_t), pointer :: zGrid
    class(abs_1d_grid_t), pointer :: lambdaGrid
    class(abs_Profile_t), pointer :: mdlDensity
    type(string_t)                :: Handle

    write(*,*) Iam,'entering'

    Handle = 'Vertical Z'             ; zGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength' ; lambdaGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Air'                    ; mdlDensity => ProfileWareHouse%get_Profile( Handle )

    nzdim = zGrid%ncells_ + iONE
    modelDens = mdlDensity%edge_val_

    allocate( quantum_yield(lambdaGrid%ncells_,nzdim) )
    allocate( quantum_yield_wrk(lambdaGrid%ncells_) )
    quantum_yield = rZERO

    do vertNdx = iONE,nzdim
      air_dens_fac = modelDens(vertNdx)/2.45e19_dk
      ! quantum yields:
      ! use Stern-Volmer pressure dependence:
      where( this%quantum_yield_parms(1)%array(:,1) < pzero )
        quantum_yield_wrk = rZERO
      elsewhere
        quantum_yield_wrk = rONE/(rONE + (rONE/this%quantum_yield_parms(1)%array(:,1) - rONE)*air_dens_fac)
        quantum_yield_wrk = min( rONE,quantum_yield_wrk )
      endwhere
      quantum_yield(:,vertNdx) = quantum_yield_wrk
    enddo

    quantum_yield = transpose( quantum_yield )

    write(*,*) Iam,'exiting'

  end function run

end module tuvx_quantum_yield_c2h5cho_c2h5_hco
