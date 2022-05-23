! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This ch3cho+hv->ch3+hco quantum yield module

!> The ch3cho+hv->ch3+hco quantum yield type and related functions
module micm_ch3cho_ch3_hco_quantum_yield_type

  use micm_base_quantum_yield_type,    only : base_quantum_yield_t

  implicit none

  private
  public :: ch3cho_ch3_hco_quantum_yield_t

  !> Calculator for ch3cho+hv->ch3+hco quantum yield
  type, extends(base_quantum_yield_t) :: ch3cho_ch3_hco_quantum_yield_t
  contains
    !> Calculate the quantum yield
    procedure :: calculate => run
  end type ch3cho_ch3_hco_quantum_yield_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the photorate quantum yield for a given set of environmental conditions
  function run( this, gridWareHouse, ProfileWareHouse ) result( quantum_yield )

    use musica_constants,           only : dk => musica_dk, ik => musica_ik
    use micm_grid_warehouse,        only : grid_warehouse_t
    use micm_1d_grid,               only : abs_1d_grid_t
    use micm_Profile_warehouse,     only : Profile_warehouse_t
    use micm_Profile,               only : abs_Profile_t
    use musica_string,              only : string_t

    !> Arguments
    class(ch3cho_ch3_hco_quantum_yield_t), intent(in) :: this
    !> The warehouses
    type(grid_warehouse_t), intent(inout)    :: gridWareHouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse
    !> Calculated quantum_yield
    real(kind=dk), allocatable               :: quantum_yield(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = 'ch3cho+hv->ch3_hco quantum yield calculate: '
    integer(ik), parameter :: iONE = 1_ik
    real(dk), parameter ::    rZERO = 0.0_dk
    real(dk), parameter ::    rONE  = 1.0_dk
    integer(ik)           :: nzdim, vertNdx
    real(dk)              :: air_dens_factor
    real(dk), allocatable :: quantum_yield_chnl1(:)
    real(dk), allocatable :: quantum_yield_chnl2(:)
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

    quantum_yield_chnl1 = this%quantum_yield_parms(1)%array(:,2)
    quantum_yield_chnl2 = rONE - this%quantum_yield_parms(1)%array(:,1)
    where( quantum_yield_chnl1 > rZERO )
      quantum_yield_wrk = quantum_yield_chnl2/quantum_yield_chnl1 - rONE
    elsewhere
      quantum_yield_wrk = rZERO
    endwhere
    do vertNdx = iONE,nzdim
      air_dens_factor = modelDens(vertNdx)/2.465e19_dk
      quantum_yield(:,vertNdx) = &
                      quantum_yield_chnl1*(rONE + quantum_yield_wrk) &
                      /(rONE + quantum_yield_wrk*air_dens_factor)
      quantum_yield(:,vertNdx) = min( rONE,max(rZERO,quantum_yield(:,vertNdx)) )
    enddo

    quantum_yield = transpose( quantum_yield )

    write(*,*) Iam,'exiting'

  end function run

end module micm_ch3cho_ch3_hco_quantum_yield_type
