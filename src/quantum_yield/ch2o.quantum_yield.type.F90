! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This ch2o+hv->h2+co quantum yield module

!> The ch2o+hv->h2+co quantum yield type and related functions
module micm_ch2o_h2_co_quantum_yield_type

  use micm_base_quantum_yield_type,    only : base_quantum_yield_t

  implicit none

  private
  public :: ch2o_h2_co_quantum_yield_t


  !> Calculator for ch2o+hv->h2+co quantum yield
  type, extends(base_quantum_yield_t) :: ch2o_h2_co_quantum_yield_t
  contains
    !> Calculate the quantum yield
    procedure :: calculate => run
  end type ch2o_h2_co_quantum_yield_t

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
    class(ch2o_h2_co_quantum_yield_t), intent(in) :: this
    !> The warehouses
    type(grid_warehouse_t), intent(inout)    :: gridWareHouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse
    !> Calculated quantum_yield
    real(kind=dk), allocatable               :: quantum_yield(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = 'ch2o+hv->h2_co quantum yield calculate: '
    integer(ik), parameter :: iONE = 1_ik
    real(dk), parameter ::    rZERO = 0.0_dk
    real(dk), parameter ::    rONE  = 1.0_dk
    real(dk), parameter  :: lambdaL = 330._dk
    real(dk), parameter  :: lambdaU = 360._dk

    integer(ik)          :: nzdim, vertNdx
    real(dk) :: air_den_factor, Tfactor
    real(dk), allocatable :: quantum_yield_tmp(:)
    real(dk), allocatable :: quantum_yield_wrk(:)
    real(dk), allocatable         :: modelTemp(:), modelDens(:)
    class(abs_1d_grid_t), pointer :: zGrid
    class(abs_1d_grid_t), pointer :: lambdaGrid
    class(abs_Profile_t), pointer :: mdlTemperature, mdlDensity
    type(string_t)                :: Handle

    write(*,*) Iam,'entering'

    Handle = 'Vertical Z'             ; zGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength' ; lambdaGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Temperature'            ; mdlTemperature => ProfileWareHouse%get_Profile( Handle )
    Handle = 'Air'                    ; mdlDensity => ProfileWareHouse%get_Profile( Handle )

    nzdim = zGrid%ncells_ + iONE
    modelTemp = mdlTemperature%edge_val_
    modelDens = mdlDensity%edge_val_

    allocate( quantum_yield(lambdaGrid%ncells_,nzdim) )
    quantum_yield = rZERO

    associate( quantum_yield_chnl1 => this%quantum_yield_parms(1)%array(:,1), &
               quantum_yield_chnl2 => this%quantum_yield_parms(1)%array(:,2) )
    quantum_yield_tmp   = rONE - quantum_yield_chnl1
    allocate( quantum_yield_wrk(lambdaGrid%ncells_) )
    do vertNdx = iONE,nzdim
      Tfactor = (300._dk - modelTemp(vertNdx))/80._dk
      where( lambdaGrid%mid_ >= lambdaL .and. lambdaGrid%mid_ < lambdaU &
                                        .and. quantum_yield_chnl2 > rZERO )
        quantum_yield_wrk = (rONE - (quantum_yield_chnl1 + quantum_yield_chnl2)) &
                             /(2.45e19_dk*quantum_yield_chnl2*quantum_yield_tmp)
        quantum_yield_wrk = quantum_yield_wrk*(rONE &
                          + .05_dk*(lambdaGrid%mid_ - 329._dk)*Tfactor)
        quantum_yield(:,vertNdx) = rONE/(rONE/quantum_yield_tmp + quantum_yield_wrk*modelDens(vertNdx))
      elsewhere
        quantum_yield(:,vertNdx) = quantum_yield_chnl2
      endwhere
    enddo
    end associate

    quantum_yield = transpose( quantum_yield )

    write(*,*) Iam,'exiting'

  end function run

end module micm_ch2o_h2_co_quantum_yield_type
