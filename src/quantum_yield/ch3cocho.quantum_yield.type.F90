! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This ch3cocho+hv->ch3co+hco quantum yield module

!> The ch3cocho+hv->ch3co+hco quantum yield type and related functions
module micm_ch3cocho_ch3co_hco_quantum_yield_type

  use micm_base_quantum_yield_type,    only : base_quantum_yield_t

  implicit none

  private
  public :: ch3cocho_ch3co_hco_quantum_yield_t

  !> Calculator for ch3cocho+hv->ch3co+hco quantum yield
  type, extends(base_quantum_yield_t) :: ch3cocho_ch3co_hco_quantum_yield_t
  contains
    !> Calculate the quantum yield
    procedure :: calculate => run
  end type ch3cocho_ch3co_hco_quantum_yield_t

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
    class(ch3cocho_ch3co_hco_quantum_yield_t), intent(in) :: this
    !> The warehouses
    type(grid_warehouse_t), intent(inout)    :: gridWareHouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse
    !> Calculated quantum_yield
    real(kind=dk), allocatable               :: quantum_yield(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = 'ch3cocho+hv->ch3co_hco quantum yield calculate: '
    integer(ik), parameter :: iONE  = 1_ik
    real(dk), parameter ::    rZERO = 0.0_dk
    real(dk), parameter ::    rONE  = 1.0_dk
    real(dk), parameter ::  lambdaL = 380._dk
    real(dk), parameter ::  lambdaU = 440._dk

    integer(ik)           :: nzdim, vertNdx
    integer(ik)           :: lambdaNdx
    real(dk)              :: phi0, kq, lambda, airfac, qy
    real(dk), allocatable :: modelDens(:)
    class(abs_1d_grid_t), pointer :: zGrid
    class(abs_1d_grid_t), pointer :: lambdaGrid
    class(abs_Profile_t), pointer :: mdlDensity
    type(string_t)                :: Handle
               
    write(*,*) Iam // 'entering'

    Handle = 'Vertical Z'             ; zGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength' ; lambdaGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Air'                    ; mdlDensity => ProfileWareHouse%get_Profile( Handle )

    nzdim = zGrid%ncells_ + iONE
    modelDens = mdlDensity%edge_val_

    allocate( quantum_yield(lambdaGrid%ncells_,nzdim) )
    quantum_yield = rZERO

    ! zero pressure yield:
    ! 1.0 for wc < 380 nm
    ! 0.0 for wc > 440 nm
    ! linear in between:

    ! Pressure correction: quenching coefficient, torr-1
    ! in air, Koch and Moortgat:
    do vertNdx = iONE,nzdim
      airfac = modelDens(vertNdx) * 760._dk/2.456E19_dk
      do lambdaNdx = iONE,lambdaGrid%ncells_
        lambda = lambdaGrid%mid_(lambdaNdx)
        phi0 = rONE - (lambda - 380._dk)/60._dk
        phi0 = max( min(phi0,rONE),rZERO )
        kq = 1.36e8_dk * exp( -8793._dk/lambda )
        if( phi0 > rZERO ) then
          if( lambda >= lambdaL .and. lambda <= lambdaU ) then
            qy = phi0 / (phi0 + kq * airfac)
          else
            qy = phi0
          endif
        else
          qy = rZERO
        endif
        quantum_yield(lambdaNdx,vertNdx) = qy
      enddo
    enddo

    quantum_yield = transpose( quantum_yield )

    write(*,*) Iam,'exiting'

  end function run

end module micm_ch3cocho_ch3co_hco_quantum_yield_type
