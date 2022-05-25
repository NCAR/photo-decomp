! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This ch3coch3+hv->ch3co_ch3 quantum_yield module

!> The ch3coch3+hv->ch3co+ch3_quantum_yield type and related functions
module tuvx_quantum_yield_ch3coch3_ch3co_ch3

  use tuvx_quantum_yield_base,    only : base_quantum_yield_t

  implicit none

  private
  public :: ch3coch3_ch3co_ch3_quantum_yield_t

  !> Calculator for acetone quantum_yield
  type, extends(base_quantum_yield_t) :: ch3coch3_ch3co_ch3_quantum_yield_t
  contains
    !> Initialize the quantum_yield
    procedure :: calculate => run
  end type ch3coch3_ch3co_ch3_quantum_yield_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the photorate quantum_yield for a given set of environmental conditions
  function run( this, gridWareHouse, ProfileWareHouse ) result( quantum_yield )
!     qyacet - q.y. for acetone, based on Blitz et al. (2004)
! Compute acetone quantum yields according to the parameterization of:
! Blitz, M. A., D. E. Heard, M. J. Pilling, S. R. Arnold, and M. P. Chipperfield 
!       (2004), Pressure and temperature-dependent quantum yields for the 
!       photodissociation of acetone between 279 and 327.5 nm, Geophys. 
!       Res. Lett., 31, L06111, doi:10.1029/2003GL018793.

    use musica_constants,           only : dk => musica_dk, ik => musica_ik, lk => musica_lk
    use tuvx_grid_warehouse,        only : grid_warehouse_t
    use tuvx_grid,               only : abs_1d_grid_t
    use tuvx_profile_warehouse,     only : Profile_warehouse_t
    use tuvx_profile,               only : abs_Profile_t
    use musica_string,              only : string_t

    !> Arguments
    class(ch3coch3_ch3co_ch3_quantum_yield_t), intent(in) :: this
    !> The warehouses
    type(grid_warehouse_t), intent(inout)    :: gridWareHouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse
    !> Calculated quantum_yield
    real(kind=dk), allocatable               :: quantum_yield(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = 'ch3coch3+hv->ch3co+ch3 quantum_yield calculate: '
    integer(ik), parameter :: iONE  = 1_ik
    real(dk), parameter :: rZERO = 0.0_dk
    real(dk), parameter :: rONE  = 1.0_dk

    integer(ik)                   :: lambdaNdx
    integer(ik)                   :: nzdim, vertNdx
    real(dk), allocatable         :: modelTemp(:), modelDens(:)
    class(abs_1d_grid_t), pointer :: zGrid
    class(abs_1d_grid_t), pointer :: lambdaGrid
    class(abs_Profile_t), pointer :: mdlTemperature, mdlDensity
    type(string_t)                :: Handle

    ! w = wavelength, nm
    ! T = temperature, K
    ! M = air number density, molec. cm-3
    real(dk)    :: w, wadj, Tadj, M
    real(dk)    :: a0, a1, a2, a3, a4
    real(dk)    :: b0, b1, b2, b3, b4
    real(dk)    :: c3
    real(dk)    :: cA0, cA1, cA2, cA3, cA4
    real(dk)    :: dumexp
    real(dk)    :: fco, fac

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

vert_loop: &
    do vertNdx = iONE,nzdim
      Tadj = modelTemp(vertNdx)/295._dk
      M    = modelDens(vertNdx) 
lambda_loop: &
      do lambdaNdx = iONE,lambdaGrid%ncells_
        w = lambdaGrid%mid_(lambdaNdx)
        if(w < 279._dk) then
           fac = 0.95_dk
        elseif(w > 327._dk) then
           fac = rZERO
        else
!   CO (carbon monoxide) quantum yields:
          a0 = 0.350_dk * Tadj**(-1.28_dk)
          b0 = 0.068_dk * Tadj**(-2.65_dk)
! SM: prevent exponent overflow in rare cases:
          dumexp = b0*(w - 248._dk)
          if (dumexp > 80._dk) then
            cA0 = 5.e34_dk
          else
            cA0 = exp(dumexp) * a0 / (rONE - a0)
          endif

          fco = rONE / (rONE + cA0)
!   CH3CO (acetyl radical) quantum yields:
          wadj = 1.e7_dk/w
          if(w >= 279._dk .and. w < 302._dk) then
            a1 = 1.600E-19_dk * Tadj**(-2.38_dk)
            b1 = 0.55E-3_dk   * Tadj**(-3.19_dk)
            cA1 = a1 * EXP(-b1*(wadj - 33113._dk))
            fac = (rONE - fco) / (rONE + cA1 * M)
          elseif(w >= 302._dk .AND. w <= 327._dk) then
            a2 = 1.62E-17_dk * Tadj**(-10.03_dk)
            b2 = 1.79E-3_dk  * Tadj**(-1.364_dk)
            cA2 = a2 * EXP(-b2*(wadj - 30488._dk))

            a3 = 26.29_dk   * Tadj**(-6.59_dk)
            b3 = 5.72E-7_dk * Tadj**(-2.93_dk)
            c3 = 30006._dk  * Tadj**(-0.064_dk)
            ca3 = a3 * EXP(-b3*(wadj - c3)**2)

            a4 = 1.67E-15_dk * Tadj**(-7.25_dk)
            b4 = 2.08E-3_dk  * Tadj**(-1.16_dk)
            cA4 = a4 * EXP(-b4*(wadj - 30488._dk))

            fac = (rONE - fco) * (rONE + cA3 + cA4 * M) &
                  / ((rONE + cA3 + cA2 * M)*(rONE + cA4 * M))

          endif
        endif
        quantum_yield(lambdaNdx,vertNdx) = fac
      enddo lambda_loop
    enddo vert_loop

    quantum_yield = transpose( quantum_yield )

    write(*,*) Iam,'exiting'

  end function run

end module tuvx_quantum_yield_ch3coch3_ch3co_ch3
