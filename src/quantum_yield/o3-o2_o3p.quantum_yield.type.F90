! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This o3+hv->o2+o3p quantum yield module

!> The o3+hv->o2+o3p quantum yield type and related functions
module micm_o3_o2_o3p_quantum_yield_type

  use micm_base_quantum_yield_type,    only : base_quantum_yield_t
  use musica_constants,                only : dk => musica_dk, ik => musica_ik

  implicit none

  private
  public :: o3_o2_o3p_quantum_yield_t

  integer(ik), parameter :: iONE = 1_ik
  real(dk), parameter ::   rZERO = 0.0_dk
  real(dk), parameter ::   rONE  = 1.0_dk

  !> Calculator for o3+hv->o2+o3p quantum yield
  type, extends(base_quantum_yield_t) :: o3_o2_o3p_quantum_yield_t
  contains
    !> Calculate the quantum yield
    procedure :: calculate => run
  end type o3_o2_o3p_quantum_yield_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the photorate quantum yield for a given set of environmental conditions
  function run( this, gridWareHouse, ProfileWareHouse ) result( quantum_yield )

    use musica_string,           only : string_t
    use micm_grid_warehouse,     only : grid_warehouse_t
    use micm_Profile_warehouse,  only : Profile_warehouse_t
    use micm_1d_grid,            only : abs_1d_grid_t
    use micm_Profile,            only : abs_Profile_t

    !> Arguments
    class(o3_o2_o3p_quantum_yield_t), intent(in) :: this
    type(grid_warehouse_t), intent(inout)        :: gridWareHouse
    type(Profile_warehouse_t), intent(inout)     :: ProfileWareHouse
    !> Calculated quantum yield
    real(kind=dk), allocatable                   :: quantum_yield(:,:)

    !> Local variables
    real(dk), parameter :: a(3)  = (/ 0.8036_dk, 8.9061_dk, 0.1192_dk /)
    real(dk), parameter :: x(3)  = (/ 304.225_dk, 314.957_dk, 310.737_dk /)
    real(dk), parameter :: om(3) = (/ 5.576_dk, 6.601_dk, 2.187_dk /)

    character(len=*), parameter :: Iam = 'o3+hv->o2+o3p quantum yield calculate: '

    integer(ik) :: wNdx, vertNdx
    real(dk)    :: kt, q1, q2, T300, lambda
    real(dk)    :: qfac1, qfac2
    type(string_t)                :: Handle
    class(abs_1d_grid_t), pointer :: lambdaGrid, zGrid
    class(abs_Profile_t), pointer :: Temperature

    write(*,*) Iam,'entering'

    Handle = 'Photolysis, wavelength' ; lambdaGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Vertical Z'             ; zGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Temperature'            ; Temperature => ProfileWareHouse%get_Profile( Handle )

!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
! function to calculate the quantum yield O3 + hv -> O(1D) + O2,             =*
! according to:                                                             
! Matsumi, Y., F. J. Comes, G. Hancock, A. Hofzumanhays, A. J. Hynes,
! M. Kawasaki, and A. R. Ravishankara, QUantum yields for production of O(1D)
! in the ultraviolet photolysis of ozone:  Recommendation based on evaluation
! of laboratory data, J. Geophys. Res., 107, 10.1029/2001JD000510, 2002.
!-----------------------------------------------------------------------------*


      allocate( quantum_yield(lambdaGrid%ncells_,zGrid%ncells_+1) )
      quantum_yield = rZERO

      associate( w => lambdaGrid%mid_, Temp => Temperature%edge_val_ )

      do vertNdx = iONE,zGrid%ncells_+iONE
        kt = 0.695_dk * Temp(vertNdx)
        q1 = rONE
        q2 = exp( -825.518_dk/kt )
        qfac1 = q1/(q1 + q2)
        qfac2 = q2/(q1 + q2)

        T300 = Temp(vertNdx)/300._dk

        where( w(:) <= 305._dk )
          quantum_yield(:,vertNdx) = 0.90_dk
        elsewhere( w(:) > 328._dk .and. w(:) <= 340._dk )
          quantum_yield(:,vertNdx) = 0.08_dk
        endwhere
        do wNdx = iONE,size(w)
          lambda = w(wNdx)
          if( lambda > 305._dk .and. lambda <= 328._dk ) then
            quantum_yield(wNdx,vertNdx) = 0.0765_dk &
            + a(1)          *qfac1*EXP( -((x(1) - lambda)/om(1))**4 ) &
            + a(2)*T300*T300*qfac2*EXP( -((x(2) - lambda)/om(2))**2 ) &
            + a(3)*T300**1.5_dk*EXP( -((x(3) - lambda)/om(3))**2 )
          endif
        enddo
      enddo

      end associate

      quantum_yield = transpose( rONE - quantum_yield )

    write(*,*) Iam,'exiting'

  end function run

end module micm_o3_o2_o3p_quantum_yield_type
