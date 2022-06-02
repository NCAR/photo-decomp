! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This h2o2_oh_oh_cross_section module

!> The h2o2+hv->oh_oh cross_section type and related functions
module tuvx_cross_section_h2o2_oh_oh

  use tuvx_cross_section, only : base_cross_section_t, base_constructor
  use musica_constants,   only : dk => musica_dk, ik => musica_ik, lk => musica_lk

  implicit none

  private
  public :: h2o2_oh_oh_cross_section_t

  !> Calculator for base_cross_section
  type, extends(base_cross_section_t) :: h2o2_oh_oh_cross_section_t
  contains
    !> Calculate the cross section
    procedure :: calculate => run
  end type h2o2_oh_oh_cross_section_t

  !> Constructor
  interface h2o2_oh_oh_cross_section_t
    module procedure constructor
  end interface h2o2_oh_oh_cross_section_t

contains

  !> Initialize the cross section
  function constructor( config, gridWareHouse, ProfileWareHouse, atMidPoint ) result ( this )
 
    use musica_config,    only : config_t
    use musica_constants, only : lk => musica_lk
    use tuvx_grid_warehouse,    only : grid_warehouse_t
    use tuvx_profile_warehouse, only : Profile_warehouse_t
 
 
    !> Cross section calculator
    logical(lk), optional, intent(in)          :: atMidPoint
    class(base_cross_section_t), pointer  :: this
    type(config_t), intent(inout)              :: config
    type(grid_warehouse_t), intent(inout)      :: gridWareHouse
    type(Profile_warehouse_t), intent(inout)   :: ProfileWareHouse

    allocate ( h2o2_oh_oh_cross_section_t :: this )
    call base_constructor( this, config, gridWareHouse, ProfileWareHouse, atMidPoint )
  end function

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the photorate cross section for a given set of environmental conditions
  function run( this, gridWareHouse, ProfileWareHouse, atMidPoint ) result( cross_section )

    use tuvx_grid_warehouse,    only : grid_warehouse_t
    use tuvx_profile_warehouse, only : Profile_warehouse_t
    use tuvx_grid,           only : abs_1d_grid_t
    use tuvx_profile,           only : abs_Profile_t
    use musica_string,          only : string_t

    !> arguments
    class(h2o2_oh_oh_cross_section_t), intent(in) :: this
    !> the warehouses
    type(grid_warehouse_t), intent(inout)         :: gridWareHouse
    type(Profile_warehouse_t), intent(inout)      :: ProfileWareHouse
    !> Calculated cross section
    real(kind=dk), allocatable                    :: cross_section(:,:)
    logical(lk), optional, intent(in)             :: atMidPoint

    !> local variables
    integer(ik), parameter ::  iONE = 1_ik
    real(dk), parameter ::  rONE = 1.0_dk
    real(dk), parameter ::  A0 = 6.4761E+04_dk
    real(dk), parameter ::  A1 = -9.2170972E+02_dk
    real(dk), parameter ::  A2 = 4.535649_dk
    real(dk), parameter ::  A3 = -4.4589016E-03_dk
    real(dk), parameter ::  A4 = -4.035101E-05_dk
    real(dk), parameter ::  A5 = 1.6878206E-07_dk
    real(dk), parameter ::  A6 = -2.652014E-10_dk
    real(dk), parameter ::  A7 = 1.5534675E-13_dk

    real(dk), parameter ::  B0 = 6.8123E+03_dk
    real(dk), parameter ::  B1 = -5.1351E+01_dk
    real(dk), parameter ::  B2 = 1.1522E-01_dk
    real(dk), parameter ::  B3 = -3.0493E-05_dk
    real(dk), parameter ::  B4 = -1.0924E-07_dk

    character(len=*), parameter :: Iam = 'h2o2+hv->oh+oh cross section calculate: '
    integer(ik)    :: vertNdx, wNdx
    real(dk)       :: lambda, sumA, sumB, t, chi, xs
    type(string_t) :: Handle
    class(abs_1d_grid_t), pointer  :: zGrid, lambdaGrid
    class(abs_Profile_t), pointer  :: temperature

    write(*,*) Iam,'entering'

    Handle = 'Vertical Z'             ; zGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength' ; lambdaGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Temperature'            ; temperature => ProfileWareHouse%get_Profile( Handle )

    allocate( cross_section(lambdaGrid%ncells_,zGrid%ncells_+1) )

    associate( wl => lambdaGrid%edge_, wc => lambdaGrid%mid_ )
    do vertNdx = iONE,zGrid%ncells_+iONE
      do wNdx = iONE,lambdaGrid%ncells_
! Parameterization (JPL94)
! Range 260-350 nm; 200-400 K
        if( wl(wNdx) >= 260._dk .and. wl(wNdx) < 350._dk ) then
           lambda = wc(wNdx)
           sumA = ((((((A7*lambda + A6)*lambda + A5)*lambda + A4)*lambda +A3)*lambda + A2)*lambda + A1)*lambda + A0
           sumB = (((B4*lambda + B3)*lambda + B2)*lambda + B1)*lambda + B0
           t = min(max(temperature%edge_val_(vertNdx),200._dk),400._dk)            
           chi = rONE/(rONE + exp(-1265._dk/t))
           cross_section(wNdx,vertNdx) = (chi * sumA + (rONE - chi)*sumB)*1.E-21_dk
         else
           cross_section(wNdx,vertNdx) = this%cross_section_parms(1)%array(wNdx,1)
         endif
      enddo
    enddo
    end associate

    cross_section = transpose( cross_section )

    write(*,*) Iam,'exiting'

  end function run

end module tuvx_cross_section_h2o2_oh_oh
