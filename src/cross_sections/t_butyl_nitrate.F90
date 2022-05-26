! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This t_butyl_nitrate cross_section module

!> The t_butyl_nitrate_cross_section type and related functions
module tuvx_cross_section_t_butyl_nitrate

  use tuvx_cross_section_base,    only : base_cross_section_t

  implicit none

  private
  public :: t_butyl_nitrate_cross_section_t

  !> Calculator for t_butyl_nitrate cross section
  type, extends(base_cross_section_t) :: t_butyl_nitrate_cross_section_t
  contains
    !> Calculate the cross section
    procedure :: calculate => run
  end type t_butyl_nitrate_cross_section_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the photorate cross section for a given set of environmental conditions
  function run( this, gridWareHouse, ProfileWareHouse, atMidPoint ) result( cross_section )

    use musica_constants,           only : dk => musica_dk, ik => musica_ik, lk => musica_lk
    use tuvx_grid_warehouse,        only : grid_warehouse_t
    use tuvx_grid,               only : abs_1d_grid_t
    use tuvx_profile_warehouse,     only : Profile_warehouse_t
    use musica_string,              only : string_t

    !> Arguments
    class(t_butyl_nitrate_cross_section_t), intent(in) :: this
    logical(lk), optional, intent(in)        :: atMidPoint
    !> The warehouses
    type(grid_warehouse_t), intent(inout)    :: gridWareHouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse
    !> Calculated cross section
    real(kind=dk), allocatable               :: cross_section(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = 't_butyl_nitrate cross section calculate: '
    integer(ik), parameter :: iONE = 1_ik
    integer(ik), parameter :: iTWO = 2_ik
    real(dk), parameter :: rZERO = 0.0_dk
    real(dk), parameter :: a = -0.993E-3_dk
    real(dk), parameter :: b = 0.5307_dk
    real(dk), parameter :: c = -115.5_dk
    integer(ik)           :: nzdim, vertNdx
    class(abs_1d_grid_t), pointer :: zGrid
    class(abs_1d_grid_t), pointer :: lambdaGrid
    type(string_t) :: Handle

    write(*,*) Iam,'entering'

    Handle = 'Vertical Z'             ; zGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength' ; lambdaGrid => gridWareHouse%get_grid( Handle )

    nzdim = zGrid%ncells_ + iONE
    if( present(atMidPoint) ) then
      if( atMidpoint ) then
        nzdim = nzdim - iONE
      endif
    endif

    allocate( cross_section(lambdaGrid%ncells_,nzdim) )
    cross_section = rZERO

    where( lambdaGrid%mid_ >= 270._dk .and. lambdaGrid%mid_ <= 330._dk )
      cross_section(:,1) = exp( c + lambdaGrid%mid_*(b + a*lambdaGrid%mid_) )
    elsewhere
      cross_section(:,1) = rZERO
    endwhere
    do vertNdx = iTWO,nzdim
      cross_section(:,vertNdx) = cross_section(:,1)
    enddo

    cross_section = transpose( cross_section )

    write(*,*) Iam,'exiting'

  end function run

end module tuvx_cross_section_t_butyl_nitrate
