! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This nitroxy_acetone cross_section module

!> The nitroxy_acetone_cross_section type and related functions
module tuvx_cross_section_nitroxy_acetone

  use tuvx_cross_section_base,    only : base_cross_section_t

  implicit none

  private
  public :: nitroxy_acetone_cross_section_t

  !> Calculator for nitroxy_acetone cross section
  type, extends(base_cross_section_t) :: nitroxy_acetone_cross_section_t
  contains
    !> Calculate the cross section
    procedure :: calculate => run
  end type nitroxy_acetone_cross_section_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the photorate cross section for a given set of environmental conditions
  function run( this, gridWareHouse, ProfileWareHouse, atMidPoint ) result( cross_section )

    use musica_constants,           only : dk => musica_dk, ik => musica_ik, lk => musica_lk
    use tuvx_grid_warehouse,        only : grid_warehouse_t
    use tuvx_grid,               only : abs_1d_grid_t
    use tuvx_profile_warehouse,     only : Profile_warehouse_t
    use tuvx_profile,               only : abs_Profile_t
    use musica_string,              only : string_t

    !> Arguments
    class(nitroxy_acetone_cross_section_t), intent(in) :: this
    logical(lk), optional, intent(in)        :: atMidPoint
    !> The warehouses
    type(grid_warehouse_t), intent(inout)    :: gridWareHouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse
    !> Calculated cross section
    real(dk), allocatable                    :: cross_section(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = 'nitroxy_acetone cross section calculate: '
    integer(ik), parameter :: iONE = 1_ik
    real(dk), parameter :: rZERO = 0.0_dk
    real(dk), parameter :: a = -1.365E-3_dk
    real(dk), parameter :: b = 0.7834_dk
    real(dk), parameter :: c = -156.8_dk

    integer(ik)            :: nzdim
    integer(ik)            :: vertNdx
    real(dk), allocatable  :: wrkCrossSection(:)
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
    allocate( wrkCrossSection(lambdaGrid%ncells_) )

    where( lambdaGrid%mid_ >= 284._dk .and. lambdaGrid%mid_ <= 335._dk )
      wrkCrossSection = exp( c + lambdaGrid%mid_*(b + a*lambdaGrid%mid_) )
    elsewhere
      wrkCrossSection = rZERO
    endwhere

    do vertNdx = iONE,nzdim
      cross_section(:,vertNdx) = wrkCrossSection
    enddo

    cross_section = transpose( cross_section )

    write(*,*) Iam,'exiting'

  end function run

end module tuvx_cross_section_nitroxy_acetone
