! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This rayliegh_cross_section module

!> The rayliegh_cross_section type and related functions
module micm_radXfer_rayliegh_cross_section_type

  use micm_radXfer_abs_cross_section_type, only : abs_cross_section_t
  use musica_constants,                    only : musica_dk, musica_ik, lk => musica_lk

  implicit none

  private
  public :: rayliegh_cross_section_t

  !> Calculator for rayliegh_cross_section
  type, extends(abs_cross_section_t) :: rayliegh_cross_section_t
  contains
    !> Initialize the cross section
    procedure :: initialize
    !> Calculate the cross section
    procedure :: calculate => run
    !> clean up
    final     :: finalize
  end type rayliegh_cross_section_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize rayliegh_cross_section_t object
  subroutine initialize( this, config, gridWareHouse, ProfileWareHouse, atMidPoint )

    use musica_config,               only : config_t
    use micm_grid_warehouse,         only : grid_warehouse_t
    use micm_Profile_warehouse, only : Profile_warehouse_t

    !> rayliegh cross section type
    class(rayliegh_cross_section_t), intent(inout) :: this
    logical(lk), optional, intent(in)              :: atMidPoint
    !> cross section configuration object
    type(config_t), intent(inout)                  :: config
    !> The warehouses
    type(grid_warehouse_t), intent(inout)          :: gridWareHouse
    type(Profile_warehouse_t), intent(inout)       :: ProfileWareHouse

    character(len=*), parameter :: Iam = 'rayliegh cross section initialize: '

    write(*,*) Iam,'entering'

    write(*,*) Iam,'exiting'

  end subroutine initialize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the photorate cross section for a given set of environmental conditions
  function run( this, gridWareHouse, ProfileWareHouse, atMidPoint ) result( cross_section )

    use micm_grid_warehouse,         only : grid_warehouse_t
    use micm_1d_grid,                only : abs_1d_grid_t
    use micm_Profile_warehouse,      only : Profile_warehouse_t
    use musica_string,               only : string_t

    !> rayliegh cross section
    class(rayliegh_cross_section_t), intent(in)    :: this
    logical(lk), optional, intent(in)              :: atMidPoint
    !> The warehouses
    type(grid_warehouse_t), intent(inout)          :: gridWareHouse
    type(Profile_warehouse_t), intent(inout)       :: ProfileWareHouse
    !> Calculated cross section
    real(kind=musica_dk), allocatable              :: cross_section(:,:)

    !> Local variables
    integer :: colndx, nzdim
    character(len=*), parameter :: Iam = 'radXfer rayliegh cross section calculate: '
    class(abs_1d_grid_t), pointer :: zGrid
    class(abs_1d_grid_t), pointer :: lambdaGrid
    type(string_t)                :: Handle
    real(musica_dk)               :: wmicrn
    real(musica_dk), allocatable  :: pwr(:), wrk(:)
    real(musica_dk), allocatable  :: wrkCrossSection(:,:)

    write(*,*) Iam,'entering'

    Handle = 'Vertical Z'
    zGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength'
    lambdaGrid => gridWareHouse%get_grid( Handle )

    nzdim = zGrid%ncells_ + 1
    if( present(atMidPoint) ) then
      if( atMidpoint ) then
        nzdim = nzdim - 1
      endif
    endif

    allocate( wrkCrossSection(lambdaGrid%ncells_,nzdim) )

!> Rayleigh scattering cross section from WMO 1985 (originally from
!> Nicolet, M., On the molecular scattering in the terrestrial atmosphere:
!> An empirical formula for its calculation in the homoshpere, Planet.
!> Space Sci., 32, 1467-1468, 1984.
    allocate( pwr(lambdaGrid%ncells_) )
    wrk = 1.e-3_musica_dk * lambdaGrid%mid_
    where( wrk <= 0.55_musica_dk )
      pwr = 3.6772_musica_dk + 0.389_musica_dk*wrk + 0.09426_musica_dk/wrk
    elsewhere
      pwr = 4.04_musica_dk
    endwhere

    wrkCrossSection(:,1) = 4.02e-28_musica_dk/(wrk)**pwr

    do colndx = 2,nzdim
      wrkCrossSection(:,colndx) = wrkCrossSection(:,1)
    enddo

    cross_section = transpose( wrkCrossSection )

    write(*,*) Iam,'exiting'

  end function run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!> finalize the cross section type
   subroutine finalize( this )

   type(rayliegh_cross_section_t), intent(inout) :: this

   end subroutine finalize

end module micm_radXfer_rayliegh_cross_section_type
