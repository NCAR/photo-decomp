! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the photo_grid module

!> Test module for the grid_t type
program test_grid

  implicit none

  call test_grid_t( )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Tests of the grid_t type
  subroutine test_grid_t( )

    use musica_config,    only : config_t
    use musica_string,    only : string_t
    use musica_assert,    only : assert, almost_equal
    use musica_constants, only : ik => musica_ik, dk => musica_dk
    use micm_grid_warehouse, only : grid_warehouse_t
    use micm_1d_grid,    only : abs_1d_grid_t

    !> local variables
    character(len=*), parameter :: Iam = 'test_grid: '
    character(len=*), parameter :: config_flsp = 'data/grid.tst.config.json'
    type(config_t) :: grid_tst_config
    type(grid_warehouse_t), pointer :: thewarehouse
    type(string_t) :: Handle
    class(abs_1d_grid_t), pointer   :: aGrid

    write(*,*) Iam // 'entering'

    call grid_tst_config%from_file( config_flsp )
    thewarehouse => grid_warehouse_t( grid_tst_config )

    Handle = 'Vertical Z'
    aGrid => thewarehouse%get_grid( Handle )
    call assert( 412238768, aGrid%ncells_ .eq. 120_ik )
    call assert( 412238769, all( aGrid%delta_ .eq. 1._dk ) )

    write(*,*) ' '
    write(*,*) Iam // 'Grid = ',aGrid%handle_
    write(*,*) 'There are ',aGrid%ncells_,' grid cells'
    write(*,*) 'Grid edges'
    write(*,'(1p10g15.7)') aGrid%edge_
    write(*,*) 'Grid midpoints'
    write(*,'(1p10g15.7)') aGrid%mid_
    write(*,*) 'Grid deltas'
    write(*,'(1p10g15.7)') aGrid%delta_

    deallocate( aGrid )

    Handle = 'Photolysis, wavelength'
    aGrid => thewarehouse%get_grid( Handle )
    call assert( 412238769, all( aGrid%delta_ > 0._dk ) )

    write(*,*) ' '
    write(*,*) Iam // 'Grid = ',aGrid%handle_
    write(*,*) 'There are ',aGrid%ncells_,' grid cells'
    write(*,*) 'Grid edges'
    write(*,'(1p10g15.7)') aGrid%edge_
    write(*,*) 'Grid midpoints'
    write(*,'(1p10g15.7)') aGrid%mid_
    write(*,*) 'Grid deltas'
    write(*,'(1p10g15.7)') aGrid%delta_

    deallocate( aGrid )

    Handle = 'Time, hrs'
    aGrid => thewarehouse%get_grid( Handle )
    call assert( 412238769, all( aGrid%delta_ > 0._dk ) )

    write(*,*) ' '
    write(*,*) Iam // 'Grid = ',aGrid%handle_
    write(*,*) 'There are ',aGrid%ncells_,' grid cells'
    write(*,*) 'Grid edges'
    write(*,'(1p10g15.7)') aGrid%edge_
    write(*,*) 'Grid midpoints'
    write(*,'(1p10g15.7)') aGrid%mid_
    write(*,*) 'Grid deltas'
    write(*,'(1p10g15.7)') aGrid%delta_

    deallocate( aGrid )

    write(*,*) Iam // 'leaving'

  end subroutine test_grid_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_grid
