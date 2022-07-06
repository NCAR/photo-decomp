! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the base cross_section_t type
program test_cross_section

  use tuvx_cross_section, only : cross_section_t
  use tuvx_cross_section_chcl3
  use tuvx_test_cross_section_utils, only : check_values

  implicit none

  call test_cross_section_chcl3_t( )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine test_cross_section_chcl3_t( )

    use musica_assert,                 only : assert
    use musica_constants,              only : dk => musica_dk
    use musica_config,                 only : config_t
    use musica_iterator,               only : iterator_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    class(grid_warehouse_t),    pointer :: grids
    class(profile_warehouse_t), pointer :: profiles
    class(cross_section_t),     pointer :: cross_section

    character(len=*), parameter :: Iam = "chcl3 cross section tests"
    type(config_t) :: config, cs_set, cs_config
    class(iterator_t), pointer :: iter
    real(kind=dk), allocatable :: results(:,:)
    real(dk), allocatable :: expected(:,:)
    allocate(expected(10, 6))

    ! All of these values were produced by one run of the cross section.
    ! So, these tests are testing that any changes don't produce unexpected
    ! changes. The values here are meaningless.
    expected = reshape([                                                      &
      1.006e-18, 9.994e-19, 9.921e-19, 9.849e-19, 9.778e-19, 9.70e-19,        &
      8.214e-19, 8.134e-19, 8.054e-19, 7.975e-19, 7.898e-19, 7.82e-19,        &
      6.920e-19, 6.831e-19, 6.743e-19, 6.656e-19, 6.571e-19, 6.48e-19,        &
      5.759e-19, 5.664e-19, 5.572e-19, 5.481e-19, 5.391e-19, 5.30e-19,        &
      4.678e-19, 4.583e-19, 4.491e-19, 4.400e-19, 4.312e-19, 4.22e-19,        &
      3.751e-19, 3.660e-19, 3.571e-19, 3.485e-19, 3.400e-19, 3.31e-19,        &
      2.987e-19, 2.902e-19, 2.820e-19, 2.739e-19, 2.661e-19, 2.58e-19,        &
      2.312e-19, 2.236e-19, 2.163e-19, 2.092e-19, 2.024e-19, 1.95e-19,        &
      1.720e-19, 1.656e-19, 1.595e-19, 1.536e-19, 1.479e-19, 1.42e-19,        &
      1.233e-19, 1.183e-19, 1.134e-19, 1.087e-19, 1.042e-19, 9.99e-20],       &
      (/ size(expected, 2), size(expected, 1) /)                              &
    )

    ! load test grids
    call config%from_file( "test/data/grid.190-210.config.json" )
    grids => grid_warehouse_t( config )

    ! load test profiles
    call config%from_file( "test/data/profile.temperature.config.json" )
    profiles => profile_warehouse_t( config, grids )

    ! get cross section config data
    call config%from_file( "test/data/cross_section.chcl3.config.json" )
    call config%get( "cross sections", cs_set, Iam )
    iter => cs_set%get_iterator( )

    ! load and test cross section w/o extrapolation
    call assert( 101264914, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_chcl3_t( cs_config, grids, profiles )
    results = cross_section%calculate( grids, profiles )
    call check_values( results, expected, .01_dk )
    deallocate( cross_section )

    ! load and test cross section with lower extrapolation
    call assert( 101264915, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_chcl3_t( cs_config, grids, profiles )
    results = cross_section%calculate( grids, profiles )
    call check_values( results, expected, .01_dk )
    deallocate( cross_section )

    ! load and test cross section with lower and upper extrapolation
    call assert( 101264916, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_chcl3_t( cs_config, grids, profiles )
    results = cross_section%calculate( grids, profiles )
    call check_values( results, expected, .01_dk )
    deallocate( cross_section )

    ! clean up
    deallocate( iter )
    deallocate( grids )
    deallocate( profiles )
    deallocate( expected )

  end subroutine test_cross_section_chcl3_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_cross_section
