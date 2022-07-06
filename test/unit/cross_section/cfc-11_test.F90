! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the base cross_section_t type
program test_cross_section

  use tuvx_cross_section, only : cross_section_t
  use tuvx_cross_section_cfc11
  use tuvx_test_cross_section_utils, only : check_values

  implicit none

  call test_cross_section_cfc11_t( )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine test_cross_section_cfc11_t( )

    use musica_assert,                 only : assert
    use musica_constants,              only : dk => musica_dk
    use musica_config,                 only : config_t
    use musica_iterator,               only : iterator_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    class(grid_warehouse_t),    pointer :: grids
    class(profile_warehouse_t), pointer :: profiles
    class(cross_section_t),     pointer :: cross_section

    character(len=*), parameter :: Iam = "cfc-11 cross section tests"
    type(config_t) :: config, cs_set, cs_config
    class(iterator_t), pointer :: iter
    real(kind=dk), allocatable :: results(:,:)
    real(dk), allocatable :: expected(:,:)
    allocate(expected(10, 6))

    ! All of these values were produced by one run of the cross section.
    ! So, these tests are testing that any changes don't produce unexpected
    ! changes. The values here are meaningless.
    expected = reshape([                                                      &
       1.625e-18, 1.618e-18, 1.612e-18, 1.605e-18, 1.599e-18, 1.593e-18,      &
       1.349e-18, 1.342e-18, 1.335e-18, 1.328e-18, 1.321e-18, 1.314e-18,      &
       1.099e-18, 1.091e-18, 1.084e-18, 1.077e-18, 1.070e-18, 1.063e-18,      &
       8.848e-19, 8.779e-19, 8.710e-19, 8.642e-19, 8.575e-19, 8.507e-19,      &
       7.140e-19, 7.075e-19, 7.010e-19, 6.946e-19, 6.883e-19, 6.820e-19,      &
       5.684e-19, 5.624e-19, 5.566e-19, 5.508e-19, 5.451e-19, 5.394e-19,      &
       4.400e-19, 4.349e-19, 4.298e-19, 4.248e-19, 4.198e-19, 4.149e-19,      &
       3.338e-19, 3.294e-19, 3.252e-19, 3.209e-19, 3.168e-19, 3.127e-19,      &
       2.470e-19, 2.435e-19, 2.400e-19, 2.366e-19, 2.332e-19, 2.299e-19,      &
       1.787e-19, 1.759e-19, 1.731e-19, 1.705e-19, 1.678e-19, 1.652e-19],     &
      (/ size(expected, 2), size(expected, 1) /)                              &
    )

    ! load test grids
    call config%from_file( "test/data/grid.190-210.config.json" )
    grids => grid_warehouse_t( config )

    ! load test profiles
    call config%from_file( "test/data/profile.temperature.config.json" )
    profiles => profile_warehouse_t( config, grids )

    ! get cross section config data
    call config%from_file( "test/data/cross_section.cfc-11.config.json" )
    call config%get( "cross sections", cs_set, Iam )
    iter => cs_set%get_iterator( )

    ! load and test cross section w/o extrapolation
    call assert( 101264914, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_cfc11_t( cs_config, grids, profiles )
    results = cross_section%calculate( grids, profiles )
    call check_values( results, expected, .01_dk )
    deallocate( cross_section )

    ! load and test cross section with lower extrapolation
    call assert( 101264914, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_cfc11_t( cs_config, grids, profiles )
    results = cross_section%calculate( grids, profiles )
    call check_values( results, expected, .01_dk )
    deallocate( cross_section )

    ! load and test cross section with lower and upper extrapolation
    call assert( 101264914, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_cfc11_t( cs_config, grids, profiles )
    results = cross_section%calculate( grids, profiles )
    call check_values( results, expected, .01_dk )
    deallocate( cross_section )

    ! clean up
    deallocate( iter )
    deallocate( grids )
    deallocate( profiles )
    deallocate( expected )
    ! deallocate( lower_extrap )
    ! deallocate( lower_upper_extrap )

  end subroutine test_cross_section_cfc11_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_cross_section
