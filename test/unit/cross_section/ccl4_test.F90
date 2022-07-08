! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the base cross_section_t type
program test_cross_section

  use tuvx_cross_section, only : cross_section_t
  use tuvx_cross_section_ccl4
  use tuvx_test_cross_section_utils, only : check_values

  implicit none

  call test_cross_section_ccl4_t( )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine test_cross_section_ccl4_t( )

    use musica_assert,                 only : assert
    use musica_constants,              only : dk => musica_dk
    use musica_config,                 only : config_t
    use musica_iterator,               only : iterator_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    class(grid_warehouse_t),    pointer :: grids
    class(profile_warehouse_t), pointer :: profiles
    class(cross_section_t),     pointer :: cross_section

    character(len=*), parameter :: Iam = "ccl4 cross section tests"
    type(config_t) :: config, cs_set, cs_config
    class(iterator_t), pointer :: iter
    real(kind=dk), allocatable :: results(:,:)
    real(dk), allocatable :: no_extrap(:,:)
    real(dk), allocatable :: lower_extrap(:,:)
    real(dk), allocatable :: upper_extrap(:,:)
    allocate(no_extrap(10, 6))
    allocate(lower_extrap(10, 6))
    allocate(upper_extrap(10, 6))

    ! All of these values were produced by one run of the cross section.
    ! So, these tests are testing that any changes don't produce unexpected
    ! changes. The values here are meaningless.
    no_extrap = reshape([                                                     &
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0,                                           &
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0,                                           &
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0,                                           &
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0,                                           &
      3.299e-22, 3.299e-22, 3.299e-22, 3.299e-22, 3.299e-22, 3.299e-22,       &
      6.486e-19, 6.483e-19, 6.480e-19, 6.477e-19, 6.474e-19, 6.470e-19,       &
      6.229e-19, 6.220e-19, 6.210e-19, 6.200e-19, 6.191e-19, 6.181e-19,       &
      5.880e-19, 5.861e-19, 5.843e-19, 5.824e-19, 5.806e-19, 5.787e-19,       &
      5.444e-19, 5.415e-19, 5.387e-19, 5.358e-19, 5.330e-19, 5.302e-19,       &
      4.929e-19, 4.890e-19, 4.852e-19, 4.814e-19, 4.776e-19, 4.739e-19],      &
      (/ size(no_extrap, 2), size(no_extrap, 1) /)                            &
    )

    lower_extrap = reshape([                                                  &
      180.0, 180.0, 180.0, 180.0, 180.0, 180.0,                               &
      180.0, 180.0, 180.0, 180.0, 180.0, 180.0,                               &
      179.776, 179.564, 179.352, 179.141, 178.930, 178.719,                   &
      179.953, 179.909, 179.865, 179.821, 179.777, 179.733,                   &
      179.903, 179.898, 179.892, 179.886, 179.880, 179.874,                   &
      6.486e-19, 6.483e-19, 6.480e-19, 6.477e-19, 6.474e-19, 6.470e-19,       &
      6.229e-19, 6.220e-19, 6.210e-19, 6.200e-19, 6.191e-19, 6.181e-19,       &
      5.880e-19, 5.861e-19, 5.843e-19, 5.824e-19, 5.806e-19, 5.787e-19,       &
      5.444e-19, 5.415e-19, 5.387e-19, 5.358e-19, 5.330e-19, 5.302e-19,       &
      4.929e-19, 4.890e-19, 4.852e-19, 4.814e-19, 4.776e-19, 4.739e-19],      &
      (/ size(lower_extrap, 2), size(lower_extrap, 1) /)                      &
    )

    upper_extrap = reshape([                                            &
      6.599e-19, 6.599e-19, 6.599e-19, 6.599e-19, 6.599e-19, 6.599e-19,       &
      6.599e-19, 6.599e-19, 6.599e-19, 6.599e-19, 6.599e-19, 6.599e-19,       &
      6.591e-19, 6.584e-19, 6.576e-19, 6.568e-19, 6.560e-19, 6.553e-19,       &
      6.598e-19, 6.596e-19, 6.595e-19, 6.593e-19, 6.591e-19, 6.590e-19,       &
      6.599e-19, 6.599e-19, 6.599e-19, 6.599e-19, 6.598e-19, 6.598e-19,       &
      6.486e-19, 6.483e-19, 6.480e-19, 6.477e-19, 6.474e-19, 6.470e-19,       &
      6.229e-19, 6.220e-19, 6.210e-19, 6.200e-19, 6.191e-19, 6.181e-19,       &
      5.880e-19, 5.861e-19, 5.843e-19, 5.824e-19, 5.806e-19, 5.787e-19,       &
      5.444e-19, 5.415e-19, 5.387e-19, 5.358e-19, 5.330e-19, 5.302e-19,       &
      4.929e-19, 4.890e-19, 4.852e-19, 4.814e-19, 4.776e-19, 4.739e-19],      &
      (/ size(upper_extrap, 2), size(upper_extrap, 1) /)          &
    )


    ! load test grids
    call config%from_file( "test/data/grid.190-210.config.json" )
    grids => grid_warehouse_t( config )

    ! load test profiles
    call config%from_file( "test/data/profile.temperature.config.json" )
    profiles => profile_warehouse_t( config, grids )

    ! get cross section config data
    call config%from_file( "test/data/cross_section.ccl4.config.json" )
    call config%get( "cross sections", cs_set, Iam )
    iter => cs_set%get_iterator( )

    ! load and test cross section w/o extrapolation
    call assert( 101264914, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_ccl4_t( cs_config, grids, profiles )
    results = cross_section%calculate( grids, profiles )
    call check_values( results, no_extrap, .01_dk )
    deallocate( cross_section )

    ! load and test cross section with lower extrapolation
    call assert( 101264914, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_ccl4_t( cs_config, grids, profiles )
    results = cross_section%calculate( grids, profiles )
    call check_values( results, lower_extrap, .01_dk )
    deallocate( cross_section )

    ! load and test cross section with lower and upper extrapolation
    call assert( 101264914, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_ccl4_t( cs_config, grids, profiles )
    results = cross_section%calculate( grids, profiles )
    call check_values( results, upper_extrap, .01_dk )
    deallocate( cross_section )

    ! clean up
    deallocate( iter )
    deallocate( grids )
    deallocate( profiles )
    deallocate( no_extrap )
    deallocate( lower_extrap )
    deallocate( upper_extrap )

  end subroutine test_cross_section_ccl4_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_cross_section
