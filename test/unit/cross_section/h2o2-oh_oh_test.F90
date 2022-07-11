! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the base cross_section_t type
program test_cross_section

  use tuvx_cross_section, only : cross_section_t
  use tuvx_cross_section_clono2
  use tuvx_test_cross_section_utils, only : check_values

  implicit none

  call test_cross_section_clono2_t( )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine test_cross_section_clono2_t( )

    use musica_assert,                 only : assert
    use musica_constants,              only : dk => musica_dk
    use musica_config,                 only : config_t
    use musica_iterator,               only : iterator_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    class(grid_warehouse_t),    pointer :: grids
    class(profile_warehouse_t), pointer :: profiles
    class(cross_section_t),     pointer :: cross_section

    character(len=*), parameter :: Iam = "clono2 cross section tests"
    type(config_t) :: config, cs_set, cs_config
    class(iterator_t), pointer :: iter
    real(kind=dk), allocatable :: results(:,:)
    real(dk), allocatable :: no_extrap(:,:)
    real(dk), allocatable :: lower_extrap(:,:)
    real(dk), allocatable :: upper_extrap(:,:)
    allocate(no_extrap(20, 6))
    allocate(lower_extrap(20, 6))
    allocate(upper_extrap(20, 6))

    ! All of these values were produced by one run of the cross section.
    ! So, these tests are testing that any changes don't produce unexpected
    ! changes. The values here are meaningless.
    no_extrap = reshape([                                                     &
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0,                                           &
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0,                                           &
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0,                                           &
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0,                                           &
      3.19e-22, 3.19e-22, 3.19e-22, 3.19e-22, 3.19e-22, 3.19e-22,             &
      6.50e-19, 6.50e-19, 6.50e-19, 6.50e-19, 6.50e-19, 6.50e-19,             &
      6.07e-19, 6.07e-19, 6.07e-19, 6.07e-19, 6.07e-19, 6.07e-19,             &
      5.64e-19, 5.64e-19, 5.64e-19, 5.64e-19, 5.64e-19, 5.64e-19,             &
      5.28e-19, 5.28e-19, 5.28e-19, 5.28e-19, 5.28e-19, 5.28e-19,             &
      4.92e-19, 4.92e-19, 4.92e-19, 4.92e-19, 4.92e-19, 4.92e-19,             &
      4.61e-19, 4.61e-19, 4.61e-19, 4.61e-19, 4.61e-19, 4.61e-19,             &
      4.34e-19, 4.34e-19, 4.34e-19, 4.34e-19, 4.34e-19, 4.34e-19,             &
      4.08e-19, 4.08e-19, 4.08e-19, 4.08e-19, 4.08e-19, 4.08e-19,             &
      3.87e-19, 3.87e-19, 3.87e-19, 3.87e-19, 3.87e-19, 3.87e-19,             &
      3.67e-19, 3.67e-19, 3.67e-19, 3.67e-19, 3.67e-19, 3.67e-19,             &
      1.87e-22, 1.87e-22, 1.87e-22, 1.87e-22, 1.87e-22, 1.87e-22,             &
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0,                                           &
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0,                                           &
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0,                                           &
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0],                                          &
      (/ size(no_extrap, 2), size(no_extrap, 1) /)                            &
    )

    lower_extrap = reshape([ real::                                           &
      -29328.00, -53764.23, -78192.96, -102614.16, -127027.84, -151430.24,    &
      188, 188, 188, 188, 188, 188,                                           &
      188, 188, 188, 188, 188, 188,                                           &
      188, 188, 188, 188, 188, 188,                                           &
      187.91, 187.91, 187.91, 187.91, 187.91, 187.91,                         &
      6.50e-19, 6.50e-19, 6.50e-19, 6.50e-19, 6.50e-19, 6.50e-19,             &
      6.07e-19, 6.07e-19, 6.07e-19, 6.07e-19, 6.07e-19, 6.07e-19,             &
      5.64e-19, 5.64e-19, 5.64e-19, 5.64e-19, 5.64e-19, 5.64e-19,             &
      5.28e-19, 5.28e-19, 5.28e-19, 5.28e-19, 5.28e-19, 5.28e-19,             &
      4.92e-19, 4.92e-19, 4.92e-19, 4.92e-19, 4.92e-19, 4.92e-19,             &
      4.61e-19, 4.61e-19, 4.61e-19, 4.61e-19, 4.61e-19, 4.61e-19,             &
      4.34e-19, 4.34e-19, 4.34e-19, 4.34e-19, 4.34e-19, 4.34e-19,             &
      6.93e-15, 2.31e-14, 4.88e-14, 8.40e-14, 1.28e-13, 1.82e-13,             &
      6.41e-15, 2.14e-14, 4.52e-14, 7.78e-14, 1.19e-13, 1.69e-13,             &
      5.93e-15, 1.98e-14, 4.18e-14, 7.19e-14, 1.10e-13, 1.56e-13,             &
      2.95e-18, 9.86e-18, 2.08e-17, 3.58e-17, 5.48e-17, 7.79e-17,             &
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0,                                           &
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0,                                           &
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0,                                           &
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0],                                          &
      (/ size(lower_extrap, 2), size(lower_extrap, 1) /)                      &
    )

    upper_extrap = reshape([                                                  &
      7.82e-15, 2.79e-14, 6.02e-14, 1.04e-13, 1.61e-13, 2.31e-13,             &   
      9.06e-15, 3.03e-14, 6.39e-14, 1.10e-13, 1.68e-13, 2.39e-13,             &   
      8.15e-15, 2.86e-14, 6.17e-14, 1.07e-13, 1.65e-13, 2.35e-13,             &   
      -9.64e-16, -1.76e-15, -2.56e-15, -3.36e-15, -4.16e-15, -4.95e-15,       &         
      6.47e-15, 2.31e-14, 4.99e-14, 8.70e-14, 1.34e-13, 1.91e-13,             &   
      6.34e-15, 2.26e-14, 4.89e-14, 8.51e-14, 1.31e-13, 1.87e-13,             &   
      5.98e-15, 2.13e-14, 4.61e-14, 8.03e-14, 1.24e-13, 1.77e-13,             &   
      5.62e-15, 2.00e-14, 4.34e-14, 7.56e-14, 1.16e-13, 1.66e-13,             &   
      5.32e-15, 1.89e-14, 4.10e-14, 7.14e-14, 1.10e-13, 1.57e-13,             &   
      5.01e-15, 1.78e-14, 3.86e-14, 6.73e-14, 1.03e-13, 1.48e-13,             &   
      4.74e-15, 1.69e-14, 3.66e-14, 6.37e-14, 9.83e-14, 1.40e-13,             &   
      4.51e-15, 1.61e-14, 3.48e-14, 6.06e-14, 9.36e-14, 1.33e-13,             &   
      4.29e-15, 1.53e-14, 3.31e-14, 5.76e-14, 8.89e-14, 1.26e-13,             &   
      4.11e-15, 1.46e-14, 3.17e-14, 5.52e-14, 8.51e-14, 1.21e-13,             &   
      3.93e-15, 1.40e-14, 3.03e-14, 5.28e-14, 8.15e-14, 1.16e-13,             &   
      2293439.85, 8183418.06, 17685530.33, 30796439.68, 47512810.16, 67827898.80, &
      2317444.08, 8268931.61, 17870233.03, 31117976.56, 48008791.44, 68535864.37, &
      2340243.62, 8350146.60, 18045645.95, 31423336.81, 48479815.40, 69208201.58, &
      2363043.16, 8431361.60, 18221058.87, 31728697.07, 48950839.35, 69880538.79, &
      2385842.70, 8512576.60, 18396471.79, 32034057.33, 49421863.31, 70552876.01],&
      (/ size(upper_extrap, 2), size(upper_extrap, 1) /)                      &
    )

    ! load test grids
    call config%from_file( "test/data/grid.180-220.config.json" )
    grids => grid_warehouse_t( config )

    ! load test profiles
    call config%from_file( "test/data/profile.temperature.config.json" )
    profiles => profile_warehouse_t( config, grids )

    ! get cross section config data
    call config%from_file( "test/data/cross_section.h2o2-oh_oh.config.json" )
    call config%get( "cross sections", cs_set, Iam )
    iter => cs_set%get_iterator( )

    ! load and test cross section w/o extrapolation
    call assert( 101264914, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_clono2_t( cs_config, grids, profiles )
    results = cross_section%calculate( grids, profiles )
    call check_values( results, no_extrap, .01_dk )
    deallocate( cross_section )

    ! load and test cross section with lower extrapolation
    call assert( 101264915, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_clono2_t( cs_config, grids, profiles )
    results = cross_section%calculate( grids, profiles )
    call check_values( results, lower_extrap, .01_dk )
    deallocate( cross_section )

    ! load and test cross section with lower and upper extrapolation
    call assert( 101264916, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_clono2_t( cs_config, grids, profiles )
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

  end subroutine test_cross_section_clono2_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_cross_section
