! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the base cross_section_t type
program test_cross_section

  use tuvx_cross_section, only : cross_section_t
  use tuvx_cross_section_o3_tint
  use tuvx_test_cross_section_utils, only : check_values

  implicit none

  call test_cross_section_o3_tint_t( )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine test_cross_section_o3_tint_t( )

    use musica_assert,                 only : assert
    use musica_constants,              only : dk => musica_dk
    use musica_config,                 only : config_t
    use musica_iterator,               only : iterator_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    class(grid_warehouse_t),    pointer :: grids
    class(profile_warehouse_t), pointer :: profiles
    class(cross_section_t),     pointer :: cross_section

    character(len=*), parameter :: Iam = "o3_tint cross section tests"
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
    no_extrap = reshape([ real::                                              &
      0, 0, 0, 0, 0, 0,                                                       &
      3.3431e-19, 3.3391e-19, 3.3351e-19, 3.3311e-19, 3.3271e-19, 3.3231e-19, &
      3.4057e-19, 3.4006e-19, 3.3954e-19, 3.3903e-19, 3.3851e-19, 3.3800e-19, &
      3.2812e-19, 3.2756e-19, 3.2701e-19, 3.2645e-19, 3.2590e-19, 3.2535e-19, &
      3.1880e-19, 3.1835e-19, 3.1789e-19, 3.1743e-19, 3.1698e-19, 3.1652e-19, &
      3.1385e-19, 3.1348e-19, 3.1312e-19, 3.1275e-19, 3.1238e-19, 3.1201e-19, &
      3.1361e-19, 3.1332e-19, 3.1303e-19, 3.1274e-19, 3.1245e-19, 3.1215e-19, &
      3.1889e-19, 3.1864e-19, 3.1838e-19, 3.1813e-19, 3.1788e-19, 3.1763e-19, &
      3.2980e-19, 3.2961e-19, 3.2942e-19, 3.2924e-19, 3.2905e-19, 3.2886e-19, &
      2.1600e-20, 2.1586e-20, 2.1572e-20, 2.1559e-20, 2.1545e-20, 2.1532e-20],&
      (/ size(no_extrap, 2), size(no_extrap, 1) /)                            &
    )

    lower_extrap = reshape([ real::                                           &
      0.1000, 0.1000, 0.1000, 0.1000, 0.1000, 0.1000,                         &
      0.0060, 0.0060, 0.0060, 0.0060, 0.0060, 0.0060,                         &
      3.4057e-19, 3.4006e-19, 3.3954e-19, 3.3903e-19, 3.3851e-19, 3.3800e-19, &
      3.2812e-19, 3.2756e-19, 3.2701e-19, 3.2645e-19, 3.2590e-19, 3.2535e-19, &
      3.1880e-19, 3.1835e-19, 3.1789e-19, 3.1743e-19, 3.1698e-19, 3.1652e-19, &
      3.1385e-19, 3.1348e-19, 3.1312e-19, 3.1275e-19, 3.1238e-19, 3.1201e-19, &
      3.1361e-19, 3.1332e-19, 3.1303e-19, 3.1274e-19, 3.1245e-19, 3.1215e-19, &
      3.1889e-19, 3.1864e-19, 3.1838e-19, 3.1813e-19, 3.1788e-19, 3.1763e-19, &
      3.2980e-19, 3.2961e-19, 3.2942e-19, 3.2924e-19, 3.2905e-19, 3.2886e-19, &
      2.1600e-20, 2.1586e-20, 2.1572e-20, 2.1559e-20, 2.1545e-20, 2.1532e-20],&
      (/ size(lower_extrap, 2), size(lower_extrap, 1) /)                      &
    )

    upper_extrap = reshape([ real::                                           &
      3.4855e-19, 3.4813e-19, 3.4771e-19, 3.4728e-19, 3.4686e-19, 3.4644e-19, &
      3.5616e-19, 3.5573e-19, 3.5531e-19, 3.5489e-19, 3.5446e-19, 3.5404e-19, &
      3.4057e-19, 3.4006e-19, 3.3954e-19, 3.3903e-19, 3.3851e-19, 3.3800e-19, &
      3.2812e-19, 3.2756e-19, 3.2701e-19, 3.2645e-19, 3.2590e-19, 3.2535e-19, &
      3.1880e-19, 3.1835e-19, 3.1789e-19, 3.1743e-19, 3.1698e-19, 3.1652e-19, &
      3.1385e-19, 3.1348e-19, 3.1312e-19, 3.1275e-19, 3.1238e-19, 3.1201e-19, &
      3.1361e-19, 3.1332e-19, 3.1303e-19, 3.1274e-19, 3.1245e-19, 3.1215e-19, &
      3.1889e-19, 3.1864e-19, 3.1838e-19, 3.1813e-19, 3.1788e-19, 3.1763e-19, &
      3.2980e-19, 3.2961e-19, 3.2942e-19, 3.2924e-19, 3.2905e-19, 3.2886e-19, &
      0.9360, 0.9360, 0.9360, 0.9360, 0.9360, 0.9360],                        &
      (/ size(upper_extrap, 2), size(upper_extrap, 1) /)                      &
    )

    ! load test grids
    call config%from_file( "test/data/grid.195-205.config.json" )
    grids => grid_warehouse_t( config )

    ! load test profiles
    call config%from_file( "test/data/profile.temperature.config.json" )
    profiles => profile_warehouse_t( config, grids )

    ! get cross section config data
    call config%from_file( "test/data/cross_section.o3_tint.config.json" )
    call config%get( "cross sections", cs_set, Iam )
    iter => cs_set%get_iterator( )

    ! load and test cross section w/o extrapolation
    call assert( 101264914, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_o3_tint_t( cs_config, grids, profiles )
    results = cross_section%calculate( grids, profiles )
    call check_values( results, no_extrap, .01_dk )
    deallocate( cross_section )

    ! load and test cross section with lower extrapolation
    call assert( 101264915, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_o3_tint_t( cs_config, grids, profiles )
    results = cross_section%calculate( grids, profiles )
    call check_values( results, lower_extrap, .01_dk )
    deallocate( cross_section )

    ! load and test cross section with upper extrapolation
    call assert( 101264914, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_o3_tint_t( cs_config, grids, profiles )
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

  end subroutine test_cross_section_o3_tint_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_cross_section
