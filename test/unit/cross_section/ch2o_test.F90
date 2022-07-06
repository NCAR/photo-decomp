! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the base cross_section_t type
program test_cross_section

  use tuvx_cross_section, only : cross_section_t
  use tuvx_cross_section_ch2o
  use tuvx_test_cross_section_utils, only : check_values

  implicit none

  call test_cross_section_ch2o_t( )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine test_cross_section_ch2o_t( )

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
    allocate(expected(15, 6))

    ! All of these values were produced by one run of the cross section.
    ! So, these tests are testing that any changes don't produce unexpected
    ! changes. The values here are meaningless.
    expected = reshape([                                                      &
       2.847e-20, 2.844e-20, 2.841e-20, 2.839e-20, 2.836e-20, 2.833e-20,      &
       3.236e-20, 3.234e-20, 3.232e-20, 3.230e-20, 3.228e-20, 3.226e-20,      &
       3.647e-20, 3.645e-20, 3.644e-20, 3.642e-20, 3.641e-20, 3.639e-20,      &
       3.373e-20, 3.372e-20, 3.371e-20, 3.370e-20, 3.369e-20, 3.369e-20,      &
       1.363e-20, 1.362e-20, 1.362e-20, 1.361e-20, 1.360e-20, 1.359e-20,      &
       2.699e-20, 2.699e-20, 2.699e-20, 2.699e-20, 2.699e-20, 2.699e-20,      &
       1.979e-20, 1.980e-20, 1.980e-20, 1.981e-20, 1.982e-20, 1.982e-20,      &
       1.620e-20, 1.618e-20, 1.617e-20, 1.615e-20, 1.613e-20, 1.611e-20,      &
       1.788e-20, 1.786e-20, 1.783e-20, 1.780e-20, 1.778e-20, 1.775e-20,      &
       2.349e-21, 2.343e-21, 2.336e-21, 2.330e-21, 2.323e-21, 2.316e-21,      &
       3.651e-21, 3.645e-21, 3.640e-21, 3.634e-21, 3.629e-21, 3.623e-21,      &
       3.511e-21, 3.503e-21, 3.495e-21, 3.487e-21, 3.479e-21, 3.471e-21,      &
       8.750e-23, 8.750e-23, 8.750e-23, 8.750e-23, 8.750e-23, 8.750e-23,      &
       3.620e-22, 3.620e-22, 3.620e-22, 3.620e-22, 3.620e-22, 3.620e-22,      &
       3.615e-22, 3.615e-22, 3.615e-22, 3.615e-22, 3.615e-22, 3.615e-22],     &
      (/ size(expected, 2), size(expected, 1) /)                              &
    )


    ! load test grids
    call config%from_file( "test/data/grid.300-375.config.json" )
    grids => grid_warehouse_t( config )

    ! load test profiles
    call config%from_file( "test/data/profile.temperature.config.json" )
    profiles => profile_warehouse_t( config, grids )

    ! get cross section config data
    call config%from_file( "test/data/cross_section.ch2o.config.json" )
    call config%get( "cross sections", cs_set, Iam )
    iter => cs_set%get_iterator( )

    ! load and test cross section w/o extrapolation
    call assert( 101264914, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_ch2o_t( cs_config, grids, profiles )
    results = cross_section%calculate( grids, profiles )
    call check_values( results, expected, .01_dk )
    deallocate( cross_section )

    ! load and test cross section with lower extrapolation
    call assert( 101264914, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_ch2o_t( cs_config, grids, profiles )
    results = cross_section%calculate( grids, profiles )
    call check_values( results, expected, .01_dk )
    deallocate( cross_section )

    ! load and test cross section with lower and upper extrapolation
    call assert( 101264914, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_ch2o_t( cs_config, grids, profiles )
    results = cross_section%calculate( grids, profiles )
    call check_values( results, expected, .01_dk )
    deallocate( cross_section )

    ! clean up
    deallocate( iter )
    deallocate( grids )
    deallocate( profiles )
    deallocate( expected )

  end subroutine test_cross_section_ch2o_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_cross_section
