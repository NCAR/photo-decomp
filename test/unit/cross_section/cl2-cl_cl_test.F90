! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the base cross_section_t type
program test_cross_section

  use tuvx_cross_section, only : cross_section_t
  use tuvx_cross_section_cl2_cl_cl
  use tuvx_test_cross_section_utils, only : check_values

  implicit none

  call test_cross_section_cl2_cl_cl_t( )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine test_cross_section_cl2_cl_cl_t( )

    use musica_assert,                 only : assert
    use musica_constants,              only : dk => musica_dk
    use musica_config,                 only : config_t
    use musica_iterator,               only : iterator_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    class(grid_warehouse_t),    pointer :: grids
    class(profile_warehouse_t), pointer :: profiles
    class(cross_section_t),     pointer :: cross_section

    character(len=*), parameter :: Iam = "cl2_cl_cl cross section tests"
    type(config_t) :: config, cs_set, cs_config
    class(iterator_t), pointer :: iter
    real(kind=dk), allocatable :: results(:,:)
    real(dk), allocatable :: expected(:,:)
    allocate(expected(10, 6))

    ! All of these values were produced by one run of the cross section.
    ! So, these tests are testing that any changes don't produce unexpected
    ! changes. The values here are meaningless.
    expected = reshape([                                                      &
      1.25e-30, 1.02e-30, 8.46e-31, 6.99e-31, 5.79e-31, 4.82e-31,             &
      3.35e-30, 2.77e-30, 2.30e-30, 1.91e-30, 1.60e-30, 1.34e-30,             &
      8.72e-30, 7.27e-30, 6.08e-30, 5.09e-30, 4.28e-30, 3.61e-30,             &
      2.20e-29, 1.85e-29, 1.56e-29, 1.31e-29, 1.11e-29, 9.47e-30,             &
      5.44e-29, 4.60e-29, 3.90e-29, 3.31e-29, 2.82e-29, 2.41e-29,             &
      1.30e-28, 1.11e-28, 9.48e-29, 8.10e-29, 6.95e-29, 5.98e-29,             &
      3.04e-28, 2.61e-28, 2.24e-28, 1.93e-28, 1.66e-28, 1.44e-28,             &
      6.94e-28, 5.99e-28, 5.17e-28, 4.48e-28, 3.89e-28, 3.38e-28,             &
      1.54e-27, 1.34e-27, 1.16e-27, 1.01e-27, 8.86e-28, 7.76e-28,             &
      3.35e-27, 2.92e-27, 2.55e-27, 2.24e-27, 1.97e-27, 1.73e-27],            &
      (/ size(expected, 2), size(expected, 1) /)                              &
    )

    ! load test grids
    call config%from_file( "test/data/grid.190-210.config.json" )
    grids => grid_warehouse_t( config )

    ! load test profiles
    call config%from_file( "test/data/profile.temperature.config.json" )
    profiles => profile_warehouse_t( config, grids )

    ! get cross section config data
    call config%from_file( "test/data/cross_section.cl2_cl_cl.config.json" )
    call config%get( "cross sections", cs_set, Iam )
    iter => cs_set%get_iterator( )

    ! load and test cross section w/o extrapolation
    call assert( 101268914, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_cl2_cl_cl_t( cs_config, grids, profiles )
    results = cross_section%calculate( grids, profiles )
    call check_values( results, expected, .01_dk )
    deallocate( cross_section )

    ! clean up
    deallocate( iter )
    deallocate( grids )
    deallocate( profiles )
    deallocate( expected )

  end subroutine test_cross_section_cl2_cl_cl_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_cross_section
