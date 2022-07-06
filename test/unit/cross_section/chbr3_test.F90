! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the base cross_section_t type
program test_cross_section

  use tuvx_cross_section, only : cross_section_t
  use tuvx_cross_section_chbr3
  use tuvx_test_cross_section_utils, only : check_values

  implicit none

  call test_cross_section_chbr3_t( )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine test_cross_section_chbr3_t( )

    use musica_assert,                 only : assert
    use musica_constants,              only : dk => musica_dk
    use musica_config,                 only : config_t
    use musica_iterator,               only : iterator_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    class(grid_warehouse_t),    pointer :: grids
    class(profile_warehouse_t), pointer :: profiles
    class(cross_section_t),     pointer :: cross_section

    character(len=*), parameter :: Iam = "chbr3 cross section tests"
    type(config_t) :: config, cs_set, cs_config
    class(iterator_t), pointer :: iter
    real(kind=dk), allocatable :: results(:,:)
    real(dk), allocatable :: expected(:,:)
    allocate(expected(10, 6))

    ! All of these values were produced by one run of the cross section.
    ! So, these tests are testing that any changes don't produce unexpected
    ! changes. The values here are meaningless.
    expected = reshape([                                                      &
      3.795e-18, 3.795e-18, 3.795e-18, 3.795e-18, 3.795e-18, 3.795e-18,       &
      3.555e-18, 3.555e-18, 3.555e-18, 3.555e-18, 3.555e-18, 3.555e-18,       &
      3.584e-18, 3.584e-18, 3.584e-18, 3.584e-18, 3.584e-18, 3.584e-18,       &
      3.795e-18, 3.795e-18, 3.795e-18, 3.795e-18, 3.795e-18, 3.795e-18,       &
      4.044e-18, 4.044e-18, 4.044e-18, 4.044e-18, 4.044e-18, 4.044e-18,       &
      4.245e-18, 4.245e-18, 4.245e-18, 4.245e-18, 4.245e-18, 4.245e-18,       &
      4.364e-18, 4.364e-18, 4.364e-18, 4.364e-18, 4.364e-18, 4.364e-18,       &
      4.425e-18, 4.425e-18, 4.425e-18, 4.425e-18, 4.425e-18, 4.425e-18,       &
      4.48e-18, 4.48e-18, 4.48e-18, 4.48e-18, 4.48e-18, 4.48e-18,             &
      4.594e-18, 4.594e-18, 4.594e-18, 4.594e-18, 4.594e-18, 4.594e-18],      &
      (/ size(expected, 2), size(expected, 1) /)                              &
    )

    ! load test grids
    call config%from_file( "test/data/grid.190-210.config.json" )
    grids => grid_warehouse_t( config )

    ! load test profiles
    call config%from_file( "test/data/profile.temperature.config.json" )
    profiles => profile_warehouse_t( config, grids )

    ! get cross section config data
    call config%from_file( "test/data/cross_section.chbr3.config.json" )
    call config%get( "cross sections", cs_set, Iam )
    iter => cs_set%get_iterator( )

    ! load and test cross section w/o extrapolation
    call assert( 101264914, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_chbr3_t( cs_config, grids, profiles )
    results = cross_section%calculate( grids, profiles )
    call check_values( results, expected, .01_dk )
    deallocate( cross_section )

    ! load and test cross section with lower extrapolation
    call assert( 101264914, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_chbr3_t( cs_config, grids, profiles )
    results = cross_section%calculate( grids, profiles )
    call check_values( results, expected, .01_dk )
    deallocate( cross_section )

    ! load and test cross section with lower and upper extrapolation
    call assert( 101264914, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_chbr3_t( cs_config, grids, profiles )
    results = cross_section%calculate( grids, profiles )
    call check_values( results, expected, .01_dk )
    deallocate( cross_section )

    ! clean up
    deallocate( iter )
    deallocate( grids )
    deallocate( profiles )
    deallocate( expected )

  end subroutine test_cross_section_chbr3_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_cross_section
