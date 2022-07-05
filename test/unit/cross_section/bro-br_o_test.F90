! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the base cross_section_t type
program test_cross_section

  use tuvx_cross_section, only : cross_section_t
  use tuvx_cross_section_bro_br_o
  use tuvx_test_cross_section_utils, only : check_values

  implicit none

  call test_cross_section_bro_br_o_t( )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine test_cross_section_bro_br_o_t( )

    use musica_assert,                 only : assert
    use musica_constants,              only : dk => musica_dk
    use musica_config,                 only : config_t
    use musica_iterator,               only : iterator_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    class(grid_warehouse_t),    pointer :: grids
    class(profile_warehouse_t), pointer :: profiles
    class(cross_section_t),     pointer :: cross_section

    character(len=*), parameter :: Iam = "acetone cross section tests"
    type(config_t) :: config, cs_set, cs_config
    class(iterator_t), pointer :: iter
    real(kind=dk), allocatable :: results(:,:)
    real(dk), allocatable :: no_extrap(:,:)
    ! real(dk), allocatable :: lower_upper_extrap(:,:)
    allocate(no_extrap(15, 6))
    ! allocate(lower_extrap(4, 5))
    ! allocate(lower_upper_extrap(4, 6))

    ! All of these values were produced by one run of the cross section.
    ! So, these tests are testing that any changes don't produce unexpected
    ! changes. The values here are meaningless.
    no_extrap = reshape([                                                     &
      2.00e-18, 2.00e-18, 2.00e-18, 2.00e-18, 2.00e-18, 2.00e-18,             &
      2.59e-18, 2.59e-18, 2.59e-18, 2.59e-18, 2.59e-18, 2.59e-18,             &
      4.53e-18, 4.53e-18, 4.53e-18, 4.53e-18, 4.53e-18, 4.53e-18,             &
      3.91e-18, 3.91e-18, 3.91e-18, 3.91e-18, 3.91e-18, 3.91e-18,             &
      6.00e-18, 6.00e-18, 6.00e-18, 6.00e-18, 6.00e-18, 6.00e-18,             &
      7.53e-18, 7.53e-18, 7.53e-18, 7.53e-18, 7.53e-18, 7.53e-18,             &
      6.27e-18, 6.27e-18, 6.27e-18, 6.27e-18, 6.27e-18, 6.27e-18,             &
      5.89e-18, 5.89e-18, 5.89e-18, 5.89e-18, 5.89e-18, 5.89e-18,             &
      5.15e-18, 5.15e-18, 5.15e-18, 5.15e-18, 5.15e-18, 5.15e-18,             &
      3.99e-18, 3.99e-18, 3.99e-18, 3.99e-18, 3.99e-18, 3.99e-18,             &
      2.27e-18, 2.27e-18, 2.27e-18, 2.27e-18, 2.27e-18, 2.27e-18,             &
      1.72e-18, 1.72e-18, 1.72e-18, 1.72e-18, 1.72e-18, 1.72e-18,             &
      1.60e-18, 1.60e-18, 1.60e-18, 1.60e-18, 1.60e-18, 1.60e-18,             &
      9.19e-19, 9.19e-19, 9.19e-19, 9.19e-19, 9.19e-19, 9.19e-19,             &
      5.09e-19, 5.09e-19, 5.09e-19, 5.09e-19, 5.09e-19, 5.09e-19],            &
      (/ size(no_extrap, 2), size(no_extrap, 1) /)                            &
    )

    ! load test grids
    call config%from_file( "test/data/grid.300-375.config.json" )
    grids => grid_warehouse_t( config )

    ! load test profiles
    call config%from_file( "test/data/profile.simple.config.json" )
    profiles => profile_warehouse_t( config, grids )

    ! get cross section config data
    call config%from_file( "test/data/cross_section.BrO.config.json" )
    call config%get( "cross sections", cs_set, Iam )
    iter => cs_set%get_iterator( )

    ! load and test cross section w/o extrapolation
    call assert( 101264914, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_bro_br_o_t( cs_config, grids, profiles )
    results = cross_section%calculate( grids, profiles )
    call check_values( results, no_extrap, .01_dk )
    deallocate( cross_section )

    ! clean up
    deallocate( iter )
    deallocate( grids )
    deallocate( profiles )

  end subroutine test_cross_section_bro_br_o_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_cross_section
