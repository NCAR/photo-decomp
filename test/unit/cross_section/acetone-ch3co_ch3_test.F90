! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the base cross_section_t type
program test_cross_section

  use tuvx_cross_section, only : cross_section_t
  use tuvx_cross_section_ch3coch3_ch3co_ch3

  implicit none

  call test_cross_section_ch3coch3_ch3co_ch3_t( )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine test_cross_section_ch3coch3_ch3co_ch3_t( )

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
    ! real(dk), dimension(4:6) :: acetone_no_extrap
    real(dk) :: acetone_no_extrap(:,:)

    acetone_no_extrap = reshape([ &
      7.0301862E+08,6.5655223E+08,6.1219315E+08,5.6989122E+08,5.2959639E+08,4.9126447E+08, &
      1.1707670E+09,1.0933860E+09,1.0195143E+09,9.4906831E+08,8.8196471E+08,8.1812988E+08, &
      1.5530895E+09,1.4504406E+09,1.3524467E+09,1.2589971E+09,1.1699813E+09,1.0853017E+09, &
      9.3653653E+08,8.7463863E+08,8.1554773E+08,7.5919704E+08,7.0551989E+08,6.5445740E+08],&
      (/ 4, 6 /))

    ! load test grids
    call config%from_file( "test/data/grid.simple.config.json" )
    grids => grid_warehouse_t( config )

    ! load test profiles
    call config%from_file( "test/data/profile.acetone.config.json" )
    profiles => profile_warehouse_t( config, grids )

    ! get cross section config data
    call config%from_file( "test/data/cross_section.acetone.config.json" )
    call config%get( "cross sections", cs_set, Iam )
    iter => cs_set%get_iterator( )

    ! load and test cross section w/o extrapolation
    call assert( 560066370, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_ch3coch3_ch3co_ch3_t( cs_config, grids, profiles )
    results = cross_section%calculate( grids, profiles )
    call check_values( results, acetone_no_extrap )
    deallocate( cross_section )

    ! ! load and test cross section w/ fixed lower extrapolation and no upper
    ! ! extrapolation
    ! call assert( 102622205, iter%next( ) )
    ! call cs_set%get( iter, cs_config, Iam )
    ! cross_section => cross_section_ch3coch3_ch3co_ch3_t( cs_config, grids, profiles )
    ! results = cross_section%calculate( grids, profiles, at_mid_point = .true. )
    ! input = input_base
    ! input_grid = input_grid_base
    ! call add_points( input, input_grid, 12.5_dk, 0.0_dk )
    ! call check_values( results, acetone_no_extrap )
    ! deallocate( input )
    ! deallocate( input_grid )
    ! deallocate( cross_section )

    ! ! load and test cross section w/ extrpolation from lower boundary and
    ! ! fixed upper extrpolation
    ! call assert( 101168966, iter%next( ) )
    ! call cs_set%get( iter, cs_config, Iam )
    ! cross_section => cross_section_ch3coch3_ch3co_ch3_t( cs_config, grids, profiles )
    ! results = cross_section%calculate( grids, profiles, at_mid_point = .false. )
    ! input = input_base
    ! input_grid = input_grid_base
    ! call add_points( input, input_grid, 5.0_dk, 32.3_dk )
    ! call check_values( results, input, input_grid, 6 )
    ! deallocate( input )
    ! deallocate( input_grid )
    ! deallocate( cross_section )

    ! clean up
    deallocate( iter )
    deallocate( grids )
    deallocate( profiles )

  end subroutine test_cross_section_ch3coch3_ch3co_ch3_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Adds additional points input data
  subroutine add_points( values, grid, lower_val, upper_val )

    use musica_constants,              only : dk => musica_dk
    use tuvx_util,                     only : addpnt

    real(kind=dk), allocatable, intent(inout) :: values(:)
    real(kind=dk), allocatable, intent(inout) :: grid(:)
    real(kind=dk),              intent(in)    :: lower_val
    real(kind=dk),              intent(in)    :: upper_val

    call addpnt( x = grid, y = values,                                        &
                 xnew = ( 1.0_dk - 1.0e-5_dk ) * grid(1), ynew = lower_val )
    call addpnt( x = grid, y = values,                                        &
                 xnew = 0.0_dk, ynew = lower_val )
    call addpnt( x = grid, y = values,                                        &
                 xnew = ( 1.0_dk + 1.0e-5_dk ) * grid( size( grid ) ),        &
                 ynew = upper_val )
    call addpnt( x = grid, y = values,                                        &
                 xnew = 1.0e38_dk, ynew = upper_val )

  end subroutine add_points

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Checks results against expectations
  subroutine check_values( results, expected_results )

    use musica_assert,                 only : assert, almost_equal
    use musica_constants,              only : dk => musica_dk
    use tuvx_util,                     only : inter2

    real(kind=dk), intent(in) :: results(:,:)
    real(kind=dk), intent(in) :: expected_results(:,:)

    integer :: i_level, i_wavelength

    call assert( 577098581, &
      size( results, dim = 1 ) == size( expected_results, dim = 1) )
    call assert( 696108875, &
      size( results, dim = 2 ) == size( expected_results, dim = 2) )
    do i_wavelength = 1, size( results, dim = 2 )
      do i_level = 2, size( results, dim = 1 )
        call assert( 179372912, almost_equal( &
          results( i_level, i_wavelength ), &
          expected_results( i_level, i_wavelength ), &
          0.005_dk+8))
      end do
    end do

  end subroutine check_values

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_cross_section
