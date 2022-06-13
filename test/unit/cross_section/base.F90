! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the base cross_section_t type
program test_cross_section

  use tuvx_cross_section

  implicit none

  call test_cross_section_t( )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine test_cross_section_t( )

    use musica_assert,                 only : assert
    use musica_constants,              only : dk => musica_dk
    use musica_config,                 only : config_t
    use musica_iterator,               only : iterator_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    class(grid_warehouse_t),    pointer :: grids
    class(profile_warehouse_t), pointer :: profiles
    class(cross_section_t),     pointer :: cross_section

    character(len=*), parameter :: Iam = "base cross section tests"
    type(config_t) :: config, cs_set, cs_config
    class(iterator_t), pointer :: iter
    real(kind=dk), allocatable :: results(:,:)
    real(kind=dk), allocatable :: input(:), input_grid(:)
    real(kind=dk) :: input_base(4) =                                          &
      (/ 5.0_dk, 10.0_dk, 40.0_dk, 50.0_dk /)
    real(kind=dk) :: input_grid_base(4) =                                     &
      (/ 101.0_dk, 102.0_dk, 103.0_dk, 104.0_dk /)

    ! load test grids
    call config%from_file( "test/data/grid.simple.config.json" )
    grids => grid_warehouse_t( config )

    ! load test profiles
    call config%from_file( "test/data/profile.simple.config.json" )
    profiles => profile_warehouse_t( config, grids )

    ! get cross section config data
    call config%from_file( "test/data/cross_section.base.config.json" )
    call config%get( "cross sections", cs_set, Iam )
    iter => cs_set%get_iterator( )

    ! load and test cross section w/o extrapolation
    call assert( 560066370, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_t( cs_config, grids, profiles )
    results = cross_section%calculate( grids, profiles )
    input = input_base
    input_grid = input_grid_base
    call add_points( input, input_grid, 0.0_dk, 0.0_dk )
    call check_values( results, input, input_grid, 6 )
    deallocate( input )
    deallocate( input_grid )
    deallocate( cross_section )

    ! load and test cross section w/ fixed lower extrapolation and no upper
    ! extrapolation
    call assert( 102622205, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_t( cs_config, grids, profiles )
    results = cross_section%calculate( grids, profiles, at_mid_point = .true. )
    input = input_base
    input_grid = input_grid_base
    call add_points( input, input_grid, 12.5_dk, 0.0_dk )
    call check_values( results, input, input_grid, 5 )
    deallocate( input )
    deallocate( input_grid )
    deallocate( cross_section )

    ! load and test cross section w/ extrpolation from lower boundary and
    ! fixed upper extrpolation
    call assert( 101168966, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_t( cs_config, grids, profiles )
    results = cross_section%calculate( grids, profiles, at_mid_point = .false. )
    input = input_base
    input_grid = input_grid_base
    call add_points( input, input_grid, 5.0_dk, 32.3_dk )
    call check_values( results, input, input_grid, 6 )
    deallocate( input )
    deallocate( input_grid )
    deallocate( cross_section )

    ! clean up
    deallocate( iter )
    deallocate( grids )
    deallocate( profiles )

  end subroutine test_cross_section_t

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
  subroutine check_values( results, input, input_grid, n_levels )

    use musica_assert,                 only : assert, almost_equal
    use musica_constants,              only : dk => musica_dk
    use tuvx_util,                     only : inter2

    real(kind=dk), intent(in) :: results(:,:)
    real(kind=dk), intent(in) :: input(:)
    real(kind=dk), intent(in) :: input_grid(:)
    integer,       intent(in) :: n_levels

    integer :: i_level, i_wavelength
    real(kind=dk) :: output_grid(5) =                                         &
      (/ 100.0_dk, 101.5_dk, 102.0_dk, 103.0_dk, 104.5_dk /)
    integer :: i_error
    real(kind=dk) :: output(4)

    call assert( 577098581, size( results, dim = 1 ) == n_levels )
    call assert( 696108875, size( results, dim = 2 ) == size( output ) )
    do i_wavelength = 1, size( results, dim = 2 )
      do i_level = 2, size( results, dim = 1 )
        call assert( 179372912, results( i_level, i_wavelength ) ==           &
                                results( 1,       i_wavelength ) )
      end do
    end do

    ! x data are assumed to be at interfaces and y data at midpoints
    call inter2( xto   = output_grid, yto   = output,                         &
                 xfrom = input_grid,  yfrom = input,  ierr = i_error )
    call assert( 271086324, i_error == 0 )
    call assert( 992044813, almost_equal( results( 1, 1 ), output( 1 ) ) )
    call assert( 871103388, almost_equal( results( 1, 2 ), output( 2 ) ) )
    call assert( 418471235, almost_equal( results( 1, 3 ), output( 3 ) ) )
    call assert( 313322731, almost_equal( results( 1, 4 ), output( 4 ) ) )

  end subroutine check_values

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_cross_section
