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

    ! load test grids
    call config%from_file( "data/grid.simple.config.json" )
    grids => grid_warehouse_t( config )

    ! load test profiles
    call config%from_file( "data/profile.simple.config.json" )
    profiles => profile_warehouse_t( config, grids )

    ! get cross section config data
    call config%from_file( "data/cross_section.base.config.json" )
    call config%get( "cross sections", cs_set, Iam )
    iter => cs_set%get_iterator( )

    ! load and test cross section w/o extrapolation
    call assert( 560066370, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_t( cs_config, grids, profiles )

    deallocate( cross_section )

    ! load and test cross section w/ fixed lower extrapolation and no upper
    ! extrapolation
    call assert( 102622205, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_t( cs_config, grids, profiles )

    deallocate( cross_section )

    ! load and test cross section w/ extrpolation from lower boundary and
    ! fixed upper extrpolation
    call assert( 101168966, iter%next( ) )
    call cs_set%get( iter, cs_config, Iam )
    cross_section => cross_section_t( cs_config, grids, profiles )

    deallocate( cross_section )

    ! clean up
    deallocate( iter )
    deallocate( grids )
    deallocate( profiles )

  end subroutine test_cross_section_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_cross_section
