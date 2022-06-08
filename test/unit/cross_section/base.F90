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

    use musica_config,                 only : config_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    class(grid_warehouse_t),    pointer :: grids
    class(profile_warehouse_t), pointer :: profiles
    class(cross_section_t),     pointer :: cross_section

    type(config_t) :: config

    ! load test grids
    call config%from_file( "data/grid.simple.config.json" )
    grids => grid_warehouse_t( config )

    ! load test profiles
    call config%from_file( "data/profile.simple.config.json" )
    profiles => profile_warehouse_t( config, grids )

    ! clean up
    deallocate( grids )
    deallocate( profiles )

  end subroutine test_cross_section_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_cross_section
