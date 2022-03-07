! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests the photolysis_radiator module

!> Test module for the radiator_t type
program test_radiator

  use photolysis_radiator,             only : radiator_t
  use musica_string,                   only : string_t

  implicit none

  type(string_t) :: radiator_name

  radiator_name = "Air"
  call test_radiator_t( radiator_name )
  radiator_name = "foo"
  call test_radiator_t( radiator_name )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Test radiator_t functionality
  subroutine test_radiator_t( radiator_name )

    use micm_grid_warehouse,           only : grid_warehouse_t
    use micm_profile,                  only : abs_profile_t
    use test_mock_profile_warehouse,   only : mock_profile_warehouse_t
    use micm_radxfer_xsect_warehouse,  only : radxfer_xsect_warehouse_t
    use micm_radiator_factory,         only : radiator_builder
    use musica_assert,                 only : assert, die
    use musica_config,                 only : config_t
    use musica_constants,              only : dk => musica_dk

    type(string_t), intent(in) :: radiator_name

    character(len=*), parameter :: Iam = "radiator_t tests"
    type(config_t) :: grids_config, radiator_config, profiles_config
    type(config_t) :: cross_sections_config
    class(grid_warehouse_t), pointer :: grids
    class(radiator_t), pointer :: radiator
    class(mock_profile_warehouse_t), pointer :: profiles
    class(radXfer_xsect_warehouse_t), pointer :: cross_sections
    class(abs_profile_t), pointer :: profile

    integer :: i_cell, i_wavelength

    integer, parameter :: NUM_CELLS = 8
    integer, parameter :: NUM_WAVELENGTHS = 4
    real(kind=dk) :: cross_section_air( NUM_WAVELENGTHS )
    real(kind=dk) :: cross_section_foo( NUM_WAVELENGTHS )

    ! file data are averaged over the bin
    cross_section_air(:) = (/ 50.5,   5.5, 505.0, 1000.0 /)
    cross_section_foo(:) = (/ 55.0, 550.0, 500.5,    1.0 /)

    call grids_config%from_file( "data/unit/radiator/grids_config.json" )
    grids => grid_warehouse_t( grids_config )
    call radiator_config%from_file( "data/unit/radiator/radiator_config.json" )
    call radiator_config%add( "Handle", radiator_name, Iam )
    radiator => radiator_builder( radiator_config, grids )
    call profiles_config%from_file( "data/unit/radiator/profiles_config.json" )
    profiles => mock_profile_warehouse_t( profiles_config, grids )
    call cross_sections_config%from_file(                                     &
             "data/unit/radiator/cross_sections_config.json" )
    cross_sections => radxfer_xsect_warehouse_t( cross_sections_config,       &
                                                 grids, profiles )

    ! check shapes of optical property arrays
    call assert( 110554666, size( radiator%state_%layer_OD_,  1 ) .eq. NUM_CELLS       )
    call assert( 784464736, size( radiator%state_%layer_OD_,  2 ) .eq. NUM_WAVELENGTHS )
    call assert( 381048062, size( radiator%state_%layer_SSA_, 1 ) .eq. NUM_CELLS       )
    call assert( 210891158, size( radiator%state_%layer_SSA_, 2 ) .eq. NUM_WAVELENGTHS )
    call assert( 105742654, size( radiator%state_%layer_G_,   1 ) .eq. NUM_CELLS       )
    call assert( 553110500, size( radiator%state_%layer_G_,   2 ) .eq. NUM_WAVELENGTHS )

    ! calculate radiator optical properties
    call radiator%updateState( grids, profiles, cross_sections )

    ! check calculated optical properties
    if( radiator_name .eq. "Air" ) then
      do i_cell = 1, NUM_CELLS
        do i_wavelength = 1, NUM_WAVELENGTHS
          call assert( 258157950,                                             &
                       radiator%state_%layer_OD_( i_cell, i_wavelength ) .eq. &
                       ( i_cell * 5.0_dk ) * cross_section_air( i_wavelength ) )
          call assert( 420883487,                                             &
                       radiator%state_%layer_SSA_( i_cell, i_wavelength ) .eq.&
                       1.0_dk )
          call assert( 412260407,                                             &
                       radiator%state_%layer_G_( i_cell, i_wavelength ) .eq.  &
                       0.0_dk )
        end do
      end do
    else if( radiator_name .eq. "foo" ) then
      do i_cell = 1, NUM_CELLS
        do i_wavelength = 1, NUM_WAVELENGTHS
          call assert( 188076012,                                             &
                       radiator%state_%layer_OD_( i_cell, i_wavelength ) .eq. &
                       ( i_cell * 5.0_dk ) * cross_section_foo( i_wavelength ) )
          call assert( 917919107,                                             &
                       radiator%state_%layer_SSA_( i_cell, i_wavelength ) .eq.&
                       0.0_dk )
          call assert( 130237453,                                             &
                       radiator%state_%layer_G_( i_cell, i_wavelength ) .eq.  &
                       0.0_dk )
        end do
      end do
    else
      call die( 108159667 )
    end if

    deallocate( grids          )
    deallocate( radiator       )
    deallocate( profiles       )
    deallocate( cross_sections )

  end subroutine test_radiator_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_radiator
