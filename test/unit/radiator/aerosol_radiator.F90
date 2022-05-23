! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests the photolysis_aerosol_radiator module

!> Test module for the aerosol_radiator_t type
program test_aerosol_radiator

  use musica_config,                   only : config_t
  use musica_iterator,                 only : iterator_t
  use photolysis_aerosol_radiator,     only : aerosol_radiator_t

  implicit none

  character(len=*), parameter :: Iam = "aerosol_radiator_t tests"
  type(config_t) :: config, radiator_set, radiator
  class(iterator_t), pointer :: iter

  call config%from_file( "data/unit/radiator/aerosol_radiator_config.json" )
  call config%get( "radiators", radiator_set, Iam )
  iter => radiator_set%get_iterator( )
  do while( iter%next( ) )
    call radiator_set%get( iter, radiator, Iam )
    call test_aerosol_radiator_t( radiator )
  end do
  deallocate( iter )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Test aerosol_radiator_t functionality
  subroutine test_aerosol_radiator_t( radiator_config )

    use micm_grid_warehouse,           only : grid_warehouse_t
    use micm_profile,                  only : abs_profile_t
    use test_mock_profile_warehouse,   only : mock_profile_warehouse_t
    use micm_radxfer_xsect_warehouse,  only : radxfer_xsect_warehouse_t
    use micm_radiator_factory,         only : radiator_builder
    use musica_assert,                 only : almost_equal, assert, die
    use musica_constants,              only : dk => musica_dk
    use musica_string,                 only : string_t
    use photolysis_radiator,           only : radiator_t

    type(config_t), intent(inout) :: radiator_config

    character(len=*), parameter :: Iam = "aerosol_radiator_t tests"
    type(config_t) :: grids_config, profiles_config
    type(config_t) :: cross_sections_config
    class(grid_warehouse_t), pointer :: grids
    class(radiator_t), pointer :: radiator
    class(mock_profile_warehouse_t), pointer :: profiles
    class(radXfer_xsect_warehouse_t), pointer :: cross_sections
    class(abs_profile_t), pointer :: profile
    type(string_t) :: radiator_name

    integer :: i_cell, i_wavelength
    real(kind=dk) :: base_OD(8)
    real(kind=dk) :: cell_mid
    real(kind=dk) :: true_OD, true_SSA, true_G

    integer, parameter :: NUM_CELLS = 8
    integer, parameter :: NUM_WAVELENGTHS = 4

    call grids_config%from_file( "data/unit/radiator/grids_config.json" )
    grids => grid_warehouse_t( grids_config )
    radiator => radiator_builder( radiator_config, grids )
    call profiles_config%from_file( "data/unit/radiator/profiles_config.json" )
    profiles => mock_profile_warehouse_t( profiles_config, grids )
    call cross_sections_config%from_file(                                     &
             "data/unit/radiator/cross_sections_config.json" )
    cross_sections => radxfer_xsect_warehouse_t( cross_sections_config,       &
                                                 grids, profiles )

    ! check shapes of optical property arrays
    call assert( 971205997, size( radiator%state_%layer_OD_,  1 ) .eq. NUM_CELLS       )
    call assert( 518573844, size( radiator%state_%layer_OD_,  2 ) .eq. NUM_WAVELENGTHS )
    call assert( 630892189, size( radiator%state_%layer_SSA_, 1 ) .eq. NUM_CELLS       )
    call assert( 178260036, size( radiator%state_%layer_SSA_, 2 ) .eq. NUM_WAVELENGTHS )
    call assert( 625627882, size( radiator%state_%layer_G_,   1 ) .eq. NUM_CELLS       )
    call assert( 172995729, size( radiator%state_%layer_G_,   2 ) .eq. NUM_WAVELENGTHS )

    ! calculate radiator optical properties
    call radiator%updateState( grids, profiles, cross_sections )

    ! check calculated optical properties
    base_OD = (/ 15.0_dk, 30.0_dk, 35.0_dk, 45.0_dk,                          &
                  0.0_dk,  0.0_dk,  0.0_dk,  0.0_dk  /)
    call radiator_config%get( "Handle", radiator_name, Iam )
    if( radiator_name .eq. "aerosol 1" ) then
      do i_wavelength = 1, NUM_WAVELENGTHS
        do i_cell = 1, NUM_CELLS
          if( i_wavelength .lt. NUM_WAVELENGTHS ) then
            cell_mid = 35.0_dk + i_wavelength * 10.0_dk
          else
            cell_mid = 73.0_dk
          end if
          true_OD = base_OD( i_cell ) * ( 340.0_dk / cell_mid )
          if ( i_cell .le. 4 ) then
            true_SSA = 42.35_dk
            true_G   = 312.4_dk
          else
            true_SSA = 1.0_dk
            true_G   = 0.0_dk
          end if
          call assert( 994706659,                                             &
            almost_equal( radiator%state_%layer_OD_( i_cell, i_wavelength ),  &
                          true_OD ) )
          call assert( 245461934,                                             &
            almost_equal( radiator%state_%layer_SSA_( i_cell, i_wavelength ), &
                          true_SSA ) )
          call assert( 124972804,                                             &
            almost_equal( radiator%state_%layer_G_( i_cell, i_wavelength ),   &
                          true_G ) )
        end do
      end do
    else if( radiator_name .eq. "aerosol 2" ) then
      do i_wavelength = 1, NUM_WAVELENGTHS
        do i_cell = 1, NUM_CELLS
          if( i_wavelength .lt. NUM_WAVELENGTHS ) then
            cell_mid = 35.0_dk + i_wavelength * 10.0_dk
          else
            cell_mid = 73.0_dk
          end if
          true_OD = base_OD( i_cell )                                         &
                    * 20.0_dk / ( sum( base_OD ) )                            &
                      * ( 550.0_dk / 340.0_dk )**(0.5_dk)                     &
                    * ( 340.0_dk / cell_mid )**(0.5_dk)
          if ( i_cell .le. 4 ) then
            true_SSA = 42.35_dk
            true_G   = 312.4_dk
          else
            true_SSA = 1.0_dk
            true_G   = 0.0_dk
          end if
          call assert( 550671989,                                             &
            almost_equal( radiator%state_%layer_OD_( i_cell, i_wavelength ),  &
                          true_OD ) )
          call assert( 357066458,                                             &
            almost_equal( radiator%state_%layer_SSA_( i_cell, i_wavelength ), &
                          true_SSA ) )
          call assert( 521959055,                                             &
            almost_equal( radiator%state_%layer_G_( i_cell, i_wavelength ),   &
                          true_G ) )
        end do
      end do
    else if( radiator_name .eq. "aerosol 3" ) then
      do i_wavelength = 1, NUM_WAVELENGTHS
        do i_cell = 1, NUM_CELLS
          if( i_wavelength .lt. NUM_WAVELENGTHS ) then
            cell_mid = 35.0_dk + i_wavelength * 10.0_dk
          else
            cell_mid = 73.0_dk
          end if
          true_OD  = 22.45_dk * 340.0_dk / cell_mid
          true_SSA = 42.35_dk
          true_G   = 312.4_dk
          call assert( 699197419,                                             &
            almost_equal( radiator%state_%layer_OD_( i_cell, i_wavelength ),  &
                          true_OD ) )
          call assert( 864178397,                                             &
            almost_equal( radiator%state_%layer_SSA_( i_cell, i_wavelength ), &
                          true_SSA ) )
          call assert( 694021493,                                             &
            almost_equal( radiator%state_%layer_G_( i_cell, i_wavelength ),   &
                          true_G ) )
        end do
      end do
    else
      call die( 320380640 )
    end if

    deallocate( grids          )
    deallocate( radiator       )
    deallocate( profiles       )
    deallocate( cross_sections )

  end subroutine test_aerosol_radiator_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_aerosol_radiator
