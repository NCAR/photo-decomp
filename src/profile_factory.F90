! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The tuvx_profile_factory module

!> Build profile objects
module tuvx_profile_factory


  implicit none

  private
  public :: profile_builder

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function profile_builder( config, gridWareHouse ) result( new_profile_t )

    use musica_assert,                   only : die_msg
    use musica_config,                   only : config_t
    use musica_string,                   only : string_t
    use tuvx_profile_from_csv_file,      only : fromCsvFile_t
    use tuvx_profile_extraterrestrial_flux, only : etflfromCsvFile_t
    use tuvx_profile_air,                only : airfromCsvFile_t
    use tuvx_profile_o2,                 only : o2fromCsvFile_t
    use tuvx_profile_o3,                 only : o3fromCsvFile_t
    use tuvx_profile_solar_zenith_angle, only : sza_from_time_t
    use tuvx_profile_earth_sun_distance, only : earth_sun_distance_t
    use tuvx_profile,                    only : profile_t
    use tuvx_grid_warehouse,             only : grid_warehouse_t
    use tuvx_profile_from_config,        only : fromConfig_t
    use tuvx_profile_surface_albedo,     only : srfAlbedofromConfig_t

    !> Arguments
    !> Grid configuration data
    type(config_t), intent(inout)         :: config
    type(grid_warehouse_t), intent(inout) :: gridWareHouse

    !> New profile object
    class(profile_t), pointer :: new_profile_t

    !> Local variables
    character(len=*), parameter :: Iam = 'profile builder: '
    type(string_t) :: profile_type

    write(*,*) Iam,'entering'

    new_profile_t => null()
    call config%get( 'Profile type', profile_type, Iam )

    select case( profile_type%to_char() )
      case( 'From csv file' )
        new_profile_t => fromCsvFile_t( config, gridWareHouse )
      case( 'Etfl from csv file' )
        new_profile_t => etflfromCsvFile_t( config, gridWareHouse )
      case( 'From config file' )
        new_profile_t => fromConfig_t( config, gridWareHouse )
      case( 'SrfAlbedo from config file' )
        new_profile_t => srfAlbedofromConfig_t( config, gridWareHouse )
      case( 'Air from csv file' )
        new_profile_t => airfromCsvFile_t( config, gridWareHouse )
      case( 'O2 from csv file' )
        new_profile_t => o2fromCsvFile_t( config, gridWareHouse )
      case( 'O3 from csv file' )
        new_profile_t => o3fromCsvFile_t( config, gridWareHouse )
      case( 'Sza from time' )
        new_profile_t => sza_from_time_t( config, gridWareHouse )
      case( 'Earth sun distance' )
        new_profile_t => earth_sun_distance_t( config, gridWareHouse )
      case default
        call die_msg( 460768215, "Invalid profile type: '" // profile_type%to_char()//"'" )
    end select

    write(*,*) Iam,'exiting'

  end function profile_builder

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_profile_factory
