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
    use tuvx_profile,                    only : abs_profile_t
    use tuvx_grid_warehouse,             only : grid_warehouse_t
    use tuvx_profile_from_config,        only : fromConfig_t
    use tuvx_profile_surface_albedo,     only : srfAlbedofromConfig_t

    !> Arguments
    !> Grid configuration data
    type(config_t), intent(inout)         :: config
    type(grid_warehouse_t), intent(inout) :: gridWareHouse

    !> New profile object
    class(abs_profile_t), pointer :: new_profile_t

    !> Local variables
    character(len=*), parameter :: Iam = 'profile builder: '
    type(string_t) :: profile_type

    write(*,*) Iam,'entering'

    new_profile_t => null()
    call config%get( 'Profile type', profile_type, Iam )

    select case( profile_type%to_char() )
      case( 'From csv file' )
        allocate( fromCsvFile_t :: new_profile_t )
      case( 'Etfl from csv file' )
        allocate( etflfromCsvFile_t :: new_profile_t )
      case( 'From config file' )
        allocate( fromConfig_t :: new_profile_t )
      case( 'SrfAlbedo from config file' )
        allocate( srfAlbedofromConfig_t :: new_profile_t )
      case( 'Air from csv file' )
        allocate( airfromCsvFile_t :: new_profile_t )
      case( 'O2 from csv file' )
        allocate( o2fromCsvFile_t :: new_profile_t )
      case( 'O3 from csv file' )
        allocate( o3fromCsvFile_t :: new_profile_t )
      case( 'Sza from time' )
        allocate( sza_from_time_t :: new_profile_t )
      case( 'Earth sun distance' )
        allocate( earth_sun_distance_t :: new_profile_t )
      case default
        call die_msg( 460768215, "Invalid profile type: '" // profile_type%to_char()//"'" )
    end select

    call new_profile_t%initialize( config, gridWareHouse )

    write(*,*) Iam,'exiting'

  end function profile_builder

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_profile_factory
