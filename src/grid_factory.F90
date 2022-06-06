! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The tuvx_grid_factory module

!> Build grid objects
module tuvx_grid_factory

  use tuvx_grid,                       only : grid_t
  use tuvx_grid_equal_delta,           only : equalDelta_t
  use tuvx_grid_from_csv_file,         only : fromCsvFile_t
  use tuvx_grid_from_config,           only : fromConfig_t

  implicit none

  private
  public :: grid_builder

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function grid_builder( config ) result( new_grid_t )

    use musica_assert,                 only : die_msg
    use musica_config,                 only : config_t
    use musica_string,                 only : string_t

    !> Arguments
    !> Grid configuration data
    type(config_t), intent(inout) :: config

    !> New grid object
    class(grid_t), pointer :: new_grid_t

    !> Local variables
    type(string_t) :: grid_type
    character(len=*), parameter :: Iam = 'Grid builder: '

    write(*,*) Iam,'entering'
    new_grid_t => null()
    call config%get( 'Grid type', grid_type, Iam )

    select case( grid_type%to_char() )
      case( 'Equal interval' )
        new_grid_t => equalDelta_t( config )
      case( 'From csv file' )
        new_grid_t => fromCsvFile_t( config )
      case( 'From config file' )
        new_grid_t => fromConfig_t( config )
      case default
        call die_msg( 460768215, "Invalid grid type: '" // grid_type%to_char()//"'" )
    end select

    write(*,*) Iam,'exiting'

  end function grid_builder

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_grid_factory
