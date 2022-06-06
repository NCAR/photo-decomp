! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The tuvx_radiator_factory module

!> Build radiator objects
module tuvx_radiator_factory

  use tuvx_radiator,       only : base_radiator_t, base_constructor
  use tuvx_radiator_aerosol,    only : aerosol_radiator_t

  implicit none

  private
  public :: radiator_builder

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function radiator_builder( config, gridWareHouse ) result( new_radiator_t )

    use musica_assert,                 only : die_msg
    use musica_config,                 only : config_t
    use musica_string,                 only : string_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t

    !> Arguments
    !> Radiator configuration data
    type(config_t), intent(inout)         :: config
    type(grid_warehouse_t), intent(inout) :: gridWareHouse

    !> New radiator object
    class(base_radiator_t), pointer :: new_radiator_t

    !> Local variables
    type(string_t) :: radiator_type
    character(len=*), parameter :: Iam = 'Radiator builder: '

    write(*,*) Iam,'entering'
    new_radiator_t => null()
    call config%get( 'type', radiator_type, Iam )

    select case( radiator_type%to_char() )
      case( 'base' )
        allocate( new_radiator_t )
        call base_constructor( new_radiator_t, config, gridWareHouse )
      case( 'aerosol' )
        new_radiator_t => aerosol_radiator_t( config, gridWareHouse )
      case default
        call die_msg( 460768245, "Invalid radiator type: '" // radiator_type%to_char()//"'" )
    end select

    write(*,*) Iam,'exiting'

  end function radiator_builder

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_radiator_factory
