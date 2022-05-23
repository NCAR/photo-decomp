! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests the photo_decomp Profile module

!> Test module for the Profile_t type
program test_Profile

  use musica_string,    only : string_t

  implicit none

  !> Command-line options
  character(len=256) :: argument
  type(string_t)     :: configFileSpec
  !> Command-line argument index
  integer :: i_arg

  !> Get the model configuration file and options from the command line
  if( command_argument_count( ) /= 1 ) call fail_run( )
  call get_command_argument( command_argument_count( ), argument )

  configFileSpec = argument
  call test_Profile_t( configFileSpec )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Tests the Profile_t type
  subroutine test_Profile_t( config_flsp )

    use musica_config,    only : config_t
    use musica_string,    only : string_t
    use musica_assert,    only : assert, almost_equal
    use musica_constants, only : ik => musica_ik, dk => musica_dk
    use micm_grid_warehouse, only : grid_warehouse_t
    use micm_1d_grid,    only : abs_1d_grid_t
    use micm_Profile_warehouse, only : Profile_warehouse_t
    use micm_Profile,           only : abs_Profile_t

    !> Arguments
    type(string_t), intent(in) :: config_flsp
    !> Local variables
    character(len=*), parameter :: Iam = 'test_Profile: '
    type(config_t)              :: tst_config
    type(grid_warehouse_t), pointer :: theGridWarehouse
    class(abs_1d_grid_t), pointer   :: zGrid, lambdaGrid
    type(Profile_warehouse_t), pointer :: theProfileWarehouse
    class(abs_Profile_t), pointer      :: aProfile
    class(abs_Profile_t), pointer      :: AirProfile, TemperatureProfile
    class(abs_Profile_t), pointer      :: O3Profile
    type(string_t)                  :: Handle

    write(*,*) Iam // 'entering'

    !> master configuration -> config type
    call tst_config%from_file( config_flsp%to_char() )

    !> Initialize grid warehouse
    theGridWarehouse => grid_warehouse_t( tst_config )

    !> Get copy of grid
    Handle = 'Vertical Z'
    zGrid => theGridWarehouse%get_grid( Handle )
    call assert( 412238768, zGrid%ncells_ .eq. 120_ik )
    call assert( 412238769, all( zGrid%delta_ .eq. 1._dk ) )

    !> Get copy of wavelength grid
    Handle = 'Photolysis, wavelength'
    lambdaGrid => theGridWarehouse%get_grid( Handle )

    !> Initialize profile warehouse
    theProfileWarehouse => Profile_warehouse_t( tst_config, theGridWareHouse )

    !> Get copy of the Air Profile
    Handle = 'Air'
    AirProfile => theProfileWarehouse%get_Profile( Handle )
    call assert( 412238771, all( AirProfile%delta_val_ < 0._dk ) )

    !> Get copy of the temperature Profile
    Handle = 'Temperature'
    TemperatureProfile => theProfileWarehouse%get_Profile( Handle )
    call assert( 412238772, all( TemperatureProfile%edge_val_ < 400._dk ) )
    call assert( 412238772, all( TemperatureProfile%edge_val_ > 150._dk ) )
    call assert( 412238773, all( abs(TemperatureProfile%delta_val_) < 20._dk ) )

    !> Get copy of the Unit test
    Handle = 'UnitTest'
    aProfile => theProfileWarehouse%get_Profile( Handle )
    call assert( 412238770, all( aProfile%edge_val_ .eq. 7.7e11_dk ) )

    deallocate( aProfile )

    !> Get copy of O2 profile
    Handle = 'O2'
    aProfile => theProfileWarehouse%get_Profile( Handle )

    deallocate( aProfile )

    !> Get copy of ozone profile
    Handle = 'O3'
    O3Profile => theProfileWarehouse%get_Profile( Handle )

    write(*,*) Iam // 'Handle = ',O3Profile%handle_
    write(*,*) ' '
    write(*,*) ' '
    write(*,*) Iam // 'O3 on model z grid'
    write(*,'(1p10g15.7)') O3Profile%edge_val_

    write(*,*) Iam // 'leaving'

  end subroutine test_Profile_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Fail run and print usage info
  subroutine fail_run( )

    write(*,*) "Usage: ./Profile_test configuration_file.json"
    stop 'Improper arguments'

  end subroutine fail_run

end program test_Profile
