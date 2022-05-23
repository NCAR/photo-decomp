! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests the photo_decomp radiator module

!> Test module for the radiator_core_t type
program radiator_test

  use musica_string,    only : string_t
  use radiator_core,     only : radiator_core_t

  implicit none

  class(radiator_core_t), pointer :: radiator_core

  !> Command-line options
  character(len=256) :: argument
  type(string_t)     :: configFileSpec

  !> Get the model configuration file and options from the command line
! if( command_argument_count( ) /= 1 ) call fail_run( )
! call get_command_argument( command_argument_count( ), argument )
  argument = 'data/radiator.tst.config.json'

  configFileSpec = argument

  !> instatiate and initialize radiator core object
  radiator_core => radiator_core_t( configFileSpec )

  !> set radiator cross sections
  call radiator_core%test()

  contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Fail run and print usage info
  subroutine fail_run( )

    write(*,*) "Usage: ./radiator_test configuration_file.json"
    stop 'Improper arguments'

  end subroutine fail_run

end program radiator_test
