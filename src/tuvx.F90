! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The photolysis program

!> Driver for the photolysis utility
program tuvx

  use musica_string,    only : string_t
  use tuvx_core,  only : photolysis_core_t

  implicit none

  class(photolysis_core_t), pointer :: photolysis_core

  !> Command-line options
  character(len=256) :: argument
  type(string_t)     :: configFileSpec

  !> Get the model configuration file and options from the command line
  if( command_argument_count() /= 1 ) then
    call fail_run( )
  endif
  call get_command_argument( 1, argument )

  configFileSpec = argument

  !> instatiate and initialize photolysis core object
  photolysis_core => photolysis_core_t( configFileSpec )

  !> run photolysis
  call photolysis_core%run()

  contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Fail run and print usage info
  subroutine fail_run( )

    write(*,*) "Usage: ./photolysis configuration_file.json"
    stop 'Photolysis: Improper arguments'

  end subroutine fail_run

end program tuvx
