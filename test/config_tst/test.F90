! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the musica config type

program config_tst

  implicit none

  call test_config_t( )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Tests of the grid_t type
  subroutine test_config_t( )

    use test_config,      only : config_t
    use musica_string,    only : string_t
    use musica_assert,    only : assert, almost_equal
    use musica_constants, only : ik => musica_ik, dk => musica_dk

    !> local variables
    character(len=*), parameter :: Iam = 'test_config: '
    character(len=*), parameter :: config_flsp = '../test/data/tst.config.json'

    integer :: ndx
    real(dk), allocatable :: Angles(:)
    type(config_t) :: config_tst_config
    type(string_t), allocatable :: components(:)

    write(*,*) Iam // 'entering'

    call config_tst_config%from_file( config_flsp )
    call config_tst_config%get( "Components", components, Iam )
    write(*,*) Iam // 'size of components = ',size(components)
    do ndx = 1,size(components)
      write(*,*) components(ndx)%to_char()
    enddo

    write(*,*) ' '
    call config_tst_config%get( "Angles", Angles, Iam )
    write(*,*) Iam // 'size of Angles = ',size(Angles)
    do ndx = 1,size(Angles)
      write(*,*) Angles(ndx)
    enddo

    write(*,*) Iam // 'leaving'

  end subroutine test_config_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program config_tst
