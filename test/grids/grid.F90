! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the photo_grid module

!> Test module for the grid_t type
program test_grid

  implicit none

  call test_grid_t( )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Tests of the grid_t type
  subroutine test_grid_t( )

    use musica_assert,                 only : assert, almost_equal
    use musica_constants,              only : dk => musica_dk

    real(kind=dk) :: a, b

    call assert( 412238768, .true. )
    a = 1.0_dk
    b = 1.0_dk + 1.0e-7_dk
    call assert( 689449710, .not. almost_equal( a, b ) )
    call assert( 699099331,                                                   &
                 almost_equal( a, b, relative_tolerance = 1.0e-5_dk ) )
    call assert( 415170843,                                                   &
                 almost_equal( a, b, absolute_tolerance = 1.0e-3_dk ) )

  end subroutine test_grid_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_grid
