! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
! one dimension grid type
module tuvx_grid

  use musica_constants, only : musica_dk, musica_ik
  use musica_string,    only : string_t

  implicit none

  private
  public :: grid_t, grid_ptr, base_constructor

  type, abstract ::  grid_t
    !> grid handle
    type(string_t) :: handle_
    !> number of wavelength grid cells
    integer(musica_ik) :: ncells_
    !> cell centers
    real(musica_dk), allocatable :: mid_(:)
    !> cell edges
    real(musica_dk), allocatable :: edge_(:)
    !> cell deltas
    real(musica_dk), allocatable :: delta_(:)
  contains
  end type grid_t

  !> Pointer type for building sets of spectral wght objects
  type :: grid_ptr
    class(grid_t), pointer :: val_ => null( )
  end type grid_ptr

interface

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !> construct the grid
    subroutine base_constructor( this, grid_config )
      
      use musica_config, only : config_t

      import grid_t
      class(grid_t), intent(inout)  :: this
      type(config_t), intent(inout) :: grid_config
    end subroutine base_constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end interface

end module tuvx_grid
