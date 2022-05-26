! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!profile type
module tuvx_profile

  use musica_constants, only : musica_dk, musica_ik
  use musica_string,    only : string_t

  implicit none

  private
  public :: abs_profile_t, abs_profile_ptr

  type, abstract ::  abs_profile_t
    !> grid handle
    type(string_t) :: handle_
    !> number of wavelength grid cells
    integer(musica_ik)           :: ncells_
    !> scale heigth
    real(musica_dk)              :: hscale_
    !> cell centers
    real(musica_dk), allocatable :: mid_val_(:)
    !> cell edges
    real(musica_dk), allocatable :: edge_val_(:)
    !> cell deltas
    real(musica_dk), allocatable :: delta_val_(:)
    !> layer densities
    real(musica_dk), allocatable :: layer_dens_(:)
    !> layer densities including "exo" model layer
    real(musica_dk), allocatable :: exo_layer_dens_(:)
    !> overhead column burden
    real(musica_dk), allocatable :: burden_dens_(:)
  contains
    !> Initialize grid
    procedure(initial), deferred :: initialize
  end type abs_profile_t

  !> Pointer type for building sets of spectral wght objects
  type :: abs_profile_ptr
    class(abs_profile_t), pointer :: ptr_ => null( )
  end type abs_profile_ptr

interface

    !> Initialize grid
    subroutine initial( this, profile_config, gridWareHouse )
      
      use musica_config, only : config_t
      use musica_constants, only : musica_dk
      use tuvx_grid_warehouse,  only : grid_warehouse_t

      import abs_profile_t
      class(abs_profile_t), intent(inout)      :: this
      type(config_t), intent(inout)            :: profile_config
      type(grid_warehouse_t), intent(inout)    :: gridWareHouse
    end subroutine initial

end interface

end module tuvx_profile
