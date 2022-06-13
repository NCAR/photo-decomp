! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> Surface albedo profile type
module tuvx_profile_surface_albedo

  use musica_constants, only : &
    dk => musica_dk, ik => musica_ik, lk => musica_lk
  use tuvx_profile,     only : profile_t

  implicit none

  private
  public :: surface_albedo_t

  type, extends(profile_t) :: surface_albedo_t
  contains
  end type surface_albedo_t

  !> Constructor
  interface surface_albedo_t
    module procedure constructor
  end interface surface_albedo_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize grid
  function constructor( profile_config, grid_warehouse ) result( this )

    use musica_config, only : config_t
    use musica_string, only : string_t
    use musica_assert, only : die_msg
    use tuvx_grid,  only : grid_t
    use tuvx_grid_warehouse,  only : grid_warehouse_t

    type(config_t), intent(inout)         :: profile_config
    type(surface_albedo_t), pointer       :: this
    type(grid_warehouse_t), intent(inout) :: grid_warehouse

    ! Local variables
    real(dk)                    :: uniformValue
    integer(ik)                 :: ndx
    type(string_t)              :: Handle
    class(grid_t), pointer      :: lambdaGrid
    character(len=*), parameter :: Iam = 'From config profile initialize: '

    allocate( this )

    ! Get the handle
    call profile_config%get( 'Handle', this%handle_, Iam, default = 'None' )

    Handle = 'Photolysis, wavelength'
    lambdaGrid => grid_warehouse%get_grid( Handle )

    ! Get values from config file
    call profile_config%get( "Uniform Value", uniformValue, Iam )

    this%ncells_ = lambdaGrid%ncells_

    this%edge_val_ = (/ (uniformValue,ndx=1,this%ncells_+1_ik) /)

    this%mid_val_ = .5_dk * ( &
      this%edge_val_(1_ik:this%ncells_) + &
      this%edge_val_(2_ik:this%ncells_+1_ik) &
    )

    this%delta_val_ = this%edge_val_(2_ik:this%ncells_+1_ik) - &
      this%edge_val_(1_ik:this%ncells_)

    deallocate( lambdaGrid )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_profile_surface_albedo
