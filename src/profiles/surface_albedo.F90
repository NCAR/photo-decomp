! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
! profile specified in json config file
module tuvx_profile_surface_albedo

  use musica_constants, only : dk => musica_dk, ik => musica_ik, lk => musica_lk
  use tuvx_profile,     only : abs_profile_t

  implicit none

  private
  public :: srfAlbedofromConfig_t

  type, extends(abs_profile_t) :: srfAlbedofromConfig_t
  contains
    !> Initialize grid
    procedure :: initialize
  end type srfAlbedofromConfig_t

contains
  !> Initialize grid
  subroutine initialize( this, profile_config, gridWareHouse )
      
    use musica_config, only : config_t
    use musica_string, only : string_t
    use musica_assert, only : die_msg
    use tuvx_grid,  only : abs_1d_grid_t
    use tuvx_grid_warehouse,  only : grid_warehouse_t

    !> Arguments
    class(srfAlbedofromConfig_t), intent(inout) :: this
    type(config_t), intent(inout)               :: profile_config
    type(grid_warehouse_t), intent(inout)       :: gridWareHouse

    !> Local variables
    character(len=*), parameter :: Iam = 'From config profile initialize: '
    integer(ik)                   :: ndx
    real(dk)                      :: uniformValue
    class(abs_1d_grid_t), pointer :: lambdaGrid
    type(string_t)                :: Handle
 
    !> Get the handle
    call profile_config%get( 'Handle', this%handle_, Iam, default = 'None' )

    Handle = 'Photolysis, wavelength'
    lambdaGrid => gridWareHouse%get_grid( Handle )

    !> Get values from config file
    call profile_config%get( "Uniform Value", uniformValue, Iam )

    this%ncells_ = lambdaGrid%ncells_
    this%edge_val_ = (/ (uniformValue,ndx=1,this%ncells_+1_ik) /)
    this%mid_val_ = .5_dk &
                   *(this%edge_val_(1_ik:this%ncells_) + this%edge_val_(2_ik:this%ncells_+1_ik))
    this%delta_val_ = (this%edge_val_(2_ik:this%ncells_+1_ik) - this%edge_val_(1_ik:this%ncells_))

  end subroutine initialize

end module tuvx_profile_surface_albedo
