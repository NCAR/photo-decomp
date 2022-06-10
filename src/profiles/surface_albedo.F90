! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
! profile specified in json config file
module tuvx_profile_surface_albedo

  use musica_constants, only : dk => musica_dk, ik => musica_ik, lk => musica_lk
  use tuvx_profile,     only : profile_t

  implicit none

  private
  public :: srfAlbedofromConfig_t

  type, extends(profile_t) :: srfAlbedofromConfig_t
  contains
  end type srfAlbedofromConfig_t

  !> Constructor
  interface srfAlbedofromConfig_t
    module procedure constructor
  end interface srfAlbedofromConfig_t

contains
  !> Initialize grid
  function constructor( profile_config, gridWareHouse ) result( this )
      
    use musica_config, only : config_t
    use musica_string, only : string_t
    use musica_assert, only : die_msg
    use tuvx_grid,  only : grid_t
    use tuvx_grid_warehouse,  only : grid_warehouse_t

    !> Arguments
    type(srfAlbedofromConfig_t), pointer :: this
    type(config_t), intent(inout)               :: profile_config
    type(grid_warehouse_t), intent(inout)       :: gridWareHouse

    !> Local variables
    character(len=*), parameter :: Iam = 'From config profile initialize: '
    integer(ik)                   :: ndx
    real(dk)                      :: uniformValue
    class(grid_t), pointer :: lambdaGrid
    type(string_t)                :: Handle

    allocate( this )
 
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

    deallocate( lambdaGrid )

  end function constructor

end module tuvx_profile_surface_albedo
