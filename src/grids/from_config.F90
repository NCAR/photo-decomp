! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
! 1d grid specified in json config file
module tuvx_grid_from_config

  use musica_constants, only : dk => musica_dk, ik => musica_ik, lk => musica_lk
  use tuvx_grid,     only : grid_t

  implicit none

  public :: fromConfig_t

  type, extends(grid_t) :: fromConfig_t
  contains
  end type fromConfig_t

  !> Constructor
  interface fromConfig_t
    module procedure constructor
  end interface fromConfig_t

contains
  !> Initialize grid
  function constructor( grid_config ) result ( this )
      
    use musica_config, only : config_t
    use musica_string, only : string_t
    use musica_assert, only : die_msg

    !> Arguments
    type(fromConfig_t), pointer :: this
    type(config_t), intent(inout)      :: grid_config

    !> Local variables
    character(len=*), parameter :: Iam = 'From config grid initialize: '

    allocate( this )
 
    !> Get the handle
    call grid_config%get( 'Handle', this%handle_, Iam, default = 'None' )

    !> Get values from config file
    call grid_config%get( "Values", this%edge_, Iam )

    this%ncells_ = size(this%edge_) - 1_ik
    this%mid_ = .5_dk &
                   *(this%edge_(1_ik:this%ncells_) + this%edge_(2_ik:this%ncells_+1_ik))
    this%delta_ = (this%edge_(2_ik:this%ncells_+1_ik) - this%edge_(1_ik:this%ncells_))

  end function constructor

end module tuvx_grid_from_config
