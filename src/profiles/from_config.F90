! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
! profile specified in json config file
module tuvx_profile_from_config

  use musica_constants, only : dk => musica_dk, ik => musica_ik, lk => musica_lk
  use tuvx_profile,     only : profile_t

  implicit none

  private
  public :: fromConfig_t

  type, extends(profile_t) :: fromConfig_t
  contains
  end type fromConfig_t

  !> Constructor
  interface fromConfig_t
    module procedure constructor
  end interface fromConfig_t

contains
  !> Initialize grid
  function constructor( profile_config, gridWareHouse ) result( this )
      
    use musica_config, only : config_t
    use musica_string, only : string_t
    use musica_assert, only : die_msg
    use tuvx_grid,  only : grid_t
    use tuvx_grid_warehouse,  only : grid_warehouse_t

    !> Arguments
    type(fromConfig_t), pointer :: this
    type(config_t), intent(inout)      :: profile_config
    type(grid_warehouse_t), intent(inout) :: gridWareHouse

    !> Local variables
    character(len=*), parameter :: Iam = 'From config profile initialize: '
    integer(ik)                 :: ndx
    real(dk)                    :: uniformValue
    logical(lk)                 :: found
    type(string_t)              :: gridHandle
    class(grid_t), pointer :: theGrid

    allocate( this )
 
    !> Get the handle
    call profile_config%get( 'Handle', this%handle_, Iam, default = 'None' )

    !> Get values from config file
    call profile_config%get( "Values", this%edge_val_, Iam, found=found )
    if( .not. found ) then
      call profile_config%get( "Uniform value", uniformValue, Iam, found=found )
      if( found ) then
        call profile_config%get( "Grid", gridHandle, Iam, found=found )
        if( found ) then
          theGrid => gridWareHouse%get_grid( gridHandle )
          this%edge_val_ = (/ (uniformValue,ndx=1,theGrid%ncells_+1_ik) /)
        else
          call die_msg( 123456,"Grid " // gridHandle%to_char() // " not in grid warehouse" )
        endif
      else
        call die_msg( 123457,"Neither 'Values' or 'Uniform value' keyword specified" )
      endif
    endif

    this%ncells_ = size(this%edge_val_) - 1_ik
    this%mid_val_ = .5_dk &
                   *(this%edge_val_(1_ik:this%ncells_) + this%edge_val_(2_ik:this%ncells_+1_ik))
    this%delta_val_ = (this%edge_val_(2_ik:this%ncells_+1_ik) - this%edge_val_(1_ik:this%ncells_))

    deallocate( theGrid )

  end function constructor

end module tuvx_profile_from_config
