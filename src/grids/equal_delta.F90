! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0

module tuvx_grid_equal_delta
!
! Grid from equal delta
! ^^^^^^^^^^^^^^^^^^^^^
! one dimension, equally spaced  grid type
!

  use musica_constants, only : &
    dk => musica_dk, ik => musica_ik, lk => musica_lk
  use tuvx_grid,        only : grid_t

  implicit none

  public :: equal_delta_t

  type, extends(grid_t) :: equal_delta_t
  contains
  end type equal_delta_t

  !> Constructor
  interface equal_delta_t
    module procedure constructor
  end interface equal_delta_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function constructor( grid_config ) result ( this )
    ! Initialize grid
      
    use musica_config, only : config_t
    use musica_string, only : string_t

    !> arguments
    type(config_t), intent(inout) :: grid_config

    !> local variables
    integer(ik) :: n
    logical(lk) :: found
    real(dk)    :: Lower_val, Upper_val, Delta_val
    character(len=*), parameter :: Iam = 'EqualDelta grid initialize: '
    type(equal_delta_t), pointer  :: this

    allocate ( this )

    call grid_config%get( 'begins at', Lower_val, Iam )
    call grid_config%get( 'ends at', Upper_val, Iam )
    call grid_config%get( 'cell delta', Delta_val, Iam )
    call grid_config%get( 'name', this%handle_, Iam, default = "none" )
    call grid_config%get( 'units', this%units_, Iam )

    this%ncells_ = int( (Upper_val - Lower_val)/Delta_val,kind=ik )
    if( mod((Upper_val - Lower_val),Delta_val ) /= 0._dk ) then
      this%ncells_ = this%ncells_ + 1
    endif
    allocate( this%mid_(this%ncells_) )
    allocate( this%delta_(this%ncells_) )
    allocate( this%edge_(this%ncells_+1_ik) )
    do n = 1,this%ncells_+1_ik
      this%edge_(n) = &
        min( real((n - 1_ik), kind=dk) * Delta_val + Lower_val, Upper_val )
    enddo
    this%mid_(:) = .5_dk * &
      (this%edge_(1_ik:this%ncells_) + this%edge_(2_ik:this%ncells_+1_ik))
    this%delta_(:) = &
      this%edge_(2_ik:this%ncells_+1_ik) - this%edge_(1_ik:this%ncells_)

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_grid_equal_delta
