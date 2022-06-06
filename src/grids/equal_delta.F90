! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
! one dimension, equally spaced  grid type
module tuvx_grid_equal_delta

  use musica_constants, only : dk => musica_dk, ik => musica_ik, lk => musica_lk
  use tuvx_grid,     only : grid_t

  implicit none

  public :: equalDelta_t

  type, extends(grid_t) :: equalDelta_t
  contains
  end type equalDelta_t

  !> Constructor
  interface equalDelta_t
    module procedure constructor
  end interface equalDelta_t

contains
  !> Initialize grid
  function constructor( grid_config ) result ( this )
      
    use musica_config, only : config_t
    use musica_string, only : string_t

    !> arguments
    type(equalDelta_t), pointer :: this
    type(config_t), intent(inout)      :: grid_config
    !> local variables
    integer(ik) :: n
    real(dk)    :: Lower_val, Upper_val, Delta_val
    character(len=*), parameter :: Iam = 'EqualDelta grid initialize: '
    logical(lk) :: found

    allocate ( this )

    call grid_config%get( 'Grid begins at', Lower_val, Iam )
    call grid_config%get( 'Grid ends at', Upper_val, Iam )
    call grid_config%get( 'Grid cell delta', Delta_val, Iam )
    call grid_config%get( 'Handle', this%handle_, Iam, found = found )
    if( .not. found ) then
      this%handle_ = "None"
    endif

    this%ncells_ = int( (Upper_val - Lower_val)/Delta_val,kind=ik )
    if( mod((Upper_val - Lower_val),Delta_val ) /= 0._dk ) then
      this%ncells_ = this%ncells_ + 1
    endif
    allocate( this%mid_(this%ncells_) )
    allocate( this%delta_(this%ncells_) )
    allocate( this%edge_(this%ncells_+1_ik) )
    do n = 1,this%ncells_+1_ik
      this%edge_(n) = min( real((n - 1_ik),kind=dk)*Delta_val + Lower_val,Upper_val )
    enddo
    this%mid_(:) = .5_dk &
                   *(this%edge_(1_ik:this%ncells_) + this%edge_(2_ik:this%ncells_+1_ik))
    this%delta_(:) = (this%edge_(2_ik:this%ncells_+1_ik) - this%edge_(1_ik:this%ncells_))

  end function constructor

end module tuvx_grid_equal_delta
