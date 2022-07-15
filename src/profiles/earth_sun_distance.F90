! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> Earth-Sun distance profile type
module tuvx_profile_earth_sun_distance

  use musica_constants, only : &
    dk => musica_dk, ik => musica_ik, lk => musica_lk
  use musica_assert,    only : die_msg
  use tuvx_profile,     only : profile_t
  use tuvx_profile_utils, only : julian_day_of_year,  earth_sun_distance

  implicit none

  private
  public :: earth_sun_distance_t

  type, extends(profile_t) :: earth_sun_distance_t
  contains
    final     :: finalize
  end type earth_sun_distance_t

  !> Constructor
  interface earth_sun_distance_t
    module procedure constructor
  end interface earth_sun_distance_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize distance between sun, earth in AU
  function constructor( profile_config, grid_warehouse ) result ( this )

    use musica_config, only : config_t
    use musica_string, only : string_t
    use tuvx_grid,     only : grid_t
    use tuvx_grid_warehouse,  only : grid_warehouse_t

    !> Arguments
    type(earth_sun_distance_t), pointer   :: this
    type(config_t), intent(inout)         :: profile_config
    type(grid_warehouse_t), intent(inout) :: grid_warehouse

    !> Local variables
    real(dk), parameter :: NINETY  = 90._dk
    integer(ik) :: n, tNdx
    integer(ik) :: Year, Month, Day
    integer(ik) :: Jday
    real(dk)    :: tmzone, ut, soldst
    real(dk)    :: Lon, Lat
    character(len=*), parameter :: Iam = 'earth sun distance initialize: '
    class(grid_t), pointer :: timeGrid

    allocate ( this )

    timeGrid => grid_warehouse%get_grid( "time", "hours" )
    this%ncells_ = timeGrid%ncells_

    call profile_config%get( 'name', this%handle_, Iam, default='none' )
    call profile_config%get( 'units', this%units_, Iam )

    allocate( this%edge_val_(0) )

    !> Map solar zenith angle as function of time
    call profile_config%get( 'year', Year, Iam )
    call profile_config%get( 'month', Month, Iam )
    call profile_config%get( 'day', Day, Iam )
    call profile_config%get( 'time zone', tmzone, Iam, default=0.0_dk )
    call profile_config%get( 'longitude', Lon, Iam, default=0.0_dk )
    call profile_config%get( 'latitude', Lat, Iam, default=0.0_dk )

    Jday = julian_day_of_year(Year, Month, Day )

    do tNdx = 1_ik,this%ncells_+1_ik
      ut = timeGrid%edge_(tNdx) - tmzone
      soldst = earth_sun_distance(Year, Jday, ut )

      this%edge_val_  = [this%edge_val_,soldst]
    enddo

    this%mid_val_ = .5_dk * ( &
      this%edge_val_(1_ik:this%ncells_) + &
      this%edge_val_(2_ik:this%ncells_+1_ik) &
    )

    this%delta_val_ = (this%edge_val_(2_ik:this%ncells_+1_ik) - &
      this%edge_val_(1_ik:this%ncells_))

    deallocate( timeGrid )

  end function constructor


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine finalize( this )

    type(earth_sun_distance_t), intent(inout) :: this

    if( allocated( this%edge_val_ ) ) then
      deallocate( this%edge_val_ )
    endif
    if( allocated( this%mid_val_ ) ) then
      deallocate( this%mid_val_ )
    endif
    if( allocated( this%delta_val_ ) ) then
      deallocate( this%delta_val_ )
    endif

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_profile_earth_sun_distance
