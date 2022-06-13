! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> Solar zenith angle from time type
module tuvx_profile_solar_zenith_angle

  use musica_constants,   only : &
    dk => musica_dk, ik => musica_ik, lk => musica_lk
  use tuvx_profile,       only : profile_t
  use musica_assert,      only : die_msg
  use tuvx_profile_utils, only : julian_day_of_year, solar_zenith_angle

  implicit none

  public :: sza_from_time_t

  type, extends(profile_t) :: sza_from_time_t
  contains
    final     :: finalize
  end type sza_from_time_t

  !> Constructor
  interface sza_from_time_t
    module procedure constructor
  end interface sza_from_time_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize solar zenith angle profile from time
  function constructor( profile_config, grid_warehouse ) result( this )

    use musica_config, only : config_t
    use musica_string, only : string_t
    use tuvx_grid,  only : grid_t
    use tuvx_grid_warehouse,  only : grid_warehouse_t

    !> Arguments
    type(sza_from_time_t), pointer :: this
    type(config_t), intent(inout)         :: profile_config
    type(grid_warehouse_t), intent(inout) :: grid_warehouse

    !> Local variables
    real(dk), parameter :: NINETY  = 90._dk
    integer(ik) :: n, tNdx
    integer(ik) :: Year, Month, Day
    integer(ik) :: Jday
    real(dk)    :: tmzone, ut, solarElevation
    real(dk)    :: Lon, Lat
    character(len=*), parameter :: Iam = 'sza from time initialize: '
    type(string_t) :: Handle
    class(grid_t), pointer :: timeGrid

    allocate( this )

    Handle = 'Time, hrs'
    timeGrid => grid_warehouse%get_grid( Handle )
    this%ncells_ = timeGrid%ncells_

    call profile_config%get( 'Handle', this%handle_, Iam, default='None' )

    allocate( this%edge_val_(0) )

    ! Map solar zenith angle as function of time
    call profile_config%get( 'Year', Year, Iam, default=2002 )
    call profile_config%get( 'Month', Month, Iam, default=3 )
    call profile_config%get( 'Day', Day, Iam, default=21 )
    call profile_config%get( 'TimeZone', tmzone, Iam, default=0.0_dk )
    call profile_config%get( 'Longitude', Lon, Iam, default=0.0_dk )
    call profile_config%get( 'Latitude', Lat, Iam, default=0.0_dk )

    Jday = julian_day_of_year(Year, Month, Day )

    do tNdx = 1_ik,this%ncells_+1_ik
      ut = timeGrid%edge_(tNdx) - tmzone
      solarElevation = solar_zenith_angle(Year, Jday, ut, Lat, Lon )
      this%edge_val_ = [this%edge_val_,NINETY - solarElevation]
    enddo

    this%mid_val_ = .5_dk * ( &
      this%edge_val_(1_ik:this%ncells_) + &
      this%edge_val_(2_ik:this%ncells_+1_ik) &
    )

    this%delta_val_ = this%edge_val_(2_ik:this%ncells_+1_ik) - &
      this%edge_val_(1_ik:this%ncells_)

    deallocate( timeGrid )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine finalize( this )

    type(sza_from_time_t), intent(inout) :: this

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

end module tuvx_profile_solar_zenith_angle
