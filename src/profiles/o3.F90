! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> O3 profile type
module tuvx_profile_o3

  use musica_constants,  only : &
    dk => musica_dk, ik => musica_ik, lk => musica_lk
  use tuvx_profile,      only : profile_t

  implicit none

  public :: o3_from_csv_file_t

  type, extends(profile_t) :: o3_from_csv_file_t
  contains
    final     :: finalize
  end type o3_from_csv_file_t

  !> Constructor
  interface o3_from_csv_file_t
    module procedure constructor
  end interface o3_from_csv_file_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize grid
  function constructor( profile_config, grid_warehouse ) result( this )

    use musica_config, only : config_t
    use musica_string, only : string_t
    use musica_assert, only : die_msg
    use tuvx_grid,     only : grid_t
    use tuvx_grid_warehouse,  only : grid_warehouse_t
    use tuvx_interpolate

    type(config_t), intent(inout)         :: profile_config
    type(o3_from_csv_file_t), pointer     :: this
    type(grid_warehouse_t), intent(inout) :: grid_warehouse

    ! local variables
    integer(ik), parameter :: Ok = 0_ik
    integer(ik), parameter :: inUnit = 20_ik
    real(dk), parameter    :: km2cm = 1.e5_dk
    character(len=*), parameter :: Iam = &
      'O3_from_csv_file profile initialize: '

    integer(ik) :: istat, m
    real(dk)    :: zd, Value, zTop
    real(dk)    :: rfact
    real(dk)    :: Scale2DU, InputDU, O3ScaleFactor
    logical(lk) :: found
    character(len=132) :: InputLine
    type(string_t)     :: Filespec, Interpolator
    real(dk), allocatable :: zdata(:)
    real(dk), allocatable :: profile(:)
    class(grid_t), pointer :: zGrid
    class(abs_interpolator_t), pointer :: theInterpolator

    allocate( this )

    ! Get the configuration settings
    call profile_config%get( 'file path', Filespec, Iam )
    call profile_config%get( 'name', this%handle_, Iam, &
      default = 'none' )
    call profile_config%get( 'units', this%units_, Iam )
    call profile_config%get( 'interpolator', Interpolator, Iam, &
      default = 'interp1' )
    call profile_config%get( 'scale heigth', this%hscale_, Iam, &
      default = 4.5_dk )
    call profile_config%get( 'reference column', Scale2DU, Iam, &
      default = 300._dk )

    ! Does input grid file exist?
    inquire( file=Filespec%to_char(), exist=found )
    if( .not. found ) then
      call die_msg( 560768215, "File " // Filespec%to_char() // " not found" )
    endif

    open(unit=inUnit,file=Filespec%to_char(),iostat=istat)
    if( istat /= Ok ) then
      call die_msg( 560768231, "Error opening " // Filespec%to_char() )
    endif

    ! Skip the header
    do
      read(inUnit,'(a)',iostat=istat) InputLine
      if( istat /= Ok ) then
        call die_msg( 560768227, "Error reading " // Filespec%to_char() )
      elseif( verify( InputLine(1:1),'#!$%*' ) /= 0 ) then
        exit
      endif
    enddo

    allocate( profile(0) )
    allocate( zdata(0) )
    !> Read the data
    do
      read(InputLine,*,iostat=istat) zd, Value
      if( istat /= Ok ) then
        call die_msg( 560768229, "Invalid data format in " &
          // Filespec%to_char() )
      endif
      profile = [profile,Value]
      zdata = [zdata,zd]
      read(inUnit,'(a)',iostat=istat) InputLine
      if( istat /= Ok ) then
        exit
      endif
    enddo

    close(unit=inUnit)

    zGrid => grid_warehouse%get_grid( "height", "km" )
    this%ncells_ = zGrid%ncells_

    ! Set o3 concentration if data ztop < mdl top
    ztop = zGrid%edge_(this%ncells_+1)
    if( this%hscale_ /= 0.0_dk ) then
      rfact = exp( -1.0_dk/this%hscale_ )
    else
      rfact = 0.0_dk
    endif
    m = size(profile)
    do
      zdata   = [zdata,zdata(m) + 1.0_dk]
      profile = [profile,profile(m) * rfact]
      m = m + 1
      if(zdata(m) > zTop ) then
        exit
      endif
    enddo

    ! assign actual interpolator for this profile
    select case( Interpolator%to_char() )
      case( 'interp1' )
        allocate( interp1_t :: theInterpolator )
      case( 'interp2' )
        allocate( interp2_t :: theInterpolator )
      case( 'interp3' )
        allocate( interp3_t :: theInterpolator )
      case( 'interp4' )
        allocate( interp4_t :: theInterpolator )
      case default
        call die_msg( 560768275, "interpolator " // Interpolator%to_char() &
          // " not a valid selection" )
    end select

    this%edge_val_ = theInterpolator%interpolate( zGrid%edge_, zdata,profile )

    this%mid_val_ = .5_dk * ( &
      this%edge_val_(1:this%ncells_) + &
      this%edge_val_(2:this%ncells_+1) &
    )

    this%delta_val_ = this%edge_val_(2:this%ncells_+1) - &
     this%edge_val_(1:this%ncells_)

    this%layer_dens_ = zGrid%delta_ * this%mid_val_ * km2cm

    this%layer_dens_(this%ncells_) = this%layer_dens_(this%ncells_) + &
      this%edge_val_(this%ncells_+1) * this%hscale_ * km2cm

    if( Scale2DU /= 1.0_dk ) then
      InputDU = sum( this%layer_dens_ ) / 2.687e16_dk
      O3ScaleFactor = Scale2DU/InputDU

      if( O3ScaleFactor /= 1.0_dk ) then
        this%edge_val_ = O3ScaleFactor * this%edge_val_

        this%mid_val_ = .5_dk * ( &
          this%edge_val_(1:this%ncells_) + &
          this%edge_val_(2:this%ncells_+1) &
        )

        this%delta_val_ = this%edge_val_(2:this%ncells_+1) - &
          this%edge_val_(1:this%ncells_)

        this%layer_dens_ = zGrid%delta_ * this%mid_val_ * km2cm

        this%layer_dens_(this%ncells_) = this%layer_dens_(this%ncells_) + &
          this%edge_val_(this%ncells_+1) * this%hscale_ * km2cm

      endif
    endif

    deallocate( zGrid )
    deallocate( theInterpolator )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine finalize( this )

    type(o3_from_csv_file_t), intent(inout) :: this

    if( allocated( this%edge_val_ ) ) then
      deallocate( this%edge_val_ )
    endif
    if( allocated( this%mid_val_ ) ) then
      deallocate( this%mid_val_ )
    endif
    if( allocated( this%delta_val_ ) ) then
      deallocate( this%delta_val_ )
    endif
    if( allocated( this%layer_dens_ ) ) then
      deallocate( this%layer_dens_ )
    endif

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_profile_o3
