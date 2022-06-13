! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> O2 profile from csv file type
module tuvx_profile_o2

  use musica_constants,  only : &
    dk => musica_dk, ik => musica_ik, lk => musica_lk
  use tuvx_profile,      only : profile_t

  implicit none

  public :: o2_from_csv_file_t

  type, extends(profile_t) :: o2_from_csv_file_t
  contains
    final     :: finalize
  end type o2_from_csv_file_t

  !> Constructor
  interface o2_from_csv_file_t
    module procedure constructor
  end interface o2_from_csv_file_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize grid
  function constructor( profile_config, grid_warehouse ) result( this )

    use musica_config, only : config_t
    use musica_string, only : string_t
    use musica_assert, only : die_msg
    use tuvx_grid,     only : grid_t
    use tuvx_grid_warehouse, only : grid_warehouse_t
    use tuvx_interpolate

    type(config_t), intent(inout)         :: profile_config
    type(o2_from_csv_file_t), pointer     :: this
    type(grid_warehouse_t), intent(inout) :: grid_warehouse

    ! local variables
    integer(ik), parameter :: Ok = 0_ik
    integer(ik), parameter :: inUnit = 20_ik
    real(dk), parameter    :: o2Vmr = .2095_dk
    real(dk), parameter    :: km2cm = 1.e5_dk

    character(len=*), parameter :: Iam = 'From_csv_file profile initialize: '

    integer(ik) :: istat, nData
    real(dk)    :: zd, Value
    real(dk)    :: exo_layer_dens
    logical(lk) :: found
    character(len=132) :: InputLine
    type(string_t)     :: Filespec, Interpolator
    type(string_t)     :: Handle
    real(dk), allocatable :: zdata(:)
    real(dk), allocatable :: profile(:)
    real(dk), allocatable :: airlog(:)
    class(grid_t), pointer :: zGrid
    class(abs_interpolator_t), pointer :: theInterpolator

    allocate( this )

    ! Get the configuration settings
    call profile_config%get( 'Filespec', Filespec, Iam )
    call profile_config%get( 'Handle', this%handle_, Iam, &
      default = 'None' )
    call profile_config%get( 'Interpolator', Interpolator, Iam, &
    default = 'interp1' )
    call profile_config%get( 'Scale heigth', this%hscale_, Iam, &
      default = 8.01_dk )

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
        exit
      elseif( verify( InputLine(1:1),'#!$%*' ) /= 0 ) then
        exit
      endif
    enddo

    allocate( profile(0) )
    allocate( zdata(0) )
    ! Read the data
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

    Handle = 'Vertical Z'
    zGrid => grid_warehouse%get_grid( Handle )
    this%ncells_ = zGrid%ncells_

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

    nData = size( zdata )
    zdata(nData) = zdata(nData) + .001_dk
    airlog = log( profile )
    this%edge_val_ = theInterpolator%interpolate( zGrid%edge_, zdata,airlog )
    this%edge_val_ = exp( this%edge_val_ )
    this%edge_val_ = o2Vmr * this%edge_val_

    this%mid_val_ = .5_dk * ( &
      this%edge_val_(1_ik:this%ncells_) + &
      this%edge_val_(2_ik:this%ncells_+1_ik) &
    )

    this%delta_val_ = (this%edge_val_(2_ik:this%ncells_+1_ik) - &
      this%edge_val_(1_ik:this%ncells_))

    this%layer_dens_ = zGrid%delta_ * &
      sqrt( this%edge_val_(1_ik:this%ncells_) ) * &
      sqrt( this%edge_val_(2_ik:this%ncells_+1_ik) ) * km2cm

    exo_layer_dens = this%edge_val_(this%ncells_+1_ik) * this%hscale_ * km2cm

    this%exo_layer_dens_ = [ this%layer_dens_, exo_layer_dens ]

    this%layer_dens_(this%ncells_) = this%layer_dens_(this%ncells_) + &
      exo_layer_dens

    deallocate( zGrid )
    deallocate( theInterpolator )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine finalize( this )

    type(o2_from_csv_file_t), intent(inout) :: this

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
    if( allocated( this%exo_layer_dens_ ) ) then
      deallocate( this%exo_layer_dens_ )
    endif

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_profile_o2
