! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
! one dimension, equally spaced  grid type
module tuvx_grid_from_csv_file

  use musica_constants, only : &
    dk => musica_dk, ik => musica_ik, lk => musica_lk
  use tuvx_grid,        only : grid_t

  implicit none

  public :: from_csv_file_t

  type, extends(grid_t) :: from_csv_file_t
  contains
  end type from_csv_file_t

  !> Constructor
  interface from_csv_file_t
    module procedure constructor
  end interface from_csv_file_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize grid
  function constructor( grid_config ) result ( this )

    use musica_config, only : config_t
    use musica_string, only : string_t
    use musica_assert, only : die_msg

    !> arguments
    type(config_t), intent(inout) :: grid_config

    !> local variables
    integer(ik), parameter :: Ok = 0_ik
    integer(ik), parameter :: inUnit = 20_ik
    character(len=*), parameter :: Iam = 'From_csv_file grid initialize: '
    type(from_csv_file_t), pointer :: this
 
    integer(ik) :: istat
    real(dk)    :: Value
    logical(lk) :: found
    character(len=132) :: InputLine
    type(string_t)     :: Filespec
    

    allocate( this )

    call grid_config%get( 'Filespec', Filespec, Iam )
    call grid_config%get( 'Handle', this%handle_, Iam, default = 'None' )

    inquire( file=Filespec%to_char(), exist=found )
    if( .not. found ) then
      call die_msg( 560768215, "File " // Filespec%to_char() // " not found" )
    endif

    open(unit=inUnit,file=Filespec%to_char(),iostat=istat)
    if( istat /= Ok ) then
        call die_msg( 560768225, "Error reading " // Filespec%to_char() )
    endif

    ! The first line of the file contains the number of grid cells, 
    ! throw it away
    read(inUnit,*,iostat=istat) InputLine
    if( istat /= Ok ) then
      call die_msg( 560768226, "Error reading " // Filespec%to_char() )
    endif

    !> Now read the gride edge boundaries until the end of the file
    allocate( this%edge_(0) )
    do
      read(inUnit,*,iostat=istat) Value
      if( istat /= Ok ) then
        exit
      endif
      this%edge_ = [this%edge_,Value]
    enddo

    this%ncells_ = size(this%edge_)-1_ik
    allocate( this%mid_(this%ncells_) )
    allocate( this%delta_(this%ncells_) )
    this%mid_(:) = .5_dk * &
      (this%edge_(1_ik:this%ncells_) + this%edge_(2_ik:this%ncells_+1_ik))
    this%delta_(:) = &
      this%edge_(2_ik:this%ncells_+1_ik) - this%edge_(1_ik:this%ncells_)

    close(unit=inUnit)
  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_grid_from_csv_file
