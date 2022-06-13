! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The tuvx_profile_warehouse module

!> The profile warehouse type and related functions
module tuvx_profile_warehouse

  use tuvx_profile, only : profile_ptr

  implicit none

  private
  public :: profile_warehouse_t

  !> profile warehouse type
  type :: profile_warehouse_t
    private
    !> profile objects
    type(profile_ptr), allocatable :: profile_objs_(:)
  contains
    !> get a copy of a profile object
    procedure :: get_profile
    !> Finalize the object
    final :: finalize
  end type profile_warehouse_t

  !> Grid warehouse_t constructor
  interface profile_warehouse_t
    module procedure :: constructor
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> profile warehouse constructor
  function constructor( config, grid_warehouse) &
    result( profile_warehouse )

    use musica_config,        only : config_t
    use musica_iterator,      only : iterator_t
    use musica_string,        only : string_t
    use tuvx_grid_warehouse,  only : grid_warehouse_t
    use tuvx_profile_factory, only : profile_builder

    !> profile configuration data
    type(config_t),             intent(inout) :: config
    type(grid_warehouse_t),     intent(inout) :: grid_warehouse
    !> New profile_warehouse_obj
    class(profile_warehouse_t), pointer       :: profile_warehouse

    ! local variables
    type(config_t)              :: profile_config
    class(iterator_t), pointer  :: iter
    type(profile_ptr)           :: profile_obj
    character(len=32)           :: keychar
    type(string_t)              :: aswkey
    character(len=*), parameter :: Iam = "profile warehouse constructor: "
    class(profile_warehouse_t), pointer :: profile_warehouse_ptr

    allocate( profile_warehouse )
    allocate( profile_warehouse%profile_objs_(0) )

    ! iterate over profiles
    iter => config%get_iterator( )
    do while( iter%next( ) )
      keychar = config%key( iter )
      aswkey  = keychar
      call config%get( iter, profile_config, Iam )
      call profile_config%add( 'Handle', aswkey, Iam )

      ! Build profile objects
      profile_obj%val_ => profile_builder( profile_config, grid_warehouse )
      profile_warehouse%profile_objs_ =                                       &
          [ profile_warehouse%profile_objs_, profile_obj ]
    end do

    deallocate( iter )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Get copy of a profile object
  function get_profile( this, profile_handle ) result( profile_ptr )

    use musica_string,     only : string_t
    use musica_constants,  only : lk => musica_lk, ik => musica_ik
    use musica_assert,     only : die_msg
    use tuvx_profile,      only : profile_t

    class(profile_warehouse_t), intent(inout) :: this
    type(string_t),             intent(in)    :: profile_handle
    class(profile_t),           pointer       :: profile_ptr

    ! Local variables
    character(len=*), parameter :: Iam = 'profile warehouse get_profile: '
    integer(ik) :: ndx
    logical(lk) :: found

    found = .false._lk
    do ndx = 1, size( this%profile_objs_ )
      if( profile_handle .eq. this%profile_objs_(ndx)%val_%handle_ ) then
        found = .true._lk
        exit
      endif
    end do

    if( .not. found ) then
      call die_msg( 460768214, "Invalid profile handle: '"// &
        profile_handle%to_char()//"'" )
    endif

    allocate( profile_ptr, source = this%profile_objs_(ndx)%val_ )

  end function get_profile

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalize profile warehouse
  subroutine finalize( this )

    use musica_constants, only : ik => musica_ik

    type(profile_warehouse_t), intent(inout) :: this

    ! Local variables
    integer(kind=ik) :: ndx
    character(len=*), parameter :: Iam = 'profile warehouse finalize: '

    if( allocated( this%profile_objs_ ) ) then
      do ndx = 1, size( this%profile_objs_ )
        if( associated( this%profile_objs_( ndx )%val_ ) ) then
          deallocate( this%profile_objs_( ndx )%val_ )
        end if
      end do
      deallocate( this%profile_objs_ )
    endif

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_profile_warehouse
