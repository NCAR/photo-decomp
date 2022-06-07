! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The tuvx_profile_warehouse module

!> The profile warehouse type and related functions
module tuvx_profile_warehouse

  use tuvx_profile, only : grid_ptr

  implicit none

  private
  public :: profile_warehouse_t

  !> profile warehouse type
  type :: profile_warehouse_t
    private
    !> profile objects
    type(grid_ptr), allocatable :: profile_objs_(:)
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
  function constructor( config, gridwarehouse ) result( profile_warehouse_obj )

    use musica_config,                 only : config_t
    use musica_iterator,               only : iterator_t
    use musica_string,                 only : string_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile_factory,     only : profile_builder

    !> Arguments
    !> profile configuration data
    type(config_t), intent(inout) :: config
    type(grid_warehouse_t), intent(inout) :: gridwarehouse

    !> New profile_warehouse_obj
    class(profile_warehouse_t), pointer :: profile_warehouse_obj

    !> local variables
    character(len=*), parameter :: Iam = "profile warehouse constructor: "
    type(config_t)              :: profile_set, profile_config
    class(iterator_t), pointer  :: iter
    class(profile_warehouse_t), pointer :: profile_warehouse_ptr
    type(grid_ptr)            :: profile_obj
    character(len=32)           :: keychar
    type(string_t)              :: aswkey

    write(*,*) Iam // 'entering'

    allocate( profile_warehouse_obj )

    associate(new_obj=>profile_warehouse_obj)

    allocate( new_obj%profile_objs_(0) )

    call config%get( 'Profiles', profile_set, Iam )
    iter => profile_set%get_iterator()
!-----------------------------------------------------------------------------
!> iterate over profiles
!-----------------------------------------------------------------------------
    do while( iter%next() )
      keychar = profile_set%key(iter)
      aswkey  = keychar 
      write(*,*) ' '
      write(*,*) Iam,'key = ',trim(keychar)
      call profile_set%get( iter, profile_config, Iam )
      call profile_config%add( 'Handle', aswkey, Iam )
!-----------------------------------------------------------------------------
!> Build profile objects
!-----------------------------------------------------------------------------
      profile_obj%ptr_ => profile_builder( profile_config, gridwarehouse )
      new_obj%profile_objs_ = [new_obj%profile_objs_,profile_obj]
    end do

    deallocate( iter )

    write(*,*) ' '
    write(*,'(a,''There are '',i3,'' profile objects'')') Iam,size(new_obj%profile_objs_)

    end associate

    write(*,*) Iam // 'exiting'

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Get copy of a profile object
  function get_profile( this, profile_handle ) result( profile_ptr )

    use musica_string,     only : string_t
    use musica_constants,  only : lk => musica_lk, ik => musica_ik
    use musica_assert,     only : die_msg
    use tuvx_profile, only : profile_t

    !> Arguments
    class(profile_warehouse_t), intent(inout) :: this
    type(string_t), intent(in)                     :: profile_handle

    class(profile_t), pointer          :: profile_ptr

    !> Local variables
    character(len=*), parameter :: Iam = 'profile warehouse get_profile: '
    integer(ik) :: ndx
    logical(lk) :: found

    write(*,*) ' '
    write(*,*) Iam,'entering'

    found = .false._lk
    do ndx = 1,size(this%profile_objs_)
      if( profile_handle .eq. this%profile_objs_(ndx)%ptr_%handle_ ) then
        found = .true._lk
        exit
      endif
    end do

    if( found ) then
      allocate( profile_ptr, source = this%profile_objs_(ndx)%ptr_ )
    else
      call die_msg( 460768214, "Invalid profile handle: '"// profile_handle%to_char()//"'" )
    endif

    write(*,*) Iam,'exiting'

  end function get_profile

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalize profile warehouse
  subroutine finalize( this )

    use musica_constants, only : ik => musica_ik

    !> Arguments
    type(profile_warehouse_t), intent(inout) :: this

    !> Local variables
    integer(kind=ik) :: ndx
    character(len=*), parameter :: Iam = 'profile warehouse finalize: '

    write(*,*) Iam,'entering'

    if( allocated( this%profile_objs_ ) ) then
      deallocate( this%profile_objs_ )
    endif

    write(*,*) Iam,'exiting'

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_profile_warehouse
