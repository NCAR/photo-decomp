! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0

module tuvx_grid_warehouse
! Grid Warehouse
! ^^^^^^^^^^^^^^

  use tuvx_grid, only : grid_ptr

  implicit none

  private
  public :: grid_warehouse_t

  !> Grid warehouse type
  type :: grid_warehouse_t
    private
    !> grid objects
    type(grid_ptr), allocatable :: grid_objs_(:)
  contains
    !> get a copy of a grid object
    procedure, private :: get_grid_char, get_grid_string
    generic :: get_grid => get_grid_char, get_grid_string
    !> Finalize the object
    final :: finalize
  end type grid_warehouse_t

  !> Grid warehouse_t constructor
  interface grid_warehouse_t
    module procedure :: constructor
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Grid warehouse constructor
  function constructor( config ) result( grid_warehouse )

    use musica_config,                 only : config_t
    use musica_iterator,               only : iterator_t
    use musica_string,                 only : string_t
    use tuvx_grid_factory,             only : grid_builder

    !> Arguments
    !> grid configuration data
    type(config_t), intent(inout) :: config

    !> New grid_warehouse
    class(grid_warehouse_t), pointer :: grid_warehouse

    !> local variables
    character(len=*), parameter :: Iam = "Grid warehouse constructor: "
    type(config_t)              :: grid_config
    class(iterator_t), pointer  :: iter
    class(grid_warehouse_t), pointer :: grid_warehouse_ptr
    type(grid_ptr)              :: grid_obj
    character(len=32)           :: keychar
    type(string_t)              :: aswkey

    allocate( grid_warehouse )
    allocate( grid_warehouse%grid_objs_(0) )

    ! iterate over grids
    iter => config%get_iterator()
    do while( iter%next() )
      keychar = config%key(iter)
      aswkey  = keychar
      call config%get( iter, grid_config, Iam )
      call grid_config%add( 'name', aswkey, Iam )

      ! Build grid objects
      grid_obj%val_ => grid_builder( grid_config )
      grid_warehouse%grid_objs_ = [ grid_warehouse%grid_objs_, grid_obj ]
    end do
    deallocate( iter )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Get copy of a grid object
  function get_grid_char( this, name, units ) result( grid_ptr )

    use musica_assert,                 only : assert_msg
    use musica_string,                 only : string_t
    use tuvx_grid,                     only : grid_t

    class(grid_warehouse_t), intent(inout) :: this
    character(len=*),        intent(in)    :: name
    character(len=*),        intent(in)    :: units
    class(grid_t), pointer                 :: grid_ptr

    integer :: ndx
    logical :: found

    found = .false.
    do ndx = 1, size( this%grid_objs_ )
      if( name .eq. this%grid_objs_( ndx )%val_%handle_ ) then
        found = .true.
        exit
      endif
    end do
    call assert_msg( 345804219, found, "Invalid grid name: '"//name//"'" )
    call assert_msg( 509243577,                                               &
                     units .eq. this%grid_objs_( ndx )%val_%units( ),         &
                     "Grid '"//name//"' has units of '"//                     &
                     this%grid_objs_( ndx )%val_%units( )//"' not '"//        &
                     units//"' as requested." )
    allocate( grid_ptr, source = this%grid_objs_( ndx )%val_ )

  end function get_grid_char

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Get a copy of a grid object
  function get_grid_string( this, name, units ) result( grid_ptr )

    use musica_string,                 only : string_t
    use tuvx_grid,                     only : grid_t

    class(grid_warehouse_t), intent(inout) :: this
    type(string_t),          intent(in)    :: name
    type(string_t),          intent(in)    :: units
    class(grid_t), pointer                 :: grid_ptr

    grid_ptr => this%get_grid_char( name%to_char( ), units%to_char( ) )

  end function get_grid_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalize grid warehouse
  subroutine finalize( this )

    use musica_constants, only : ik => musica_ik

    !> Arguments
    type(grid_warehouse_t), intent(inout) :: this

    integer(kind=ik) :: ndx

    if( allocated( this%grid_objs_ ) ) then
      do ndx = 1, size( this%grid_objs_ )
        if( associated( this%grid_objs_( ndx )%val_ ) ) then
          deallocate( this%grid_objs_( ndx )%val_ )
        end if
      end do
      deallocate( this%grid_objs_ )
    endif

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_grid_warehouse
