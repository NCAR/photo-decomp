! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The tuvx_radiator warehouse module

!> The radiator_warehouse_t type and related functions
module tuvx_radiator_warehouse

  use musica_constants,                only : musica_dk
  use musica_iterator,                 only : iterator_t
  use musica_string,                   only : string_t
  use tuvx_radiator,                   only : radiator_ptr

  implicit none

  private
  public :: radiator_warehouse_t, warehouse_iterator_t

  !> Radiator warehouse
  type :: radiator_warehouse_t
    private
    !> Radiators
    type(radiator_ptr), allocatable :: radiators_(:)
    !> Radiator "handles"
    type(string_t),     allocatable :: handle_(:)
  contains
    !> @name Returns a pointer to a requested radiator
    !! @{
    procedure, private :: get_radiator_from_handle
    procedure, private :: get_radiator_from_iterator
    generic   :: get_radiator => get_radiator_from_handle, get_radiator_from_iterator
    !> Returns the index associated with a given radiator name
    procedure, public  :: get_radiator_ndx_from_handle
    !> @}
    !> Returns whether a radiator exists in the warehouse
    procedure :: in_warehouse
    !> Gets an iterator for the warehouse
    procedure :: get_iterator
    !> Cleans up memory
    final     :: finalize
  end type radiator_warehouse_t

  !>  Radiator warehouse iterator
  type :: warehouse_iterator_t
    !> Pointer to the radiator warehouse
    type(radiator_warehouse_t), pointer :: warehouse_ => null( )
    !> Current index in the data set
    integer :: id_ = 0
  contains
    !> Advances to the next key-value pair
    procedure :: next => iterator_next
    !> Resets the iterator
    procedure :: reset => iterator_reset
  end type warehouse_iterator_t

  !> radiator_warehouse_t constructor
  interface radiator_warehouse_t
    module procedure :: constructor
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructs radiator_warehouse_t abjects
  function constructor( config, grid_warehouse ) result( radiator_warehouse )

    use musica_assert,                 only : die_msg
    use musica_config,                 only : config_t
    use musica_iterator,               only : iterator_t
    use musica_string,                 only : string_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_radiator_factory,         only : radiator_builder

    !> Radiator configuration
    type(config_t), intent(inout)         :: config
    !> Grid warehouse
    type(grid_warehouse_t), intent(inout) :: grid_warehouse
    !> Radiator warehouse
    class(radiator_warehouse_t), pointer :: radiator_warehouse

    ! local variables
    character(len=*), parameter :: Iam = "Radiator warehouse constructor: "
    integer                     :: ndx
    type(config_t)              :: radiator_config
    class(iterator_t), pointer  :: iter
    type(radiator_ptr)          :: aRadiator

    allocate( radiator_warehouse )
    allocate( radiator_warehouse%radiators_(0) )
    allocate( radiator_warehouse%handle_(0) )

    iter => config%get_iterator()
    do while( iter%next() )
      call config%get( iter, radiator_config, Iam )

      ! build and store the radiator
      aRadiator%val_ => radiator_builder( radiator_config, grid_warehouse )
      radiator_warehouse%radiators_ =                                         &
          [ radiator_warehouse%radiators_, aRadiator ]
      radiator_warehouse%handle_ =                                            &
          [ radiator_warehouse%handle_, aRadiator%val_%handle_ ]
    end do
    deallocate( iter )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns a pointer to a requested radiator
  function get_radiator_from_handle( this, radiator_handle )                  &
      result( radiator )

    use musica_assert,                 only : assert_msg
    use musica_string,                 only : string_t
    use tuvx_radiator,                 only : radiator_t

    !> Radiator warehouse
    class(radiator_warehouse_t), intent(inout) :: this
    !> Name associated with requested radiator
    type(string_t),              intent(in)    :: radiator_handle
    !> Pointer to requested radiator
    class(radiator_t),           pointer       :: radiator

    ! Local variables
    character(len=*), parameter :: Iam = 'radiator warehouse get radiator'
    integer :: ndx
    logical :: found

    found = .false.
    do ndx = 1, size( this%handle_ )
      if( radiator_handle .eq. this%handle_( ndx ) ) then
        found = .true.
        exit
      endif
    end do
    call assert_msg( 200578546, found,                                        &
                     "Invalid radiator handle: '"//                           &
                     radiator_handle%to_char()//"'" )
    radiator => this%radiators_(ndx)%val_

  end function get_radiator_from_handle

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the index associated with a given radiator
  !!
  !! If the radiator is not found, returns -1
  function get_radiator_ndx_from_handle( this, radiator_handle )              &
      result( index )

    use tuvx_radiator,                 only : radiator_t
    use musica_string,                 only : string_t

    !> Radiator warehouse
    class(radiator_warehouse_t), intent(inout) :: this
    !> Requested radiator name
    type(string_t),              intent(in)    :: radiator_handle
    !> Index of requested radiator in warehouse
    integer                                    :: index

    ! Local variables
    character(len=*), parameter :: Iam =                                      &
        'radiator warehouse get radiator index'
    logical :: found

    found = .false.
    do index = 1, size( this%handle_ )
      if( radiator_handle .eq. this%handle_( index ) ) then
        found = .true.
        exit
      endif
    end do
    if( .not. found ) index = -1

  end function get_radiator_ndx_from_handle

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns a pointer to a radiator in the warehouse from an iterator
  function get_radiator_from_iterator( this, iterator ) result( radiator )

    use tuvx_radiator,                 only : radiator_t

    !> Radiator warehouse
    class(radiator_warehouse_t), intent(inout) :: this
    !> Radiator warehouse iterator
    type(warehouse_iterator_t),  intent(in)    :: iterator
    !> Pointer to requested radiator
    class(radiator_t),           pointer       :: radiator

    ! Local variables
    character(len=*), parameter :: Iam =                                      &
        'radiator warehouse get radiator from iterator'

    radiator => this%radiators_( iterator%id_ )%val_

  end function get_radiator_from_iterator

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns whether a radiator exists in the warehouse
  function in_warehouse( this, radiator_handle )

    use musica_string,      only : string_t
    use musica_assert,      only : die_msg

    !> Radiator warehouse
    class(radiator_warehouse_t), intent(inout) :: this
    !> Name associated with requested radiator
    type(string_t),              intent(in)    :: radiator_handle
    !> Flag indicating whether the radiator exists in the warehouse
    logical                                    :: in_warehouse

    ! Local variables
    character(len=*), parameter :: Iam = 'radiator in warehouse'
    integer :: ndx

    in_warehouse = .false.
    do ndx = 1, size( this%handle_ )
      if( radiator_handle == this%handle_( ndx ) ) then
        in_warehouse = .true.
        exit
      endif
    end do

  end function in_warehouse

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets an interator for the radiator warehouse
  function get_iterator( this )

    use musica_assert,                 only : assert

    !> Pointer to the iterator
    class(warehouse_iterator_t), pointer            :: get_iterator
    !> Radiator warehouse
    class(radiator_warehouse_t), intent(in), target :: this

    call assert( 753334333, allocated( this%radiators_ ) )
    allocate( warehouse_iterator_t :: get_iterator )
    select type( iter => get_iterator )
      type is( warehouse_iterator_t )
        iter%warehouse_ => this
    end select

  end function get_iterator

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Advances the iterator
  !!
  !! Returns false if the end of the collection has been reached
  function iterator_next( this ) result( continue )

    !> Iterator
    class(warehouse_iterator_t), intent(inout) :: this

    logical :: continue

    this%id_ = this%id_ + 1
    continue = this%id_ <= size( this%warehouse_%radiators_ )

  end function iterator_next

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Resets the iterator
  subroutine iterator_reset( this )

    !> Iterator
    class(warehouse_iterator_t), intent(inout) :: this

    this%id_ = 0

  end subroutine iterator_reset

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalize the radiator warehouse
  subroutine finalize( this )

    !> Radiator warehouse
    type(radiator_warehouse_t), intent(inout) :: this

    integer :: ndx
    character(len=*), parameter :: Iam = 'radiator_warehouse finalize: '

    if( allocated( this%radiators_ ) ) then
      do ndx = 1, size( this%radiators_ )
        if( associated( this%radiators_( ndx )%val_ ) ) then
          deallocate( this%radiators_( ndx )%val_ )
        end if
      end do
      deallocate( this%radiators_ )
    endif

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_radiator_warehouse
