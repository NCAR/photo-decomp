! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The tuvx_radXfer_xsect_warehouse module

!> The cross_section_warehouse_t type and related functions
!!
module tuvx_cross_section_warehouse

  use musica_constants,                only : musica_dk
  use musica_string,                   only : string_t
  use tuvx_cross_section,              only : cross_section_ptr

  implicit none

  private
  public :: cross_section_warehouse_t

  !> Radiative xfer cross section type
  type :: cross_section_warehouse_t
    private
    !> cross section calculators
    type(cross_section_ptr), allocatable :: cross_section_objs_(:)
    !> cross section "handle"
    type(string_t), allocatable          :: handles_(:)
  contains
    !> Get a copy of a specific radXfer cross section
    procedure :: get
    !> Finalize the object
    final :: finalize
  end type cross_section_warehouse_t

  !> cross_section_warehouse_t constructor
  interface cross_section_warehouse_t
    module procedure :: constructor
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructor of cross_section_warehouse_t objects
  function constructor( config, grid_warehouse, profile_warehouse )           &
      result( new_obj )

    use musica_assert,                 only : die_msg
    use musica_config,                 only : config_t
    use musica_iterator,               only : iterator_t
    use tuvx_cross_section_factory,    only : cross_section_builder
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile_warehouse,        only : Profile_warehouse_t

    !> New radiative cross section object
    class(cross_section_warehouse_t), pointer :: new_obj
    !> Cross section configuration data
    type(config_t),             intent(inout) :: config
    !> Grid warehouse
    type(grid_warehouse_t),     intent(inout) :: grid_warehouse
    !> Profile warehouse
    type(Profile_warehouse_t),  intent(inout) :: profile_warehouse

    ! local variables
    integer :: ndx
    character(len=*), parameter :: Iam = "Cross section constructor"
    type(config_t) :: cross_section_config
    class(iterator_t), pointer :: iter
    type(cross_section_ptr) :: cross_section_ptr
    character(len=32)           :: keychar
    type(string_t)              :: cross_section_name
    type(string_t), allocatable :: netcdfFiles(:)
    logical                     :: found

    allocate( new_obj )
    allocate( string_t :: new_obj%handles_(0) )
    allocate( new_obj%cross_section_objs_(0) )

    ! iterate over cross sections
    iter => config%get_iterator( )
    do while( iter%next( ) )
      call config%get( iter, cross_section_config, Iam )

      ! save the cross section name for lookups
      call cross_section_config%get( "name", cross_section_name, Iam )
      new_obj%handles_ = [ new_obj%handles_, cross_section_name ]

      ! build and store cross section object
      cross_section_ptr%val_ => cross_section_builder( cross_section_config,  &
                                           grid_warehouse, profile_warehouse )
      new_obj%cross_section_objs_ =                                         &
          [ new_obj%cross_section_objs_, cross_section_ptr ]
    end do
    deallocate( iter )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Get a copy of a specific radXfer cross section object
  function get( this, cross_section_name ) result( cross_section_ptr )

    use musica_assert,                 only : die_msg
    use musica_string,                 only : string_t
    use tuvx_cross_section,            only : cross_section_t

    !> Pointer to a copy of the requested cross section
    class(cross_section_t),           pointer       :: cross_section_ptr
    !> Cross section warehouse
    class(cross_section_warehouse_t), intent(inout) :: this
    !> Name of the cross section to find
    type(string_t),                   intent(in)    :: cross_section_name

    ! Local variables
    character(len=*), parameter :: Iam = 'radXfer cross section warehouse get: '
    integer :: ndx
    logical :: found

    found = .false.
    do ndx = 1, size( this%handles_ )
      if( cross_section_name .eq. this%handles_( ndx ) ) then
        found = .true.
        exit
      endif
    end do

    if( .not. found ) then
      call die_msg( 980636382, "Invalid cross_section_name: '"//              &
                               cross_section_name%to_char()//"'" )
    endif
    allocate( cross_section_ptr,                                              &
              source = this%cross_section_objs_( ndx )%val_ )

  end function get

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalize the cross section warehouse
  subroutine finalize( this )

    !> cross section warehouse
    type(cross_section_warehouse_t), intent(inout) :: this

    integer :: ndx

    if( allocated( this%cross_section_objs_ ) ) then
      do ndx = 1,size( this%cross_section_objs_ )
        if( associated( this%cross_section_objs_( ndx )%val_ ) ) then
          deallocate( this%cross_section_objs_( ndx )%val_ )
        endif
      enddo
      deallocate( this%cross_section_objs_ )
    end if

    if( allocated( this%handles_ ) ) then
      deallocate( this%handles_ )
    end if

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_cross_section_warehouse
