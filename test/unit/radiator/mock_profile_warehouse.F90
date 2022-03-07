! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
! Mock profile warehouse for tests
module test_mock_profile_warehouse

  use micm_profile,                    only : abs_profile_ptr
  use micm_profile_warehouse,          only : profile_warehouse_t
  use musica_constants,                only : dk => musica_dk

  implicit none
  private

  public :: mock_profile_warehouse_t

  type, extends(profile_warehouse_t) :: mock_profile_warehouse_t
    private
    type(abs_profile_ptr), allocatable :: profiles_(:)
  contains
    procedure :: get_profile
  end type mock_profile_warehouse_t

  interface mock_profile_warehouse_t
    module procedure :: constructor
  end interface mock_profile_warehouse_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Build a warehouse with preset profiles
  function constructor( config, grids ) result( mock_warehouse )

    use musica_config,                 only : config_t
    use musica_iterator,               only : iterator_t
    use musica_string,                 only : string_t
    use micm_grid_warehouse,           only : grid_warehouse_t
    use micm_profile_factory,          only : profile_builder

    class(mock_profile_warehouse_t), pointer       :: mock_warehouse
    type(config_t),                  intent(inout) :: config
    class(grid_warehouse_t),         intent(inout) :: grids

    character(len=*), parameter ::                                            &
        my_name = "mock_profile_warehouse_t constructor"
    type(config_t)             :: profile_set, profile_config
    class(iterator_t), pointer :: iter
    type(abs_profile_ptr)      :: profile
    type(string_t)             :: key

    allocate( mock_warehouse )
    allocate( mock_warehouse%profiles_( 0 ) )
    call config%get( 'Profiles', profile_set, my_name )
    iter => profile_set%get_iterator( )
    do while( iter%next( ) )
      key = profile_set%key( iter )
      call profile_set%get( iter, profile_config, my_name )
      call profile_config%add( 'Handle', key, my_name )
      profile%ptr_ => profile_builder( profile_config, grids )
      mock_warehouse%profiles_ = [ mock_warehouse%profiles_, profile ]
    end do
    deallocate( iter )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns a profile with certain preset values
  function get_profile( this, profile_handle ) result( profile_ptr )

    use musica_string,                 only : string_t
    use musica_assert,                 only : assert
    use micm_profile,                  only : abs_profile_t

    class(abs_profile_t),            pointer       :: profile_ptr
    class(mock_profile_warehouse_t), intent(inout) :: this
    type(string_t),                  intent(in)    :: profile_handle

    logical :: found
    integer :: i_profile, i_cell

    found = .false.
    do i_profile = 1, size( this%profiles_ )
      if( profile_handle .eq. this%profiles_( i_profile )%ptr_%handle_ ) then
        found = .true.
        exit
      end if
    end do

    call assert( 424091095, found )
    allocate( profile_ptr, source = this%profiles_( i_profile )%ptr_ )

    ! set certain values for tests
    do i_cell = 1, size( profile_ptr%layer_dens_ )
      profile_ptr%layer_dens_( i_cell ) = i_cell * 5.0_dk
    end do

  end function get_profile

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module test_mock_profile_warehouse
