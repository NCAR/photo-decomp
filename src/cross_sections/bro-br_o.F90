! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This bro+hv->br_o cross_section module

!> The bro+hv->br+o_cross_section type and related functions
module tuvx_cross_section_bro_br_o

  use tuvx_cross_section,              only : cross_section_t

  implicit none

  private
  public :: cross_section_bro_br_o_t

  !> Calculator for base_cross_section
  type, extends(cross_section_t) :: cross_section_bro_br_o_t
  contains
  end type cross_section_bro_br_o_t

  interface cross_section_bro_br_o_t
    module procedure constructor
  end interface cross_section_bro_br_o_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize cross_section_bro_br_o_t object
  function constructor( config, grid_warehouse, profile_warehouse )           &
      result( this )

    use musica_assert,                 only : assert_msg, die_msg
    use musica_config,                 only : config_t
    use musica_constants,              only : dk => musica_dk
    use musica_string,                 only : string_t
    use tuvx_netcdf_util,              only : netcdf_t
    use tuvx_grid,                     only : grid_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t
    use tuvx_util,                     only : inter4

    !> New cross section calculator
    type(cross_section_bro_br_o_t), pointer  :: this
    !> Cross section configuration object
    type(config_t),            intent(inout) :: config
    !> Grid warehouse
    type(grid_warehouse_t),    intent(inout) :: grid_warehouse
    !> Profile warehouse
    type(profile_warehouse_t), intent(inout) :: profile_warehouse

    ! Local variables
    character(len=*), parameter :: Iam = 'bro->br+o cross section initialize: '
    character(len=*), parameter :: Hdr = 'cross_section_'

    integer :: nParms
    integer :: parmNdx, fileNdx
    real(dk), allocatable :: data_lambda(:)
    real(dk), allocatable :: data_parameter(:)
    logical :: found
    character(len=:), allocatable :: msg
    type(netcdf_t), allocatable :: netcdf_obj
    type(string_t)              :: Handle
    type(string_t), allocatable :: netcdfFiles(:)
    class(grid_t), pointer :: lambdaGrid
    class(grid_t), pointer :: zGrid
    type(string_t) :: required_keys(2), optional_keys(1)

    required_keys(1) = "type"
    required_keys(2) = "netcdf files"
    optional_keys(1) = "name"
    call assert_msg( 290238001,                                               &
                     config%validate( required_keys, optional_keys ),         &
                     "Bad configuration data format for "//                   &
                     "bro->bro+o cross section." )

    allocate( this )

    ! Get model wavelength grids
    Handle = 'Photolysis, wavelength'
    lambdaGrid => grid_warehouse%get_grid( Handle )
    Handle = 'Vertical Z'
    zGrid => grid_warehouse%get_grid( Handle )

    ! Get cross section netcdf filespec
    call config%get( 'netcdf files', netcdfFiles, Iam, found = found )

has_netcdf_file: &
    if( found ) then
      allocate( this%cross_section_parms( size( netcdfFiles ) ) )
file_loop: &
      do fileNdx = 1, size( this%cross_section_parms )
        allocate( netcdf_obj )

        ! read netcdf cross section parameters
        call netcdf_obj%read_netcdf_file(                                     &
                     filespec = netcdfFiles( fileNdx )%to_char( ), Hdr = Hdr )
        nParms = size( netcdf_obj%parameters, dim = 2 )
        if( nParms < 1 ) then
          write(msg,*) Iam//'File: ',                                         &
                      trim( netcdfFiles( fileNdx )%to_char( ) ),              &
                      '  parameters array has < 1 column'
          call die_msg( 213420086, msg )
        endif

        ! interpolate from data to model wavelength grid
        if( allocated( netcdf_obj%wavelength ) ) then
          if( .not. allocated( this%cross_section_parms( fileNdx )%array) )   &
              then
            allocate( this%cross_section_parms( fileNdx )%array(              &
                                                 lambdaGrid%ncells_,nParms ) )
          endif
          do parmNdx = 1, nParms
            data_lambda    = netcdf_obj%wavelength
            data_parameter = netcdf_obj%parameters( :, parmNdx )
            call inter4( xto = lambdaGrid%edge_,                              &
                         yto = this%cross_section_parms(                      &
                                                fileNdx )%array( :, parmNdx ),&
                         xfrom = data_lambda,                                 &
                         yfrom = data_parameter, Foldin = 1 )
          enddo
        else
          this%cross_section_parms( fileNdx )%array = netcdf_obj%parameters
        endif
        if( allocated( netcdf_obj%temperature ) ) then
          this%cross_section_parms( fileNdx )%temperature =                   &
              netcdf_obj%temperature
        endif
        deallocate( netcdf_obj )
      enddo file_loop
    endif has_netcdf_file

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_cross_section_bro_br_o
