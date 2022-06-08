! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This cross_section module

!> The cross_section type and related functions
module tuvx_cross_section

  use musica_constants,                only : dk => musica_dk

  implicit none

  private
  public :: cross_section_t, cross_section_parms_t, base_constructor,         &
            cross_section_ptr

  !> Common cross section parameters
  type cross_section_parms_t
    !> \todo what are these used for, what axis are they on, and what units?
    real(dk), allocatable :: temperature(:)
    real(dk), allocatable :: deltaT(:)
    real(dk), allocatable :: array(:,:)
  end type cross_section_parms_t

  !> Calculator for cross_section
  type :: cross_section_t
    !> The cross section array \todo what axis are these on?
    type(cross_section_parms_t), allocatable :: cross_section_parms(:)
  contains
    !> Calculate the cross section
    procedure :: calculate => run
    !> Add points to the cross section grid based on configuration data
    procedure :: add_points
    final     :: finalize
  end type cross_section_t

  interface cross_section_t
    module procedure :: constructor
  end interface

  !> Pointer type for building sets of photo rate constants
  type :: cross_section_ptr
    class(cross_section_t), pointer :: val_ => null( )
  end type cross_section_ptr

  real(dk), parameter    :: rZERO = 0.0_dk
  real(dk), parameter    :: rONE  = 1.0_dk

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Create an instance of the base cross section type
  function constructor( config, grid_warehouse, profile_warehouse )           &
      result( new_obj )

    use musica_assert,                 only : assert_msg
    use musica_config,                 only : config_t
    use musica_string,                 only : string_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    !> Base cross section type
    class(cross_section_t),    pointer       :: new_obj
    !> Cross section configuration object
    type(config_t),            intent(inout) :: config
    !> Grid warehouse
    type(grid_warehouse_t),    intent(inout) :: grid_warehouse
    !> profile warehouse
    type(profile_warehouse_t), intent(inout) :: profile_warehouse

    type(string_t) :: required_keys(1), optional_keys(4)

    required_keys(1) = "type"
    optional_keys(1) = "netcdf files"
    optional_keys(2) = "lower extrapolation"
    optional_keys(3) = "upper extrapolation"
    optional_keys(4) = "name"
    call assert_msg( 124969900,                                               &
                     config%validate( required_keys, optional_keys ),         &
                     "Bad configuration data format for "//                   &
                     "n2o_n2+o1d cross section." )
    allocate( new_obj )
    call base_constructor( new_obj, config, grid_warehouse, profile_warehouse )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize cross_section_t objects
  subroutine base_constructor( new_obj, config, grid_warehouse,               &
      profile_warehouse )

    use musica_config,                 only : config_t
    use musica_string,                 only : string_t
    use tuvx_netcdf_util,              only : netcdf_t
    use tuvx_util,                     only : inter2
    use musica_assert,                 only : die_msg
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_grid,                     only : grid_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t
    use tuvx_profile,                  only : profile_t

    !> Base cross section type
    class(cross_section_t),    pointer       :: new_obj
    !> Cross section configuration object
    type(config_t),            intent(inout) :: config
    !> Grid warehouse
    type(grid_warehouse_t),    intent(inout) :: grid_warehouse
    !> profile warehouse
    type(profile_warehouse_t), intent(inout) :: profile_warehouse

    !   local variables
    character(len=*), parameter   :: Iam = 'base cross section initialize'
    character(len=*), parameter   :: Hdr = 'cross_section_'

    integer :: retcode
    integer :: parmNdx, fileNdx
    integer :: nParms
    real(dk), allocatable :: data_lambda(:)
    real(dk), allocatable :: data_parameter(:)
    logical :: found
    character(len=:), allocatable :: msg
    type(netcdf_t), allocatable :: netcdf_obj
    type(string_t)              :: Handle
    type(string_t), allocatable :: netcdfFiles(:)
    class(grid_t), pointer :: lambdaGrid

    !> Get model wavelength grids
    Handle = 'Photolysis, wavelength'
    lambdaGrid => grid_warehouse%get_grid( Handle )

    !> get cross section netcdf filespec
    call config%get( 'netcdf files', netcdfFiles, Iam, found = found )

has_netcdf_file: &
    if( found ) then
      allocate( new_obj%cross_section_parms( size( netcdfFiles ) ) )
file_loop: &
      do fileNdx = 1, size( new_obj%cross_section_parms )
        allocate( netcdf_obj )
        ! read netcdf cross section parameters
        call netcdf_obj%read_netcdf_file( filespec =                          &
                                netcdfFiles( fileNdx )%to_char( ), Hdr = Hdr )
        nParms = size( netcdf_obj%parameters, dim = 2 )
        if( nParms < 1 ) then
          write(msg,*) Iam//'File: ',                                         &
                       trim( netcdfFiles( fileNdx )%to_char( ) ),             &
                       '  parameters array has < 1 column'
          call die_msg( 520647236, msg )
        endif

        ! interpolate from data to model wavelength grid
        if( allocated( netcdf_obj%wavelength ) ) then
          if( .not. allocated( new_obj%cross_section_parms( fileNdx )%array) )&
              then
            allocate( new_obj%cross_section_parms( fileNdx )%array(           &
                                                lambdaGrid%ncells_, nParms ) )
          endif
          do parmNdx = 1, nParms
            data_lambda    = netcdf_obj%wavelength
            data_parameter = netcdf_obj%parameters( :, parmNdx )
            call new_obj%add_points( config, data_lambda, data_parameter )
            call inter2(xto = lambdaGrid%edge_,                               &
                        yto = new_obj%cross_section_parms(                    &
                                                fileNdx )%array( :, parmNdx ),&
                        xfrom = data_lambda,                                  &
                        yfrom = data_parameter, ierr = retcode )
          enddo
        else
          new_obj%cross_section_parms( fileNdx )%array = netcdf_obj%parameters
        endif
        if( allocated( netcdf_obj%temperature ) ) then
          new_obj%cross_section_parms( fileNdx )%temperature =                &
              netcdf_obj%temperature
        endif
        deallocate( netcdf_obj )
      enddo file_loop
    endif has_netcdf_file

  end subroutine base_constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the cross section for a given set of environmental conditions
  function run( this, grid_warehouse, profile_warehouse, at_mid_point )       &
      result( cross_section )

    use musica_string,                 only : string_t
    use tuvx_grid,                     only : grid_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    !> Calculated cross section
    real(kind=dk), allocatable               :: cross_section(:,:)
    !> Base cross section
    class(cross_section_t),    intent(in)    :: this
    !> Grid warehouse
    type(grid_warehouse_t),    intent(inout) :: grid_warehouse
    !> Profile warehouse
    type(profile_warehouse_t), intent(inout) :: profile_warehouse
    !> Flag indicating that the cross section data is at grid mid-points
    !!
    !! If omitted or false, data is assumed to be at interfaces
    logical, optional,         intent(in)    :: at_mid_point

    !> Local variables
    integer :: colndx
    integer :: nzdim
    character(len=*), parameter :: Iam =                                      &
        'radXfer base cross section calculate: '
    class(grid_t), pointer :: zGrid
    type(string_t)                :: Handle
    real(dk), allocatable         :: wrkCrossSection(:,:)

    Handle = 'Vertical Z'
    zGrid => grid_warehouse%get_grid( Handle )

    nzdim = zGrid%ncells_ + 1
    if( present( at_mid_point ) ) then
      if( at_mid_point ) then
        nzdim = nzdim - 1
      endif
    endif

    allocate( wrkCrossSection( size( this%cross_section_parms(1)%array,       &
                                                          dim = 1 ), nzdim ) )

    !> Just copy the lambda interpolated array
    do colndx = 1, nzdim
      wrkCrossSection( :, colndx ) = this%cross_section_parms(1)%array(:,1)
    enddo

    cross_section = transpose( wrkCrossSection )

  end function run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Adds points to the cross section grid based on configuration data
  subroutine add_points( this, config, data_lambda, data_parameter )

    use musica_config,                 only : config_t
    use musica_string,                 only : string_t
    use tuvx_util,                     only : addpnt

    class(cross_section_t), intent(in)    :: this
    type(config_t),         intent(inout) :: config
    real(dk), allocatable,  intent(inout) :: data_lambda(:)
    real(dk), allocatable,  intent(inout) :: data_parameter(:)

    real(dk), parameter :: rZERO = 0.0_dk
    real(dk), parameter :: rONE  = 1.0_dk
    real(dk), parameter :: deltax = 1.e-5_dk
    character(len=*), parameter :: Iam = 'cross_section; addpnts: '

    integer  :: nRows
    real(dk) :: lowerLambda, upperLambda
    real(dk) :: addpnt_val_lower, addpnt_val_upper
    type(string_t)  :: addpnt_type_
    logical         :: found
    character(len=:), allocatable :: number

    !> add endpoints to data arrays; first the lower bound
    nRows = size( data_lambda )
    lowerLambda = data_lambda(1) ; upperLambda = data_lambda( nRows )
    call config%get( 'lower extrapolation', addpnt_type_, Iam, found = found )
    if( .not. found ) then
      addpnt_val_lower = rZERO
    elseif( addpnt_type_ == 'boundary' ) then
      addpnt_val_lower = data_parameter(1)
    else
      number = addpnt_type_%to_char( )
      read( number, '(g30.20)' ) addpnt_val_lower
    endif

    !> add endpoints to data arrays; now the upper bound
    call config%get( 'upper extrapolation', addpnt_type_, Iam, found=found )
    if( .not. found ) then
      addpnt_val_upper = rZERO
    elseif( addpnt_type_ == 'boundary' ) then
      addpnt_val_upper = data_parameter( nRows )
    else
      number = addpnt_type_%to_char( )
      read( number, '(g30.20)' ) addpnt_val_upper
    endif

    call addpnt( x = data_lambda, y = data_parameter,                         &
                 xnew = ( rONE - deltax ) * lowerLambda,                      &
                 ynew = addpnt_val_lower )
    call addpnt( x = data_lambda, y = data_parameter, xnew = rZERO,           &
                 ynew = addpnt_val_lower )
    call addpnt( x = data_lambda, y = data_parameter,                         &
                 xnew = ( rONE + deltax ) * upperLambda,                      &
                 ynew = addpnt_val_upper )
    call addpnt( x = data_lambda, y = data_parameter, xnew = 1.e38_dk,        &
                 ynew = addpnt_val_upper )

  end subroutine add_points

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> finalize the cross section type
  subroutine finalize( this )

    type(cross_section_t), intent(inout) :: this

    integer :: ndx

    if( allocated( this%cross_section_parms ) ) then
      do ndx = 1,size( this%cross_section_parms )
        if( allocated( this%cross_section_parms( ndx )%array ) ) then
          deallocate( this%cross_section_parms( ndx )%array )
        endif
        if( allocated( this%cross_section_parms( ndx )%temperature ) ) then
          deallocate( this%cross_section_parms( ndx )%temperature )
        endif
      enddo
      deallocate( this%cross_section_parms )
    endif

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_cross_section
