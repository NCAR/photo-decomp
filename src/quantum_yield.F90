! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This base quantum yield module

!> The base quantum yield type and related functions
module tuvx_quantum_yield

  use musica_constants,                only : dk => musica_dk

  implicit none

  private
  public :: quantum_yield_t, quantum_yield_ptr, base_constructor

  type quantum_yield_parms_t
    !> Temperature \todo include units - what is this used for?
    real(dk), allocatable :: temperature(:)
    !> Parameters for calculating quantum yields (wavelength, parameter)
    real(dk), allocatable :: array(:,:)
  end type quantum_yield_parms_t

  !> Calculator for base quantum yield
  type :: quantum_yield_t
    type(quantum_yield_parms_t), allocatable :: quantum_yield_parms(:)
  contains
    procedure :: calculate => run
    procedure :: add_points
    final     :: finalize
  end type quantum_yield_t

  !> Pointer type for building sets of quantum yields
  type :: quantum_yield_ptr
    class(quantum_yield_t), pointer :: val_ => null( )
  end type quantum_yield_ptr

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Performs default initialization for quantum yield_t object
  !!
  !! Should only be called by sub-class constructors. Sub-classes can decide
  !! whether to call this function during construction to load standard
  !! NetCDF files and configuration options.
  !!
  !! Reads NetCDF files specified in configuration array 'netcdf files'.
  !! Data from each NetCDF file will be loaded into an element of the
  !! \c quantum_yield_parms data member. If a NetCDF variable named
  !! \c quantum_yield_parameters is present, it will be used to populate
  !! the \c array data member of the \c quantum_yield_parms_t object.
  !! If a NetCDF variable named \c temperature is present, it will be
  !! used to populate the \c temperature data member of the
  !! \c quantum_yield_parms_t object.
  subroutine base_constructor( this, config, grid_warehouse,                  &
      profile_warehouse )

    use musica_assert,                 only : die_msg
    use musica_config,                 only : config_t
    use musica_string,                 only : string_t
    use tuvx_grid,                     only : grid_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_netcdf_util,              only : netcdf_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t
    use tuvx_util,                     only : inter2

    !> Quantum yield calculator
    class(quantum_yield_t),    intent(inout) :: this
    !> Configuration data
    type(config_t),            intent(inout) :: config
    !> Grid warehouse
    type(grid_warehouse_t),    intent(inout) :: grid_warehouse
    !> Profile warehouse
    type(profile_warehouse_t), intent(inout) :: profile_warehouse

    ! Local variables
    character(len=*), parameter :: Iam = 'base quantum yield constructor'
    character(len=*), parameter :: Hdr = 'quantum_yield_'
    real(dk), parameter    :: rZERO = 0.0_dk
    real(dk), parameter    :: rONE  = 1.0_dk

    integer :: retcode
    integer :: parmNdx, fileNdx
    integer :: nParms
    real(dk)    :: quantum_yield_constant
    real(dk), allocatable :: data_lambda(:)
    real(dk), allocatable :: data_parameter(:)
    logical :: found
    character(len=:), allocatable :: msg
    type(netcdf_t), allocatable   :: netcdf_obj
    type(string_t)                :: Handle
    type(string_t), allocatable   :: netcdfFiles(:)
    class(grid_t),  pointer       :: lambdaGrid => null( )

    ! Get model wavelength grid
    Handle = 'Photolysis, wavelength'
    lambdaGrid => grid_warehouse%get_grid( Handle )

    ! get quantum yield netcdf filespec
    call config%get( 'netcdf files', netcdfFiles, Iam, found = found )
has_netcdf_file: &
    if( found ) then
      allocate( this%quantum_yield_parms( size( netcdfFiles ) ) )
file_loop: &
      do fileNdx = 1, size( netcdfFiles )
        allocate( netcdf_obj )
        ! read netcdf file quantum yield data
        call netcdf_obj%read_netcdf_file(                                     &
                     filespec = netcdfFiles( fileNdx )%to_char( ), Hdr = Hdr )
        nParms = size( netcdf_obj%parameters, dim = 2 )
        if( nParms < 1 ) then
          write(msg,*) Iam//'File: ',                                         &
              trim( netcdfFiles( fileNdx )%to_char( ) ),                      &
              ' parameters array has < 1 parameter'
          call die_msg( 493253966, msg )
        endif
        ! interpolate from data to model wavelength grid
        if( allocated( netcdf_obj%wavelength ) ) then
          if( .not. allocated( this%quantum_yield_parms( fileNdx )%array ) )  &
              then
            allocate( this%quantum_yield_parms( fileNdx )%array(              &
                                                lambdaGrid%ncells_, nParms ) )
          endif
          do parmNdx = 1, nParms
            data_lambda    = netcdf_obj%wavelength
            data_parameter = netcdf_obj%parameters(:,parmNdx)
            call this%add_points( config, data_lambda, data_parameter )
            call inter2( xto = lambdaGrid%edge_,                              &
               yto = this%quantum_yield_parms( fileNdx )%array( :, parmNdx ), &
               xfrom = data_lambda,                                           &
               yfrom = data_parameter, ierr = retcode )
          enddo
        else
          this%quantum_yield_parms( fileNdx )%array = netcdf_obj%parameters
        endif
        if( allocated( netcdf_obj%temperature ) ) then
          this%quantum_yield_parms( fileNdx )%temperature =                   &
              netcdf_obj%temperature
        endif
        deallocate( netcdf_obj )
      enddo file_loop
    else has_netcdf_file
      ! check for quantum yield constant
      call config%get( 'constant value', quantum_yield_constant, Iam,         &
                       found = found )
      if( found ) then
        allocate( this%quantum_yield_parms(1) )
        allocate( this%quantum_yield_parms(1)%array( lambdaGrid%ncells_, 1 ) )
        this%quantum_yield_parms(1)%array(:,1) = quantum_yield_constant
      endif
    endif has_netcdf_file

    deallocate( lambdaGrid )

  end subroutine base_constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculates the quantum yield
  !!
  !! Uses the interpolated first quantum yield parameter as the quantum
  !! yield.
  function run( this, grid_warehouse, profile_warehouse )                     &
      result( quantum_yield )

    use musica_string,                 only : string_t
    use tuvx_grid,                     only : grid_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    !> Quantum yield calculator
    class(quantum_yield_t),    intent(in)    :: this
    !> Grid warehouse
    type(grid_warehouse_t),    intent(inout) :: grid_warehouse
    !> Profile warehouse
    type(profile_warehouse_t), intent(inout) :: profile_warehouse
    !> Calculated quantum yield
    real(dk), allocatable                    :: quantum_yield(:,:)

    ! Local variables
    character(len=*), parameter :: Iam = 'base quantum yield calculate'
    integer                     :: vertNdx
    class(grid_t),  pointer     :: zGrid => null( )
    type(string_t)              :: Handle
    real(dk),       allocatable :: wrkQuantumYield(:,:)

    Handle = 'Vertical Z'
    zGrid => grid_warehouse%get_grid( Handle )

    allocate( wrkQuantumYield(                                                &
      size( this%quantum_yield_parms(1)%array, dim = 1 ), zGrid%ncells_ + 1 ) )

    ! Just copy the lambda interpolated array
    do vertNdx = 1, zGrid%ncells_ + 1
      wrkQuantumYield( :, vertNdx ) =                                         &
          this%quantum_yield_parms(1)%array( :, 1 )
    enddo

    quantum_yield = transpose( wrkQuantumYield )

    deallocate( zGrid )

  end function run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Addw points to the cross-section gridded data based on configuration
  !! options
  subroutine add_points( this, config, data_lambda, data_parameter )

    use musica_assert,                 only : assert_msg, die_msg
    use musica_config,                 only : config_t
    use musica_string,                 only : string_t
    use tuvx_util,                     only : addpnt

    class(quantum_yield_t), intent(in)    :: this
    type(config_t),         intent(inout) :: config
    real(dk), allocatable,  intent(inout) :: data_lambda(:)
    real(dk), allocatable,  intent(inout) :: data_parameter(:)

    real(dk), parameter :: rZERO = 0.0_dk
    real(dk), parameter :: rONE  = 1.0_dk
    real(dk), parameter :: deltax = 1.e-5_dk
    character(len=*), parameter :: Iam = 'cross_section; addpnts: '

    integer :: nRows
    real(dk) :: lowerLambda, upperLambda
    real(dk) :: addpnt_val
    type(string_t)  :: addpnt_type
    type(config_t)  :: extrap_config
    logical         :: found
    character(len=:), allocatable :: number
    type(string_t) :: required_keys(1), optional_keys(1)

    required_keys(1) = "type"
    optional_keys(1) = "value"

    nRows = size( data_lambda )
    lowerLambda = data_lambda(1) ; upperLambda = data_lambda( nRows )

    ! add endpoints to data arrays; first the lower bound
    addpnt_val = rZERO
    call config%get( 'lower extrapolation', extrap_config, Iam, found = found )
    if( found ) then
      call assert_msg( 231583250,                                             &
                       extrap_config%validate( required_keys, optional_keys ),&
                       "Bad format for extrapolation in base quantum yield." )
      call extrap_config%get( "type", addpnt_type, Iam )
      if( addpnt_type == 'boundary' ) then
        addpnt_val = data_parameter(1)
      elseif( addpnt_type == 'constant' ) then
        call extrap_config%get( "value", addpnt_val, Iam )
      else
        call die_msg( 163668372,                                              &
                      "Bad extrapolation type: '"//addpnt_type//"'" )
      endif
    endif
    call addpnt( x = data_lambda, y = data_parameter, xnew = rZERO,           &
                 ynew = addpnt_val )
    call addpnt( x = data_lambda, y = data_parameter,                         &
                 xnew = ( rONE - deltax ) * lowerLambda, ynew = addpnt_val )

    ! add endpoints to data arrays; now the upper bound
    addpnt_val = rZERO
    call config%get( 'upper extrapolation', extrap_config, Iam, found = found )
    if( found ) then
      call assert_msg( 104376574,                                             &
                       extrap_config%validate( required_keys, optional_keys ),&
                       "Bad format for extrapolation in base quantum yield." )
      call extrap_config%get( "type", addpnt_type, Iam )
      if( addpnt_type == 'boundary' ) then
        addpnt_val = data_parameter( nRows )
      elseif( addpnt_type == 'constant' ) then
        call extrap_config%get( "value", addpnt_val, Iam )
      else
        call die_msg( 216694919,                                              &
                      "Bad extrapolation type: '"//addpnt_type//"'" )
      endif
    endif
    call addpnt( x = data_lambda, y = data_parameter,                         &
                 xnew = ( rONE + deltax ) * upperLambda, ynew = addpnt_val )
    call addpnt( x = data_lambda, y = data_parameter, xnew = 1.e38_dk, &
                 ynew = addpnt_val )

  end subroutine add_points

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalizes the quantum yield object
  subroutine finalize( this )
    type(quantum_yield_t), intent(inout) :: this

    integer :: ndx

    if( allocated( this%quantum_yield_parms ) ) then
      do ndx = 1, size( this%quantum_yield_parms )
        if( allocated( this%quantum_yield_parms( ndx )%array ) ) then
          deallocate( this%quantum_yield_parms( ndx )%array )
        endif
        if( allocated( this%quantum_yield_parms( ndx )%temperature ) ) then
          deallocate( this%quantum_yield_parms( ndx )%temperature )
        endif
      enddo
      deallocate( this%quantum_yield_parms )
    endif
  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_quantum_yield
