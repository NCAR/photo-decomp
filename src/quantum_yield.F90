! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This base quantum yield module

!> The base quantum yield type and related functions
module tuvx_quantum_yield

  use musica_constants,              only : dk => musica_dk

  implicit none

  private
  public :: quantum_yield_t, quantum_yield_ptr, base_constructor

  type quantum_yield_parms_t
    real(dk), allocatable :: temperature(:)
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

  !> Initialize base quantum yield_t object
  subroutine base_constructor( this, config, grid_warehouse, profile_warehouse )

    use musica_assert,                 only : die_msg
    use musica_config,                 only : config_t
    use musica_string,                 only : string_t
    use tuvx_grid,                     only : grid_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_netcdf_util,              only : netcdf_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t
    use tuvx_util,                     only : inter2

    class(quantum_yield_t),    intent(inout) :: this
    !> quantum yield configuration data
    type(config_t),            intent(inout) :: config
    type(grid_warehouse_t),    intent(inout) :: grid_warehouse
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
    class(grid_t), pointer :: lambdaGrid

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
          this%quantum_yield_parms( fileNdx )%temperature = netcdf_obj%temperature
        endif
        deallocate( netcdf_obj )
      enddo file_loop
    else has_netcdf_file
      ! check for quantum yield constant
      call config%get( 'quantum yield constant', quantum_yield_constant, Iam, &
                       found = found )
      if( found ) then
        allocate( this%quantum_yield_parms(1) )
        allocate( this%quantum_yield_parms(1)%array( lambdaGrid%ncells_, 1 ) )
        this%quantum_yield_parms(1)%array(:,1) = quantum_yield_constant
      endif
    endif has_netcdf_file

  end subroutine base_constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the quantum yield
  function run( this, grid_warehouse, profile_warehouse )                     &
      result( quantum_yield )

    use musica_string,                 only : string_t
    use tuvx_grid,                     only : grid_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    class(quantum_yield_t),    intent(in)    :: this
    type(grid_warehouse_t),    intent(inout) :: grid_warehouse
    type(profile_warehouse_t), intent(inout) :: profile_warehouse
    !> Calculated quantum yield
    real(dk), allocatable                    :: quantum_yield(:,:)

    ! Local variables
    character(len=*), parameter :: Iam = 'base quantum yield calculate'
    integer                 :: vertNdx
    class(grid_t), pointer :: zGrid
    type(string_t)              :: Handle
    real(dk), allocatable       :: wrkQuantumYield(:,:)

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

  end function run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Add points to the cross-section gridded data based on configuration
  !! options
  subroutine add_points( this, config, data_lambda, data_parameter )

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
    logical         :: found
    character(len=:), allocatable :: number

    ! add endpoints to data arrays; first the lower bound
    nRows = size( data_lambda )
    lowerLambda = data_lambda(1) ; upperLambda = data_lambda( nRows )
    call config%get( 'lower extrapolation', addpnt_type, Iam, found = found )
    if( .not. found ) then
      addpnt_val = rZERO
    elseif( addpnt_type == 'boundary' ) then
      addpnt_val = data_parameter(1)
    else
      number = addpnt_type%to_char()
      read( number, '(g30.20)' ) addpnt_val
    endif

    call addpnt( x = data_lambda, y = data_parameter, xnew = rZERO,           &
                 ynew = addpnt_val )
    call addpnt( x = data_lambda, y = data_parameter,                         &
                 xnew = ( rONE - deltax ) * lowerLambda, ynew = addpnt_val )
    ! add endpoints to data arrays; now the upper bound
    call config%get( 'upper extrapolation', addpnt_type, Iam, found = found )
    if( .not. found ) then
      addpnt_val = rZERO
    elseif( addpnt_type == 'boundary' ) then
      addpnt_val = data_parameter( nRows )
    else
      number = addpnt_type%to_char()
      read( number, '(g30.20)' ) addpnt_val
    endif

    call addpnt( x = data_lambda, y = data_parameter,                         &
                 xnew = ( rONE + deltax ) * upperLambda, ynew = addpnt_val )
    call addpnt( x = data_lambda, y = data_parameter, xnew = 1.e38_dk, &
                 ynew = addpnt_val )

  end subroutine add_points

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> finalize the quantum yield object
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
