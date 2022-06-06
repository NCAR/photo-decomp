! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This rono2 cross_section module

!> The rono2_cross_section type and related functions
module tuvx_cross_section_rono2

  use tuvx_cross_section,              only : cross_section_t

  implicit none

  private
  public :: cross_section_rono2_t

  !> Calculator for rono2 cross section
  type, extends(cross_section_t) :: cross_section_rono2_t
  contains
    !> Calculate the cross section
    procedure :: calculate => run
  end type cross_section_rono2_t

  !> Constructor
  interface cross_section_rono2_t
    module procedure constructor
  end interface cross_section_rono2_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize cross_section_t object
  function constructor( config, grid_warehouse, profile_warehouse )           &
      result ( this )

    use musica_assert,                 only : die_msg
    use musica_config,                 only : config_t
    use musica_constants,              only : dk => musica_dk
    use musica_string,                 only : string_t
    use tuvx_grid,                     only : grid_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_netcdf_util,              only : netcdf_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t
    use tuvx_util,                     only : inter2

    type(cross_section_rono2_t), pointer       :: this
    type(config_t),              intent(inout) :: config
    type(grid_warehouse_t),      intent(inout) :: grid_warehouse
    type(profile_warehouse_t),   intent(inout) :: profile_warehouse

    ! Local variables
    character(len=*), parameter :: Iam = 'rono2 cross section initialize'
    character(len=*), parameter :: Hdr = 'cross_section_'
    real(dk), parameter :: rZERO = 0.0_dk
    real(dk), parameter :: rONE  = 1.0_dk

    integer :: retcode, nmdlLambda
    integer :: parmNdx, fileNdx, nParms
    real(dk), allocatable :: data_lambda(:)
    real(dk), allocatable :: data_parameter(:)
    logical :: found
    character(len=:), allocatable :: msg
    character(len=:), allocatable :: addpntKey
    type(netcdf_t), allocatable :: netcdf_obj
    type(string_t), allocatable :: netcdfFiles(:)
    type(config_t)              :: tmp_config
    type(string_t)              :: addpntVal
    type(string_t)              :: Handle
    class(grid_t), pointer :: lambdaGrid

    allocate( this )

    ! Get model wavelength grids
    Handle = 'Photolysis, wavelength'
    lambdaGrid => grid_warehouse%get_grid( Handle )

    ! get cross section netcdf filespec
    call config%get( 'netcdf files', netcdfFiles, Iam, found = found )

has_netcdf_file:                                                              &
    if( found ) then
      allocate( this%cross_section_parms( size( netcdfFiles ) ) )
file_loop:                                                                    &
      do fileNdx = 1, size( this%cross_section_parms )
        allocate( netcdf_obj )
        ! read netcdf cross section parameters
        call netcdf_obj%read_netcdf_file(                                     &
                     filespec = netcdfFiles( fileNdx )%to_char( ), Hdr = Hdr )
        nParms = size( netcdf_obj%parameters, dim = 2 )
        if( nParms < 1 ) then
          write(msg,*) Iam//'File: ',                                         &
                       trim(netcdfFiles( fileNdx )%to_char( ) ),              &
                       '  parameters array has < 1 column'
          call die_msg( 817384176, msg )
        endif

        ! interpolate from data to model wavelength grid
        if( allocated( netcdf_obj%wavelength ) ) then
          if( .not. allocated( this%cross_section_parms( fileNdx )%array ) )  &
              then
            allocate( this%cross_section_parms(                               &
                               fileNdx )%array( lambdaGrid%ncells_, nParms ) )
          endif
          do parmNdx = 1, nParms
            data_lambda    = netcdf_obj%wavelength
            data_parameter = netcdf_obj%parameters( :, parmNdx )
            if( parmNdx == 1 ) then
              call this%add_points( config, data_lambda, data_parameter )
            elseif( parmNdx == 2 ) then
              tmp_config = config
              addpntKey = 'lower extrapolation'
              addpntVal = 'boundary'
              call tmp_config%add( addpntKey, addpntVal, Iam )
              addpntKey = 'upper extrapolation'
              addpntVal = 'boundary'
              call tmp_config%add( addpntKey, addpntVal, Iam )
              call this%add_points( tmp_config, data_lambda, data_parameter )
            endif
            call inter2( xto = lambdaGrid%edge_,                              &
                         yto = this%cross_section_parms(                      &
                                               fileNdx )%array( :, parmNdx ), &
                         xfrom = data_lambda,                                 &
                         yfrom = data_parameter, ierr = retcode )
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

  !> Calculate the cross section for a given set of environmental conditions
  function run( this, grid_warehouse, profile_warehouse, at_mid_point )       &
      result( cross_section )

    use musica_constants,              only : dk => musica_dk
    use musica_string,                 only : string_t
    use tuvx_grid,                     only : grid_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile,                  only : profile_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    !> Calculated cross section
    real(kind=dk), allocatable                  :: cross_section(:,:)
    !> Cross section calculator
    class(cross_section_rono2_t), intent(in)    :: this
    !> Grid warehouse
    type(grid_warehouse_t),       intent(inout) :: grid_warehouse
    !> Profile warehouse
    type(profile_warehouse_t),    intent(inout) :: profile_warehouse
    !> Flag indicating whether cross-section data should be at mid-points on
    !! the wavelength grid.
    !!
    !! If this is false or omitted, cross-section data are calculated at
    !! interfaces on the wavelength grid.
    logical, optional,            intent(in)    :: at_mid_point

    ! Local variables
    character(len=*), parameter :: Iam = 'rono2 cross section calculate'
    real(dk), parameter :: rZERO = 0.0_dk
    real(dk), parameter :: rONE  = 1.0_dk

    integer :: vertNdx
    integer :: nzdim
    real(dk), parameter :: T0 = 298._dk
    real(dk) :: Temp
    real(dk), allocatable :: modelTemp(:)
    class(grid_t), pointer :: zGrid, lambdaGrid
    class(profile_t), pointer :: mdlTemperature
    type(string_t)                :: Handle

    Handle = 'Photolysis, wavelength'
    lambdaGrid => grid_warehouse%get_grid( Handle )
    Handle = 'Vertical Z'
    zGrid => grid_warehouse%get_grid( Handle )
    Handle = 'Temperature'
    mdlTemperature => profile_warehouse%get_Profile( Handle )

    nzdim = zGrid%ncells_ + 1
    if( present( at_mid_point ) ) then
      if( at_mid_point ) then
        nzdim = nzdim - 1
        modelTemp = mdlTemperature%mid_val_
      else
        modelTemp = mdlTemperature%edge_val_
      endif
    else
      modelTemp = mdlTemperature%edge_val_
    endif

    allocate( cross_section( lambdaGrid%ncells_, nzdim ) )
    cross_section = rZERO

    do vertNdx = 1, nzdim
      Temp = modelTemp( vertNdx ) - T0
      cross_section( :, vertNdx ) = this%cross_section_parms(1)%array(:,1)    &
                        * exp( this%cross_section_parms(1)%array(:,2) * Temp )
    enddo

    cross_section = transpose( cross_section )

  end function run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_cross_section_rono2
