! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> no2_tint_cross_section module

!> The temperature interpolation cross_section type and related functions
module tuvx_cross_section_no2_tint

  use tuvx_cross_section,              only : cross_section_t

  implicit none

  private
  public :: cross_section_no2_tint_t

  !> Calculator for tint_cross_section
  type, extends(cross_section_t) :: cross_section_no2_tint_t
  contains
    !> Calculate the cross section
    procedure :: calculate => run
    !> clean up
    final     :: finalize
  end type cross_section_no2_tint_t

  !> Constructor
  interface cross_section_no2_tint_t
    module procedure constructor
  end interface cross_section_no2_tint_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize cross_section_no2_tint_t object
  function constructor( config, grid_warehouse, profile_warehouse )           &
      result ( this )

    use musica_assert,                 only : die_msg
    use musica_constants,              only : dk => musica_dk
    use musica_config,                 only : config_t
    use musica_string,                 only : string_t
    use tuvx_cross_section,            only : cross_section_parms_t
    use tuvx_grid,                     only : grid_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_netcdf_util,              only : netcdf_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t
    use tuvx_util,                     only : inter2

    type(cross_section_no2_tint_t), pointer       :: this
    type(config_t),                 intent(inout) :: config
    type(grid_warehouse_t),         intent(inout) :: grid_warehouse
    type(profile_warehouse_t),      intent(inout) :: profile_warehouse

    ! local variables
    character(len=*), parameter :: Iam = 'no2 tint cross section constructor'
    character(len=*), parameter :: Hdr = 'cross_section_'

    real(dk), parameter    :: rZERO = 0.0_dk
    real(dk), parameter    :: rONE  = 1.0_dk

    integer :: retcode
    integer :: parmNdx, fileNdx, Ndxl, Ndxu
    integer :: nParms, nTemps
    real(dk)              :: tmp
    real(dk), allocatable :: data_lambda(:)
    real(dk), allocatable :: data_parameter(:)
    logical :: found, monopos
    character(len=:), allocatable :: msg
    type(netcdf_t), allocatable :: netcdf_obj
    type(string_t), allocatable :: netcdfFiles(:)
    class(grid_t), pointer :: lambdaGrid
    type(string_t)     :: Handle

    allocate(this)

    ! Get model wavelength grids
    Handle = 'Photolysis, wavelength'
    lambdaGrid => grid_warehouse%get_grid( Handle )

    ! Get cross section netcdf filespec
    call config%get( 'netcdf files', netcdfFiles, Iam, found = found )

has_netcdf_file:                                                              &
    if( found ) then
      allocate( this%cross_section_parms( size( netcdfFiles ) ) )
file_loop: &
      do fileNdx = 1,size( this%cross_section_parms )
        allocate( netcdf_obj )
        ! read netcdf cross section parameters
        call netcdf_obj%read_netcdf_file(                                     &
                     filespec = netcdfFiles( fileNdx )%to_char( ), Hdr = Hdr )
        nParms = size( netcdf_obj%parameters, dim = 2 )
        ! must have at least one parameter
        if( nParms < 2 ) then
          write(msg,*) Iam//'File: ',                                         &
                       trim( netcdfFiles( fileNdx )%to_char( ) ),             &
                       '  array must have 2 or more parameters'
          call die_msg( 299570478, msg )
        endif
        associate( Xsection => this%cross_section_parms( fileNdx ) )
        ! interpolation temperatures must be in netcdf file
        if( allocated( netcdf_obj%temperature ) ) then
          Xsection%temperature = netcdf_obj%temperature
          nTemps = size( Xsection%temperature )
          ! must have two or more interpolation temperatures
          if( nTemps < 2 ) then
            write(msg,*) Iam//'File: ',                                       &
                         trim( netcdfFiles( fileNdx )%to_char( ) ),           &
                         '  temperature array has < 2 entries'
            call die_msg( 403265743, msg )
          elseif( nTemps < nParms ) then
            write(msg,*) Iam//'File: ',                                       &
                         trim( netcdfFiles( fileNdx )%to_char( ) ),           &
                         '  temperature array < number parameters'
            call die_msg( 905113375, msg )
          endif
          Xsection%deltaT = Xsection%temperature( 2 : nParms )                &
                            - Xsection%temperature( 1 : nParms - 1 )
          monopos = all( Xsection%deltaT > rZERO )
          if( .not. monopos ) then
            if( any( Xsection%deltaT > rZERO ) ) then
              write(msg,*) Iam//'File: ',                                     &
                           trim( netcdfFiles( fileNdx )%to_char( ) ),         &
                           '  temperature array not monotonic'
              call die_msg( 561440794, msg )
            endif
            do Ndxl = 1, nParms / 2
              Ndxu = nParms - Ndxl + 1
              tmp = Xsection%temperature( Ndxl )
              Xsection%temperature( Ndxl ) = Xsection%temperature( Ndxu )
              Xsection%temperature( Ndxu ) = tmp
              data_parameter = netcdf_obj%parameters( :, Ndxl )
              netcdf_obj%parameters( :, Ndxl ) =                              &
                  netcdf_obj%parameters( :, Ndxu )
              netcdf_obj%parameters( :, Ndxu ) = data_parameter
            enddo
            Xsection%deltaT = Xsection%temperature( 2 : nParms )              &
                              - Xsection%temperature( 1 : nParms - 1 )
          endif
        else
          write(msg,*) Iam//'File: ',                                         &
                       trim( netcdfFiles( fileNdx )%to_char( ) ),             &
                       ' does not have interpolation temperatures'
          call die_msg( 326727785, msg )
        endif

        ! interpolate from data to model wavelength grid
        if( allocated( netcdf_obj%wavelength ) ) then
          if( .not. allocated( Xsection%array ) ) then
            allocate( Xsection%array( lambdaGrid%ncells_, nParms ) )
          endif
          do parmNdx = 1, nParms
            data_lambda    = netcdf_obj%wavelength
            data_parameter = netcdf_obj%parameters( :, parmNdx )
            call this%add_points( config, data_lambda, data_parameter )
            call inter2( xto = lambdaGrid%edge_,                              &
                         yto = Xsection%array( :, parmNdx ),                  &
                         xfrom = data_lambda,                                 &
                         yfrom = data_parameter, ierr = retcode )
          enddo
        else
          Xsection%array = netcdf_obj%parameters
        endif
        end associate
        deallocate( netcdf_obj )
      enddo file_loop
    else has_netcdf_file
      write(msg,*) Iam//'must have at least one netcdf input file'
      call die_msg( 832386485, msg )
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
    real(kind=dk), allocatable                     :: cross_section(:,:)
    !> Cross section calculator
    class(cross_section_no2_tint_t), intent(in)    :: this
    !> Grid warehouse
    type(grid_warehouse_t),          intent(inout) :: grid_warehouse
    !> Grid warehouse
    type(profile_warehouse_t),       intent(inout) :: profile_warehouse
    !> Flag indicating whether cross-section data should be at mid-points on
    !! the wavelength grid.
    !!
    !! If this is false or omitted, cross-section data are calculated at
    !! interfaces on the wavelength grid.
    logical, optional,               intent(in)    :: at_mid_point

    ! Local variables
    character(len=*), parameter :: Iam =                                      &
        'no2 tint cross section calculate'
    real(dk), parameter    :: rZERO = 0.0_dk
    real(dk), parameter    :: rONE  = 1.0_dk

    integer :: nTemp, nzdim
    integer :: fileNdx, tNdx, vertNdx
    real(dk)    :: Tadj, Tstar
    real(dk), allocatable  :: wrkCrossSection(:,:)
    real(dk), allocatable  :: modelTemp(:)
    class(grid_t), pointer :: zGrid
    class(grid_t), pointer :: lambdaGrid
    class(profile_t), pointer :: mdlTemperature
    type(string_t)     :: Handle

    Handle = 'Vertical Z'
    zGrid => grid_warehouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength'
    lambdaGrid => grid_warehouse%get_grid( Handle )
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

    allocate( wrkCrossSection( lambdaGrid%ncells_, nzdim ) )
    wrkCrossSection = rZERO

    do fileNdx = 1, size( this%cross_section_parms )
      associate( dataTemp => this%cross_section_parms( fileNdx )%temperature, &
                 wrkXsect => this%cross_section_parms( fileNdx ) )
      nTemp = size( dataTemp )
      do vertNdx = 1, nzdim
        Tadj = min( max( modelTemp( vertNdx ), dataTemp(1) ),                 &
                    dataTemp( nTemp ) )
        do tNdx = 2, nTemp
          if( Tadj <= dataTemp( tNdx ) ) then
            exit
          endif
        enddo
        tNdx = min( nTemp, tNdx ) - 1
        Tstar = ( Tadj - dataTemp( tNdx ) ) / wrkXsect%deltaT( tNdx )
        wrkCrossSection( :, vertNdx ) = wrkCrossSection( :, vertNdx )         &
                                        + wrkXsect%array( :, tNdx )           &
                                    + Tstar * ( wrkXsect%array( :, tNdx + 1 ) &
                                                - wrkXsect%array( :, tNdx ) )
      enddo
      end associate
    enddo

    cross_section = transpose( wrkCrossSection )

  end function run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> finalize the cross section type
  subroutine finalize( this )

  type(cross_section_no2_tint_t), intent(inout) :: this

  integer :: ndx

  if( allocated( this%cross_section_parms ) ) then
    do ndx = 1, size( this%cross_section_parms )
      associate( Xsection => this%cross_section_parms( ndx ) )
      if( allocated( Xsection%array ) ) then
        deallocate( Xsection%array )
      endif
      if( allocated( Xsection%temperature ) ) then
        deallocate( Xsection%temperature )
      endif
      if( allocated( Xsection%deltaT ) ) then
        deallocate( Xsection%deltaT )
      endif
      end associate
    enddo
    deallocate( this%cross_section_parms )
  endif

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_cross_section_no2_tint
