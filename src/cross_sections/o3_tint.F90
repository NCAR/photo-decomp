! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This o3_tint_cross_section module

!> The o3 temperature interpolation cross_section type and related functions
module tuvx_cross_section_o3_tint

  use musica_constants,                only : dk => musica_dk
  use tuvx_cross_section_tint,         only : cross_section_tint_t

  implicit none

  private
  public :: cross_section_o3_tint_t

  !> Calculator for o3_tint_cross_section
  type, extends(cross_section_tint_t) :: cross_section_o3_tint_t
    real(dk) :: v185(1), v195(1), v345(1)
  contains
    !> Calculate the cross section
    procedure :: calculate => run
    !> refraction
    procedure :: refraction
  end type cross_section_o3_tint_t

  !> Constructor
  interface cross_section_o3_tint_t
    module procedure constructor
  end interface cross_section_o3_tint_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize cross_section_o3_tint_t object
  function constructor( config, grid_warehouse, profile_warehouse )           &
      result ( this )

    use musica_assert,                 only : assert_msg, die_msg
    use musica_config,                 only : config_t
    use musica_string,                 only : string_t
    use tuvx_grid,                     only : grid_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_netcdf_util,              only : netcdf_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t
    use tuvx_util,                     only : inter2

    type(cross_section_o3_tint_t), pointer       :: this
    type(config_t),                intent(inout) :: config
    type(grid_warehouse_t),        intent(inout) :: grid_warehouse
    type(profile_warehouse_t),     intent(inout) :: profile_warehouse

    ! Local variables
    real(dk), parameter :: rZERO = 0.0_dk
    real(dk), parameter :: rONE  = 1.0_dk
    real(dk), parameter  :: refracDensity = 2.45e19_dk
    real(dk), parameter  :: w185 = 185._dk
    real(dk), parameter  :: w195 = 195._dk
    real(dk), parameter  :: w345 = 345._dk

    character(len=*), parameter :: Iam = 'o3 tint cross section constructor'
    character(len=*), parameter :: Hdr = 'cross_section_'

    integer :: retcode, nmdlLambda
    integer :: parmNdx, fileNdx, Ndxl, Ndxu
    integer :: nParms, nTemps
    real(dk)              :: tmp
    real(dk), allocatable :: refracNdx(:)
    real(dk), allocatable :: data_lambda(:)
    real(dk), allocatable :: data_parameter(:)
    logical :: found, monopos
    character(len=:), allocatable :: msg
    type(netcdf_t), allocatable   :: netcdf_obj
    type(string_t), allocatable   :: netcdfFiles(:)
    class(grid_t), pointer :: lambdaGrid
    type(string_t)     :: Handle
    type(string_t) :: required_keys(2), optional_keys(3)

    required_keys(1) = "type"
    required_keys(2) = "netcdf files"
    optional_keys(1) = "lower extrapolation"
    optional_keys(2) = "upper extrapolation"
    optional_keys(3) = "name"
    call assert_msg( 988762814,                                               &
                     config%validate( required_keys, optional_keys ),         &
                     "Bad configuration data format for "//                   &
                     "o3 tint cross section." )

    allocate( this )

    ! Get model wavelength grids
    Handle = 'Photolysis, wavelength'
    lambdaGrid => grid_warehouse%get_grid( Handle )

    ! get cross section netcdf filespec
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
        ! must have at least one parameter
        if( nParms < 1 ) then
          write(msg,*) Iam//'File: ',                                         &
                       trim( netcdfFiles( fileNdx )%to_char( ) ),             &
                       '  array must have 1 or more parameters'
          call die_msg( 417465850, msg )
        endif
        ! refraction index
        refracNdx = this%refraction( netcdf_obj%wavelength, refracDensity )
        netcdf_obj%wavelength = refracNdx * netcdf_obj%wavelength

        associate( Xsection => this%cross_section_parms( fileNdx ) )
        ! interpolation temperatures must be in netcdf file
        if( allocated( netcdf_obj%temperature ) ) then
          Xsection%temperature = netcdf_obj%temperature
          nTemps = size( Xsection%temperature )
          if( nTemps > 1 ) then
            Xsection%deltaT = Xsection%temperature( 2 : nParms )              &
                              - Xsection%temperature( 1 : nParms - 1 )
            monopos = all( Xsection%deltaT > rZERO )
            if( .not. monopos ) then
              if( any( Xsection%deltaT > rZERO ) ) then
                write(msg,*) Iam//'File: ',                                   &
                             trim( netcdfFiles( fileNdx )%to_char( ) ),       &
                             '  temperature array not monotonic'
                call die_msg( 175583000, msg )
              endif
              do Ndxl = 1, nParms / 2
                Ndxu = nParms - Ndxl + 1
                tmp = Xsection%temperature( Ndxl )
                Xsection%temperature( Ndxl ) = Xsection%temperature( Ndxu )
                Xsection%temperature( Ndxu ) = tmp
                data_parameter = netcdf_obj%parameters( :, Ndxl )
                netcdf_obj%parameters( :, Ndxl ) =                            &
                    netcdf_obj%parameters( :, Ndxu )
                netcdf_obj%parameters( :, Ndxu ) = data_parameter
              enddo
              Xsection%deltaT = Xsection%temperature( 2 : nParms )            &
                                - Xsection%temperature( 1 : nParms - 1 )
            endif
          endif
        else
          write(msg,*) Iam//'File: ',                                         &
                       trim( netcdfFiles( fileNdx )%to_char( ) ),             &
                       ' does not have interpolation temperatures'
          call die_msg( 393502144, msg )
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
      this%v185 = this%refraction( (/ w185 /), refracDensity ) * w185
      this%v195 = this%refraction( (/ w195 /), refracDensity ) * w195
      this%v345 = this%refraction( (/ w345 /), refracDensity ) * w345
      this%cross_section_parms(2)%array(:,4) =                                &
          this%cross_section_parms(1)%array(:,1)
    else has_netcdf_file
      write(msg,*) Iam//'must have at least one netcdf input file'
      call die_msg( 729003940, msg )
    endif has_netcdf_file

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the cross section for a given set of environmental conditions
  function run( this, grid_warehouse, profile_warehouse, at_mid_point )       &
      result( cross_section )

    use musica_string,                 only : string_t
    use tuvx_grid,                     only : grid_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile,                  only : profile_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    !> Calculated cross section
    real(kind=dk), allocatable                    :: cross_section(:,:)
    !> O3 tint cross section
    class(cross_section_o3_tint_t), intent(in)    :: this
    !> Grid warehouse
    type(grid_warehouse_t),         intent(inout) :: grid_warehouse
    !> Grid warehouse
    type(profile_warehouse_t),      intent(inout) :: profile_warehouse
    !> Flag indicating whether cross-section data should be at mid-points on
    !! the wavelength grid.
    !!
    !! If this is false or omitted, cross-section data are calculated at
    !! interfaces on the wavelength grid.
    logical, optional,               intent(in)   :: at_mid_point

    character(len=*), parameter :: Iam = 'o3 tint cross section calculate'
    real(dk), parameter :: rZERO = 0.0_dk
    real(dk), parameter :: rONE  = 1.0_dk
    integer :: k
    integer :: nTemp
    integer :: fileNdx, tNdx, wNdx, nzdim
    real(dk)    :: Tadj, Tstar
    real(dk), allocatable :: modelTemp(:)
    real(dk), allocatable :: wrkCrossSection(:,:)
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

    ! temperature at model cell midpoint or edge
    nzdim     = zGrid%ncells_ + 1
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

vert_loop:                                                                    &
    do k = 1, nzdim
lambda_loop:                                                                  &
    do wNdx = 1, lambdaGrid%ncells_
      associate( lambda => lambdaGrid%edge_( wNdx ) )
        if( lambda < this%v185(1) ) then
          fileNdx = 3
        elseif( this%v185(1) <= lambda .and. lambda < this%v195(1) ) then
          fileNdx = 4
        elseif( this%v195(1) <= lambda .and. lambda < this%v345(1) ) then
          fileNdx = 2
        else
          wrkCrossSection( wNdx, k ) = wrkCrossSection( wNdx, k )             &
                                + this%cross_section_parms(1)%array( wNdx, 1 )
          cycle lambda_loop
        endif
      end associate
      associate( dataTemp => this%cross_section_parms( fileNdx )%temperature, &
                 wrkXsect => this%cross_section_parms( fileNdx ) )
      nTemp = size( dataTemp )
      Tadj  = min( max( modelTemp( k ),dataTemp(1) ),dataTemp( nTemp ) )
      do tNdx = 2, nTemp
        if( Tadj <= dataTemp( tNdx ) ) then
          exit
        endif
      enddo
      tNdx = tNdx - 1
      Tstar = ( Tadj - dataTemp( tNdx ) ) / wrkXsect%deltaT( tNdx )
      wrkCrossSection( wNdx, k ) = wrkCrossSection( wNdx, k )                 &
                                   + wrkXsect%array( wNdx, tNdx )             &
                                 + Tstar * ( wrkXsect%array( wNdx, tNdx + 1 ) &
                                             - wrkXsect%array( wNdx, tNdx ) )
      end associate
    enddo lambda_loop
    enddo vert_loop

    cross_section = transpose( wrkCrossSection )

  end function run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the refractive index for standard air
  !!
  !! (dry air at 15 deg. C, 101.325 kPa, 0.03% CO2)
  !! from CRC Handbook, originally from Edlen, B., Metrologia, 2, 71, 1966.
  !! valid from 200 nm to 2000 nm
  !! beyond this range, use constant value
  function refraction( this, wavelength, atmDensity ) result( refrac )

    !> O3 tint cross section
    class(cross_section_o3_tint_t), intent(in) :: this
    !> Air density [molec cm-3]
    real(dk),                       intent(in) :: atmDensity
    !> Vacuum wavelength [nm]
    real(dk),                       intent(in) :: wavelength(:)
    !> Refractive index of standard air
    real(dk)                                   :: refrac( size( wavelength ) )

    real(dk), parameter :: rONE = 1.0_dk
    real(dk), parameter :: divisor = 2.69e19_dk * 273.15_dk / 288.15_dk
    integer :: wNdx
    real(dk) :: wrk, sig, sigsq


    do wNdx = 1, size( wavelength )
      wrk = min( max( wavelength( wNdx ), 200._dk ), 2000._dk )
      sig = 1.e3_dk / wrk
      sigsq = sig * sig

      wrk = 8342.13_dk + 2406030._dk / ( 130._dk - sigsq )                    &
          + 15997._dk / ( 38.9_dk - sigsq )

      ! adjust to local air density
      wrk = wrk * atmDensity / divisor

      ! index of refraction
      refrac(wNdx) = rONE + 1.e-8_dk * wrk
    enddo

    end function refraction

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_cross_section_o3_tint
