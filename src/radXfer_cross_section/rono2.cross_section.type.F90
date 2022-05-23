! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This rono2 cross_section module

!> The rono2_cross_section type and related functions
module micm_radXfer_rono2_cross_section_type

  use micm_radXfer_base_cross_section_type,    only : base_cross_section_t
  use musica_constants,                only : dk => musica_dk, ik => musica_ik, lk => musica_lk

  implicit none

  private
  public :: rono2_cross_section_t

  integer(ik), parameter :: iONE = 1_ik
  real(dk), parameter :: rZERO = 0.0_dk
  real(dk), parameter :: rONE  = 1.0_dk

  !> Calculator for rono2 cross section
  type, extends(base_cross_section_t) :: rono2_cross_section_t
  contains
    !> Initialize the cross section
    procedure :: initialize
    !> Calculate the cross section
    procedure :: calculate => run
  end type rono2_cross_section_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize base_cross_section_t object
  subroutine initialize( this, config, gridWareHouse, ProfileWareHouse, atMidPoint )

    use musica_config,                   only : config_t
    use musica_string,                   only : string_t
    use netcdf_util,                     only : netcdf_t
    use photo_utils,                     only : inter2
    use musica_assert,                   only : die_msg
    use micm_grid_warehouse,             only : grid_warehouse_t
    use micm_1d_grid,                    only : abs_1d_grid_t
    use micm_Profile_warehouse,          only : Profile_warehouse_t

    !> Arguments
    class(rono2_cross_section_t), intent(inout) :: this
    !> cross section configuration object
    type(config_t), intent(inout)               :: config
    logical(lk), optional, intent(in)           :: atMidPoint
    !> The warehouses
    type(grid_warehouse_t), intent(inout)       :: gridWareHouse
    type(Profile_warehouse_t), intent(inout)    :: ProfileWareHouse

    !> Local variables
    character(len=*), parameter :: Iam = 'rono2 cross section initialize: '
    character(len=*), parameter :: Hdr = 'cross_section_'

    integer(ik) :: retcode, nmdlLambda
    integer(ik) :: parmNdx, fileNdx, nParms
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
    class(abs_1d_grid_t), pointer :: lambdaGrid

    write(*,*) Iam,'entering'

    !> Get model wavelength grids
    Handle = 'Photolysis, wavelength' ; lambdaGrid => gridWareHouse%get_grid( Handle )

    !> get cross section netcdf filespec
    call config%get( 'netcdf files', netcdfFiles, Iam, found=found )

has_netcdf_file: &
    if( found ) then
      allocate( this%cross_section_parms(size(netcdfFiles)) )
file_loop: &
      do fileNdx = iONE,size(this%cross_section_parms)
        allocate( netcdf_obj )
    !> read netcdf cross section parameters
        call netcdf_obj%read_netcdf_file( filespec=netcdfFiles(fileNdx)%to_char(), Hdr=Hdr )
        nParms = size(netcdf_obj%parameters,dim=2)
        if( nParms < iONE ) then
          write(msg,*) Iam//'File: ',trim(netcdfFiles(fileNdx)%to_char()),'  parameters array has < 1 column'
          call die_msg( 400000002, msg )
        endif

    !> interpolate from data to model wavelength grid
        if( allocated(netcdf_obj%wavelength) ) then
          if( .not. allocated(this%cross_section_parms(fileNdx)%array) ) then
            allocate(this%cross_section_parms(fileNdx)%array(lambdaGrid%ncells_,nParms))
          endif
          do parmNdx = iONE,nParms
            data_lambda    = netcdf_obj%wavelength
            data_parameter = netcdf_obj%parameters(:,parmNdx)
            if( parmNdx == 1 ) then
              call this%addpnts( config, data_lambda, data_parameter )
            elseif( parmNdx == 2 ) then
              tmp_config = config
              addpntKey = 'lower extrapolation'
              addpntVal = 'boundary'
              call tmp_config%add( addpntKey, addpntVal, Iam )
              addpntKey = 'upper extrapolation'
              addpntVal = 'boundary'
              call tmp_config%add( addpntKey, addpntVal, Iam )
              call this%addpnts( tmp_config, data_lambda, data_parameter )
            endif
            call inter2(xto=lambdaGrid%edge_, &
                        yto=this%cross_section_parms(fileNdx)%array(:,parmNdx), &
                        xfrom=data_lambda, &
                        yfrom=data_parameter,ierr=retcode)
          enddo
        else
          this%cross_section_parms(fileNdx)%array = netcdf_obj%parameters
        endif
        if( allocated(netcdf_obj%temperature) ) then
          this%cross_section_parms(fileNdx)%temperature = netcdf_obj%temperature
        endif
        deallocate( netcdf_obj )
      enddo file_loop
    endif has_netcdf_file

    write(*,*) Iam,'exiting'

  end subroutine initialize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the photorate cross section for a given set of environmental conditions
  function run( this, gridWareHouse, ProfileWareHouse, atMidPoint ) result( cross_section )

    use micm_grid_warehouse,         only : grid_warehouse_t
    use micm_1d_grid,                only : abs_1d_grid_t
    use micm_Profile_warehouse,      only : Profile_warehouse_t
    use micm_Profile,                only : abs_Profile_t
    use musica_string,               only : string_t

    !> Arguments
    class(rono2_cross_section_t), intent(in) :: this
    logical(lk), optional, intent(in)        :: atMidPoint
    !> The warehouses
    type(grid_warehouse_t), intent(inout)    :: gridWareHouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse
    !> Calculated cross section
    real(kind=dk), allocatable               :: cross_section(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = 'rono2 cross section calculate: '
    integer(ik) :: vertNdx
    integer(ik) :: nzdim
    real(dk), parameter         :: T0 = 298._dk
    real(dk) :: Temp
    real(dk), allocatable :: modelTemp(:)
    class(abs_1d_grid_t), pointer :: zGrid, lambdaGrid
    class(abs_Profile_t), pointer :: mdlTemperature
    type(string_t)                :: Handle

    write(*,*) Iam,'entering'

    Handle = 'Photolysis, wavelength' ; lambdaGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Vertical Z'             ; zGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Temperature'            ; mdlTemperature => ProfileWareHouse%get_Profile( Handle )

    nzdim = zGrid%ncells_ + iONE
    if( present(atMidPoint) ) then
      if( atMidpoint ) then
        nzdim = nzdim - iONE
        modelTemp = mdlTemperature%mid_val_
      else
        modelTemp = mdlTemperature%edge_val_
      endif
    else
      modelTemp = mdlTemperature%edge_val_
    endif

    allocate( cross_section(lambdaGrid%ncells_,nzdim) )
    cross_section = rZERO

    do vertNdx = iONE,nzdim
      Temp = modelTemp(vertNdx) - T0
      cross_section(:,vertNdx) = this%cross_section_parms(1)%array(:,1)*exp( this%cross_section_parms(1)%array(:,2)*Temp )
    enddo

    cross_section = transpose( cross_section )

    write(*,*) Iam,'exiting'

  end function run

end module micm_radXfer_rono2_cross_section_type
