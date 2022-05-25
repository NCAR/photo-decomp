! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This hno3->oh+no2 cross_section module

!> The hno3->oh+no2_cross_section type and related functions
module tuvx_cross_section_hno3_oh_no2

  use tuvx_cross_section_base,    only : base_cross_section_t
  use musica_constants,                        only : dk => musica_dk, ik => musica_ik, lk => musica_lk
  use musica_string,                           only : string_t
  use tuvx_grid_warehouse,                     only : grid_warehouse_t
  use tuvx_profile_warehouse,                  only : Profile_warehouse_t
  use tuvx_grid,                            only : abs_1d_grid_t

  implicit none

  private
  public :: hno3_oh_no2_cross_section_t

  integer(ik), parameter :: iONE  = 1_ik
  real(dk), parameter    :: rZERO = 0.0_dk
  real(dk), parameter    :: rONE  = 1.0_dk

  !> Calculator for hno3-oh_no2 cross section
  type, extends(base_cross_section_t) :: hno3_oh_no2_cross_section_t
  contains
    !> Initialize the cross section
    procedure :: initialize
    !> Calculate the cross section
    procedure :: calculate => run
  end type hno3_oh_no2_cross_section_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize base_cross_section_t object
  subroutine initialize( this, config, gridWareHouse, ProfileWareHouse, atMidPoint )

    use musica_config,                   only : config_t
    use tuvx_netcdf_util,                     only : netcdf_t
    use tuvx_util,                     only : inter2
    use musica_assert,                   only : die_msg

    !> base cross section type
    class(hno3_oh_no2_cross_section_t), intent(inout) :: this
    !> cross section configuration object
    type(config_t), intent(inout)            :: config
    type(grid_warehouse_t), intent(inout)    :: gridWareHouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse
    logical(lk), intent(in), optional        :: atMidPoint

    !> local variables
    character(len=*), parameter :: Iam = 'hno3 cross section initialize: '
    character(len=*), parameter :: Hdr = 'cross_section_'

    integer(ik) :: retcode
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

    Handle = 'Photolysis, wavelength' ; lambdaGrid => gridWareHouse%get_grid( Handle )

    !> get cross section netcdf filespec
    call config%get( 'netcdf files', netcdfFiles, Iam, found=found )

has_netcdf_file: &
    if( found ) then
      allocate( this%cross_section_parms(size(netcdfFiles)) )
file_loop: &
      do fileNdx = 1,size(this%cross_section_parms)
        allocate( netcdf_obj )
    !> read netcdf cross section parameters
        call netcdf_obj%read_netcdf_file( filespec=netcdfFiles(fileNdx)%to_char(), Hdr=Hdr )
        nParms = size(netcdf_obj%parameters,dim=2)
        if( nParms < 1 ) then
          write(msg,*) Iam//'File: ',trim(netcdfFiles(fileNdx)%to_char()),'  parameters array has < 1 column'
          call die_msg( 400000002, msg )
        endif

    !> interpolate from data to model wavelength grid
        if( allocated(netcdf_obj%wavelength) ) then
          if( .not. allocated(this%cross_section_parms(fileNdx)%array) ) then
            allocate(this%cross_section_parms(fileNdx)%array(lambdaGrid%ncells_,nParms))
          endif
          do parmNdx = 1,nParms
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

    use tuvx_profile, only : abs_Profile_t
    !> arguments
    class(hno3_oh_no2_cross_section_t), intent(in) :: this
    !> cross section configuration object
    type(grid_warehouse_t), intent(inout)    :: gridWareHouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse
    logical(lk), intent(in), optional        :: atMidPoint
    !> Calculated cross section
    real(kind=dk), allocatable               :: cross_section(:,:)

    !> local variables
    character(len=*), parameter :: Iam = 'hno3->oh+no2 cross section calculate: '
    real(dk), parameter         :: T0 = 298._dk
    integer(ik)           :: vertNdx
    real(dk), allocatable :: Temp(:)
    type(string_t)                :: Handle
    class(abs_1d_grid_t), pointer :: lambdaGrid
    class(abs_1d_grid_t), pointer :: zGrid
    class(abs_Profile_t), pointer :: temperature

    write(*,*) Iam,'entering'

    Handle = 'Photolysis, wavelength' ; lambdaGrid  => gridWareHouse%get_grid( Handle )
    Handle = 'Vertical Z'             ; zGrid       => gridWareHouse%get_grid( Handle )
    Handle = 'Temperature'            ; temperature => ProfileWareHouse%get_Profile( Handle )

    allocate( cross_section(lambdaGrid%ncells_,zGrid%ncells_+iONE) )

    Temp = temperature%edge_val_ - T0
    do vertNdx = iONE,zGrid%ncells_+iONE
      cross_section(:,vertNdx) = this%cross_section_parms(1)%array(:,1)*exp( this%cross_section_parms(1)%array(:,2)*Temp(vertNdx) )
    enddo

    cross_section = transpose( cross_section )

    write(*,*) Iam,'exiting'

  end function run

end module tuvx_cross_section_hno3_oh_no2
