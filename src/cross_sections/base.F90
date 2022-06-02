! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This base_cross_section module

!> The base_cross_section type and related functions
module tuvx_cross_section

  use musica_constants, only : dk => musica_dk, ik => musica_ik, lk => musica_lk

  implicit none

  private
  public :: base_cross_section_t, cross_section_parms_t, base_constructor, base_cross_section_ptr

  type cross_section_parms_t
    real(dk), allocatable :: temperature(:)
    real(dk), allocatable :: deltaT(:)
    real(dk), allocatable :: array(:,:)
  end type cross_section_parms_t

  !> Calculator for base_cross_section
  type :: base_cross_section_t
    !> The cross section array
    type(cross_section_parms_t), allocatable :: cross_section_parms(:)
  contains
    !> Calculate the cross section
    procedure :: calculate => run
    procedure :: addpnts
    !> clean up
    final     :: finalize
  end type base_cross_section_t

  !> Pointer type for building sets of photo rate constants
  type :: base_cross_section_ptr
    class(base_cross_section_t), pointer :: val_ => null( )
  end type base_cross_section_ptr

  integer(ik), parameter :: iONE = 1_ik
  real(dk), parameter    :: rZERO = 0.0_dk
  real(dk), parameter    :: rONE  = 1.0_dk

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize base_cross_section_t object
  subroutine base_constructor( base_component, config, gridWareHouse, ProfileWareHouse, atMidPoint )

    use musica_config,                   only : config_t
    use musica_string,                   only : string_t
    use tuvx_netcdf_util,                     only : netcdf_t
    use tuvx_util,                     only : inter2
    use musica_assert,                   only : die_msg
    use tuvx_grid_warehouse,             only : grid_warehouse_t
    use tuvx_grid,                    only : abs_1d_grid_t
    use tuvx_profile_warehouse,          only : Profile_warehouse_t
    use tuvx_profile,                    only : abs_Profile_t

    !> base cross section type
    class(base_cross_section_t), pointer :: base_component
    logical(lk), optional, intent(in)          :: atMidPoint
    !> cross section configuration object
    type(config_t), intent(inout) :: config
    !> The warehouses
    type(grid_warehouse_t), intent(inout)    :: gridWareHouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse

    !   local variables
    character(len=*), parameter   :: Iam = 'base cross section initialize: '
    character(len=*), parameter   :: Hdr = 'cross_section_'

    integer(ik) :: retcode
    integer(ik) :: parmNdx, fileNdx
    integer(ik) :: nParms
    real(dk), allocatable :: data_lambda(:)
    real(dk), allocatable :: data_parameter(:)
    logical :: found
    character(len=:), allocatable :: msg
    type(netcdf_t), allocatable :: netcdf_obj
    type(string_t)              :: Handle
    type(string_t), allocatable :: netcdfFiles(:)
    class(abs_1d_grid_t), pointer :: lambdaGrid

    write(*,*) Iam,'entering'

    !> Get model wavelength grids
    Handle = 'Photolysis, wavelength'
    lambdaGrid => gridWareHouse%get_grid( Handle )

    !> get cross section netcdf filespec
    call config%get( 'netcdf files', netcdfFiles, Iam, found=found )

has_netcdf_file: &
    if( found ) then
      allocate( base_component%cross_section_parms(size(netcdfFiles)) )
file_loop: &
      do fileNdx = iONE,size(base_component%cross_section_parms)
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
          if( .not. allocated(base_component%cross_section_parms(fileNdx)%array) ) then
            allocate(base_component%cross_section_parms(fileNdx)%array(lambdaGrid%ncells_,nParms))
          endif
          do parmNdx = iONE,nParms
            data_lambda    = netcdf_obj%wavelength
            data_parameter = netcdf_obj%parameters(:,parmNdx)
            call base_component%addpnts( config, data_lambda, data_parameter )
            call inter2(xto=lambdaGrid%edge_, &
                        yto=base_component%cross_section_parms(fileNdx)%array(:,parmNdx), &
                        xfrom=data_lambda, &
                        yfrom=data_parameter,ierr=retcode)
          enddo
        else
          base_component%cross_section_parms(fileNdx)%array = netcdf_obj%parameters
        endif
        if( allocated(netcdf_obj%temperature) ) then
          base_component%cross_section_parms(fileNdx)%temperature = netcdf_obj%temperature
        endif
        deallocate( netcdf_obj )
      enddo file_loop
    endif has_netcdf_file

    write(*,*) Iam,'exiting'

  end subroutine base_constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the photorate cross section for a given set of environmental conditions
  function run( this, gridWareHouse, ProfileWareHouse, atMidPoint ) result( cross_section )

    use tuvx_grid_warehouse,         only : grid_warehouse_t
    use tuvx_grid,                only : abs_1d_grid_t
    use tuvx_profile_warehouse,      only : Profile_warehouse_t
    use musica_string,               only : string_t

    !> base cross section
    class(base_cross_section_t), intent(in)    :: this
    logical(lk), optional, intent(in)          :: atMidPoint
    !> The warehouses
    type(grid_warehouse_t), intent(inout)      :: gridWareHouse
    type(Profile_warehouse_t), intent(inout)   :: ProfileWareHouse
    !> Calculated cross section
    real(kind=dk), allocatable          :: cross_section(:,:)

    !> Local variables
    integer(ik) :: colndx
    integer(ik) :: nzdim
    character(len=*), parameter :: Iam = 'radXfer base cross section calculate: '
    class(abs_1d_grid_t), pointer :: zGrid
    type(string_t)                :: Handle
    real(dk), allocatable         :: wrkCrossSection(:,:)

    write(*,*) Iam,'entering'

    Handle = 'Vertical Z'
    zGrid => gridWareHouse%get_grid( Handle )

    nzdim = zGrid%ncells_ + iONE
    if( present(atMidPoint) ) then
      if( atMidpoint ) then
        nzdim = nzdim - iONE
      endif
    endif

    allocate( wrkCrossSection(size(this%cross_section_parms(1)%array,dim=1),nzdim) )

    !> Just copy the lambda interpolated array
    do colndx = iONE,nzdim
      wrkCrossSection(:,colndx) = this%cross_section_parms(1)%array(:,1)
    enddo
 
    cross_section = transpose( wrkCrossSection )

    write(*,*) Iam,'exiting'

  end function run

  subroutine addpnts( this, config, data_lambda, data_parameter )
    use musica_config, only : config_t
    use musica_string, only : string_t
    use tuvx_util,   only : addpnt

    class(base_cross_section_t)    :: this
    type(config_t), intent(inout) :: config
    real(dk), allocatable, intent(inout) :: data_lambda(:)
    real(dk), allocatable, intent(inout) :: data_parameter(:)

    real(dk), parameter :: rZERO = 0.0_dk
    real(dk), parameter :: rONE  = 1.0_dk
    real(dk), parameter :: deltax = 1.e-5_dk
    character(len=*), parameter :: Iam = 'cross_section; addpnts: '

    integer(ik) :: nRows
    real(dk) :: lowerLambda, upperLambda
    real(dk) :: addpnt_val_lower, addpnt_val_upper
    type(string_t)  :: addpnt_type_
    logical         :: found
    character(len=:), allocatable :: number

    write(*,*) Iam,'entering'

    !> add endpoints to data arrays; first the lower bound
    nRows = size(data_lambda)
    lowerLambda = data_lambda(1) ; upperLambda = data_lambda(nRows)
    call config%get( 'lower extrapolation', addpnt_type_, Iam, found=found )
    if( .not. found ) then
      addpnt_val_lower = rZERO
    elseif( addpnt_type_ == 'boundary' ) then
      addpnt_val_lower = data_parameter(1)
    else
      number = addpnt_type_%to_char()
      read( number, '(g30.20)' ) addpnt_val_lower
    endif

    !> add endpoints to data arrays; now the upper bound
    call config%get( 'upper extrapolation', addpnt_type_, Iam, found=found )
    if( .not. found ) then
      addpnt_val_upper = rZERO
    elseif( addpnt_type_ == 'boundary' ) then
      addpnt_val_upper = data_parameter(nRows)
    else
      number = addpnt_type_%to_char()
      read( number, '(g30.20)' ) addpnt_val_upper
    endif

    call addpnt(x=data_lambda,y=data_parameter,xnew=(rONE-deltax)*lowerLambda,ynew=addpnt_val_lower) 
    call addpnt(x=data_lambda,y=data_parameter,xnew=rZERO,ynew=addpnt_val_lower) 
    call addpnt(x=data_lambda,y=data_parameter,xnew=(rONE+deltax)*upperLambda,ynew=addpnt_val_upper) 
    call addpnt(x=data_lambda,y=data_parameter,xnew=1.e38_dk,ynew=addpnt_val_upper) 

    write(*,*) Iam,'exiting'

  end subroutine addpnts

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!> finalize the cross section type
   subroutine finalize( this )

   type(base_cross_section_t), intent(inout) :: this

   character(len=*), parameter :: Iam = 'base cross section finalize: '
   integer(ik) :: ndx

   write(*,*) Iam,'entering'

   if( allocated(this%cross_section_parms) ) then
     do ndx = 1,size(this%cross_section_parms)
       if( allocated(this%cross_section_parms(ndx)%array ) ) then
         deallocate(this%cross_section_parms(ndx)%array )
       endif
       if( allocated(this%cross_section_parms(ndx)%temperature ) ) then
         deallocate(this%cross_section_parms(ndx)%temperature )
       endif
     enddo
     deallocate(this%cross_section_parms)
   endif

   write(*,*) Iam,'exiting'
   
   end subroutine finalize

end module tuvx_cross_section
