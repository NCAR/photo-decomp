! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This o2_cross_section module

!> The o2_cross_section type and related functions
module tuvx_cross_section_o2

  use musica_constants, only : musica_dk, musica_ik
  use tuvx_cross_section, only : base_cross_section_t, cross_section_parms_t
  use la_srb_type,      only : la_srb_t

  implicit none

  private
  public :: o2_cross_section_t

  !> Calculator for o2_cross_section
  type, extends(base_cross_section_t) :: o2_cross_section_t
    type(la_srb_t), allocatable              :: la_srb_obj_
  contains
    !> Calculate the cross section
    procedure :: calculate => run
    !> clean up
    final     :: finalize
  end type o2_cross_section_t

  !> Constructor
  interface o2_cross_section_t
    module procedure constructor
  end interface o2_cross_section_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize o2_cross_section_t object
  function constructor( config, gridWareHouse, ProfileWareHouse ) result( this )

    use musica_config,                   only : config_t
    use musica_string,                   only : string_t
    use tuvx_netcdf_util,                     only : netcdf_t
    use tuvx_util,                     only : inter2
    use musica_assert,                   only : die_msg
    use tuvx_grid_warehouse,             only : grid_warehouse_t
    use tuvx_grid,                    only : abs_1d_grid_t
    use tuvx_profile_warehouse,          only : Profile_warehouse_t
    use tuvx_profile,                    only : abs_Profile_t

    !> o2 cross section type
    type(o2_cross_section_t), pointer :: this
    !> cross section configuration object
    type(config_t), intent(inout) :: config
    !> The warehouses
    type(grid_warehouse_t), intent(inout)    :: gridWareHouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse

!   local variables
    integer(musica_ik), parameter :: iONE = 1_musica_ik
    real(musica_dk), parameter :: rZERO   = 0.0_musica_dk
    real(musica_dk), parameter :: rONE    = 1.0_musica_dk
    character(len=*), parameter :: Iam = 'radXfer o2 cross section initialize: '
    character(len=*), parameter :: Hdr = 'cross_section_'

    integer(musica_ik) :: retcode
    integer(musica_ik) :: parmNdx, fileNdx
    integer(musica_ik) :: nParms
    real(musica_dk), allocatable :: data_lambda(:)
    real(musica_dk), allocatable :: data_parameter(:)
    logical :: found
    character(len=:), allocatable :: msg
    type(netcdf_t), allocatable :: netcdf_obj
    type(string_t)              :: Handle
    type(string_t), allocatable :: netcdfFiles(:)
    class(abs_1d_grid_t), pointer :: lambdaGrid

    write(*,*) Iam,'entering'

    allocate( this )

    !> Get model wavelength grids
    Handle = 'Photolysis, wavelength'
    lambdaGrid => gridWareHouse%get_grid( Handle )

    !> allocate la_srb object
    if( allocated(this%la_srb_obj_) ) then
      deallocate(this%la_srb_obj_)
    endif
    allocate( this%la_srb_obj_ )

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
            call this%addpnts( config, data_lambda, data_parameter )
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

    call this%la_srb_obj_%initialize( lambdaGrid )

    write(*,*) Iam,'exiting'

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the photorate cross section for a given set of environmental conditions
  function run( this, gridWareHouse, ProfileWareHouse ) result( cross_section )

    use tuvx_grid_warehouse,         only : grid_warehouse_t
    use tuvx_grid,                only : abs_1d_grid_t
    use tuvx_profile_warehouse, only : Profile_warehouse_t
    use musica_string,               only : string_t

    !> base cross section
    class(o2_cross_section_t), intent(in)    :: this
    !> The warehouses
    type(grid_warehouse_t), intent(inout)    :: gridWareHouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse
    !> Calculated cross section
    real(kind=musica_dk), allocatable        :: cross_section(:,:)

    !> Local variables
    integer :: colndx
    character(len=*), parameter :: Iam = 'radXfer o2 cross section calculate: '
    class(abs_1d_grid_t), pointer :: zGrid
    type(string_t)                :: Handle
    real(musica_dk), allocatable  :: wrkCrossSection(:,:)

    write(*,*) Iam,'entering'

    Handle = 'Vertical Z'
    zGrid => gridWareHouse%get_grid( Handle )

    allocate( wrkCrossSection(size(this%cross_section_parms(1)%array,dim=1),zGrid%ncells_) )

    !> Just copy the lambda interpolated array
    do colndx = 1,zGrid%ncells_
      wrkCrossSection(:,colndx) = this%cross_section_parms(1)%array(:,1)
    enddo
 
    cross_section = transpose( wrkCrossSection )

    write(*,*) Iam,'exiting'

  end function run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!> finalize the cross section type
   subroutine finalize( this )

   type(o2_cross_section_t), intent(inout) :: this

   character(len=*), parameter :: Iam = 'base cross section finalize: '
   integer(musica_ik) :: ndx

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

   if( allocated(this%la_srb_obj_) ) then
     deallocate(this%la_srb_obj_)
   endif

   write(*,*) Iam,'exiting'
   
   end subroutine finalize

end module tuvx_cross_section_o2
