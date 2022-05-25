! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This bro+hv->br_o cross_section module

!> The bro+hv->br+o_cross_section type and related functions
module tuvx_cross_section_bro_br_o

  use tuvx_cross_section_base,    only : base_cross_section_t

  implicit none

  private
  public :: bro_br_o_cross_section_t

  !> Calculator for base_cross_section
  type, extends(base_cross_section_t) :: bro_br_o_cross_section_t
  contains
    !> Initialize the cross section
    procedure :: initialize
  end type bro_br_o_cross_section_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize bro_br_o_cross_section_t object
  subroutine initialize( this, config, gridWareHouse, ProfileWareHouse, atMidPoint )

    use musica_constants,                only : dk => musica_dk, ik => musica_ik, lk => musica_lk
    use musica_config,                   only : config_t
    use musica_string,                   only : string_t
    use tuvx_netcdf_util,                     only : netcdf_t
    use tuvx_util,                     only : inter4
    use musica_assert,                   only : die_msg
    use tuvx_grid_warehouse,             only : grid_warehouse_t
    use tuvx_grid,                    only : abs_1d_grid_t
    use tuvx_profile_warehouse,          only : Profile_warehouse_t

    !> Arguments
    class(bro_br_o_cross_section_t), intent(inout) :: this
    logical(lk), optional, intent(in)              :: atMidPoint
    !> cross section configuration object
    type(config_t), intent(inout)                  :: config
    !> The warehouses
    type(grid_warehouse_t), intent(inout)          :: gridWareHouse
    type(Profile_warehouse_t), intent(inout)       :: ProfileWareHouse

    !> Local variables
    character(len=*), parameter :: Iam = 'bro->br+o cross section initialize: '
    character(len=*), parameter :: Hdr = 'cross_section_'
    integer(ik), parameter      :: iONE = 1_ik

    integer(ik) :: nParms
    integer(ik) :: parmNdx, fileNdx
    real(dk), allocatable :: data_lambda(:)
    real(dk), allocatable :: data_parameter(:)
    logical(ik) :: found
    character(len=:), allocatable :: msg
    type(netcdf_t), allocatable :: netcdf_obj
    type(string_t)              :: Handle
    type(string_t), allocatable :: netcdfFiles(:)
    class(abs_1d_grid_t), pointer :: lambdaGrid
    class(abs_1d_grid_t), pointer :: zGrid

    write(*,*) Iam,'entering'

    !> Get model wavelength grids
    Handle = 'Photolysis, wavelength' ; lambdaGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Vertical Z'             ; zGrid => gridWareHouse%get_grid( Handle )

    !> Get cross section netcdf filespec
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
            call inter4(xto=lambdaGrid%edge_, &
                        yto=this%cross_section_parms(fileNdx)%array(:,parmNdx), &
                        xfrom=data_lambda, &
                        yfrom=data_parameter,Foldin=1)
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

end module tuvx_cross_section_bro_br_o
