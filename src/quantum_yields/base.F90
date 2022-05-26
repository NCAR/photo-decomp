! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This base quantum yield module

!> The base quantum yield type and related functions
module tuvx_quantum_yield_base

  use tuvx_quantum_yield,     only : abs_quantum_yield_t
  use musica_constants,                only : dk => musica_dk, ik => musica_ik

  implicit none

  private
  public :: base_quantum_yield_t

  integer(ik), parameter :: iONE = 1_ik
  real(dk), parameter    :: rZERO = 0.0_dk
  real(dk), parameter    :: rONE  = 1.0_dk

  type quantum_yield_parms_t
    real(dk), allocatable :: temperature(:)
    real(dk), allocatable :: array(:,:)
  end type quantum_yield_parms_t

  !> Calculator for base quantum yield
  type, extends(abs_quantum_yield_t) :: base_quantum_yield_t
    type(quantum_yield_parms_t), allocatable :: quantum_yield_parms(:)
  contains
    !> Initialize the quantum yield
    procedure :: initialize
    !> Calculate the quantum yield
    procedure :: calculate => run
    !> clean up
    final     :: finalize
  end type base_quantum_yield_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize base quantum yield_t object
  subroutine initialize( this, config, gridWareHouse, ProfileWareHouse )

    use musica_config,                   only : config_t
    use musica_string,                   only : string_t
    use tuvx_netcdf_util,                     only : netcdf_t
    use tuvx_util,                     only : inter2
    use musica_assert,                   only : die_msg
    use tuvx_grid_warehouse,             only : grid_warehouse_t
    use tuvx_grid,                    only : abs_1d_grid_t
    use tuvx_profile_warehouse,          only : Profile_warehouse_t

    !> New base quantum yield calculator
    !> Arguments
    class(base_quantum_yield_t), intent(inout) :: this
    !> quantum yield configuration data
    type(config_t),              intent(inout) :: config
    !> The warehouses
    type(grid_warehouse_t),      intent(inout) :: gridWareHouse
    type(Profile_warehouse_t),   intent(inout) :: ProfileWareHouse

    !> Local variables
    character(len=*), parameter :: Iam = 'base quantum yield constructor: '
    character(len=*), parameter :: Hdr = 'quantum_yield_'

    integer(ik) :: retcode
    integer(ik) :: parmNdx, fileNdx
    integer(ik) :: nParms
    real(dk)    :: quantum_yield_constant
    real(dk), allocatable :: data_lambda(:)
    real(dk), allocatable :: data_parameter(:)
    logical :: found
    character(len=:), allocatable :: msg
    type(netcdf_t), allocatable   :: netcdf_obj
    type(string_t)                :: Handle
    type(string_t), allocatable   :: netcdfFiles(:)
    class(abs_1d_grid_t), pointer :: lambdaGrid

    write(*,*) Iam,'entering'

    !> Get model wavelength grid
    Handle = 'Photolysis, wavelength' ; lambdaGrid => gridWareHouse%get_grid( Handle )

    !> get quantum yield netcdf filespec
    call config%get( 'netcdf files', netcdfFiles, Iam, found=found )
has_netcdf_file: &
    if( found ) then
      allocate( this%quantum_yield_parms(size(netcdfFiles)) )
file_loop: &
      do fileNdx = iONE,size(netcdfFiles) 
        allocate( netcdf_obj )
    !> read netcdf file quantum yield data
        call netcdf_obj%read_netcdf_file( filespec=netcdfFiles(fileNdx)%to_char(), Hdr=Hdr )
        nParms = size(netcdf_obj%parameters,dim=2)
        if( nParms < iONE ) then
          write(msg,*) Iam//'File: ',trim(netcdfFiles(fileNdx)%to_char()),'  parameters array has < 1 parameter'
          call die_msg( 410000002, msg )
        endif
    !> interpolate from data to model wavelength grid
        if( allocated(netcdf_obj%wavelength) ) then
          if( .not. allocated(this%quantum_yield_parms(fileNdx)%array) ) then
            allocate(this%quantum_yield_parms(fileNdx)%array(lambdaGrid%ncells_,nParms))
          endif
          do parmNdx = iONE,nParms
            data_lambda    = netcdf_obj%wavelength
            data_parameter = netcdf_obj%parameters(:,parmNdx)
            call this%addpnts( config, data_lambda, data_parameter )
            call inter2(xto=lambdaGrid%edge_, &
                        yto=this%quantum_yield_parms(fileNdx)%array(:,parmNdx), &
                        xfrom=data_lambda, &
                        yfrom=data_parameter,ierr=retcode)
          enddo
        else
          this%quantum_yield_parms(fileNdx)%array = netcdf_obj%parameters
        endif
        if( allocated(netcdf_obj%temperature) ) then
          this%quantum_yield_parms(fileNdx)%temperature = netcdf_obj%temperature
        endif
        deallocate( netcdf_obj )
      enddo file_loop
    else has_netcdf_file
    !> check for quantum yield constant
      call config%get( 'quantum yield constant', quantum_yield_constant, Iam, found=found )
      if( found ) then
        allocate( this%quantum_yield_parms(1) )
        allocate(this%quantum_yield_parms(1)%array(lambdaGrid%ncells_,1))
        this%quantum_yield_parms(1)%array(:,1) = quantum_yield_constant
      endif
    endif has_netcdf_file

    write(*,*) Iam,'exiting'
   
  end subroutine initialize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the quantum yield
  function run( this, gridWareHouse, ProfileWareHouse ) result( quantum_yield )

    use tuvx_grid_warehouse,     only : grid_warehouse_t
    use tuvx_grid,            only : abs_1d_grid_t
    use tuvx_profile_warehouse,  only : Profile_warehouse_t
    use musica_string,           only : string_t

    !> called quantum yield object
    class(base_quantum_yield_t), intent(in)  :: this
    !> The warehouses
    type(grid_warehouse_t),    intent(inout) :: gridWareHouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse
    !> Calculated quantum yield
    real(dk), allocatable                    :: quantum_yield(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = 'base quantum yield calculate: '
    integer(ik)                 :: vertNdx
    class(abs_1d_grid_t), pointer :: zGrid
    type(string_t)              :: Handle
    real(dk), allocatable       :: wrkQuantumYield(:,:)

    write(*,*) Iam,'entering'

    Handle = 'Vertical Z' ; zGrid => gridWareHouse%get_grid( Handle )

    allocate( wrkQuantumYield(size(this%quantum_yield_parms(1)%array,dim=1),zGrid%ncells_+1) )

    !> Just copy the lambda interpolated array
    do vertNdx = iONE,zGrid%ncells_+iONE
      wrkQuantumYield(:,vertNdx) = this%quantum_yield_parms(1)%array(:,iONE)
    enddo

    quantum_yield = transpose( wrkQuantumYield )

    write(*,*) Iam,'exiting'

  end function run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!> finalize the quantum yield object
   subroutine finalize( this )

   type(base_quantum_yield_t), intent(inout) :: this

   character(len=*), parameter :: Iam = 'base quantum yield finalize: '
   integer(ik) :: ndx

   write(*,*) Iam,'entering'

   if( allocated(this%quantum_yield_parms) ) then
     do ndx = 1,size(this%quantum_yield_parms)
       if( allocated(this%quantum_yield_parms(ndx)%array ) ) then
         deallocate(this%quantum_yield_parms(ndx)%array )
       endif
       if( allocated(this%quantum_yield_parms(ndx)%temperature ) ) then
         deallocate(this%quantum_yield_parms(ndx)%temperature )
       endif
     enddo
     deallocate(this%quantum_yield_parms)
   endif

   write(*,*) Iam,'exiting'
   
   end subroutine finalize

end module tuvx_quantum_yield_base
