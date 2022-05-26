! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This tint quantum yield module

!> The tint quantum yield type and related functions
module tuvx_quantum_yield_tint

  use musica_constants,                only : dk => musica_dk, ik => musica_ik
  use musica_string,                   only : string_t
  use tuvx_quantum_yield,     only : abs_quantum_yield_t
  use tuvx_grid_warehouse,             only : grid_warehouse_t
  use tuvx_profile_warehouse,          only : Profile_warehouse_t
  use tuvx_grid,                    only : abs_1d_grid_t

  implicit none

  private
  public :: tint_quantum_yield_t

  integer(ik), parameter :: iONE = 1_ik
  integer(ik), parameter :: iTWO = 2_ik
  real(dk), parameter :: rZERO   = 0.0_dk
  real(dk), parameter :: rONE    = 1.0_dk

  type quantum_yield_t
    real(dk), allocatable :: temperature(:)
    real(dk), allocatable :: deltaT(:)
    real(dk), allocatable :: array(:,:)
  end type quantum_yield_t

  !> Calculator for tint quantum yield
  type, extends(abs_quantum_yield_t) :: tint_quantum_yield_t
    type(quantum_yield_t), allocatable :: quantum_yield_parms(:)
  contains
    !> Initialize the quantum yield
    procedure :: initialize
    !> Calculate the quantum yield
    procedure :: calculate => run
    !> clean up
    final     :: finalize
  end type tint_quantum_yield_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize tint quantum yield_t object
  subroutine initialize( this, config, gridWareHouse, ProfileWareHouse )

    use musica_config,                   only : config_t
    use tuvx_netcdf_util,                     only : netcdf_t
    use tuvx_util,                     only : inter2
    use musica_assert,                   only : die_msg

    !> quantum yield configuration data
    !> Arguments
    !> New tint quantum yield calculator
    class(tint_quantum_yield_t), intent(inout) :: this
    type(config_t),              intent(inout) :: config
    type(grid_warehouse_t),      intent(inout) :: gridWareHouse
    type(Profile_warehouse_t),   intent(inout) :: ProfileWareHouse

    !> Local variables
    character(len=*), parameter :: Iam = 'tint quantum yield constructor: '
    character(len=*), parameter :: Hdr = 'quantum_yield_'

    integer(ik) :: retcode
    integer(ik) :: parmNdx, nParms
    integer(ik) :: nTemps
    integer(ik) :: fileNdx, Ndxl, Ndxu
    real(dk)    :: tmp
    real(dk)    :: quantum_yield_constant
    real(dk), allocatable :: data_lambda(:)
    real(dk), allocatable :: data_parameter(:)
    logical     :: found, monopos
    character(len=:), allocatable :: msg
    type(netcdf_t),   allocatable :: netcdf_obj
    type(string_t)                :: Handle
    type(string_t),   allocatable :: netcdfFiles(:)
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
      do fileNdx = 1,size(netcdfFiles) 
        allocate( netcdf_obj )
    !> read netcdf file quantum yield data
        call netcdf_obj%read_netcdf_file( filespec=netcdfFiles(fileNdx)%to_char(), Hdr=Hdr )
        nParms = size(netcdf_obj%parameters,dim=2)
        if( nParms < iTWO ) then
          write(msg,*) Iam//'File: ',trim(netcdfFiles(fileNdx)%to_char()),'  array must have 2 or more parameters'
          call die_msg( 420000002, msg )
        endif
        associate( Qyield => this%quantum_yield_parms(fileNdx) )
    !> interpolation temperatures must be in netcdf file
        if( allocated(netcdf_obj%temperature) ) then
          Qyield%temperature = netcdf_obj%temperature
          nTemps = size( Qyield%temperature )
    !> must have two or more interpolation temperatures
          if( nTemps < iTWO ) then
            write(msg,*) Iam//'File: ',trim(netcdfFiles(fileNdx)%to_char()),'  temperature array has < 2 entries'
            call die_msg( 400000003, msg )
          elseif( nTemps < nParms ) then
            write(msg,*) Iam//'File: ',trim(netcdfFiles(fileNdx)%to_char()),'  temperature array < number parameters'
            call die_msg( 400000004, msg )
          endif
          Qyield%deltaT = Qyield%temperature(iTWO:nParms) - Qyield%temperature(iONE:nParms-iONE)
          monopos = all( Qyield%deltaT > rZERO )        
          if( .not. monopos ) then
            if( any( Qyield%deltaT > rZERO ) ) then
              write(msg,*) Iam//'File: ',trim(netcdfFiles(fileNdx)%to_char()),'  temperature array not monotonic'
              call die_msg( 400000005, msg )
            endif
            do Ndxl = iONE,nParms/iTWO
              Ndxu = nParms - Ndxl + iONE
              tmp = Qyield%temperature(Ndxl)
              Qyield%temperature(Ndxl) = Qyield%temperature(Ndxu)
              Qyield%temperature(Ndxu) = tmp
              data_parameter = netcdf_obj%parameters(:,Ndxl)
              netcdf_obj%parameters(:,Ndxl) = netcdf_obj%parameters(:,Ndxu)
              netcdf_obj%parameters(:,Ndxu) = data_parameter
            enddo
            Qyield%deltaT = Qyield%temperature(2:nParms) - Qyield%temperature(1:nParms-1)
          endif
        else
          write(msg,*) Iam//'File: ',trim(netcdfFiles(fileNdx)%to_char()),' does not have interpolation temperatures'
          call die_msg( 400000006, msg )
        endif
    !> interpolate from data to model wavelength grid
        if( allocated(netcdf_obj%wavelength) ) then
          if( .not. allocated(this%quantum_yield_parms(fileNdx)%array) ) then
            allocate(this%quantum_yield_parms(fileNdx)%array(lambdaGrid%ncells_,nParms))
          endif
          do parmNdx = 1,nParms
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
        end associate
        deallocate( netcdf_obj )
      enddo file_loop
    else has_netcdf_file
      write(msg,*) Iam//'must have at least one netcdf input file'
      call die_msg( 400000008, msg )
    endif has_netcdf_file

    write(*,*) Iam,'exiting'
   
  end subroutine initialize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate the photorate quantum yield for the environmental conditions
  function run( this, gridWareHouse, ProfileWareHouse ) result( quantum_yield )

    use tuvx_profile, only : abs_Profile_t

    !> tint quantum yield
    !> Arguments
    class(tint_quantum_yield_t),    intent(in) :: this
    type(grid_warehouse_t),      intent(inout) :: gridWareHouse
    type(Profile_warehouse_t),   intent(inout) :: ProfileWareHouse
    !> Calculated quantum yield
    real(dk), allocatable                      :: quantum_yield(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = 'tint quantum yield calculate: '
    integer(ik) :: nTemp
    integer(ik) :: fileNdx, tNdx, vertNdx
    real(dk)    :: Tadj, Tstar
    type(string_t) :: Handle
    class(abs_1d_grid_t), pointer :: lambdaGrid
    class(abs_1d_grid_t), pointer :: zGrid
    class(abs_Profile_t), pointer :: Temperature

    write(*,*) Iam,'entering'

    Handle = 'Photolysis, wavelength' ; lambdaGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Vertical Z'  ; zGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Temperature' ; Temperature => ProfileWareHouse%get_profile( Handle )

    allocate( quantum_yield(lambdaGrid%ncells_,zGrid%ncells_+1) )

    quantum_yield = rZERO
file_loop: &
    do fileNdx = iONE,size(this%quantum_yield_parms)
      associate( Temp => this%quantum_yield_parms(fileNdx)%temperature, wrkQyield => this%quantum_yield_parms(fileNdx) )
      nTemp = size( Temp )
      do vertNdx = iONE,zGrid%ncells_+iONE
        Tadj = min( max( Temperature%edge_val_(vertNdx),Temp(iONE) ),Temp(nTemp) )
        do tNdx = iTWO,nTemp 
          if( Tadj <= Temp(tNdx) ) then
            exit
          endif
        enddo
        tNdx = tNdx - 1
        Tstar = (Tadj - Temp(tNdx))/wrkQyield%deltaT(tNdx)
        quantum_yield(:,vertNdx) = quantum_yield(:,vertNdx) + wrkQyield%array(:,tNdx) &
                      + Tstar * (wrkQyield%array(:,tNdx+1) - wrkQyield%array(:,tNdx))
      enddo
      end associate
    enddo file_loop

    quantum_yield = transpose( quantum_yield )

    write(*,*) Iam,'exiting'

  end function run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!> finalize the quantum yield type

   subroutine finalize( this )

   type(tint_quantum_yield_t), intent(inout) :: this

   character(len=*), parameter :: Iam = 'tint quantum yield finalize: '
   integer(ik) :: ndx

   write(*,*) Iam,'entering'
   if( allocated(this%quantum_yield_parms) ) then
     do ndx = 1,size(this%quantum_yield_parms)
       associate( Qyield => this%quantum_yield_parms(ndx) )
       if( allocated(Qyield%array) ) then
         deallocate(Qyield%array)
       endif
       if( allocated(Qyield%temperature) ) then
         deallocate(Qyield%temperature)
       endif
       if( allocated(Qyield%deltaT) ) then
         deallocate(Qyield%deltaT)
       endif
       end associate
     enddo
     deallocate(this%quantum_yield_parms)
   endif

   write(*,*) Iam,'exiting'

   end subroutine finalize

end module tuvx_quantum_yield_tint
