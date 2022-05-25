! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> This tint quantum yield module

!> The tint quantum yield type and related functions
module tuvx_quantum_yield_no2_tint

  use tuvx_quantum_yield,     only : abs_quantum_yield_t
  use musica_constants,                only : dk => musica_dk, ik => musica_ik, lk => musica_lk
  use tuvx_grid_warehouse,             only : grid_warehouse_t
  use tuvx_grid,                    only : abs_1d_grid_t
  use tuvx_profile_warehouse,          only : Profile_warehouse_t
  use musica_string,                   only : string_t

  implicit none

  private
  public :: no2_tint_quantum_yield_t

  integer(ik), parameter :: iONE  = 1_ik
  integer(ik), parameter :: iTWO  = 2_ik
  real(dk), parameter    :: rZERO = 0.0_dk
  real(dk), parameter    :: rONE  = 1.0_dk

  type quantum_yield_t
    real(dk), allocatable :: temperature(:)
    real(dk), allocatable :: deltaT(:)
    real(dk), allocatable :: array(:,:)
  end type quantum_yield_t

  !> Calculator for tint quantum yield
  type, extends(abs_quantum_yield_t) :: no2_tint_quantum_yield_t
    type(quantum_yield_t), allocatable :: quantum_yield(:)
  contains
    !> Initialize the quantum yield
    procedure :: initialize
    !> Calculate the quantum yield
    procedure :: calculate => run
    !> clean up
    final     :: finalize
  end type no2_tint_quantum_yield_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize no2 tint quantum yield_t object
  subroutine initialize( this, config, gridWareHouse, ProfileWareHouse )

    use musica_config,                   only : config_t
    use tuvx_netcdf_util,                     only : netcdf_t
    use tuvx_util,                     only : inter2
    use musica_assert,                   only : die_msg

    !> quantum yield configuration data
    type(config_t), intent(inout) :: config
    !> this object
    class(no2_tint_quantum_yield_t), intent(inout) :: this
    !> The warehouses
    type(grid_warehouse_t),      intent(inout) :: gridWareHouse
    type(Profile_warehouse_t),   intent(inout) :: ProfileWareHouse

    !> Local variables
    character(len=*), parameter :: Iam = 'no2 tint quantum yield constructor: '
    character(len=*), parameter :: Hdr = 'quantum_yield_'

    integer(ik) :: retcode, nmdlLambda
    integer(ik) :: nTemps, nParms
    integer(ik) :: parmNdx, fileNdx, Ndxl, Ndxu
    real(dk)    :: tmp
    real(dk)    :: quantum_yield_constant
    real(dk), allocatable :: data_lambda(:)
    real(dk), allocatable :: data_parameter(:)
    logical(lk) :: found, monopos
    character(len=:), allocatable :: msg
    type(string_t)                :: Handle
    type(netcdf_t), allocatable   :: netcdf_obj
    type(string_t), allocatable   :: netcdfFiles(:)
    class(abs_1d_grid_t), pointer :: lambdaGrid

    write(*,*) Iam,'entering'

    !> Get model wavelength grid
    Handle = 'Photolysis, wavelength' ; lambdaGrid => gridWareHouse%get_grid( Handle )

    !> get quantum yield netcdf filespec
    call config%get( 'netcdf files', netcdfFiles, Iam, found=found )
has_netcdf_file: &
    if( found ) then
      allocate( this%quantum_yield(size(netcdfFiles)) )
file_loop: &
      do fileNdx = iONE,size(netcdfFiles) 
        allocate( netcdf_obj )
    !> read netcdf file quantum yield data
        call netcdf_obj%read_netcdf_file( filespec=netcdfFiles(fileNdx)%to_char(), Hdr=Hdr )
        nParms = size(netcdf_obj%parameters,dim=2)
        if( nParms < iTWO ) then
          write(msg,*) Iam//'File: ',trim(netcdfFiles(fileNdx)%to_char()),'  array must have 2 or more parameters'
          call die_msg( 400000002, msg )
        endif
        associate( Qyield => this%quantum_yield(fileNdx) )
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
            Qyield%deltaT = Qyield%temperature(iTWO:nParms) - Qyield%temperature(iONE:nParms-iONE)
          endif
        else
          write(msg,*) Iam//'File: ',trim(netcdfFiles(fileNdx)%to_char()),' does not have interpolation temperatures'
          call die_msg( 400000006, msg )
        endif
    !> interpolate from data to model wavelength grid
        if( allocated(netcdf_obj%wavelength) ) then
          if( .not. allocated(this%quantum_yield(fileNdx)%array) ) then
            allocate(this%quantum_yield(fileNdx)%array(lambdaGrid%ncells_,nParms))
          endif
          do parmNdx = iONE,nParms
            data_lambda    = netcdf_obj%wavelength
            data_parameter = netcdf_obj%parameters(:,parmNdx)
            call this%addpnts( config, data_lambda, data_parameter )
            call inter2(xto=lambdaGrid%edge_, &
                        yto=this%quantum_yield(fileNdx)%array(:,parmNdx), &
                        xfrom=data_lambda, &
                        yfrom=data_parameter,ierr=retcode)
          enddo
        else
          this%quantum_yield(fileNdx)%array = netcdf_obj%parameters
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

    !> Arguments
    !> this object
    class(no2_tint_quantum_yield_t), intent(in) :: this
    !> The warehouses
    type(grid_warehouse_t),       intent(inout) :: gridWareHouse
    type(Profile_warehouse_t),    intent(inout) :: ProfileWareHouse
    !> Calculated quantum yield
    real(kind=dk), allocatable                  :: quantum_yield(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = 'no2 tint quantum yield calculate: '
    integer(ik) :: nTemp
    integer(ik) :: fileNdx, tNdx, vertNdx
    real(dk)    :: Tadj, Tstar
    real(dk), allocatable :: WrkQuantumYield(:,:)
    type(string_t)                :: Handle
    class(abs_1d_grid_t), pointer :: zGrid
    class(abs_1d_grid_t), pointer :: lambdaGrid
    class(abs_Profile_t), pointer :: mdlTemperature

    write(*,*) Iam,'entering'

    Handle = 'Vertical Z'             ; zGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength' ; lambdaGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Temperature'            ; mdlTemperature => ProfileWareHouse%get_Profile( Handle )

    allocate( wrkQuantumYield(lambdaGrid%ncells_,zGrid%ncells_+iONE) )
    wrkQuantumYield = rZERO

    do fileNdx = iONE,size(this%quantum_yield)
      associate( Temp => this%quantum_yield(fileNdx)%temperature, wrkQyield => this%quantum_yield(fileNdx) )
      nTemp = size( Temp )
      do vertNdx = iONE,zGrid%ncells_+iONE
        Tadj  = mdltemperature%edge_val_(vertNdx)
        do tNdx = iTWO,nTemp 
          if( Tadj <= Temp(tNdx) ) then
            exit
          endif
        enddo
        tndx = min( nTemp,tNdx ) - iONE
        Tstar = (Tadj - Temp(tNdx))/wrkQyield%deltaT(tNdx)
        WrkQuantumYield(:,vertNdx) = WrkQuantumYield(:,vertNdx) + wrkQyield%array(:,tNdx) &
                    + Tstar * (wrkQyield%array(:,tNdx+iONE) - wrkQyield%array(:,tNdx))
      enddo
      end associate
    enddo

    quantum_yield = transpose( max(WrkQuantumYield,rZERO) )

    write(*,*) Iam,'exiting'

  end function run

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!> finalize the quantum yield type

   subroutine finalize( this )

   type(no2_tint_quantum_yield_t), intent(inout) :: this

   character(len=*), parameter :: Iam = 'no2 tint quantum yield finalize: '
   integer(ik) :: ndx

   write(*,*) Iam,'entering'
   if( allocated(this%quantum_yield) ) then
     do ndx = 1,size(this%quantum_yield)
       associate( Qyield => this%quantum_yield(ndx) )
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
     deallocate(this%quantum_yield)
   endif

   write(*,*) Iam,'exiting'

   end subroutine finalize

end module tuvx_quantum_yield_no2_tint
