! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
module tuvx_spherical_geometry

      use musica_constants, only : ik => musica_ik, dk => musica_dk
      use musica_string, only    : string_t
      use tuvx_constants, only : radius, pi

      implicit none

      private
      public :: spherical_geom_t

      type :: spherical_geom_t
        integer(ik), allocatable :: nid_(:)
        real(dk)                 :: SolarZenithAngle_
        real(dk), allocatable    :: dsdh_(:,:)  
      contains
        procedure :: setSphericalParams
        procedure :: airmas
        final     :: finalize
      end type spherical_geom_t

      real(dk), parameter ::  rZERO   = 0.0_dk
      integer(dk), parameter :: iONE  = 1_ik
      real(dk), parameter ::  d2r     = pi/180._dk
      real(dk), parameter ::  NINETY  = 90._dk

      !> Constructor
      interface spherical_geom_t
        module procedure constructor
      end interface spherical_geom_t

      contains

      function constructor( gridWareHouse ) result( this )
          use tuvx_grid_warehouse, only : grid_warehouse_t
          use tuvx_grid, only        : grid_t

          type(spherical_geom_t), pointer :: this
          !> Arguments
          type(grid_warehouse_t), intent(inout)  :: gridWareHouse

          !> Local variables
          character(len=*), parameter            :: Iam = 'sphers initialize: '

          class(grid_t), pointer          :: zGrid

          write(*,*) ' '
          write(*,*) Iam // 'entering'

          allocate( this )

          zGrid => gridWareHouse%get_grid( "height", "km" )

          allocate( this%nid_(0:zGrid%ncells_) )
          allocate( this%dsdh_(0:zGrid%ncells_,zGrid%ncells_) )

          deallocate( zGrid )
          write(*,*) ' '
          write(*,*) Iam // 'exiting'
      end function constructor

      subroutine setSphericalParams(this, zen, gridWareHouse)
!-----------------------------------------------------------------------------*
!=  purpose:                                                                 =*
!=  calculate slant path over vertical depth ds/dh in spherical geometry.    =*
!=  calculation is based on:  a.dahlback, and k.stamnes, a new spheric model =*
!=  for computing the radiation field available for photolysis and heating   =*
!=  at twilight, planet.space sci., v39, n5, pp. 671-683, 1991 (appendix b)  =*
!-----------------------------------------------------------------------------*
!=  parameters:                                                              =*
!=  nz      - integer, number of specified altitude levels in the working (i)=*
!=            grid                                                           =*
!=  z       - real(dk), specified altitude working grid (km)                  (i)=*
!=  zen     - real(dk), solar zenith angle (degrees)                          (i)=*
!=  dsdh    - real(dk), slant path of direct beam through each layer crossed  (o)=*
!=            when travelling from the top of the atmosphere to layer i;     =*
!=            dsdh(i,j), i = 0..nz-1, j = 1..nz-1                            =*
!=  nid     - integer, number of layers crossed by the direct beam when   (o)=*
!=            travelling from the top of the atmosphere to layer i;          =*
!=            nid(i), i = 0..nz-1                                            =*
!-----------------------------------------------------------------------------*
!=  edit history:                                                            =*
!=  double precision fix for shallow layers - julia lee-taylor dec 2000      =*
!-----------------------------------------------------------------------------*
!= this program is free software;  you can redistribute it and/or modify     =*
!= it under the terms of the gnu general public license as published by the  =*
!= free software foundation;  either version 2 of the license, or (at your   =*
!= option) any later version.                                                =*
!= the tuv package is distributed in the hope that it will be useful, but    =*
!= without any warranty;  without even the implied warranty of merchantibi-  =*
!= lity or fitness for a particular purpose.  see the gnu general public     =*
!= license for more details.                                                 =*
!= to obtain a copy of the gnu general public license, write to:             =*
!= free software foundation, inc., 675 mass ave, cambridge, ma 02139, usa.   =*
!-----------------------------------------------------------------------------*
!= to contact the authors, please mail to:                                   =*
!= sasha madronich, ncar/acd, p.o.box 3000, boulder, co, 80307-3000, usa  or =*
!= send email to:  sasha@ucar.edu                                            =*
!-----------------------------------------------------------------------------*

      use tuvx_grid_warehouse, only : grid_warehouse_t
      use tuvx_grid, only        : grid_t
 
! input
      real(dk), intent(in) :: zen
      class(spherical_geom_t), intent(inout) :: this
      type(grid_warehouse_t), intent(inout)  :: gridWareHouse

! more program constants
      real(dk) :: re
      real(dk), allocatable :: ze(:)

! local 
      character(len=*), parameter            :: Iam = 'sphers setSphericalParams: '
      integer(ik) :: nz
      integer(ik) :: i, j, k
      integer(ik) :: id
      integer(ik) :: nlayer
      real(dk)    :: sinrad, zenrad, rpsinz, rj, rjp1, dsj, dhj, ga, gb, sm
      real(dk), allocatable    :: zd(:)

      class(grid_t), pointer :: zGrid => null( )

      write(*,*) ' '
      write(*,*) Iam // 'entering'

      zenrad = zen*d2r

      zGrid => gridWareHouse%get_grid( "height", "km" )

      nlayer = zGrid%ncells_
      nz     = nlayer + iONE
! number of layers:

! include the elevation above sea level to the radius of the earth:
      re = radius + zGrid%edge_(iONE)
! correspondingly z changed to the elevation above earth surface:
      ze = zGrid%edge_ - zGrid%edge_(iONE)

      allocate( zd(0:nlayer) )
! inverse coordinate of z
      zd(0:nlayer) = ze(nz:iONE:-iONE)

! initialize dsdh, nid
      this%nid_  = 0_ik
      this%dsdh_ = rZERO

      sinrad = sin(zenrad)
! calculate ds/dh of every layer
      layer_loop: do i = 0_ik, nlayer
        rpsinz = (re + zd(i)) * sinrad
        if ( zen > NINETY .and. rpsinz < re ) then
          this%nid_(i) = -iONE
        else
!
! find index of layer in which the screening height lies
!
          if( zen <= NINETY ) then
            id = i
          else
            do j = iONE, nlayer
              if( rpsinz < (zd(j-1) + re) .and. &
                  rpsinz >= (zd(j) + re)  ) id = j
            enddo
          end if
 
          do j = iONE, id
            sm = 1.0_dk
            if(j == id .and. id == i .and. zen > NINETY) sm = -1.0_dk
            rj = re + zd(j-iONE)
            rjp1 = re + zd(j)
            dhj = zd(j-iONE) - zd(j)
            ga = rj*rj - rpsinz*rpsinz
            gb = rjp1*rjp1 - rpsinz*rpsinz
            ga = max( rZERO,ga )
            gb = max( rZERO,gb )
 
            if(id > i .and. j == id) then
              dsj = sqrt( ga )
            else
              dsj = sqrt( ga ) - sm*sqrt( gb )
            end if
            this%dsdh_(i,j) = dsj / dhj
          enddo
          this%nid_(i) = id
        end if
      enddo layer_loop

      this%SolarZenithAngle_ = zen

      deallocate( zGrid )

      write(*,*) ' '
      write(*,*) Iam // 'exiting'

      end subroutine setSphericalParams

      subroutine airmas(this, aircol, vcol, scol)
!-----------------------------------------------------------------------------*
!=  purpose:                                                                 =*
!=  calculate vertical and slant air columns, in spherical geometry, as a    =*
!=  function of altitude.                                                    =*
!-----------------------------------------------------------------------------*
!=  parameters:                                                              =*
!=  nz      - integer, number of specified altitude levels in the working (i)=*
!=            grid                                                           =*
!=  dsdh    - real(dk), slant path of direct beam through each layer crossed  (o)=*
!=            when travelling from the top of the atmosphere to layer i;     =*
!=            dsdh(i,j), i = 0..nz-1, j = 1..nz-1                            =*
!=  nid     - integer, number of layers crossed by the direct beam when   (o)=*
!=            travelling from the top of the atmosphere to layer i;          =*
!=            nid(i), i = 0..nz-1                                            =*
!=  vcol    - real(dk), output, vertical air column, molec cm-2, above level iz  =*
!=  scol    - real(dk), output, slant air column in direction of sun, above iz   =*
!=            also in molec cm-2                                             =*
!-----------------------------------------------------------------------------*

      use tuvx_constants, only : largest

! input:
      real(dk), intent(in)    :: aircol(:)
      class(spherical_geom_t), intent(in) :: this

! output: 
      real(dk), intent(out)   :: vcol(:), scol(:)

! internal:

      integer(ik) :: nz, nlayer
      integer(ik) :: id, j
      real(dk)    :: accum

! calculate vertical and slant column from each level:
! work downward

      nz = size(aircol)
      nlayer = nz - iONE
      accum = aircol(nz)
      do id = nlayer,iONE,-iONE
        accum = accum + aircol(id)
        vcol(id) = accum
      enddo

      scol(nz) = this%dsdh_(iONE,iONE)*aircol(nz)
      do id = iONE, nlayer
         accum = scol(nz)
         if(this%nid_(id) < 0_ik) then
            accum = largest
         else
! single pass layers:
            do j = iONE, min(this%nid_(id), id)
               accum = accum + aircol(nz-j)*this%dsdh_(id,j)
            enddo
! double pass layers:
            do j = min(this%nid_(id),id)+iONE, this%nid_(id)
               accum = accum + 2._dk*aircol(nz-j)*this%dsdh_(id,j)
            enddo
         endif
         scol(nz - id) = accum
      enddo
      
      end subroutine airmas

      subroutine finalize( this )

      type(spherical_geom_t), intent(inout) :: this

      if( allocated(this%nid_) ) then
        deallocate(this%nid_)
      endif
      if( allocated(this%dsdh_) ) then
        deallocate(this%dsdh_)
      endif

      end subroutine finalize

end module tuvx_spherical_geometry
