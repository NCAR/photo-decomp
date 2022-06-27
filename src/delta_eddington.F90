! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
module tuvx_delta_eddington

   use tuvx_radiative_transfer_solver, only : abstract_radXfer_t, radField_t
   use musica_constants,      only : ik => musica_ik, dk => musica_dk
   use tuvx_constants,            only : pi

   implicit none

   private
   public :: delta_eddington_t

   type, extends(abstract_radXfer_t) :: delta_eddington_t
   contains
     procedure :: upDateRadField
   end type delta_eddington_t

   real(dk), parameter :: rZERO = 0.0_dk
   real(dk), parameter :: rONE  = 1.0_dk
   real(dk), parameter :: rTWO  = 2.0_dk
   real(dk), parameter :: d2r   = pi/180._dk

   contains

   function upDateRadField( this, sza, nstr, nlyr, &
                            sphericalGeom, gridWareHouse, ProfileWareHouse, radiatorWareHouse ) &
                           result( radField )

   use musica_string,                   only : string_t
   use tuvx_radiator,          only : radiator_state_t
   use tuvx_grid_warehouse,             only : grid_warehouse_t
   use tuvx_profile_warehouse,          only : Profile_warehouse_t
   use tuvx_radiator_warehouse,         only : radiator_warehouse_t
   use tuvx_radiator_warehouse,         only : warehouse_iterator_t
   use tuvx_spherical_geometry,             only : spherical_geom_t
   use tuvx_grid,                    only : grid_t
   use tuvx_profile,                    only : profile_t
   use tuvx_radiator,          only : radiator_t
   use tuvx_linear_algebra_linpack,                 only : linalgebra_t
   use tuvx_diagnostic_util,                           only : diagout

   !> Arguments
   class(delta_eddington_t), intent(inout) :: this

   integer(ik), intent(in) :: nlyr
   integer(ik), intent(in) :: nstr                          ! not used in delta eddington
   real(dk), intent(in)    :: sza
   type(grid_warehouse_t), intent(inout)     :: gridWareHouse
   type(Profile_warehouse_t), intent(inout)  :: ProfileWareHouse
   type(radiator_warehouse_t), intent(inout) :: radiatorWareHouse
   type(spherical_geom_t), intent(inout)     :: sphericalGeom

   class(radField_t), pointer                :: radField

   !> Local variables
   character(len=*), parameter :: Iam = 'upDateRadField: '
   real(dk) :: sum
   real(dk) :: mu
   real(dk) :: tausla(0:nlyr), tauc(0:nlyr)
   real(dk) :: mu2(0:nlyr)

! internal coefficients and matrix
   integer(ik) :: row
   real(dk)    :: lam(nlyr),taun(nlyr),bgam(nlyr)
   real(dk)    :: e1(nlyr),e2(nlyr),e3(nlyr),e4(nlyr)
   real(dk)    :: cup(nlyr),cdn(nlyr)
   real(dk)    :: cuptn(nlyr),cdntn(nlyr)
   real(dk)    :: mu1(nlyr)
   real(dk)    :: a(2*nlyr),b(2*nlyr),d(2*nlyr)
   real(dk)    :: e(2*nlyr),y(2*nlyr)

   real(dk) :: pifs, fdn0, surfem, tempg
   real(dk) :: f, g, om
   real(dk) :: gam1, gam2, gam3, gam4
   real(dk) :: gi(nlyr), omi(nlyr)

   integer(ik) :: mrows, lev
   integer(ik) :: i, j
   real(dk) :: expon, expon0, expon1, divisr, temp, up, dn
   real(dk) :: ssfc

   !> Linear algebra package, radiation field type
   type(linalgebra_t) :: linpack

    !> Local variables
    real(dk), parameter                  :: largest = 1.e36_dk
    real(dk), parameter                  :: kfloor = rONE/largest
    real(dk), parameter                  :: precis = 1.e-7_dk
    real(dk), parameter                  :: eps    = 1.e-3_dk

    integer(ik)                          :: nlambda, lambdaNdx
    real(dk), allocatable                :: dscat(:,:)
    real(dk), allocatable                :: dscat_accum(:,:)
    real(dk), allocatable                :: dabs_accum(:,:)
    real(dk), allocatable                :: asym_accum(:,:)
    type(string_t)                       :: Handle
    type(warehouse_iterator_t), pointer  :: iter => null( )
    class(radiator_t),          pointer  :: aRadiator
    type(radiator_state_t)               :: atmRadiatorState
    class(grid_t),    pointer            :: zGrid => null( )
    class(grid_t),    pointer            :: lambdaGrid => null( )
    class(profile_t), pointer            :: surfaceAlbedo => null( )

    write(*,*) ' '
    write(*,*) Iam // 'entering'

    allocate( radField )

    Handle = 'Vertical Z' ; zGrid => GridWareHouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength' ; lambdaGrid => GridWareHouse%get_grid( Handle )
    Handle = 'Surface albedo' ; surfaceAlbedo => ProfileWareHouse%get_Profile( Handle )

    nlambda = lambdaGrid%ncells_
    allocate( dscat(nlyr,nlambda) )
    allocate( dscat_accum, mold=dscat )
    allocate( dabs_accum,  mold=dscat )
    allocate( asym_accum,  mold=dscat )
    allocate( radField%edr_(nlyr+1,nlambda) )
    allocate( radField%edn_, mold=radField%edr_ )
    allocate( radField%eup_, mold=radField%edr_ )
    allocate( radField%fdr_, mold=radField%edr_ )
    allocate( radField%fdn_, mold=radField%edr_ )
    allocate( radField%fup_, mold=radField%edr_ )
    !> initialize the accumulators
    dscat_accum = rZERO
    dabs_accum  = rZERO
    asym_accum  = rZERO

    !> iterate over radiators accumulating radiative properties
    iter => RadiatorWareHouse%get_iterator()
    do while( iter%next() )
      aRadiator => RadiatorWareHouse%get_radiator( iter )
      write(*,*) Iam // 'doing radiator ',aRadiator%handle_
      associate( OD => aRadiator%state_%layer_OD_, SSA => aRadiator%state_%layer_SSA_, &
                 G  => aRadiator%state_%layer_G_ )
        write(*,*) Iam // 'shape OD = ',size(OD,dim=1),' x ',size(OD,dim=2)
        write(*,*) Iam // 'shape OD = ',size(aRadiator%state_%layer_OD_,dim=1),' x ', &
                                        size(aRadiator%state_%layer_OD_,dim=2)
        dscat       = OD * SSA
        dscat_accum = dscat_accum + dscat
        dabs_accum  = dabs_accum + OD*(rONE - SSA)
        asym_accum  = asym_accum + G*dscat
      end associate
      nullify( aRadiator )
    enddo
    deallocate( iter )

    !> set atmosphere radiative properties
    dscat_accum = max( dscat_accum, kfloor )
    dabs_accum  = max( dabs_accum, kfloor )

      atmRadiatorState%layer_OD_ = dscat_accum + dabs_accum
      allocate( atmRadiatorState%layer_SSA_, mold = atmRadiatorState%layer_OD_ )
      where( dscat_accum == kfloor )
        atmRadiatorState%layer_SSA_ = kfloor
      elsewhere
        atmRadiatorState%layer_SSA_ = dscat_accum/atmRadiatorState%layer_OD_
      endwhere

      atmRadiatorState%layer_G_ = asym_accum/dscat_accum
    
! MU = cosine of solar zenith angle
! RSFC = surface albedo
! TAUU =  unscaled optical depth of each layer
! OMU  =  unscaled single scattering albedo
! GU   =  unscaled asymmetry factor
! NLAYER = number of layers in the atmosphere
! NLEVEL = nlayer + 1 = number of levels

   mu = cos( sza*d2r )
   associate( nid  => sphericalGeom%nid_, &
              dsdh => sphericalGeom%dsdh_ )

wavelength_loop: &
   do lambdaNdx = 1,nlambda
      associate( rsfc => surfaceAlbedo%mid_val_(lambdaNdx), &
                 tauu => atmRadiatorState%layer_OD_(nlyr:1:-1,lambdaNdx), &
                 omu  => atmRadiatorState%layer_SSA_(nlyr:1:-1,lambdaNdx), & 
                 gu   => atmRadiatorState%layer_G_(nlyr:1:-1,lambdaNdx) )

! initial conditions:  pi*solar flux = 1;  diffuse incidence = 0
      pifs = rONE
      fdn0 = rZERO
! emission at surface (for night light pollution, set pifs = 0, surfem = 1.)
      surfem = rZERO
!************* compute coefficients for each layer:
! GAM1 - GAM4 = 2-stream coefficients, different for different approximations
! EXPON0 = calculation of e when TAU is zero
! EXPON1 = calculation of e when TAU is TAUN
! CUP and CDN = calculation when TAU is zero
! CUPTN and CDNTN = calc. when TAU is TAUN
! DIVISR = prevents division by zero
      tauc   = rZERO
      tausla = rZERO
      mu2    = rONE/SQRT(largest)
! delta-scaling. Has to be done for delta-Eddington approximation, 
! delta discrete ordinate, Practical Improved Flux Method, delta function,
! and Hybrid modified Eddington-delta function methods approximations

      DO i = 1, nlyr
        f      = gu(i)*gu(i)
        gi(i)  = (gu(i) - f)/(rONE - f)
        omi(i) = (rONE - f)*omu(i)/(rONE - omu(i)*f)       
        taun(i) = (rONE - omu(i)*f)*tauu(i)
      ENDDO

      if( lambdaNdx == 1_ik ) then
        call diagout( 'tauu.new',tauu )
        call diagout( 'gu.new',gu )
        call diagout( 'omu.new',omu )
        call diagout( 'taun.new',taun )
      endif

! calculate slant optical depth at the top of the atmosphere when zen>90.
! in this case, higher altitude of the top layer is recommended which can 
! be easily changed in gridz.f.

      IF(mu < rZERO) THEN
        IF(nid(0) < 0) THEN
          tausla(0) = largest
        ELSE
          sum = rZERO
          DO j = 1, nid(0)
           sum = sum + rTWO*taun(j)*dsdh(0,j)
          END DO
          tausla(0) = sum 
        END IF
      END IF

      layer_loop: DO i = 1, nlyr

         g  = gi(i)
         om = omi(i)
         tauc(i) = tauc(i-1) + taun(i)

! stay away from 1 by precision.  For g, also stay away from -1

         tempg = MIN(abs(g),rONE - precis)
         g = SIGN(tempg,g)
         om = MIN(om,rONE-precis)


! calculate slant optical depth

         IF(nid(i) < 0) THEN
           tausla(i) = largest
         ELSE
           sum = rZERO
           DO j = 1, MIN(nid(i),i)
              sum = sum + taun(j)*dsdh(i,j)
           ENDDO
           DO j = MIN(nid(i),i)+1,nid(i)
              sum = sum + rTWO*taun(j)*dsdh(i,j)
           ENDDO
           tausla(i) = sum 
           IF(tausla(i) == tausla(i-1)) THEN
             mu2(i) = SQRT(largest)
           ELSE
             mu2(i) = (tauc(i) - tauc(i-1))/(tausla(i) - tausla(i-1))
             mu2(i) = SIGN( MAX(ABS(mu2(i)),rONE/SQRT(largest)),mu2(i) )
           END IF
         END IF

!** the following gamma equations are from pg 16,289, Table 1
!** save mu1 for each approx. for use in converting irradiance to actinic flux

! Eddington approximation(Joseph et al., 1976, JAS, 33, 2452):

         gam1 =  (7._dk - om*(4._dk + 3._dk*g))/4._dk
         gam2 = -(rONE - om*(4._dk - 3._dk*g))/4._dk
         gam3 = (rTWO - 3._dk*g*mu)/4._dk
         gam4 = rONE - gam3
         mu1(i) = 0.5_dk

         lam(i) = sqrt(gam1*gam1 - gam2*gam2)

         IF( gam2 /= rZERO) THEN
           bgam(i) = (gam1 - lam(i))/gam2
         ELSE
           bgam(i) = rZERO
         ENDIF

         expon = EXP(-lam(i)*taun(i))

! e1 - e4 = pg 16,292 equation 44
         
         e1(i) = rONE + bgam(i)*expon
         e2(i) = rONE - bgam(i)*expon
         e3(i) = bgam(i) + expon
         e4(i) = bgam(i) - expon

! the following sets up for the C equations 23, and 24
! found on page 16,290
! prevent division by zero (if LAMBDA=1/MU, shift 1/MU^2 by EPS = 1.E-3
! which is approx equiv to shifting MU by 0.5*EPS* (MU)**3

         expon0 = EXP(-tausla(i-1))
         expon1 = EXP(-tausla(i))
          
         divisr = lam(i)*lam(i) - rONE/(mu2(i)*mu2(i))
         temp   = MAX(eps,abs(divisr))
         divisr = SIGN(temp,divisr)

         up = om*pifs*((gam1 - rONE/mu2(i))*gam3 + gam4*gam2)/divisr
         dn = om*pifs*((gam1 + rONE/mu2(i))*gam4 + gam2*gam3)/divisr
         
! cup and cdn are when tau is equal to zero
! cuptn and cdntn are when tau is equal to taun

         cup(i) = up*expon0
         cdn(i) = dn*expon0
         cuptn(i) = up*expon1
         cdntn(i) = dn*expon1

      ENDDO layer_loop

      if( lambdaNdx == 1_ik ) then
        call diagout( 'e1.new',e1 )
        call diagout( 'e2.new',e2 )
        call diagout( 'e3.new',e3 )
        call diagout( 'e4.new',e4 )
        call diagout( 'cup.new',cup )
        call diagout( 'cdn.new',cdn )
        call diagout( 'cuptn.new',cuptn )
        call diagout( 'cdntn.new',cdntn )
        call diagout( 'lam.new',lam )
        call diagout( 'mu2.new',mu2 )
      endif

!**************** set up matrix ******
! ssfc = pg 16,292 equation 37  where pi Fs is one (unity).

      ssfc = rsfc*mu*EXP(-tausla(nlyr))*pifs + surfem

! MROWS = the number of rows in the matrix

      mrows = 2*nlyr     
      
! the following are from pg 16,292  equations 39 - 43.
! set up first row of matrix:

      a(1) = rZERO
      b(1) = e1(1)
      d(1) = -e2(1)
      e(1) = fdn0 - cdn(1)

! set up odd rows 3 thru (MROWS - 1):

      i = 0
      DO row = 3, mrows - 1, 2
         i = i + 1
         a(row) = e2(i)*e3(i) - e4(i)*e1(i)
         b(row) = e1(i)*e1(i + 1) - e3(i)*e3(i + 1)
         d(row) = e3(i)*e4(i + 1) - e1(i)*e2(i + 1)
         e(row) = e3(i)*(cup(i + 1) - cuptn(i)) &
                + e1(i)*(cdntn(i) - cdn(i + 1))
      ENDDO

! set up even rows 2 thru (MROWS - 2): 

      i = 0
      DO row = 2, mrows - 2, 2
         i = i + 1
         a(row) = e2(i + 1)*e1(i) - e3(i)*e4(i + 1)
         b(row) = e2(i)*e2(i + 1) - e4(i)*e4(i + 1)
         d(row) = e1(i + 1)*e4(i + 1) - e2(i + 1)*e3(i + 1)
         e(row) = (cup(i + 1) - cuptn(i))*e2(i + 1) &
                - (cdn(i + 1) - cdntn(i))*e4(i + 1)
      ENDDO

! set up last row of matrix at MROWS:

      a(mrows) = e1(nlyr) - rsfc*e3(nlyr)
      b(mrows) = e2(nlyr) - rsfc*e4(nlyr)
      d(mrows) = rZERO
      e(mrows) = ssfc - cuptn(nlyr) + rsfc*cdntn(nlyr)

      if( lambdaNdx == 1_ik ) then
        call diagout( 'a.new',a )
        call diagout( 'b.new',b )
        call diagout( 'd.new',d )
        call diagout( 'e.new',e )
        call diagout( 'tausla.new',tausla )
      endif

! solve tri-diagonal system:

      y = linpack%tridiag(a, b, d, e)

!*** unfold solution of matrix, compute output fluxes:
      
! the following equations are from pg 16,291  equations 31 & 32

      associate( edr => radField%edr_(:,lambdaNdx), eup => radField%eup_(:,lambdaNdx), &
                 edn => radField%edn_(:,lambdaNdx), &
                 fdr => radField%fdr_(:,lambdaNdx), fup => radField%fup_(:,lambdaNdx), &
                 fdn => radField%fdn_(:,lambdaNdx) )
      fdr(1) = pifs * EXP( -tausla(0) )
      edr(1) = mu * fdr(1)
      edn(1) = fdn0
      eup(1) =  y(1)*e3(1) - y(2)*e4(1) + cup(1)
      fdn(1) = edn(1)/mu1(1)
      fup(1) = eup(1)/mu1(1)

      j   = 1
      row = 1 
      DO lev = 2, nlyr + 1
         fdr(lev) = pifs * EXP(-tausla(lev-1))
         edr(lev) =  mu *fdr(lev)
         edn(lev) =  y(row)*e3(j) + y(row + 1)*e4(j) + cdntn(j)
         eup(lev) =  y(row)*e1(j) + y(row + 1)*e2(j) + cuptn(j)
         fdn(lev) = edn(lev)/mu1(j)
         fup(lev) = eup(lev)/mu1(j)

         row = row + 2
         j = j + 1
      ENDDO
      !> transform from top-down to buttom-up
      fdr = fdr(nlyr+1:1:-1) ; fup = fup(nlyr+1:1:-1) ; fdn = fdn(nlyr+1:1:-1)
      edr = edr(nlyr+1:1:-1) ; eup = eup(nlyr+1:1:-1) ; edn = edn(nlyr+1:1:-1)

      end associate

   end associate

   enddo wavelength_loop

   end associate

    deallocate( zGrid )
    deallocate( lambdaGrid )
    deallocate( surfaceAlbedo )

    write(*,*) ' '
    write(*,*) Iam // 'exiting'

   end function upDateRadField

end module tuvx_delta_eddington
