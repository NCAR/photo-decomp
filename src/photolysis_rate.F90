! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The photolysis_photorates module

!> The photorates_component_core_t type and related functions
module tuvx_photolysis_rate

  use tuvx_cross_section, only : abs_cross_section_ptr
  use tuvx_quantum_yield,     only : abs_quantum_yield_ptr
  use tuvx_grid_warehouse,             only : grid_warehouse_t
  use tuvx_profile_warehouse,          only : profile_warehouse_t
  use musica_constants,                only : dk => musica_dk, ik => musica_ik
  use musica_string,                   only : string_t
  use tuvx_grid,                    only : abs_1d_grid_t
  use tuvx_profile,                    only : abs_profile_t
  use musica_assert,                   only : die_msg

  implicit none

  private
  public :: photorates_component_core_t

  integer(ik), parameter :: iONE = 1_ik

  !> photorates component
  type :: photorates_component_core_t
    !> Photo rate constant calculators
    type(abs_cross_section_ptr), allocatable :: cross_section_objs_(:)
    type(abs_quantum_yield_ptr), allocatable :: quantum_yield_objs_(:)
    real(dk),               allocatable :: rate_constant_alias_factor_(:)
    type(string_t), allocatable              :: handle_(:)
  contains
    !> Update the object for new model state conditions
    procedure :: upDate
    !> Finalize the object
    final :: finalize
  end type photorates_component_core_t

  !> photorates_component_core_t constructor
  interface photorates_component_core_t
    module procedure :: constructor
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructor of photorates_component_core_t objects
  function constructor( reaction_set, gridWareHouse, ProfileWareHouse ) result( photorates_component )

    use musica_config,                 only : config_t
    use musica_iterator,               only : iterator_t
    use tuvx_cross_section_factory, only : cross_section_builder
    use tuvx_quantum_yield_factory,         only : quantum_yield_builder

    !> photorates component
    !> Arguments
    type(config_t), intent(inout)               :: reaction_set
    !> grid warehouse
    type(grid_warehouse_t), intent(inout)       :: gridWareHouse
    !> profile warehouse
    type(profile_warehouse_t), intent(inout)    :: profileWareHouse
    !> New photorates component
    class(photorates_component_core_t), pointer :: photorates_component

    !> Local variables
    character(len=*), parameter :: Iam = "Photorates component  constructor: "

    integer(ik)    :: nRates
    real(dk)       :: rate_aliasing_factor
    type(config_t) :: reaction_config
    type(config_t) :: cross_section_config, quantum_yield_config
    class(iterator_t), pointer :: iter
    type(abs_cross_section_ptr) :: cross_section_ptr
    type(abs_quantum_yield_ptr) :: quantum_yield_ptr
    character(len=64)           :: keychar
    type(string_t)              :: netcdfFile, Object
    type(string_t)              :: reaction_key
    type(string_t)              :: Handle
    type(string_t), allocatable :: netcdfFiles(:)

    write(*,*) ' '
    write(*,*) Iam // 'entering'

    allocate( photorates_component )

    associate(component=>photorates_component)

    allocate( string_t :: component%handle_(0) )
    allocate( component%cross_section_objs_(0) )
    allocate( component%quantum_yield_objs_(0) )
    allocate( component%rate_constant_alias_factor_(0) )

    iter => reaction_set%get_iterator( )
!-----------------------------------------------------------------------------
!> iterate over photo reactions
!-----------------------------------------------------------------------------
    do while( iter%next( ) )
      keychar = reaction_set%key(iter)
      reaction_key = keychar 
      write(*,*) ' '
      write(*,*) Iam,'key = ',trim(keychar)
      component%handle_ = [component%handle_,reaction_key]
      call reaction_set%get( iter, reaction_config, Iam )
!-----------------------------------------------------------------------------
!> cross section first
!-----------------------------------------------------------------------------
      call reaction_config%get( "cross section", cross_section_config, Iam )
      cross_section_ptr%val_ => cross_section_builder( cross_section_config, gridWareHouse, profileWareHouse )
      component%cross_section_objs_ = [component%cross_section_objs_,cross_section_ptr]
!-----------------------------------------------------------------------------
!> now quantum yield
!-----------------------------------------------------------------------------
      call reaction_config%get( "quantum yield", quantum_yield_config, Iam )
      quantum_yield_ptr%val_ => quantum_yield_builder( quantum_yield_config, gridWareHouse, profileWareHouse )
      component%quantum_yield_objs_ = [component%quantum_yield_objs_,quantum_yield_ptr]
!-----------------------------------------------------------------------------
!> finally "aliasing" factor
!-----------------------------------------------------------------------------
      call reaction_config%get( "rate constant alias factor", rate_aliasing_factor, Iam, default=1.0_dk )
      component%rate_constant_alias_factor_ = [component%rate_constant_alias_factor_,rate_aliasing_factor]
    end do

    deallocate( iter )

    nRates = size(component%cross_section_objs_)
    write(*,*) ' '
    write(*,'(a,''There are '',i3,'' cross sections'')') Iam,nRates
    nRates = size(component%quantum_yield_objs_)
    write(*,*) ' '
    write(*,'(a,''There are '',i3,'' quantum yields'')') Iam,nRates
    if( size(component%cross_section_objs_) /= nRates ) then
      call die_msg( 200000009,Iam//'# cross sections != # quantum yields' )
    endif

    end associate

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> calculate photolysis rate constants
  subroutine upDate( this, la_srb, SphericalGeom, gridWareHouse, profileWareHouse, radField, &
                           photoRates, ndxtag )

    use tuvx_radiative_transfer_solver, only : radField_t
    use tuvx_la_sr_bands,           only : la_srb_t
    use tuvx_spherical_geometry,   only : spherical_geom_t
    use musica_assert,         only : die_msg
    use tuvx_diagnostic_util,                 only : diagout
    use tuvx_constants,            only : hc

    !> Arguments
    class(photorates_component_core_t), intent(inout) :: this
    !> Spherical geometry
    type(spherical_geom_t), intent(inout)       :: SphericalGeom
    !> Lyman Alpha, SRB
    type(la_srb_t), intent(inout)               :: la_srb
    !> grid warehouse
    type(grid_warehouse_t), intent(inout)       :: gridWareHouse
    !> profile warehouse
    type(profile_warehouse_t), intent(inout)    :: profileWareHouse
    !> actinic flux
    type(radField_t), intent(in)                :: radField
    character(len=*), intent(in)                :: ndxtag
    !> photorates
    real(dk), allocatable                       :: photoRates(:,:)

    !> Local variables
    character(len=*), parameter :: Iam = "Photorates component upDate: "
    integer(ik)           :: vertNdx, rateNdx, nRates
    real(dk), allocatable :: airVcol(:), airScol(:)
    real(dk), allocatable :: bin_factor(:)
    real(dk), allocatable :: xsqyWrk(:)
    real(dk), allocatable :: cross_section(:,:)
    real(dk), allocatable :: quantum_yield(:,:)
    real(dk), allocatable :: xsqy(:,:)
    real(dk), allocatable :: actinicFlux(:,:)
    type(string_t)        :: Handle, annotatedRate
    character(len=64)     :: jlabel
    character(len=64), allocatable :: annotatedjlabel(:)
    class(abs_1d_grid_t), pointer :: zGrid => null()
    class(abs_1d_grid_t), pointer :: lambdaGrid => null()
    class(abs_profile_t), pointer :: airProfile => null()
    class(abs_profile_t), pointer :: etfl => null()

    write(*,*) ' '
    write(*,*) Iam,'entering'

    Handle = 'Vertical Z'             ; zGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Photolysis, wavelength' ; lambdaGrid => gridWareHouse%get_grid( Handle )
    Handle = 'Etfl'                   ; etfl  => profileWareHouse%get_profile( Handle )

    nRates = size(this%cross_section_objs_)
    if( .not. allocated( photoRates ) ) then
      allocate( photoRates(zGrid%ncells_+1,nRates) )
    endif

    bin_factor = (etfl%mid_val_ * 1.e-13_dk * lambdaGrid%mid_ * lambdaGrid%delta_) / hc
    actinicFlux = transpose( radField%fdr_ + radField%fup_ + radField%fdn_ )
    do vertNdx = iONE,zGrid%ncells_+iONE
      actinicFlux(:,vertNdx) = actinicFlux(:,vertNdx) * bin_factor
    enddo

    allocate( annotatedjlabel(nRates) )
    allocate( xsqyWrk(0) )

rate_loop: &
    do rateNdx = iONE, nRates
!     jlabel = this%handle_(rateNdx)%to_char() // new_line('a')
!     annotatedjlabel(rateNdx) = this%handle_(rateNdx)%to_char() // new_line('a')
!     annotatedjlabel(rateNdx) = trim(jlabel) // new_line('a')
      associate( calc_ftn => this%cross_section_objs_(rateNdx)%val_ )
        cross_section = calc_ftn%calculate( gridWareHouse, profileWareHouse )
      end associate
      associate( calc_ftn => this%quantum_yield_objs_(rateNdx)%val_ )
        quantum_yield = calc_ftn%calculate( gridWareHouse, profileWareHouse )
        if( this%handle_(rateNdx) == 'HNO4+hv->HO2+NO2' .or. &
            this%handle_(rateNdx) == 'NOCl+hv->NO+Cl' ) then
          if( any(quantum_yield /= 1._dk) ) then
            call die_msg( 54321,'HNO4 quantum yield != 1.0' )
          endif
        endif
      end associate

      ! O2 photolysis can have special la & srb band handling
      if( trim(this%handle_(rateNdx)%to_char()) == 'O2+hv->O+O' ) then
        Handle = 'Air' ; airProfile => ProfileWareHouse%get_Profile( Handle )
        allocate( airVcol(airProfile%ncells_),airScol(airProfile%ncells_+1_ik) )
        call SphericalGeom%airmas( airProfile%exo_layer_dens_, airVcol, airScol )
        call la_srb%calculate_xs( gridWareHouse, ProfileWareHouse, airVcol, airScol, cross_section )
        deallocate( airVcol,airScol )
      endif

      xsqyWrk = [xsqyWrk,reshape(cross_section*quantum_yield,(/size(cross_section)/))]
      write(*,*) Iam // 'photorate handle = ',trim(this%handle_(rateNdx)%to_char())

      annotatedRate = this%handle_(rateNdx)//'.xsect.new'
      call diagout( trim(annotatedRate%to_char()),cross_section )
      annotatedRate = this%handle_(rateNdx)//'.qyld.new'
      call diagout( trim(annotatedRate%to_char()),quantum_yield )
      annotatedRate = this%handle_(rateNdx)//'.xsqy.new'
      call diagout( trim(annotatedRate%to_char()),cross_section*quantum_yield )

      xsqy = transpose( cross_section * quantum_yield )
      do vertNdx = iONE,zGrid%ncells_+iONE
        photoRates(vertNdx,rateNdx) = dot_product( actinicFlux(:,vertNdx),xsqy(:,vertNdx) )
      enddo
      if( allocated( cross_section ) ) deallocate( cross_section )
      if( allocated( quantum_yield ) ) deallocate( quantum_yield )
    end do rate_loop

    open(unit=33,file='OUTPUTS/annotatedjlabels.new')
    do rateNdx = iONE, nRates
      jlabel = this%handle_(rateNdx)%to_char()
      write(33,*) this%handle_(rateNdx)%to_char()
    enddo
    close(unit=33)
    open(unit=33,file='OUTPUTS/xsqy.'//ndxtag//'.new',form='unformatted')
      write(33) xsqyWrk
    close(unit=33)

    write(*,*) Iam,'exiting'

  end subroutine upDate

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalize the photorates component
  subroutine finalize( this )

    !> photorates component
    !> Arguments
    type(photorates_component_core_t), intent(inout) :: this

    !> Local variables
    character(len=*), parameter :: Iam = 'photorates component finalize: '

    integer(ik) :: ndx

    write(*,*) Iam,'entering'

    if( allocated( this%cross_section_objs_ ) ) then
      do ndx = 1,size(this%cross_section_objs_)
        if( associated( this%cross_section_objs_(ndx)%val_ ) ) then
          deallocate( this%cross_section_objs_(ndx)%val_ )
        endif
      enddo
      deallocate( this%cross_section_objs_ )
    end if

    if( allocated( this%quantum_yield_objs_ ) ) then
      do ndx = 1,size(this%quantum_yield_objs_)
        if( associated( this%quantum_yield_objs_(ndx)%val_ ) ) then
          deallocate( this%quantum_yield_objs_(ndx)%val_ )
        endif
      enddo
      deallocate( this%quantum_yield_objs_ )
    end if

    if( allocated( this%rate_constant_alias_factor_ ) ) then
      deallocate( this%rate_constant_alias_factor_ )
    end if
    if( allocated( this%handle_ ) ) then
      deallocate( this%handle_ )
    end if

    write(*,*) Iam,'exiting'

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_photolysis_rate
