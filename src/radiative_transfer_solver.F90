! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
module tuvx_radiative_transfer_solver

   use musica_constants,         only : ik => musica_ik, dk => musica_dk

   implicit none

   private
   public :: abstract_radXfer_t, radField_t

   type :: radField_t
     real(dk), allocatable :: edr_(:,:), eup_(:,:), edn_(:,:)
     real(dk), allocatable :: fdr_(:,:), fup_(:,:), fdn_(:,:)
   contains
     final :: finalize
   end type radField_t

   type, abstract :: abstract_radXfer_t
     contains
     procedure(upDateRadField), deferred :: upDateRadField
   end type abstract_radXfer_t

   interface
     function upDateRadField( this, sza, nstr, nlyr, &
                   sphericalGeom, gridWareHouse, ProfileWareHouse, radiatorWareHouse ) &
                   result( radField )

     use musica_constants,         only : ik => musica_ik, dk => musica_dk
     use tuvx_grid_warehouse,      only : grid_warehouse_t
     use tuvx_profile_warehouse,   only : Profile_warehouse_t
     use tuvx_radiator_warehouse,  only : radiator_warehouse_t
     use tuvx_spherical_geometry,      only : spherical_geom_t

     import abstract_radXfer_t, radField_t

     class(abstract_radXfer_t), intent(inout)  :: this
     integer(ik), intent(in)                   :: nlyr, nstr
     real(dk), intent(in)                      :: sza
     type(grid_warehouse_t), intent(inout)     :: gridWareHouse
     type(Profile_warehouse_t), intent(inout)  :: ProfileWareHouse
     type(radiator_warehouse_t), intent(inout) :: radiatorWareHouse
     type(spherical_geom_t), intent(inout)     :: sphericalGeom

     class(radField_t), pointer                :: radField

     end function upDateRadField

   end interface

   contains

   subroutine finalize( this )

   type(radField_t), intent(inout) :: this

   if( allocated( this%edr_ ) ) deallocate( this%edr_ )
   if( allocated( this%eup_ ) ) deallocate( this%eup_ )
   if( allocated( this%edn_ ) ) deallocate( this%edn_ )
   if( allocated( this%fdr_ ) ) deallocate( this%fdr_ )
   if( allocated( this%fup_ ) ) deallocate( this%fup_ )
   if( allocated( this%fdn_ ) ) deallocate( this%fdn_ )

   end subroutine finalize

end module tuvx_radiative_transfer_solver
