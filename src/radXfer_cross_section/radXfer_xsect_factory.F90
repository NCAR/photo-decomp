! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The micm_cross_section_factory module

!> Builder of cross section calculators
module micm_radXfer_cross_section_factory

  use micm_radXfer_abs_cross_section_type,  only : abs_cross_section_t
  use micm_radXfer_base_cross_section_type, only : base_cross_section_t
  use micm_radXfer_bro_br_o_cross_section_type, only : bro_br_o_cross_section_t
  use micm_radXfer_tint_cross_section_type, only : tint_cross_section_t
  use micm_radXfer_ccl4_cross_section_type, only : ccl4_cross_section_t
  use micm_radXfer_cfc11_cross_section_type, only : cfc11_cross_section_t
  use micm_radXfer_ch3coch3_ch3co_ch3_cross_section_type, only : ch3coch3_ch3co_ch3_cross_section_t
  use micm_radXfer_chbr3_cross_section_type, only : chbr3_cross_section_t
  use micm_radXfer_chcl3_cross_section_type, only : chcl3_cross_section_t
  use micm_radXfer_ch2o_cross_section_type, only : ch2o_cross_section_t
  use micm_radXfer_ch3ono2_ch3o_no2_cross_section_type, only : ch3ono2_ch3o_no2_cross_section_t
  use micm_radXfer_cl2_cl_cl_cross_section_type, only : cl2_cl_cl_cross_section_t
  use micm_radXfer_clono2_cross_section_type, only : clono2_cross_section_t
  use micm_h2o2_oh_oh_cross_section_type,   only : h2o2_oh_oh_cross_section_t
  use micm_hno3_oh_no2_cross_section_type,  only : hno3_oh_no2_cross_section_t
  use micm_radXfer_hcfc_cross_section_type,  only : hcfc_cross_section_t
  use micm_radXfer_hobr_oh_br_cross_section_type,  only : hobr_oh_br_cross_section_t
  use micm_radXfer_n2o_n2_o1d_cross_section_type,  only : n2o_n2_o1d_cross_section_t
  use micm_radXfer_n2o5_no2_no3_cross_section_type, only : n2o5_no2_no3_cross_section_t
  use micm_radXfer_no2_tint_cross_section_type,  only : no2_tint_cross_section_t
  use micm_radXfer_nitroxy_acetone_cross_section_type,  only : nitroxy_acetone_cross_section_t
  use micm_radXfer_nitroxy_ethanol_cross_section_type,  only : nitroxy_ethanol_cross_section_t
  use micm_radXfer_o3_tint_cross_section_type,  only : o3_tint_cross_section_t
  use micm_radXfer_t_butyl_nitrate_cross_section_type,  only : t_butyl_nitrate_cross_section_t
  use micm_radXfer_oclo_cross_section_type,  only : oclo_cross_section_t
  use micm_radXfer_rayliegh_cross_section_type, only : rayliegh_cross_section_t
  use micm_radXfer_rono2_cross_section_type,  only : rono2_cross_section_t

  implicit none

  private
  public :: cross_section_builder

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function cross_section_builder( config, gridWareHouse, ProfileWareHouse ) result( new_cross_section_t )

    use musica_assert,                 only : die_msg
    use musica_config,                 only : config_t
    use musica_string,                 only : string_t
    use musica_constants,              only : musica_dk, lk => musica_lk
    use micm_grid_warehouse,           only : grid_warehouse_t
    use micm_Profile_warehouse,        only : Profile_warehouse_t

    !> New rate constant calculator
    class(abs_cross_section_t), pointer :: new_cross_section_t
    !> cross section configuration data
    type(config_t), intent(inout) :: config
    !> The warehouses
    type(grid_warehouse_t), intent(inout)    :: gridWareHouse
    type(Profile_warehouse_t), intent(inout) :: ProfileWareHouse

    type(string_t) :: cross_section_type
    character(len=*), parameter :: Iam = 'cross section builder: '

    write(*,*) Iam,'entering'
    new_cross_section_t => null( )
    call config%get( 'cross section type', cross_section_type, Iam )

    select case( cross_section_type%to_char() )
      case( 'base cross section' )
        allocate( base_cross_section_t :: new_cross_section_t )
      case( 'BrO+hv->Br+O cross section' )
        allocate( bro_br_o_cross_section_t :: new_cross_section_t )
      case( 't_butyl_nitrate+hv->Products cross section' )
        allocate( t_butyl_nitrate_cross_section_t :: new_cross_section_t )
      case( 'tint cross section' )
        allocate( tint_cross_section_t :: new_cross_section_t )
      case( 'O3 cross section' )
        allocate( o3_tint_cross_section_t :: new_cross_section_t )
      case( 'OClO+hv->Products cross section' )
        allocate( oclo_cross_section_t :: new_cross_section_t )
      case( 'CCl4+hv->Products cross section' )
        allocate( ccl4_cross_section_t :: new_cross_section_t )
      case( 'CCl3F+hv->Products cross section' )
        allocate( cfc11_cross_section_t :: new_cross_section_t )
      case( 'CH3COCH3+hv->CH3CO+CH3 cross section' )
        allocate( ch3coch3_ch3co_ch3_cross_section_t :: new_cross_section_t )
      case( 'CHBr3+hv->Products cross section' )
        allocate( chbr3_cross_section_t :: new_cross_section_t )
      case( 'CHCl3+hv->Products cross section' )
        allocate( chcl3_cross_section_t :: new_cross_section_t )
      case( 'CH2O cross section' )
        allocate( ch2o_cross_section_t :: new_cross_section_t )
      case( 'CH3ONO2+hv->CH3O+NO2 cross section' )
        allocate( ch3ono2_ch3o_no2_cross_section_t :: new_cross_section_t )
      case( 'Cl2+hv->Cl+Cl cross section' )
        allocate( cl2_cl_cl_cross_section_t :: new_cross_section_t )
      case( 'ClONO2 cross section' )
        allocate( clono2_cross_section_t :: new_cross_section_t )
      case( 'H2O2+hv->OH+OH cross section' )
        allocate( h2o2_oh_oh_cross_section_t :: new_cross_section_t )
      case( 'HCFC+hv->Products cross section' )
        allocate( hcfc_cross_section_t :: new_cross_section_t )
      case( 'HNO3+hv->OH+NO2 cross section' )
        allocate( hno3_oh_no2_cross_section_t :: new_cross_section_t )
      case( 'HOBr+hv->OH+Br cross section' )
        allocate( hobr_oh_br_cross_section_t :: new_cross_section_t )
      case( 'SO2 cross section' )
        allocate( base_cross_section_t :: new_cross_section_t )
      case( 'N2O+hv->N2+O(1D) cross section' )
        allocate( n2o_n2_o1d_cross_section_t :: new_cross_section_t )
      case( 'N2O5+hv->NO2+NO3 cross section' )
        allocate( n2o5_no2_no3_cross_section_t :: new_cross_section_t )
      case( 'NO2 tint cross section' )
        allocate( no2_tint_cross_section_t :: new_cross_section_t )
      case( 'CH3COCH2(ONO2)+hv->CH3COCH2(O.)+NO2 cross section' )
        allocate( nitroxy_acetone_cross_section_t :: new_cross_section_t )
      case( 'CH2(OH)CH2(ONO2)+hv->CH2(OH)CH2(O.)+NO2 cross section' )
        allocate( nitroxy_ethanol_cross_section_t :: new_cross_section_t )
      case( 'RONO2 cross section' )
        allocate( rono2_cross_section_t :: new_cross_section_t )
      case( 'Air cross section' )
        allocate( rayliegh_cross_section_t :: new_cross_section_t )
      case default
        call die_msg( 450768214, "Invalid cross section type: '"//              &
                                 cross_section_type%to_char( )//"'" )
    end select
    call new_cross_section_t%initialize( config, gridWareHouse, ProfileWareHouse, atMidPoint=.true._lk )
    write(*,*) Iam,'exiting'

  end function cross_section_builder

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module micm_radXfer_cross_section_factory
