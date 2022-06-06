! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The tuvx_cross_section_factory module

!> Builder of cross section calculators
module tuvx_cross_section_factory

  use tuvx_cross_section,              only : cross_section_t
  use tuvx_cross_section_bro_br_o,     only : cross_section_bro_br_o_t
  use tuvx_cross_section_ccl4,         only : cross_section_ccl4_t
  use tuvx_cross_section_cfc11,        only : cross_section_cfc11_t
  use tuvx_cross_section_ch3coch3_ch3co_ch3,                                  &
    only : cross_section_ch3coch3_ch3co_ch3_t
  use tuvx_cross_section_chbr3,        only : cross_section_chbr3_t
  use tuvx_cross_section_chcl3,        only : cross_section_chcl3_t
  use tuvx_cross_section_ch2o,         only : cross_section_ch2o_t
  use tuvx_cross_section_ch3ono2_ch3o_no2,                                    &
    only : cross_section_ch3ono2_ch3o_no2_t
  use tuvx_cross_section_cl2_cl_cl,    only : cross_section_cl2_cl_cl_t
  use tuvx_cross_section_clono2,       only : cross_section_clono2_t
  use tuvx_cross_section_h2o2_oh_oh,   only : cross_section_h2o2_oh_oh_t
  use tuvx_cross_section_hno3_oh_no2,  only : cross_section_hno3_oh_no2_t
  use tuvx_cross_section_hcfc,         only : cross_section_hcfc_t
  use tuvx_cross_section_hobr_oh_br,   only : cross_section_hobr_oh_br_t
  use tuvx_cross_section_n2o_n2_o1d,   only : cross_section_n2o_n2_o1d_t
  use tuvx_cross_section_n2o5_no2_no3, only : cross_section_n2o5_no2_no3_t
  use tuvx_cross_section_nitroxy_acetone,                                    &
    only : cross_section_nitroxy_acetone_t
  use tuvx_cross_section_nitroxy_ethanol,                                    &
    only : cross_section_nitroxy_ethanol_t
  use tuvx_cross_section_no2_tint,     only : cross_section_no2_tint_t
  use tuvx_cross_section_o3_tint,      only : cross_section_o3_tint_t
  use tuvx_cross_section_oclo,         only : cross_section_oclo_t
  use tuvx_cross_section_rayliegh,     only : cross_section_rayliegh_t
  use tuvx_cross_section_rono2,        only : cross_section_rono2_t
  use tuvx_cross_section_t_butyl_nitrate,                                    &
    only : cross_section_t_butyl_nitrate_t
  use tuvx_cross_section_tint,         only : cross_section_tint_t

  implicit none

  private
  public :: cross_section_builder

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function cross_section_builder( config, grid_warehouse, profile_warehouse ) &
      result( new_cross_section )

    use musica_assert,                 only : die_msg
    use musica_config,                 only : config_t
    use musica_constants,              only : musica_dk
    use musica_string,                 only : string_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    !> New rate constant calculator
    class(cross_section_t),    pointer       :: new_cross_section
    !> Cross section configuration data
    type(config_t),            intent(inout) :: config
    !> Grid warehouse
    type(grid_warehouse_t),    intent(inout) :: grid_warehouse
    !> Profile warehouse
    type(profile_warehouse_t), intent(inout) :: profile_warehouse

    type(string_t) :: cross_section_type
    character(len=*), parameter :: Iam = 'cross section builder'

    new_cross_section => null( )
    call config%get( 'cross section type', cross_section_type, Iam )

    select case( cross_section_type%to_char() )
      case( 'Air cross section' )
        new_cross_section => cross_section_rayliegh_t( config, grid_warehouse,&
                                                           profile_warehouse )
      case( 'base cross section' )
        new_cross_section => cross_section_t( config, grid_warehouse,         &
                                                           profile_warehouse )
      case( 'BrO+hv->Br+O cross section' )
        new_cross_section => cross_section_bro_br_o_t( config, grid_warehouse,&
                                                           profile_warehouse )
      case( 'CCl4+hv->Products cross section' )
        new_cross_section => cross_section_ccl4_t( config, grid_warehouse,    &
                                                           profile_warehouse )
      case( 'CCl3F+hv->Products cross section' )
        new_cross_section => cross_section_cfc11_t( config, grid_warehouse,   &
                                                           profile_warehouse )
      case( 'CHCl3+hv->Products cross section' )
        new_cross_section => cross_section_chcl3_t( config, grid_warehouse,   &
                                                           profile_warehouse )
      case( 'CH2(OH)CH2(ONO2)+hv->CH2(OH)CH2(O.)+NO2 cross section' )
        new_cross_section => cross_section_nitroxy_ethanol_t( config,         &
                                           grid_warehouse, profile_warehouse )
      case( 'CH2O cross section' )
        new_cross_section => cross_section_ch2o_t( config, grid_warehouse,    &
                                                           profile_warehouse )
      case( 'CH3COCH2(ONO2)+hv->CH3COCH2(O.)+NO2 cross section' )
        new_cross_section => cross_section_nitroxy_acetone_t( config,         &
                                           grid_warehouse, profile_warehouse )
      case( 'CH3COCH3+hv->CH3CO+CH3 cross section' )
        new_cross_section => cross_section_ch3coch3_ch3co_ch3_t( config,      &
                                           grid_warehouse, profile_warehouse )
      case( 'CH3ONO2+hv->CH3O+NO2 cross section' )
        new_cross_section => cross_section_ch3ono2_ch3o_no2_t( config,        &
                                           grid_warehouse, profile_warehouse )
      case( 'CHBr3+hv->Products cross section' )
        new_cross_section => cross_section_chbr3_t( config, grid_warehouse,   &
                                                           profile_warehouse )
      case( 'Cl2+hv->Cl+Cl cross section' )
        new_cross_section => cross_section_cl2_cl_cl_t( config,               &
                                           grid_warehouse, profile_warehouse )
      case( 'ClONO2 cross section' )
        new_cross_section => cross_section_clono2_t( config, grid_warehouse,  &
                                                           profile_warehouse )
      case( 'H2O2+hv->OH+OH cross section' )
        new_cross_section => cross_section_h2o2_oh_oh_t( config,              &
                                           grid_warehouse, profile_warehouse )
      case( 'HCFC+hv->Products cross section' )
        new_cross_section => cross_section_hcfc_t( config, grid_warehouse,    &
                                                           profile_warehouse )
      case( 'HNO3+hv->OH+NO2 cross section' )
        new_cross_section => cross_section_hno3_oh_no2_t( config,             &
                                           grid_warehouse, profile_warehouse )
      case( 'HOBr+hv->OH+Br cross section' )
        new_cross_section => cross_section_hobr_oh_br_t( config,              &
                                           grid_warehouse, profile_warehouse )
      case( 'N2O+hv->N2+O(1D) cross section' )
        new_cross_section => cross_section_n2o_n2_o1d_t( config,              &
                                           grid_warehouse, profile_warehouse )
      case( 'N2O5+hv->NO2+NO3 cross section' )
        new_cross_section => cross_section_n2o5_no2_no3_t( config,            &
                                           grid_warehouse, profile_warehouse )
      case( 'NO2 tint cross section' )
        new_cross_section => cross_section_no2_tint_t( config, grid_warehouse,&
                                                           profile_warehouse )
      case( 'O3 cross section' )
        new_cross_section => cross_section_o3_tint_t( config, grid_warehouse, &
                                                           profile_warehouse )
      case( 'OClO+hv->Products cross section' )
        new_cross_section => cross_section_oclo_t( config, grid_warehouse,    &
                                                           profile_warehouse )
      case( 'RONO2 cross section' )
        new_cross_section => cross_section_rono2_t( config, grid_warehouse,   &
                                                           profile_warehouse )
      case( 'SO2 cross section' )
        new_cross_section => cross_section_t( config, grid_warehouse,         &
                                                           profile_warehouse )
      case( 't_butyl_nitrate+hv->Products cross section' )
        new_cross_section => cross_section_t_butyl_nitrate_t ( config,        &
                                           grid_warehouse, profile_warehouse )
      case( 'tint cross section' )
        new_cross_section => cross_section_tint_t( config, grid_warehouse,    &
                                                           profile_warehouse )
      case default
        call die_msg( 450768214, "Invalid cross section type: '"//            &
                                 cross_section_type%to_char( )//"'" )
    end select

  end function cross_section_builder

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_cross_section_factory
