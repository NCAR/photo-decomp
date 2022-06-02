! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The tuvx_cross_section_factory module

!> Builder of cross section calculators
module tuvx_cross_section_factory

  use tuvx_cross_section, only : base_cross_section_t, base_constructor
  use tuvx_cross_section_bro_br_o, only : bro_br_o_cross_section_t
  use tuvx_cross_section_tint, only : tint_cross_section_t
  use tuvx_cross_section_ccl4, only : ccl4_cross_section_t
  use tuvx_cross_section_cfc11, only : cfc11_cross_section_t
  use tuvx_cross_section_ch3coch3_ch3co_ch3, only : ch3coch3_ch3co_ch3_cross_section_t
  use tuvx_cross_section_chbr3, only : chbr3_cross_section_t
  use tuvx_cross_section_chcl3, only : chcl3_cross_section_t
  use tuvx_cross_section_ch2o, only : ch2o_cross_section_t
  use tuvx_cross_section_ch3ono2_ch3o_no2, only : ch3ono2_ch3o_no2_cross_section_t
  use tuvx_cross_section_cl2_cl_cl, only : cl2_cl_cl_cross_section_t
  use tuvx_cross_section_clono2, only : clono2_cross_section_t
  use tuvx_cross_section_h2o2_oh_oh,   only : h2o2_oh_oh_cross_section_t
  use tuvx_cross_section_hno3_oh_no2,  only : hno3_oh_no2_cross_section_t
  use tuvx_cross_section_hcfc,  only : hcfc_cross_section_t
  use tuvx_cross_section_hobr_oh_br,  only : hobr_oh_br_cross_section_t
  use tuvx_cross_section_n2o_n2_o1d,  only : n2o_n2_o1d_cross_section_t
  use tuvx_cross_section_n2o5_no2_no3, only : n2o5_no2_no3_cross_section_t
  use tuvx_cross_section_no2_tint,  only : no2_tint_cross_section_t
  use tuvx_cross_section_nitroxy_acetone,  only : nitroxy_acetone_cross_section_t
  use tuvx_cross_section_nitroxy_ethanol,  only : nitroxy_ethanol_cross_section_t
  use tuvx_cross_section_o3_tint,  only : o3_tint_cross_section_t
  use tuvx_cross_section_t_butyl_nitrate,  only : t_butyl_nitrate_cross_section_t
  use tuvx_cross_section_oclo,  only : oclo_cross_section_t
  use tuvx_cross_section_rayliegh, only : rayliegh_cross_section_t
  use tuvx_cross_section_rono2,  only : rono2_cross_section_t

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
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_Profile_warehouse,        only : Profile_warehouse_t

    !> New rate constant calculator
    class(base_cross_section_t), pointer :: new_cross_section_t
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
        allocate( new_cross_section_t )
        call base_constructor( new_cross_section_t, config, gridWareHouse, ProfileWareHouse, atMidPoint=.true._lk )
      case( 'BrO+hv->Br+O cross section' )
        new_cross_section_t => bro_br_o_cross_section_t( config, gridWareHouse, ProfileWareHouse, atMidPoint=.true._lk)
      case( 't_butyl_nitrate+hv->Products cross section' )
        new_cross_section_t => t_butyl_nitrate_cross_section_t ( config, gridWareHouse, ProfileWareHouse, atMidPoint=.true._lk )
      case( 'tint cross section' )
        new_cross_section_t => tint_cross_section_t( config, gridWareHouse, ProfileWareHouse, atMidPoint=.true._lk)
      case( 'O3 cross section' )
        new_cross_section_t => o3_tint_cross_section_t( config, gridWareHouse, ProfileWareHouse, atMidPoint=.true._lk)
      case( 'OClO+hv->Products cross section' )
        new_cross_section_t => oclo_cross_section_t( config, gridWareHouse, ProfileWareHouse, atMidPoint=.true._lk)
      case( 'CCl4+hv->Products cross section' )
        new_cross_section_t => ccl4_cross_section_t( config, gridWareHouse, ProfileWareHouse, atMidPoint=.true._lk)
      case( 'CCl3F+hv->Products cross section' )
        new_cross_section_t => cfc11_cross_section_t( config, gridWareHouse, ProfileWareHouse, atMidPoint=.true._lk)
      case( 'CH3COCH3+hv->CH3CO+CH3 cross section' )
        new_cross_section_t => ch3coch3_ch3co_ch3_cross_section_t( config, gridWareHouse, ProfileWareHouse, atMidPoint=.true._lk)
      case( 'CHBr3+hv->Products cross section' )
        new_cross_section_t => chbr3_cross_section_t( config, gridWareHouse, ProfileWareHouse, atMidPoint=.true._lk)
      case( 'CHCl3+hv->Products cross section' )
        new_cross_section_t => chcl3_cross_section_t( config, gridWareHouse, ProfileWareHouse, atMidPoint=.true._lk)
      case( 'CH2O cross section' )
        new_cross_section_t => ch2o_cross_section_t( config, gridWareHouse, ProfileWareHouse, atMidPoint=.true._lk)
      case( 'CH3ONO2+hv->CH3O+NO2 cross section' )
        new_cross_section_t => ch3ono2_ch3o_no2_cross_section_t( config, gridWareHouse, ProfileWareHouse, atMidPoint=.true._lk)
      case( 'Cl2+hv->Cl+Cl cross section' )
        new_cross_section_t => cl2_cl_cl_cross_section_t( config, gridWareHouse, ProfileWareHouse, atMidPoint=.true._lk)
      case( 'ClONO2 cross section' )
        new_cross_section_t => clono2_cross_section_t( config, gridWareHouse, ProfileWareHouse, atMidPoint=.true._lk)
      case( 'H2O2+hv->OH+OH cross section' )
        new_cross_section_t => h2o2_oh_oh_cross_section_t( config, gridWareHouse, ProfileWareHouse, atMidPoint=.true._lk)
      case( 'HCFC+hv->Products cross section' )
        new_cross_section_t => hcfc_cross_section_t( config, gridWareHouse, ProfileWareHouse, atMidPoint=.true._lk)
      case( 'HNO3+hv->OH+NO2 cross section' )
        new_cross_section_t => hno3_oh_no2_cross_section_t( config, gridWareHouse, ProfileWareHouse, atMidPoint=.true._lk)
      case( 'HOBr+hv->OH+Br cross section' )
        new_cross_section_t => hobr_oh_br_cross_section_t( config, gridWareHouse, ProfileWareHouse, atMidPoint=.true._lk)
      case( 'SO2 cross section' )
        allocate ( new_cross_section_t )
        call base_constructor( new_cross_section_t, config, gridWareHouse, ProfileWareHouse, atMidPoint=.true._lk )
      case( 'N2O+hv->N2+O(1D) cross section' )
        new_cross_section_t => n2o_n2_o1d_cross_section_t( config, gridWareHouse, ProfileWareHouse, atMidPoint=.true._lk)
      case( 'N2O5+hv->NO2+NO3 cross section' )
        new_cross_section_t => n2o5_no2_no3_cross_section_t( config, gridWareHouse, ProfileWareHouse, atMidPoint=.true._lk)
      case( 'NO2 tint cross section' )
        new_cross_section_t => no2_tint_cross_section_t( config, gridWareHouse, ProfileWareHouse, atMidPoint=.true._lk)
      case( 'CH3COCH2(ONO2)+hv->CH3COCH2(O.)+NO2 cross section' )
        new_cross_section_t => nitroxy_acetone_cross_section_t( config, gridWareHouse, ProfileWareHouse, atMidPoint=.true._lk)
      case( 'CH2(OH)CH2(ONO2)+hv->CH2(OH)CH2(O.)+NO2 cross section' )
        new_cross_section_t => nitroxy_ethanol_cross_section_t( config, gridWareHouse, ProfileWareHouse, atMidPoint=.true._lk)
      case( 'RONO2 cross section' )
        new_cross_section_t => rono2_cross_section_t( config, gridWareHouse, ProfileWareHouse, atMidPoint=.true._lk)
      case( 'Air cross section' )
        new_cross_section_t => rayliegh_cross_section_t( config, gridWareHouse, ProfileWareHouse, atMidPoint=.true._lk)
      case default
        call die_msg( 450768214, "Invalid cross section type: '"//              &
                                 cross_section_type%to_char( )//"'" )
    end select
    write(*,*) Iam,'exiting'

  end function cross_section_builder

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_cross_section_factory
