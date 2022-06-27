! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The quantum_yield_factory module

!> Builder of quantum yield calculators
module tuvx_quantum_yield_factory

  use tuvx_quantum_yield,              only : quantum_yield_t, base_constructor
  use tuvx_quantum_yield_tint,         only : quantum_yield_tint_t
  use tuvx_quantum_yield_no2_tint,     only : quantum_yield_no2_tint_t
  use tuvx_quantum_yield_o3_o2_o1d,    only : quantum_yield_o3_o2_o1d_t
  use tuvx_quantum_yield_o3_o2_o3p,    only : quantum_yield_o3_o2_o3p_t
  use tuvx_quantum_yield_ho2_oh_o,     only : quantum_yield_ho2_oh_o_t
  use tuvx_quantum_yield_no3m_aq,      only : quantum_yield_no3m_aq_t
  use tuvx_quantum_yield_ch2o_h2_co,   only : quantum_yield_ch2o_h2_co_t
  use tuvx_quantum_yield_ch3cho_ch3_hco,                                      &
    only : quantum_yield_ch3cho_ch3_hco_t
  use tuvx_quantum_yield_c2h5cho_c2h5_hco,                                    &
    only : quantum_yield_c2h5cho_c2h5_hco_t
  use tuvx_quantum_yield_ch2chcho,     only : quantum_yield_ch2chcho_t
  use tuvx_quantum_yield_mvk,          only : quantum_yield_mvk_t
  use tuvx_quantum_yield_ch3coch3_ch3co_ch3,                                  &
    only : quantum_yield_ch3coch3_ch3co_ch3_t
  use tuvx_quantum_yield_ch3coch2ch3,  only : quantum_yield_ch3coch2ch3_t
  use tuvx_quantum_yield_ch3cocho_ch3co_hco,                                  &
    only : quantum_yield_ch3cocho_ch3co_hco_t
  use tuvx_quantum_yield_clo_cl_o1d,   only : quantum_yield_clo_cl_o1d_t
  use tuvx_quantum_yield_clo_cl_o3p,   only : quantum_yield_clo_cl_o3p_t
  use tuvx_quantum_yield_clono2_cl_no3,only : quantum_yield_clono2_cl_no3_t
  use tuvx_quantum_yield_clono2_clo_no2,                                      &
    only : quantum_yield_clono2_clo_no2_t

  implicit none

  private
  public :: quantum_yield_builder

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Builds quantum yield calculators
  function quantum_yield_builder( config, grid_warehouse, profile_warehouse ) &
      result( quantum_yield )

    use musica_assert,                 only : die_msg
    use musica_config,                 only : config_t
    use musica_string,                 only : string_t
    use tuvx_grid_warehouse,           only : grid_warehouse_t
    use tuvx_profile_warehouse,        only : profile_warehouse_t

    !> New quantum yield calculator
    class(quantum_yield_t), pointer :: quantum_yield
    !> Quantum yield configuration data
    type(config_t), intent(inout) :: config
    !> Grid warehouse
    type(grid_warehouse_t), intent(inout)    :: grid_warehouse
    !> Profile warehouse
    type(profile_warehouse_t), intent(inout) :: profile_warehouse

    type(string_t) :: quantum_yield_type
    character(len=*), parameter :: Iam = 'quantum yield builder'

    quantum_yield => null()
    call config%get( 'type', quantum_yield_type, Iam )

    select case( quantum_yield_type%to_char() )
      case( 'base' )
        allocate( quantum_yield )
        call base_constructor( quantum_yield, config, grid_warehouse,         &
                               profile_warehouse )
      case( 'tint' )
        quantum_yield => quantum_yield_tint_t( config, grid_warehouse,        &
                                               profile_warehouse )
      case( 'NO2 tint' )
        quantum_yield => quantum_yield_no2_tint_t( config, grid_warehouse,    &
                                                   profile_warehouse )
      case( 'O3+hv->O2+O(1D)' )
        quantum_yield => quantum_yield_o3_o2_o1d_t( config, grid_warehouse,   &
                                                    profile_warehouse )
      case( 'O3+hv->O2+O(3P)' )
        quantum_yield => quantum_yield_o3_o2_o3p_t( config, grid_warehouse,   &
                                                    profile_warehouse )
      case( 'HO2' )
        quantum_yield => quantum_yield_ho2_oh_o_t( config, grid_warehouse,    &
                                                   profile_warehouse )
      case( 'NO3-_(aq)+hv->NO2(aq)+O-' )
        quantum_yield => quantum_yield_no3m_aq_t( config, grid_warehouse,     &
                                                  profile_warehouse )
      case( 'CH2O' )
        quantum_yield => quantum_yield_ch2o_h2_co_t( config, grid_warehouse,  &
                                                     profile_warehouse )
      case( 'CH3CHO+hv->CH3+HCO' )
        quantum_yield => quantum_yield_ch3cho_ch3_hco_t( config,              &
                                                         grid_warehouse,      &
                                                         profile_warehouse )
      case( 'C2H5CHO' )
        quantum_yield => quantum_yield_c2h5cho_c2h5_hco_t( config,            &
                                                           grid_warehouse,    &
                                                           profile_warehouse )
      case( 'CH2CHCHO+hv->Products' )
        quantum_yield => quantum_yield_ch2chcho_t( config, grid_warehouse,    &
                                                   profile_warehouse )
      case( 'MVK+hv->Products' )
        quantum_yield => quantum_yield_mvk_t( config, grid_warehouse,         &
                                              profile_warehouse )
      case( 'CH3COCH3+hv->CH3CO+CH3' )
        quantum_yield => quantum_yield_ch3coch3_ch3co_ch3_t( config,          &
                                                            grid_warehouse,   &
                                                            profile_warehouse )
      case( 'CH3COCH2CH3+hv->CH3CO+CH2CH3' )
        quantum_yield => quantum_yield_ch3coch2ch3_t( config, grid_warehouse, &
                                                      profile_warehouse )
      case( 'CH3COCHO+hv->CH3CO+HCO' )
        quantum_yield => quantum_yield_ch3cocho_ch3co_hco_t( config,          &
                                                            grid_warehouse,   &
                                                            profile_warehouse )
      case( 'ClO+hv->Cl+O(1D)' )
        quantum_yield => quantum_yield_clo_cl_o1d_t( config, grid_warehouse,  &
                                                     profile_warehouse )
      case( 'ClO+hv->Cl+O(3P)' )
        quantum_yield => quantum_yield_clo_cl_o3p_t( config, grid_warehouse,  &
                                                     profile_warehouse )
      case( 'ClONO2+hv->Cl+NO3' )
        quantum_yield => quantum_yield_clono2_cl_no3_t( config,               &
                                                        grid_warehouse,       &
                                                        profile_warehouse )
      case( 'ClONO2+hv->ClO+NO2' )
        quantum_yield => quantum_yield_clono2_clo_no2_t( config,              &
                                                         grid_warehouse,      &
                                                         profile_warehouse )
      case default
        call die_msg( 450768214, "Invalid quantum yield type: '"//            &
                                 quantum_yield_type%to_char( )//"'" )
    end select

  end function quantum_yield_builder

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_quantum_yield_factory
