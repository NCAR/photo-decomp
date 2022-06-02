! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The quantum_yield_factory module

!> Builder of quantum yield calculators
module tuvx_quantum_yield_factory

  use tuvx_quantum_yield,              only : quantum_yield_t
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
    !> quantum yield configuration data
    type(config_t), intent(inout) :: config
    !> The warehouses
    type(grid_warehouse_t), intent(inout)    :: grid_warehouse
    type(profile_warehouse_t), intent(inout) :: profile_warehouse

    type(string_t) :: quantum_yield_type
    character(len=*), parameter :: Iam = 'quantum yield builder: '

    quantum_yield => null()
    call config%get( 'quantum yield type', quantum_yield_type, Iam )

    select case( quantum_yield_type%to_char() )
      case( 'base quantum yield' )
        allocate(quantum_yield_t :: quantum_yield )
      case( 'tint quantum yield' )
        allocate(quantum_yield_tint_t :: quantum_yield )
      case( 'NO2 tint quantum yield' )
        allocate(quantum_yield_no2_tint_t :: quantum_yield )
      case( 'O3+hv->O2+O(1D) quantum yield' )
        allocate(quantum_yield_o3_o2_o1d_t :: quantum_yield )
      case( 'O3+hv->O2+O(3P) quantum yield' )
        allocate(quantum_yield_o3_o2_o3p_t :: quantum_yield )
      case( 'HO2 quantum yield' )
        allocate(quantum_yield_ho2_oh_o_t :: quantum_yield )
      case( 'NO3-_(aq)+hv->NO2(aq)+O- quantum yield' )
        allocate(quantum_yield_no3m_aq_t :: quantum_yield )
      case( 'CH2O quantum yield' )
        allocate(quantum_yield_ch2o_h2_co_t :: quantum_yield )
      case( 'CH3CHO+hv->CH3+HCO quantum yield' )
        allocate(quantum_yield_ch3cho_ch3_hco_t :: quantum_yield )
      case( 'C2H5CHO quantum yield' )
        allocate(quantum_yield_c2h5cho_c2h5_hco_t :: quantum_yield )
      case( 'CH2CHCHO+hv->Products quantum yield' )
        allocate(quantum_yield_ch2chcho_t :: quantum_yield )
      case( 'MVK+hv->Products quantum yield' )
        allocate(quantum_yield_mvk_t :: quantum_yield )
      case( 'CH3COCH3+hv->CH3CO+CH3 quantum yield' )
        allocate(quantum_yield_ch3coch3_ch3co_ch3_t :: quantum_yield )
      case( 'CH3COCH2CH3+hv->CH3CO+CH2CH3 quantum yield' )
        allocate(quantum_yield_ch3coch2ch3_t :: quantum_yield )
      case( 'CH3COCHO+hv->CH3CO+HCO quantum yield' )
        allocate(quantum_yield_ch3cocho_ch3co_hco_t :: quantum_yield )
      case( 'ClO+hv->Cl+O(1D) quantum yield' )
        allocate(quantum_yield_clo_cl_o1d_t :: quantum_yield )
      case( 'ClO+hv->Cl+O(3P) quantum yield' )
        allocate(quantum_yield_clo_cl_o3p_t :: quantum_yield )
      case( 'ClONO2+hv->Cl+NO3 quantum yield' )
        allocate(quantum_yield_clono2_cl_no3_t :: quantum_yield )
      case( 'ClONO2+hv->ClO+NO2 quantum yield' )
        allocate(quantum_yield_clono2_clo_no2_t :: quantum_yield )
      case default
        call die_msg( 450768214, "Invalid quantum yield type: '"//              &
                                 quantum_yield_type%to_char( )//"'" )
    end select
    call quantum_yield%initialize( config, grid_warehouse, profile_warehouse )

  end function quantum_yield_builder

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module tuvx_quantum_yield_factory
