################################################################################
# Quantum yield source

set(SRC acetone-ch3co_ch3.F90
        c2h5cho.F90
        ch2chcho.F90
        ch2o.F90
        ch3cho-ch3_hco.F90
        ch3coch2ch3-ch3co_ch2ch3.F90
        ch3cocho.F90
        clo-cl_o1d.F90
        clo-cl_o3p.F90
        clono2-clo_no2.F90
        clono2-cl_no3.F90
        ho2-oh_o.F90
        mvk.F90
        no2_tint.F90
        no3_aq.F90
        o3-o2_o1d.F90
        o3-o2_o3p.F90
        tint.F90
        )

list(TRANSFORM SRC PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/")
set(QUANTUM_YIELD_SRC ${SRC} PARENT_SCOPE)

################################################################################
