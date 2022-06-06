################################################################################
# Cross section source

set(SRC acetone-ch3co_ch3.F90
        bro-br_o.F90
        ccl4.F90
        cfc-11.F90
        chbr3.F90
        chcl3.F90
        ch3ono2-ch3o_no2.F90
        ch2o.F90
        cl2-cl_cl.F90
        clono2.F90
        h2o2-oh_oh.F90
        hcfc.F90
        hno3-oh_no2.F90
        hobr-oh_br.F90
        n2o-n2_o1d.F90
        n2o5-no2_no3.F90
        nitroxy_acetone.F90
        nitroxy_ethanol.F90
        no2_tint.F90
        o3_tint.F90
        oclo.F90
        rono2.F90
        t_butyl_nitrate.F90
        tint.F90
        rayliegh.F90
        )

list(TRANSFORM SRC PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/")
set(CROSS_SECTION_SRC ${SRC} PARENT_SCOPE)

################################################################################
