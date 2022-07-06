################################################################################
# Test utilities

include(test_util)

################################################################################
# Photo-decomp tests

create_standard_test(NAME cross_section SOURCES base.F90)
create_standard_test(NAME cross_section_acetone_ch3co_ch3_t SOURCES acetone-ch3co_ch3_test.F90 cross_section_test_utils.F90)
create_standard_test(NAME cross_section_bro-br_o SOURCES bro-br_o_test.F90 cross_section_test_utils.F90)
create_standard_test(NAME cross_section_ccl4 SOURCES ccl4_test.F90 cross_section_test_utils.F90)
create_standard_test(NAME cross_section_cfc-11 SOURCES cfc-11_test.F90 cross_section_test_utils.F90)
create_standard_test(NAME cross_section_ch2o SOURCES ch2o_test.F90 cross_section_test_utils.F90)
create_standard_test(NAME cross_section_ch3ono2_ch3o_no2 SOURCES ch3ono2_ch3o_no2_test.F90 cross_section_test_utils.F90)
create_standard_test(NAME cross_section_chbr3 SOURCES chbr3_test.F90 cross_section_test_utils.F90)
create_standard_test(NAME cross_section_chcl3 SOURCES chcl3_test.F90 cross_section_test_utils.F90)

################################################################################