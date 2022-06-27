################################################################################
# Test utilities

include(test_util)

################################################################################
# Photo-decomp tests

create_standard_test(NAME cross_section SOURCES base.F90)
create_standard_test(NAME cross_section_acetone_ch3co_ch3_t SOURCES acetone-ch3co_ch3_test.F90 cross_section_test_utils.F90)
create_standard_test(NAME cross_section_bro-br_o SOURCES bro-br_o_test.F90 cross_section_test_utils.F90)

################################################################################