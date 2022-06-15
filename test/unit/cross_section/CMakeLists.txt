################################################################################
# Test utilities

include(test_util)

################################################################################
# Photo-decomp tests

create_standard_test(NAME cross_section SOURCES base.F90)
create_standard_test(NAME cross_section_ch3coch3_ch3co_ch3_t SOURCES acetone-ch3co_ch3_test.F90)

################################################################################
