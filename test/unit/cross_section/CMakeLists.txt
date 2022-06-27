################################################################################
# Test utilities

include(test_util)

################################################################################
# Photo-decomp tests

create_standard_test(NAME cross_section SOURCES base.F90)
create_standard_test(NAME cross_section_acetone_ch3co_ch3_t SOURCES acetone-ch3co_ch3_test.F90)
create_standard_test(NAME cross_section_bro-br_o SOURCES bro-br_o_test.F90)

################################################################################
# The utility file

set(SRC cross_section_test_utils.F90)

list(TRANSFORM SRC PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/")
set(CROSS_SECTION_TEST_SRC ${SRC} PARENT_SCOPE)