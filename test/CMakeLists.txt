################################################################################
# Test utilities

include(test_util)

################################################################################
# Copy test data

add_custom_target(copy_test_data ALL ${CMAKE_COMMAND} -E copy_directory
  ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_BINARY_DIR}/test)

################################################################################
# Add subdirectories

add_subdirectory(oldtuv)
add_subdirectory(unit)
add_subdirectory(regression)

################################################################################
# Photo-decomp tests

# \todo should be moved to unit or regression folder
create_standard_test(NAME profile SOURCES Profiles/test.F90
                                  WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

# \todo should be moved to unit or regression folder
create_standard_test(NAME radXfer SOURCES radXfer/radXfer_core.F90
                                          radXfer/radXfer_test.F90)

# \todo should be moved to unit or regression folder
create_standard_test(NAME radiator SOURCES radiator/radiator_core.F90
                                           radiator/radiator_test.F90)

################################################################################
