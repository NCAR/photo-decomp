################################################################################
# Test utilities

include(test_util)

################################################################################
# Radiator regression tests

add_test(NAME regression_aerosols_radiator COMMAND test/regression/radiators/radiation.aerosols.sh WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
add_test(NAME memcheck_regression_aerosols_radiator COMMAND test/regression/radiators/radiation.aerosols.memcheck.sh WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
add_test(NAME regression_o2_radiator       COMMAND test/regression/radiators/radiation.o2.sh       WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
add_test(NAME memcheck_regression_o2_radiator       COMMAND test/regression/radiators/radiation.o2.memcheck.sh       WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
add_test(NAME regression_o3_radiator       COMMAND test/regression/radiators/radiation.o3.sh       WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
add_test(NAME memcheck_regression_o3_radiator       COMMAND test/regression/radiators/radiation.o3.memcheck.sh       WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
add_test(NAME regression_rayleigh_radiator COMMAND test/regression/radiators/radiation.rayleigh.sh WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
add_test(NAME memcheck_regression_rayleigh_radiator COMMAND test/regression/radiators/radiation.rayleigh.memcheck.sh WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
add_test(NAME regression_all_radiators     COMMAND test/regression/radiators/radiation.all.sh      WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
add_test(NAME memcheck_regression_all_radiators     COMMAND test/regression/radiators/radiation.all.memcheck.sh      WORKING_DIRECTORY ${CMAKE_BINARY_DIR})

################################################################################
