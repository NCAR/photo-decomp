################################################################################
# Test utilities

include(test_util)

################################################################################
# Phototolysis rate regression tests

add_test(NAME regression_photolysis_rates COMMAND test/regression/photolysis_rates/photolysis_rates.sh WORKING_DIRECTORY ${CMAKE_BINARY_DIR})

add_test(NAME memcheck_regression_photolysis_rates COMMAND test/regression/photolysis_rates/photolysis_rates_memcheck.sh WORKING_DIRECTORY ${CMAKE_BINARY_DIR})

################################################################################
