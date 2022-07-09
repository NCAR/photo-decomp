################################################################################
# Photo-decomp tool source

set(TUVX_SRC constants.F90
             core.F90
             cross_section.F90
             cross_section_factory.F90
             cross_section_warehouse.F90
             delta_eddington.F90
             diagnostic_util.F90
             dose_rates.F90
             grid.F90
             grid_factory.F90
             grid_warehouse.F90
             interpolate.F90
             la_sr_bands.F90
             linear_algebra.F90
             linear_algebra_linpack.F90
             netcdf_util.F90
             photolysis_rates.F90
             profile.F90
             profile_factory.F90
             profile_warehouse.F90
             quantum_yield.F90
             quantum_yield_factory.F90
             radiative_transfer.F90
             radiative_transfer_solver.F90
             radiator.F90
             radiator_factory.F90
             radiator_warehouse.F90
             spectral_wght.F90
             spectral_wght_factory.F90
             spherical_geometry.F90
             tuvx.F90
             util.F90
             )

add_subdirectory(cross_sections)
add_subdirectory(grids)
add_subdirectory(profiles)
add_subdirectory(quantum_yields)
add_subdirectory(radiators)
add_subdirectory(spectral_wghts)

file(GLOB LITTLE_F_SRC *.f)
set_property(SOURCE ${LITTLE_F_SRC} APPEND PROPERTY COMPILE_OPTIONS "-fdefault-real-8")
set_property(SOURCE ${LITTLE_F_SRC} APPEND PROPERTY COMPILE_OPTIONS "-fdefault-double-8")

file(GLOB LITTLE_F90_SRC *.f90)
set_property(SOURCE ${LITTLE_F90_SRC} APPEND PROPERTY COMPILE_OPTIONS "-fdefault-real-8")
set_property(SOURCE ${LITTLE_F90_SRC} APPEND PROPERTY COMPILE_OPTIONS "-fdefault-double-8")

add_library(tuvxlib ${TUVX_SRC} ${GRID_SRC} ${PROFILE_SRC} ${RADIATOR_SRC}
                    ${CROSS_SECTION_SRC} ${QUANTUM_YIELD_SRC} ${SPECTRAL_WGHT_SRC})
target_link_libraries(tuvxlib musicalib ${NETCDF_LIBS} ${NC4F_LIB})
set_target_properties(tuvxlib PROPERTIES OUTPUT_NAME tuvx)

add_executable(tuv-x tuvx.F90)
target_link_libraries(tuv-x tuvxlib)

################################################################################
