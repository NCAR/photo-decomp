################################################################################
# Photo-decomp tool source

set(PHOTO_SRC abs_linalgebra.F90
              abstract_radXfer.F90
              delta_eddington.F90
              diagout.F90
              environment.F90
              interpolate.F90
              la_srb.type.F90
              linalgebra.F90
              netcdf_util.F90
              photolysis_core.F90
              photo_utils.F90
              radXfer_component_core.F90
              photorates_component_core.F90
              sphers.F90
              tuv_params.F90
              )

add_subdirectory(grid)
add_subdirectory(profiles)
add_subdirectory(radiator)
add_subdirectory(radXfer_cross_section)
add_subdirectory(quantum_yield)

file(GLOB LITTLE_F_SRC *.f)
set_property(SOURCE ${LITTLE_F_SRC} APPEND PROPERTY COMPILE_OPTIONS "-fdefault-real-8")
set_property(SOURCE ${LITTLE_F_SRC} APPEND PROPERTY COMPILE_OPTIONS "-fdefault-double-8")

file(GLOB LITTLE_F90_SRC *.f90)
set_property(SOURCE ${LITTLE_F90_SRC} APPEND PROPERTY COMPILE_OPTIONS "-fdefault-real-8")
set_property(SOURCE ${LITTLE_F90_SRC} APPEND PROPERTY COMPILE_OPTIONS "-fdefault-double-8")

add_library(photolib ${PHOTO_SRC} ${GRID_SRC} ${PROFILE_SRC} ${RADIATOR_SRC}
                     ${RAD_SRC} ${QY_SRC})
target_link_libraries(photolib musicalib ${NETCDF_LIBS} ${NC4F_LIB})
set_target_properties(photolib PROPERTIES OUTPUT_NAME photo)

add_executable(photo photolysis.F90)
target_link_libraries(photo photolib)

################################################################################
