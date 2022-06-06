################################################################################
# One dimensional grid source

set(SRC equal_delta.F90
        from_csv_file.F90
        from_config.F90
        )

list(TRANSFORM SRC PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/")
set(GRID_SRC ${SRC} PARENT_SCOPE)

################################################################################
