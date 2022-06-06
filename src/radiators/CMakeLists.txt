################################################################################
# Radiator transfer source

set(SRC aerosol.F90
        )

list(TRANSFORM SRC PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/")
set(RADIATOR_SRC ${SRC} PARENT_SCOPE)

################################################################################
