################################################################################
# Profile source

set(SRC air.F90
        earth_sun_distance.F90
        extraterrestrial_flux.F90
        from_config.F90
        from_csv_file.F90
        o2.F90
        o3.F90
        profile_utils.F90
        solar_zenith_angle.F90
        surface_albedo.F90
        )

list(TRANSFORM SRC PREPEND "${CMAKE_CURRENT_SOURCE_DIR}/")
set(PROFILE_SRC ${SRC} PARENT_SCOPE)

################################################################################
