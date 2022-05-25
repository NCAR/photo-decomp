################################################################################
# One dimensional grid source

set(PROFILE_SRC profiles/air.F90
                profiles/earth_sun_distance.F90
                profiles/extraterrestrial_flux.F90
                profiles/from_config.F90
                profiles/from_csv_file.F90
                profiles/o2.F90
                profiles/o3.F90
                profiles/solar_zenith_angle.F90
                profiles/surface_albedo.F90
                PARENT_SCOPE)

################################################################################
