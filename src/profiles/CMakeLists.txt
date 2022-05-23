################################################################################
# One dimensional grid source

set(PROFILE_SRC profiles/abstract.Profile.type.F90
                profiles/from_csv_file.type.F90
                profiles/etfl.from_csv_file.type.F90
                profiles/from_config.type.F90
                profiles/srfAlbedo_from_config.type.F90
                profiles/air.from_csv_file.type.F90
                profiles/o2.from_csv_file.type.F90
                profiles/o3.from_csv_file.type.F90
                profiles/sza_from_time.type.F90
                profiles/earth_sun_distance.type.F90
                profiles/Profile_factory.F90
                profiles/Profile_warehouse.F90
                PARENT_SCOPE)

################################################################################
