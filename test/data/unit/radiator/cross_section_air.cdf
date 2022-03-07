netcdf cross_section_air {
dimensions:
        bins = 5 ;
        parameters = 1 ;
        temperatures = UNLIMITED ; // (0 currently)
variables:
        double wavelength(bins) ;
                wavelength:units = "nm" ;
        double cross_section_parameters(parameters, bins) ;
                cross_section_parameters:units = "cm^2" ;

data:

 wavelength = 40.0, 50.0, 60.0, 70.0, 76.0 ;

 cross_section_parameters = 100.0, 1.0, 10.0, 1000.0, 1000.0 ;
}
