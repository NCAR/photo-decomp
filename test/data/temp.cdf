netcdf cross_section_base_2 {
dimensions:
        bins = 2 ;
        parameters = 1 ;
        temperatures = UNLIMITED ; // (0 currently)
variables:
        double wavelength(bins) ;
                wavelength:units = "nm" ;
        double cross_section_parameters(parameters, bins) ;
                cross_section_parameters:units = "cm^2" ;
data:

 wavelength = 103.0, 104.0 ;

 cross_section_parameters = 40.0, 50.0 ;
}
