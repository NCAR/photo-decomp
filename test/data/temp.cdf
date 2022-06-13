netcdf cross_section_base {
dimensions:
        bins = 4 ;
        parameters = 1 ;
        temperatures = UNLIMITED ; // (0 currently)
variables:
        double wavelength(bins) ;
                wavelength:units = "nm" ;
        double cross_section_parameters(parameters, bins) ;
                cross_section_parameters:units = "cm^2" ;
data:

 wavelength = 101.0, 102.0, 103.0, 104.0 ;

 cross_section_parameters = 5.0, 10.0, 40.0, 50.0 ;
}
