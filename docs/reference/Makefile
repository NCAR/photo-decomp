# Makefile for TUV 5.0
# Use with disort (discrete ordinate), or ps2str (2 stream approximation, 
# pseudo-spherical correction)
#----------
# EXC      : name of executable
# INCLUDES : required include files
# USE_INCL : object files referencing include file (params)
# FOBJS    : all required object files that do not use the include file
#

LIBS   = -L$(NETCDF_DIR)/lib -lnetcdf -lnetcdff -L$(JSONFORTRAN_LIB) -ljsonfortran -L$(NC4FORTRAN_LIB) -lnc4fortran
INCLUDE_MODULES = -I$(NETCDF_DIR)/include -I$(JSONFORTRAN_INC) -I$(NC4FORTRAN_INC)

.SUFFIXES: .o .F90 .f90 .f .mod

EXC = tuv

INCLUDES = params 

USE_INCL = abs_linalgebra.o linalgebra.o grids.o abstract_radXfer.o delta_eddington.o \
           disord_subs.o disord.o rtlink.o \
           rdinp.o rdetfl.o rdxs.o \
           swphys.o swbiol.o swchem.o rxn.o qys.o \
           wshift.o \
	   vpair.o vptmp.o vpo3.o \
	   odrl.o odo3.o \
           setaer.o setalb.o setcld.o setsnw.o \
           setno2.o seto2.o setso2.o \
           sphers.o  \
	   la_srb.o \
	   savout.o \
           newlst.o \
           wrflut.o \
           waters.o \
           swdom.o tuv.o 

FOBJS = tuv_params.o numer.o functs.o orbit.o terint.o
CORE_OBJS =  constants.o string.o assert.o convert.o environment.o iterator.o config.o
RADXFER_OBJS =  netcdf_util.o photo_utils.o abstract.cross_section.type.o base.cross_section.type.o \
                tint.cross_section.type.o o3.tint.cross_section.type.o no2.tint.cross_section.type.o \
                h2o2-oh_oh.cross_section.type.o n2o-n2_o1d.cross_section.type.o n2o5-no2_no3.cross_section.type.o \
                hno3-oh_no2.cross_section.type.o ch2o.cross_section.type.o ch3ono2-ch3o_no2.cross_section.type.o \
                rono2.cross_section.type.o nitroxy_ethanol.cross_section.type.o nitroxy_acetone.cross_section.type.o \
                t_butyl_nitrate.cross_section.type.o acetone-ch3co_ch3.cross_section.type.o cl2-cl_cl.cross_section.type.o \
                oclo.cross_section.type.o clono2.cross_section.type.o ccl4.cross_section.type.o chcl3.cross_section.type.o \
                cfc-11.cross_section.type.o hcfc.cross_section.type.o bro-br_o.cross_section.type.o hobr-oh_br.cross_section.type.o \
                chbr3.cross_section.type.o radXfer_xsect_factory.o radXfer_xsect_warehouse.o
XSQY_OBJS =  abstract.quantum_yield.type.o base.quantum_yield.type.o tint.quantum_yield.type.o \
             no2.tint.quantum_yield.type.o o3-o2_o1d.quantum_yield.type.o o3-o2_o3p.quantum_yield.type.o \
             ho2.quantum_yield.type.o no3-_aq.quantum_yield.type.o ch2o.quantum_yield.type.o ch3cho-ch3_hco.quantum_yield.type.o \
             c2h5cho.quantum_yield.type.o ch2chcho.quantum_yield.type.o mvk.quantum_yield.type.o acetone-ch3co_ch3.quantum_yield.type.o \
             ch3coch2ch3-ch3co_ch2ch3.quantum_yield.type.o ch3cocho.quantum_yield.type.o clo-cl_o1d.quantum_yield.type.o \
             clo-cl_o3p.quantum_yield.type.o clono2-cl_no3.quantum_yield.type.o clono2-clo_no2.quantum_yield.type.o \
             quantum_yield_factory.o photo_kinetics.o
SW_OBJS =  abstract.spectral_wght.type.o base.spectral_wght.type.o uv-b_280_315_nm.spectral_wght.type.o \
           uv-b_280_320_nm.spectral_wght.type.o uv-a_315_400_nm.spectral_wght.type.o visplus.spectral_wght.type.o \
           gaussian_305_nm_10_nm_FWHM.spectral_wght.type.o gaussian_320_nm_10_nm_FWHM.spectral_wght.type.o \
           gaussian_340_nm_10_nm_FWHM.spectral_wght.type.o gaussian_380_nm_10_nm_FWHM.spectral_wght.type.o \
           eppley_uv_photometer.spectral_wght.type.o par_400-700nm.spectral_wght.type.o exponential_decay.spectral_wght.type.o \
           scup_mice.spectral_wght.type.o standard_human_erythema.spectral_wght.type.o UV_Index.spectral_wght.type.o \
           phytoplankton_boucher.spectral_wght.type.o plant_damage.spectral_wght.type.o plant_damage_flint_caldwell.spectral_wght.type.o \
           plant_damage_flint_caldwell_ext.spectral_wght.type.o \
           spectral_wght_factory.o spectral_wght_warehouse.o

FC = gfortran

FFLAGS = -ggdb -g -ffree-line-length-none -fcheck=bounds,do,pointer -ffpe-trap=zero,overflow,invalid -O0
#FFLAGS = -ggdb -g -ffree-line-length-none -fcheck=bounds,do,pointer -ffpe-trap=zero,overflow,invalid -O2 -fopt-info-all

#VPATH = /Users/stacy/Documents/photo_rate_constant_demo/sandbox/core:radXfer_cross_section/utils:radXfer_cross_section/types:radXfer_cross_section:\

VPATH = radXfer_cross_section:cross_section:quantum_yield:quantum_yield/types:spectral_wght/types:spectral_wght:\
        /Users/stacy/Documents/photo_rate_constant_demo/sandbox/core:\
        /Users/stacy/Documents/photo_rate_constant_demo/sandbox/project:\
        /Users/stacy/Documents/photo_rate_constant_demo/sandbox/project/cross_section_types:\
        /Users/stacy/Documents/photo_rate_constant_demo/sandbox/project/quantum_yield_types

#----------

$(EXC):		$(FOBJS) $(CORE_OBJS) $(RADXFER_OBJS) $(XSQY_OBJS) $(SW_OBJS) $(USE_INCL) 
		$(FC) $(FFLAGS) $(FOBJS) $(CORE_OBJS) $(RADXFER_OBJS) $(XSQY_OBJS) $(SW_OBJS) $(USE_INCL) $(LIBS) -o $@

$(USE_INCL):	$(INCLUDES)

.F90.o:		
		${FC} ${FFLAGS} -c ${INCLUDE_MODULES} $<

.f90.o:		
		$(FC) $(FFLAGS) -c $*.f90

.f.o:		
		$(FC) $(FFLAGS) -c $*.f

clean:		
		rm -f core *.mod $(EXC) $(USE_INCL) $(FOBJS) $(CORE_OBJS) $(RADXFER_OBJS) $(XSQY_OBJS) $(SW_OBJS)
