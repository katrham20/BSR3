include ${HOME}/DEFS_03

N  = lib_bsr_mpi
 
S  =	br_bsorb.f90      br_channel.f90   br_conf_LS.f90  br_conf_LS1.f90   \
	br_core.f90       br_core1.f90     br_dets.f90     br_file.f90       \
	br_grid.f90       br_orb.f90       br_phys_orb.f90                   \
	br_symc_LS.f90    br_symt_LS.f90   br_target.f90   br_radial_overlaps.f90

# br_ovl.f90  - only version 04
 
O = $(S:.f90=.o)
 
$(N).a : $(O)
	ar rv $(N).a $(O)
	mv $(N).a $(LIBDIR)
	rm *.o 
 
clean:
	rm -f *.o  *~
 
.SUFFIXES:
.SUFFIXES: .f90 .mod .o 
 
.f90.o:
	$(FC) $(compL) $<
 
