N       = test_bs

FC      = pgf90 -Mdefaultunit -Mbounds

BSRLIBS = ${HOME}/T/LIBS
BSRMODS = ${HOME}/T/LIBS
BSRPROG = ${HOME}/BIN

Olink   = -fast -L$(BSRLIBS) -module $(BSRMODS)
Ocomp   = -c -fast -module $(BSRMODS)   

O       = test_bs.o

LIBS =  -lBS -lzcom90 -llapack -lblas

$(N): $(O)
	$(FC) -o $(N)  $(O) $(Olink) $(LIBS)
	rm *.o 

test_bs.o: test_bs.f90
	$(FC) $(Ocomp) test_bs.f90

