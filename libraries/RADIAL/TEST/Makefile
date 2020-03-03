name     = test_rad

OPTS0    = -fast -module $(BSRMODS) 
OPTS1    = -c -fast -module $(BSRMODS) 

OBJECTS  = test_rad.o

LIBS =  -lrad90 -lzcom90

test_rad: $(OBJECTS)
	$(FC) -o $(name) $(OPTS0) -L$(BSRLIBS)  \
	$(OBJECTS) $(LIBS)
	rm *.o 

test_rad.o: test_rad.f90
	$(FC) $(OPTS1) test_rad.f90

