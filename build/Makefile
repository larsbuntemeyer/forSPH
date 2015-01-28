#
# makefile for NERD 
#

F90 = gfortran
#F90 = ifort
EXE = forSPH

SOURCE  = ../source
INCLUDE = ../include

# compiler flags
CFLAGS  = #-check all

# linker flags
LFLAGS  =

vpath %.f90 $(SOURCE) 
vpath %.h   $(INCLUDE)

# Link
$(EXE): $(EXE).o $(OBJECTS) 
	$(F90) $(EXE).o $(OBJECTS) $(LFLAGS) -o $(EXE)

include $(SOURCE)/Makefile.sph
include $(SOURCE)/Makefile.particles

OBJECTS = $(sph) $(particles)

# Compile
$(EXE).o : $(OBJECTS)

%.o: %.f90 
	$(F90) -c $(CFLAGS) $<

clean:
	rm $(OBJECTS) $(EXE) $(EXE).o *.mod 
