#
# A unit testing library for Fortran
#
# Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0.
# See LICENSE.txt in the project root for license information.
#

FC      = ifort
FFLAGS  = -module ../modules -free -std18 -standard-semantics
LDFLAGS = -save-temps ../out/libfunit.o # ../out/Unit.dlyb

SOURCES    = ExampleSuite.f ExampleRunner.f Example.f
OBJECTS    = $(patsubst %.f, %.o, $(SOURCES))
EXECUTABLE = example

# DYLD_LIBRARY_PATH=../out:$DYLD_LIBRARY_PATH ./example

all: $(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	@echo 'Linking to $@...'
	@$(FC) $(LDFLAGS) $(OBJECTS) -o $@

%.o: %.f
	@echo 'Compiling $@...'
	@$(FC) $(FFLAGS) -c $< -o $@

clean:
	@echo "Cleaning..."
	@rm -rf *.o $(EXECUTABLE)
