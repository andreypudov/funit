#
# A unit testing library for Fortran
#
# Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0.
# See LICENSE.txt in the project root for license information.
#

FC      = ifort
FFLAGS  = -O0 -c -module modules -free -std18 -standard-semantics -fPIC
LDFLAGS = -shared -save-temps # -dynamiclib

SOURCES    = src/utils/Arguments.f \
             src/logger/Logger.f \
             src/FUnit.f \
             src/conditions/Conditions.f \
             src/asserts/ArrayEquals.f \
             src/asserts/Equals.f \
             src/asserts/Fail.f \
             src/asserts/False.f \
             src/asserts/NotNull.f \
             src/asserts/NotSame.f \
             src/asserts/Null.f \
             src/asserts/Same.f \
             src/asserts/True.f \
             src/conditions/ArrayEquals.f \
             src/conditions/Equals.f \
             src/conditions/False.f \
             src/conditions/NotNull.f \
             src/conditions/NotSame.f \
             src/conditions/Null.f \
             src/conditions/Same.f \
             src/conditions/True.f \
             src/core/UnitContext.f \
             src/core/UnitRunner.f \
             src/core/UnitSuite.f \
             src/expects/ArrayEquals.f \
             src/expects/Equals.f \
             src/expects/Fail.f \
             src/expects/False.f \
             src/expects/NotNull.f \
             src/expects/NotSame.f \
             src/expects/Null.f \
             src/expects/Same.f \
             src/expects/True.f \
             src/logger/ConsoleLogger.f \
             src/logger/JSONLogger.f \
             src/logger/UnitLogger.f
OBJECTS    = $(patsubst %.f, out/%.o, $(SOURCES))
EXECUTABLE = libfunit.o # FUnit.dlyb

all: $(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	@echo 'Linking to $@...'
	@$(FC) $(LDFLAGS) $(OBJECTS) -o out/$@

out/%.o: %.f
	@echo 'Compiling $@...'
	@mkdir -p modules
	@mkdir -p $(dir $@)
	@$(FC) $(FFLAGS) -c $< -o $@

clean:
	@echo "Cleaning..."
	@rm -rf modules out $(EXECUTABLE)
