#
# A unit testing library for Fortran
#
# The MIT License
#
# Copyright 2011-2016 Andrey Pudov
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the 'Software'), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#

FC      = ifort
FFLAGS  = -O0 -xHost -c -module modules -free -std15 -standard-semantics
LDFLAGS = -dynamiclib -save-temps

INTERFACES = src/utils/Arguments.f src/logger/Logger.f src/Unit.f src/conditions/Conditions.f
SOURCES    = $(INTERFACES) $(shell find src -name '*.f' | sed 's/^\.\///' | sort)
OBJECTS    = $(patsubst %.f, out/%.o, $(SOURCES))
EXECUTABLE = Unit.dlyb

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
