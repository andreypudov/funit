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
FFLAGS  = -module ../modules -free -std15 -standard-semantics
LDFLAGS = -save-temps ../out/Unit.dlyb

SOURCES    = ExampleSuite.f ExampleRunner.f Example.f
OBJECTS    = $(patsubst %.f, %.o, $(SOURCES))
EXECUTABLE = Example

# DYLD_LIBRARY_PATH=../out:$DYLD_LIBRARY_PATH ./Example

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
