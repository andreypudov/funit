FC      = ifort
FFLAGS  = -O0 -xHost -c -g3 -debug all -ftrapuv -module modules -check all -free -std15 -standard-semantics -warn all -warn nounused -WB -Winline -gen-interfaces -warn interfaces -traceback -fstack-security-check -fstack-protector-all
LDFLAGS = -dynamiclib -save-temps

INTERFACES = src/Unit.f src/conditions/Conditions.f
SOURCES    = $(INTERFACES) $(shell find src -name '*.f' | sort)
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
