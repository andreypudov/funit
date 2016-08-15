FC      = ifort
FFLAGS  = -c -std15 -standard-semantics -free -module modules -g3 -warn all -warn nounused -check all
LDFLAGS = -save-temps -dynamiclib -mmacosx-version-min=10.6

INTERFACES = src/Unit.f
SOURCES    = $(INTERFACES) $(shell find . -name '*.f' | sed 's/^\.\///' | sort)
OBJECTS    = $(patsubst %.f, out/%.o, $(SOURCES))
EXECUTABLE = Unit

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
