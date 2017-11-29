TARGET = G4Mag
TARGET1 = G4Mag_EqRhs
TARGET2 = G4Mag_UsualEqRhs
TARGET3 = G4MagneticField
INCLUDES = -I../.. -I..
FLAGS = $(CXXFLAGS) -w $(XF1) $(XF2)
TIMING = ../..
OBJECTS = $(TIMING)/timing.o $(TARGET).o $(TARGET2).o $(TARGET3).o
LIBS=-lm
ALL=$(OBJECTS) test$(TARGET)

all: $(ALL)

$(TIMING)/timing.o: $(TIMING)/timing.cpp
	@$(CXX) $(INCLUDES) $(FLAGS) -c $< -o $@

$(TARGET).o: $(TARGET1).cpp
	@$(CXX) $(FLAGS) $(INCLUDES) -c $< -o $@

$(TARGET2).o: $(TARGET2).cpp
	@$(CXX) $(FLAGS) $(INCLUDES) -c $< -o $@

$(TARGET3).o: $(TARGET3).cpp
	@$(CXX) $(FLAGS) $(INCLUDES) -c $< -o $@

test$(TARGET): test$(TARGET).cpp $(OBJECTS)
	$(CXX) $(FLAGS) $(INCLUDES) $(OBJECTS) $< -o $@ $(LIBS)

clean:
	@rm -f *~ $(ALL)
