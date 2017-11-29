TARGET = TGeoPgon
TARGET2 = TGeoPcon
INCLUDES = -I../.. -I..
FLAGS = $(CXXFLAGS) -w  $(XF1) $(XF2)
TIMING = ../..
OBJECTS = $(TIMING)/timing.o $(TARGET2).o $(TARGET).o

LIBS=-lm
ALL=$(OBJECTS) test$(TARGET)

all: $(ALL)

$(TIMING)/timing.o: $(TIMING)/timing.cpp
	@$(CXX) $(INCLUDES) -O2 -c $< -o $@

$(TARGET).o: $(TARGET).cpp
	@$(CXX) $(FLAGS) $(INCLUDES) -c $< -o $@

$(TARGET2).o: $(TARGET2).cpp
	@$(CXX) $(FLAGS) $(INCLUDES) -c $< -o $@

test$(TARGET): test$(TARGET).cpp $(OBJECTS)
	$(CXX) $(FLAGS) $(INCLUDES) $(OBJECTS) $< -o $@ $(LIBS)

clean:
	@rm -f *~ $(ALL)
