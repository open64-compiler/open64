CXX_FLAGS += -shared

JAVA_HOME :=
UNAME_S := $(shell uname -s)
DY_EXT :=
ifeq ($(UNAME_S),Darwin)
	JAVA_HOME = $(shell /usr/libexec/java_home)
	CXX_FLAGS += -I$(JAVA_HOME)/include
	CXX_FLAGS += -I$(JAVA_HOME)/include/darwin
	DY_EXT = dylib
else
	JAVA_HOME = $$JAVA_HOME
	ifeq ($JAVA_HOME,)
		$(error "please set JAVA_HOME first")
	endif
	CXX_FLAGS += -I$(JAVA_HOME)/include
	CXX_FLAGS += -I$(JAVA_HOME)/include/linux
	DY_EXT = so
endif

all:
	$(CXX) $(CXX_FLAGS) org_jetbrains_java_decompiler_modules_bgen_BGenDriver.cpp -o libBGenDriver.$(DY_EXT)

clean:
	-rm -f libBGenDriver.$(DY_EXT)