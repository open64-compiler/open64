# Link the FHE Conv asset admission fixture against ir_tools native WHIRL.
# Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.

fhe_ckks_conv_assets_test: $(OBJECTS) controls.o \
        fhe_ckks_conv_assets_test.o fhe_ckks_conv_assets.o \
        fhe_ckks_conv_recipe.o
	$(link.c++f) -o $@ fhe_ckks_conv_assets_test.o \
		fhe_ckks_conv_assets.o fhe_ckks_conv_recipe.o controls.o \
		$(OBJECTS) $(LDFLAGS)

fhe_ckks_conv_assets_test.o: \
        $(BUILD_TOT)/be/vho/tests/fhe_ckks_conv_assets_test.cxx
	$(cxx) -c $(CPPFLAGS) $(CXXFLAGS) $< -o $@

fhe_ckks_conv_assets.o: $(BUILD_TOT)/be/vho/fhe_ckks_conv_assets.cxx
	$(cxx) -c $(CPPFLAGS) $(CXXFLAGS) $< -o $@

fhe_ckks_conv_recipe.o: $(BUILD_TOT)/be/vho/fhe_ckks_conv_recipe.cxx
	$(cxx) -c $(CPPFLAGS) $(CXXFLAGS) $< -o $@
