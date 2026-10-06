# Link the FHE-owned tensor arithmetic fixture against ir_tools' native
# WHIRL objects without adding a shared ir_tools target. See
# doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.

fhe_ckks_tensor_arithmetic_test: $(OBJECTS) controls.o \
        fhe_ckks_tensor_arithmetic_test.o fhe_ckks_expand.o \
        fhe_ckks_transfer.o
	$(link.c++f) -o $@ fhe_ckks_tensor_arithmetic_test.o \
		fhe_ckks_expand.o fhe_ckks_transfer.o controls.o \
		$(OBJECTS) $(LDFLAGS)

fhe_ckks_tensor_arithmetic_test.o: \
        $(BUILD_TOT)/be/vho/tests/fhe_ckks_tensor_arithmetic_test.cxx
	$(cxx) -c $(CPPFLAGS) $(CXXFLAGS) $< -o $@

fhe_ckks_expand.o: $(BUILD_TOT)/be/vho/fhe_ckks_expand.cxx
	$(cxx) -c $(CPPFLAGS) $(CXXFLAGS) $< -o $@

fhe_ckks_transfer.o: $(BUILD_TOT)/be/vho/fhe_ckks_transfer.cxx
	$(cxx) -c $(CPPFLAGS) $(CXXFLAGS) $< -o $@
