cd ../../open64
tar zvcf c.gz osprey/common/com/fb_info.h osprey/common/com/segmented_array.h osprey/common/com/strtab.cxx osprey/common/com/symtab.cxx osprey/common/com/targ_sim_core.h osprey/common/com/wn_tree_util.h osprey/common/com/wn_util.h osprey/common/com/x8664/targ_sim.cxx osprey/common/util/errors.h osprey/common/util/memory.c osprey/common/util/resource.c osprey/common/util/vcg.cxx osprey/common/util/vstring.c osprey/include/gnu/demangle.h osprey/ir_tools/ir_size.cxx osprey/wgen/wgen_expr.cxx osprey/wgen/wgen_spin_symbol.cxx osprey/wgen/wgen_stmt.cxx osprey/libcmplrs/array_alloc.c osprey/macos
mv c.gz ../new_cmake_test/utils
cd ../new_cmake_test/utils
mv c.gz changed_new.tar.gz
