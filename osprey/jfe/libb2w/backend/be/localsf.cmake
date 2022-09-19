set(BE_BE_TARG_CXX_SRCS
    fill_align_targ.cxx
    driver_targ.cxx)

set(BE_REGION_CXX_SRCS
      ori.cxx
      rbi.cxx
      region_init.cxx
      region_bounds.cxx
      region_util.cxx)

set(BE_VHO_CXX_SRCS
      f90_lower.cxx
      f90_lower_dep.cxx
      vho_lower.cxx
      wb_f90_lower.cxx)

set(COMMON_COM_CXX_SRCS
      config.cxx
      config_cache.cxx
      dra_demangle.cxx
      dwarf_DST.cxx
      dwarf_DST_dump.cxx
      dwarf_DST_producer.cxx
      fb_info.cxx
      intrn_info.cxx
      ll.cxx
      mtypes.cxx
      opcode.cxx
      opcode_core.cxx
      wutil.cxx
      comp_decl.cxx)

set(BE_BE_CXX_SRCS
      cleanup.cxx
      fill_align.cxx
      goto_conv.cxx
      mem_ctr.cxx
      dra_ec.cxx
      dra_file_util.cxx
      dra_clone.cxx
      dra_mangle.cxx
      omp_lower.cxx
      rewrite_pragmas.cxx
      wb_omp.cxx
      wb_lwr.cxx)

set(COMMON_COM_PCH_CXX_SRCS
      const.cxx
      dwarf_DST_mem.cxx
      err_host.cxx
      f90_utils.cxx
      glob.cxx
      ir_bcom.cxx
      ir_bread.cxx
      ir_bwrite.cxx
      ir_reader.cxx
      irbdata.cxx
      pu_info.cxx
      strtab.cxx
      symtab.cxx
      symtab_verify.cxx
      ttype.cxx
      wn.cxx
      wn_map.cxx
      wn_pragmas.cxx
      wn_simp.cxx
      wn_util.cxx
      xstats.cxx)

set(IPA_OPTIMIZE_CXX_SRCS
      ipo_tlog_utils.cxx)

set(COMMON_COM_TARG_CXX_SRCS
      config_cache_targ.cxx
      config_targ.cxx
      config_elf_targ.cxx
      targ_const.cxx
      targ_sim.cxx
      config_host.c
      config_platform.c )

set(COMMON_TARG_INFO_ACCESS_SRCS
      ti_init.c)

if(BUILD_TARGET_MIPS)
set(COMMON_TARG_INFO_ACCESS_SRCS
      ti_init.c
      ti_res.c
      ti_res_count.c
      ti_res_res.c)
endif()

if(BUILD_TARGET_SL)
set(COMMON_TARG_INFO_ACCESS_SRCS
  ti_init.c
  ti_res.c
  ti_res_count.c
  ti_res_res.c)
endif()

if(BUILD_TARGET_LOONGSON)
set(COMMON_TARG_INFO_ACCESS_SRCS
  ti_init.c
  ti_res.c
  ti_res_count.c
  ti_res_res.c)
endif()

if(BUILD_TARGET_X8664)
set(COMMON_UTIL_QUAD_SRCS
  c_q_add.c
  c_q_div.c
  c_q_mul.c
  c_q_neg.c
  c_q_rel.c
  c_q_sqrt.c
  c_q_sub.c
  c_q_to_a.c
  c_qtenscale.c
  ${BUILD_TARGET_PREFIX}/c_qwmultu.c
  quadsim.c)

else()
  if (BUILD_TARGET_MIPS)
    if(BUILD_LINUX)
      set(COMMON_UTIL_QUAD_SRCS )
    else()
      set(COMMON_UTIL_QUAD_SRCS
        c_q_add.c
        c_q_div.c
        c_q_mul.c
        c_q_neg.c
        c_q_rel.c
        c_q_sqrt.c
        c_q_sub.c
        c_q_to_a.c
        c_qtenscale.c
        quadsim.c)
    endif()
  else()
    if (BUILD_TARGET_SL)
      if (BUILD_LINUX)
        set(COMMON_UTIL_QUAD_SRCS )
      else()
        set(COMMON_UTIL_QUAD_SRCS
          c_q_add.c
          c_q_div.c
          c_q_mul.c
          c_q_neg.c
          c_q_rel.c
          c_q_sqrt.c
          c_q_sub.c
          c_q_to_a.c
          c_qtenscale.c
          quadsim.c)
      endif()
    else()
      if (BUILD_LOONGSON)
        set(COMMON_UTIL_QUAD_SRCS
          softfloat.c
          c_q_to_a.c
          c_qtenscale.c
          c_qwmultu.c
          quadsim.c)
      else()
         set(COMMON_UTIL_QUAD_SRCS )
      endif()
    endif()
  endif()
endif()

if (BUILD_TYPE_NONSHARED)
    set(COMMON_UTIL_SRCS dso.c)
endif()

if(LOCAL_COMMON_UTIL_SRCS)
    set(COMMON_UTIL_SRCS ${COMMON_UTIL_SRCS} ${LOCAL_COMMON_UTIL_SRCS})
endif()


set(BE_COM_CXX_SRCS
  be_util.cxx
  be_version.cxx
  cxx_base.cxx
  cxx_graph.cxx
  cxx_hash_util.cxx
  mfmc_misc.cxx
  mfmc_query.cxx
  mfmc_setup.cxx
  mfmc_solve.cxx
  opt_addr_flags.cxx
  opt_cvtl_rule.cxx
  printsrc.cxx
  timing.cxx
  weak.cxx
  output_func_start_profiler.cxx
  wn_cyg_instrument.cxx
  wn_instrument.cxx
  wn_cfg.cxx
  tls.cxx
  be_memop_annot.cxx
  wssa_update.cxx
  wssa_utils.cxx
  wssa_defs.cxx
  wssa_sym.cxx
  wssa_mgr.cxx
  wssa_points_to.cxx
  wssa_du.cxx
  wssa_wn.cxx
  wssa_io.cxx
  comp_driver.cxx
  whirl_file_mgr.cxx
  whirl_file_ctx.cxx
  com_whirlview.cxx
  DaVinci.cxx
  privatize_common.cxx
  phase.c)

if(BUILD_SKIP_LNO)
else()
set(BE_COM_CXX_SRCS
  ipa_lno_file.cxx
  ipa_lno_summary.cxx
  ipa_lno_util.cxx)
endif()

set(BE_COM_PCH_CXX_SRCS
  alias_analyzer.cxx
  be_symtab.cxx
  constraint_graph.cxx
  constraint_graph_solve.cxx
  constraint_graph_escanal.cxx
  cse_table.cxx
  data_layout.cxx
  dep_graph.cxx
  dvector.cxx
  emulate.cxx
  fb_cfg.cxx
  fb_whirl.cxx
  ipa_be_read.cxx
  nystrom_alias_analyzer.cxx
  opt_alias_mgr.cxx
  opt_alias_rule.cxx
  opt_goto.cxx
  opt_ipaa_io.cxx
  opt_points_to_non_template.cxx
  opt_points_to_summary.cxx
  standardize.cxx
  stblock.cxx
  w2op.cxx
  wb.cxx
  wb_browser.cxx
  wb_buffer.cxx
  wb_carray.cxx
  wb_ipl.cxx
  wb_util.cxx
  whirl2src.cxx
  wn_fio.cxx
  wn_lower.cxx
  u64_lower_wn.cxx
  wn_mp.cxx
  wn_verifier.cxx
  wn_retype_expr.cxx
  wn_unroll.cxx
  $(DERIVED_SRCS)
)

set(BE_COM_TARG_CXX_SRCS
  betarget.cxx
  sections.cxx)

set(BE_COM_CXX_SRCS
  clone.cxx
  clone_DST_utils.cxx)

set(BE_LNO_CXX_SRCS
  soe.cxx
  mat.cxx)

set(COMMON_UTIL_CXX_SRCS
  cxx_memory.cxx
  errors.cxx
  vcg.cxx
  options_stack.cxx
  bitset.c
  file_util.c
  flags.c
  memory.c
  priority_queue.c
  resource.c
  tlog.c
  tracing.c
  util.c
  vstring.c
  ${COMMON_UTIL_QUAD_SRCS}
)

set(INSTR_COM_CXX_SRCS
  instr_reader.cxx)

if (BUILD_TARGET_MIPS)
    set(COMMON_UTIL_TARG_AS_SRCS
        c_qwmultu.s)
else ()
    if (BUILD_TARGET_SL)
        set(COMMON_UTIL_TARG_AS_SRCS
            c_qwmultu.s)
    else ()
        if (BUILD_TARGET_LOONGSON)
            set(COMMON_UTIL_TARG_AS_SRCS
                c_qwmultu.s)
        else ()
            set(COMMON_UTIL_TARG_AS_SRCS)
        endif ()
    endif ()
endif ()

#
# source files for be
#

set(BE_DRIVER_CXX_SRCS
  driver.cxx
  driver_util.cxx
  iter.cxx)


set(BE_DSO_DEPENDENCY m dl)

set(NON_SHARED_DEPENDENCY -Wl,--export-dynamic)


#----------------------------------------------------------------------
#  Variables for ld_plugin.so
#----------------------------------------------------------------------

set(LD_PLUGIN_CXX_SRCS
    ld_plugin.cxx
    whirl_utils.cxx
    whirl_driver.cxx)

set(LD_PLUGIN_BE_SRCS
    be/be/driver_util.cxx
    be/com/phase.cxx)

set(PREG_LIST_SRCS
    preg_list.cxx)

foreach(ONE_C_FILE IN LISTS BE_BE_TARG_CXX_SRCS)
    get_osprey_file_path(${ONE_C_FILE} ${BE_BE_DIR}/${BUILD_TARGET_PREFIX} BE_BE_OUT_SRCS)
endforeach()

foreach(ONE_C_FILE IN LISTS BE_DRIVER_CXX_SRCS)
    get_osprey_file_path(${ONE_C_FILE} ${BE_BE_DIR} BE_BE_OUT_SRCS)
endforeach()

foreach(ONE_C_FILE IN LISTS BE_REGION_CXX_SRCS)
    get_osprey_file_path(${ONE_C_FILE} ${BE_REGION_DIR} BE_BE_OUT_SRCS)
endforeach()

foreach(ONE_C_FILE IN LISTS BE_VHO_CXX_SRCS)
    get_osprey_file_path(${ONE_C_FILE} ${BE_VHO_DIR} BE_BE_OUT_SRCS)
endforeach()


foreach (ONE_C_FILE IN LISTS BE_BE_CXX_SRCS)
    get_osprey_file_path(${ONE_C_FILE} ${BE_BE_DIR} BE_BE_OUT_SRCS)
endforeach()
foreach (ONE_C_FILE IN LISTS BE_BE_TARG_CXX_SRCS)
    get_osprey_file_path(${ONE_C_FILE} ${BE_BE_TARG_DIR} BE_BE_OUT_SRCS)
endforeach()
foreach (ONE_C_FILE IN LISTS BE_COM_CXX_SRCS)
    get_osprey_file_path(${ONE_C_FILE} ${BE_COM_DIR} BE_BE_OUT_SRCS)
endforeach()
foreach (ONE_C_FILE IN LISTS BE_REGION_CXX_SRCS)
    get_osprey_file_path(${ONE_C_FILE} ${BE_REGION_DIR} BE_BE_OUT_SRCS)
endforeach()
foreach (ONE_C_FILE IN LISTS BE_LNO_CXX_SRCS)
    get_osprey_file_path(${ONE_C_FILE} ${BE_LNO_DIR} BE_BE_OUT_SRCS)
endforeach()

foreach (ONE_C_FILE IN LISTS BE_VHO_CXX_SRCS)
    get_osprey_file_path(${ONE_C_FILE} ${BE_VHO_DIR} BE_BE_OUT_SRCS)
endforeach()

foreach (ONE_C_FILE IN LISTS BE_COM_TARG_CXX_SRCS)
    get_osprey_file_path(${ONE_C_FILE} ${BE_COM_TARG_DIR} BE_BE_OUT_SRCS)
endforeach()

foreach (ONE_C_FILE IN LISTS COMMON_COM_TARG_CXX_SRCS)
    get_osprey_file_path(${ONE_C_FILE} ${COMMON_COM_TARG_DIR} BE_BE_OUT_SRCS)
endforeach()

foreach (ONE_C_FILE IN LISTS INSTR_COM_CXX_SRCS)
    get_osprey_file_path(${ONE_C_FILE} ${INSTR_COM_DIR} BE_BE_OUT_SRCS)
endforeach()

foreach (ONE_C_FILE IN LISTS IPA_OPTIMIZE_CXX_SRCS)
    get_osprey_file_path(${ONE_C_FILE} ${IPA_OPTIMIZE_DIR} BE_BE_OUT_SRCS)
endforeach()

foreach (ONE_C_FILE IN LISTS PREG_LIST_SRCS)
#    get_osprey_file_path(${ONE_C_FILE} "../../build/osprey/targdir/targ_info" BE_BE_OUT_SRCS)
endforeach()

foreach(ONE_C_FILE IN LISTS COMMON_COM_CXX_SRCS )
    get_osprey_file_path(${ONE_C_FILE} ${COMMON_COM_DIR} BE_BE_OUT_SRCS)
endforeach()

foreach(ONE_C_FILE IN LISTS COMMON_COM_TARG_SRCS )
    get_osprey_file_path(${ONE_C_FILE} ${COMMON_COM_DIR}/${BUILD_TARGET_PREFIX} BE_BE_OUT_SRCS)
endforeach()

foreach(ONE_C_FILE IN LISTS COMMON_COM_TARG_CXX_SRCS )
    get_osprey_file_path(${ONE_C_FILE} ${COMMON_COM_DIR}/${BUILD_TARGET_PREFIX} BE_BE_OUT_SRCS)
endforeach()

foreach(ONE_C_FILE IN LISTS COMMON_UTIL_CXX_SRCS )
    get_osprey_file_path(${ONE_C_FILE} ${COMMON_UTIL_DIR} BE_BE_OUT_SRCS)
endforeach()


foreach(ONE_C_FILE IN LISTS COMMON_UTIL_TARG_CXX_SRCS )
    get_osprey_file_path(${ONE_C_FILE} ${COMMON_UTIL_DIR} BE_BE_OUT_SRCS)
endforeach()

#foreach(ONE_C_FILE IN LISTS COMMON_COM_PCH_CXX_SRCS )
#    get_osprey_file_path(${ONE_C_FILE} ${COMMON_COM_DIR}/${BUILD_TARGET_PREFIX} BE_BE_OUT_SRCS)
#endforeach()

foreach(ONE_C_FILE IN LISTS LD_PLUGIN_CXX_SRCS)
    get_osprey_file_path(${ONE_C_FILE} ${LD_PLUGIN_DIR} LD_PLUGIN_SRCS)
endforeach()


foreach(ONE_C_FILE IN LISTS LD_PLUGIN_BE_SRCS)
    get_osprey_file_path(${ONE_C_FILE} "" LD_PLUGIN_SRCS)
endforeach()
