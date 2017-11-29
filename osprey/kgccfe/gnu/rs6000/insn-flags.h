/* Generated automatically by the program `genflags'
   from the machine description file `md'.  */

#ifndef GCC_INSN_FLAGS_H
#define GCC_INSN_FLAGS_H

#define HAVE_extendqidi2 (TARGET_POWERPC64)
#define HAVE_extendqisi2_ppc (TARGET_POWERPC)
#define HAVE_extendqihi2_ppc (TARGET_POWERPC)
#define HAVE_one_cmplsi2 1
#define HAVE_abssi2_nopower (! TARGET_POWER && ! TARGET_ISEL)
#define HAVE_negsi2 1
#define HAVE_ffssi2 1
#define HAVE_mulsi3_mq (TARGET_POWER)
#define HAVE_mulsi3_no_mq (! TARGET_POWER)
#define HAVE_udivsi3_mq (TARGET_POWERPC && TARGET_POWER)
#define HAVE_divsi3_mq (TARGET_POWERPC && TARGET_POWER)
#define HAVE_mulh_call (! TARGET_POWER && ! TARGET_POWERPC)
#define HAVE_mull_call (! TARGET_POWER && ! TARGET_POWERPC)
#define HAVE_divss_call (! TARGET_POWER && ! TARGET_POWERPC)
#define HAVE_divus_call (! TARGET_POWER && ! TARGET_POWERPC)
#define HAVE_quoss_call (! TARGET_POWER && ! TARGET_POWERPC)
#define HAVE_quous_call (! TARGET_POWER && ! TARGET_POWERPC)
#define HAVE_andsi3 1
#define HAVE_insvsi 1
#define HAVE_insvdi (TARGET_POWERPC64)
#define HAVE_extzvsi 1
#define HAVE_extzvdi (TARGET_POWERPC64)
#define HAVE_rotlsi3 1
#define HAVE_ashlsi3_power (TARGET_POWER)
#define HAVE_ashlsi3_no_power (! TARGET_POWER)
#define HAVE_lshrsi3_power (TARGET_POWER)
#define HAVE_lshrsi3_no_power (! TARGET_POWER)
#define HAVE_ashrsi3_power (TARGET_POWER)
#define HAVE_ashrsi3_no_power (! TARGET_POWER)
#define HAVE_extendsfdf2 (TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_truncdfsf2 (TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_aux_truncdfsf2 (! TARGET_POWERPC && TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_negdf2 (TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_absdf2 (TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_adddf3 (TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_subdf3 (TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_muldf3 (TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_divdf3 (TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_sqrtdf2 ((TARGET_PPC_GPOPT || TARGET_POWER2) && TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_fctiwz ((TARGET_POWER2 || TARGET_POWERPC) && TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_floatdidf2 (TARGET_POWERPC64 && TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_floatsidf_ppc64 (TARGET_POWERPC64 && TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_floatunssidf_ppc64 (TARGET_POWERPC64 && TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_fix_truncdfdi2 (TARGET_POWERPC64 && TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_floatdisf2_internal1 (TARGET_POWERPC64 && TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_mulsidi3_mq (TARGET_POWER)
#define HAVE_umulsidi3_mq (TARGET_POWERPC && TARGET_POWER)
#define HAVE_smulsi3_highpart_mq (TARGET_POWER)
#define HAVE_umulsi3_highpart_mq (TARGET_POWERPC && TARGET_POWER)
#define HAVE_ashldi3_power (TARGET_POWER)
#define HAVE_lshrdi3_power (TARGET_POWER)
#define HAVE_ashrdi3_power (TARGET_POWER)
#define HAVE_ashrdi3_no_power (TARGET_32BIT && !TARGET_POWER)
#define HAVE_one_cmpldi2 (TARGET_POWERPC64)
#define HAVE_absdi2 (TARGET_POWERPC64)
#define HAVE_ffsdi2 (TARGET_POWERPC64)
#define HAVE_muldi3 (TARGET_POWERPC64)
#define HAVE_smuldi3_highpart (TARGET_POWERPC64)
#define HAVE_umuldi3_highpart (TARGET_POWERPC64)
#define HAVE_udivdi3 (TARGET_POWERPC64)
#define HAVE_rotldi3 (TARGET_POWERPC64)
#define HAVE_ashldi3_internal5 (TARGET_POWERPC64 && includes_rldic_lshift_p (operands[2], operands[3]))
#define HAVE_ashldi3_internal8 (TARGET_POWERPC64 && includes_rldicr_lshift_p (operands[2], operands[3]))
#define HAVE_anddi3 (TARGET_POWERPC64)
#define HAVE_elf_high (TARGET_ELF && ! TARGET_64BIT)
#define HAVE_elf_low (TARGET_ELF && ! TARGET_64BIT)
#define HAVE_extenddftf2 (DEFAULT_ABI == ABI_AIX && TARGET_HARD_FLOAT && TARGET_FPRS \
   && TARGET_LONG_DOUBLE_128)
#define HAVE_extendsftf2 (DEFAULT_ABI == ABI_AIX && TARGET_HARD_FLOAT && TARGET_FPRS \
   && TARGET_LONG_DOUBLE_128)
#define HAVE_trunctfdf2 (DEFAULT_ABI == ABI_AIX && TARGET_HARD_FLOAT && TARGET_FPRS \
   && TARGET_LONG_DOUBLE_128)
#define HAVE_trunctfsf2 (DEFAULT_ABI == ABI_AIX && TARGET_HARD_FLOAT \
   && TARGET_FPRS && TARGET_LONG_DOUBLE_128)
#define HAVE_floatditf2 (DEFAULT_ABI == ABI_AIX && TARGET_POWERPC64 \
   && TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_LONG_DOUBLE_128)
#define HAVE_floatsitf2 (DEFAULT_ABI == ABI_AIX && TARGET_HARD_FLOAT && TARGET_FPRS \
   && TARGET_LONG_DOUBLE_128)
#define HAVE_fix_trunctfdi2 (DEFAULT_ABI == ABI_AIX && TARGET_POWERPC64 \
   && TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_LONG_DOUBLE_128)
#define HAVE_fix_trunctfsi2 (DEFAULT_ABI == ABI_AIX && TARGET_HARD_FLOAT && TARGET_FPRS \
   && TARGET_LONG_DOUBLE_128)
#define HAVE_negtf2 (DEFAULT_ABI == ABI_AIX && TARGET_HARD_FLOAT && TARGET_FPRS \
   && TARGET_LONG_DOUBLE_128)
#define HAVE_abstf2 (DEFAULT_ABI == ABI_AIX && TARGET_HARD_FLOAT && TARGET_FPRS \
   && TARGET_LONG_DOUBLE_128)
#define HAVE_movdi_update (TARGET_POWERPC64 && TARGET_UPDATE)
#define HAVE_movsi_update (TARGET_UPDATE)
#define HAVE_load_toc_aix_si (DEFAULT_ABI == ABI_AIX && TARGET_32BIT)
#define HAVE_load_toc_aix_di (DEFAULT_ABI == ABI_AIX && TARGET_64BIT)
#define HAVE_load_toc_v4_pic_si (DEFAULT_ABI == ABI_V4 && flag_pic == 1 && TARGET_32BIT)
#define HAVE_load_toc_v4_PIC_1 (TARGET_ELF && DEFAULT_ABI != ABI_AIX && flag_pic == 2)
#define HAVE_load_toc_v4_PIC_1b (TARGET_ELF && DEFAULT_ABI != ABI_AIX && flag_pic == 2)
#define HAVE_load_toc_v4_PIC_2 (TARGET_ELF && DEFAULT_ABI != ABI_AIX && flag_pic == 2)
#define HAVE_load_macho_picbase ((DEFAULT_ABI == ABI_DARWIN) && flag_pic)
#define HAVE_macho_correct_pic (DEFAULT_ABI == ABI_DARWIN)
#define HAVE_blockage 1
#define HAVE_jump 1
#define HAVE_return (direct_return ())
#define HAVE_indirect_jumpsi (TARGET_32BIT)
#define HAVE_indirect_jumpdi (TARGET_64BIT)
#define HAVE_nop 1
#define HAVE_trap 1
#define HAVE_movesi_from_cr 1
#define HAVE_stack_tie 1
#define HAVE_eh_set_lr_si (TARGET_32BIT)
#define HAVE_eh_set_lr_di (TARGET_64BIT)
#define HAVE_prefetch (TARGET_POWERPC)
#define HAVE_altivec_lvx_4si (TARGET_ALTIVEC)
#define HAVE_altivec_lvx_8hi (TARGET_ALTIVEC)
#define HAVE_altivec_lvx_16qi (TARGET_ALTIVEC)
#define HAVE_altivec_lvx_4sf (TARGET_ALTIVEC)
#define HAVE_altivec_stvx_4si (TARGET_ALTIVEC)
#define HAVE_altivec_stvx_8hi (TARGET_ALTIVEC)
#define HAVE_altivec_stvx_16qi (TARGET_ALTIVEC)
#define HAVE_altivec_stvx_4sf (TARGET_ALTIVEC)
#define HAVE_get_vrsave_internal (TARGET_ALTIVEC)
#define HAVE_addv16qi3 (TARGET_ALTIVEC)
#define HAVE_addv8hi3 (TARGET_ALTIVEC)
#define HAVE_addv4si3 (TARGET_ALTIVEC)
#define HAVE_addv4sf3 (TARGET_ALTIVEC)
#define HAVE_altivec_vaddcuw (TARGET_ALTIVEC)
#define HAVE_altivec_vaddubs (TARGET_ALTIVEC)
#define HAVE_altivec_vaddsbs (TARGET_ALTIVEC)
#define HAVE_altivec_vadduhs (TARGET_ALTIVEC)
#define HAVE_altivec_vaddshs (TARGET_ALTIVEC)
#define HAVE_altivec_vadduws (TARGET_ALTIVEC)
#define HAVE_altivec_vaddsws (TARGET_ALTIVEC)
#define HAVE_andv4si3 (TARGET_ALTIVEC)
#define HAVE_altivec_vandc (TARGET_ALTIVEC)
#define HAVE_altivec_vavgub (TARGET_ALTIVEC)
#define HAVE_altivec_vavgsb (TARGET_ALTIVEC)
#define HAVE_altivec_vavguh (TARGET_ALTIVEC)
#define HAVE_altivec_vavgsh (TARGET_ALTIVEC)
#define HAVE_altivec_vavguw (TARGET_ALTIVEC)
#define HAVE_altivec_vavgsw (TARGET_ALTIVEC)
#define HAVE_altivec_vcmpbfp (TARGET_ALTIVEC)
#define HAVE_altivec_vcmpequb (TARGET_ALTIVEC)
#define HAVE_altivec_vcmpequh (TARGET_ALTIVEC)
#define HAVE_altivec_vcmpequw (TARGET_ALTIVEC)
#define HAVE_altivec_vcmpeqfp (TARGET_ALTIVEC)
#define HAVE_altivec_vcmpgefp (TARGET_ALTIVEC)
#define HAVE_altivec_vcmpgtub (TARGET_ALTIVEC)
#define HAVE_altivec_vcmpgtsb (TARGET_ALTIVEC)
#define HAVE_altivec_vcmpgtuh (TARGET_ALTIVEC)
#define HAVE_altivec_vcmpgtsh (TARGET_ALTIVEC)
#define HAVE_altivec_vcmpgtuw (TARGET_ALTIVEC)
#define HAVE_altivec_vcmpgtsw (TARGET_ALTIVEC)
#define HAVE_altivec_vcmpgtfp (TARGET_ALTIVEC)
#define HAVE_altivec_vmaddfp (TARGET_ALTIVEC)
#define HAVE_altivec_vnmsubfp (TARGET_ALTIVEC)
#define HAVE_altivec_vmsumubm (TARGET_ALTIVEC)
#define HAVE_altivec_vmsummbm (TARGET_ALTIVEC)
#define HAVE_altivec_vmsumuhm (TARGET_ALTIVEC)
#define HAVE_altivec_vmsumshm (TARGET_ALTIVEC)
#define HAVE_altivec_vmsumuhs (TARGET_ALTIVEC)
#define HAVE_altivec_vmsumshs (TARGET_ALTIVEC)
#define HAVE_umaxv16qi3 (TARGET_ALTIVEC)
#define HAVE_smaxv16qi3 (TARGET_ALTIVEC)
#define HAVE_umaxv8hi3 (TARGET_ALTIVEC)
#define HAVE_smaxv8hi3 (TARGET_ALTIVEC)
#define HAVE_umaxv4si3 (TARGET_ALTIVEC)
#define HAVE_smaxv4si3 (TARGET_ALTIVEC)
#define HAVE_smaxv4sf3 (TARGET_ALTIVEC)
#define HAVE_altivec_vmhaddshs (TARGET_ALTIVEC)
#define HAVE_altivec_vmhraddshs (TARGET_ALTIVEC)
#define HAVE_altivec_vmladduhm (TARGET_ALTIVEC)
#define HAVE_altivec_vmrghb (TARGET_ALTIVEC)
#define HAVE_altivec_vmrghh (TARGET_ALTIVEC)
#define HAVE_altivec_vmrghw (TARGET_ALTIVEC)
#define HAVE_altivec_vmrglb (TARGET_ALTIVEC)
#define HAVE_altivec_vmrglh (TARGET_ALTIVEC)
#define HAVE_altivec_vmrglw (TARGET_ALTIVEC)
#define HAVE_uminv16qi3 (TARGET_ALTIVEC)
#define HAVE_sminv16qi3 (TARGET_ALTIVEC)
#define HAVE_uminv8hi3 (TARGET_ALTIVEC)
#define HAVE_sminv8hi3 (TARGET_ALTIVEC)
#define HAVE_uminv4si3 (TARGET_ALTIVEC)
#define HAVE_sminv4si3 (TARGET_ALTIVEC)
#define HAVE_sminv4sf3 (TARGET_ALTIVEC)
#define HAVE_altivec_vmuleub (TARGET_ALTIVEC)
#define HAVE_altivec_vmulesb (TARGET_ALTIVEC)
#define HAVE_altivec_vmuleuh (TARGET_ALTIVEC)
#define HAVE_altivec_vmulesh (TARGET_ALTIVEC)
#define HAVE_altivec_vmuloub (TARGET_ALTIVEC)
#define HAVE_altivec_vmulosb (TARGET_ALTIVEC)
#define HAVE_altivec_vmulouh (TARGET_ALTIVEC)
#define HAVE_altivec_vmulosh (TARGET_ALTIVEC)
#define HAVE_altivec_vnor (TARGET_ALTIVEC)
#define HAVE_iorv4si3 (TARGET_ALTIVEC)
#define HAVE_altivec_vpkuhum (TARGET_ALTIVEC)
#define HAVE_altivec_vpkuwum (TARGET_ALTIVEC)
#define HAVE_altivec_vpkpx (TARGET_ALTIVEC)
#define HAVE_altivec_vpkuhss (TARGET_ALTIVEC)
#define HAVE_altivec_vpkshss (TARGET_ALTIVEC)
#define HAVE_altivec_vpkuwss (TARGET_ALTIVEC)
#define HAVE_altivec_vpkswss (TARGET_ALTIVEC)
#define HAVE_altivec_vpkuhus (TARGET_ALTIVEC)
#define HAVE_altivec_vpkshus (TARGET_ALTIVEC)
#define HAVE_altivec_vpkuwus (TARGET_ALTIVEC)
#define HAVE_altivec_vpkswus (TARGET_ALTIVEC)
#define HAVE_altivec_vrlb (TARGET_ALTIVEC)
#define HAVE_altivec_vrlh (TARGET_ALTIVEC)
#define HAVE_altivec_vrlw (TARGET_ALTIVEC)
#define HAVE_altivec_vslb (TARGET_ALTIVEC)
#define HAVE_altivec_vslh (TARGET_ALTIVEC)
#define HAVE_altivec_vslw (TARGET_ALTIVEC)
#define HAVE_altivec_vslw_v4sf (TARGET_ALTIVEC)
#define HAVE_altivec_vsl (TARGET_ALTIVEC)
#define HAVE_altivec_vslo (TARGET_ALTIVEC)
#define HAVE_altivec_vsrb (TARGET_ALTIVEC)
#define HAVE_altivec_vsrh (TARGET_ALTIVEC)
#define HAVE_altivec_vsrw (TARGET_ALTIVEC)
#define HAVE_altivec_vsrab (TARGET_ALTIVEC)
#define HAVE_altivec_vsrah (TARGET_ALTIVEC)
#define HAVE_altivec_vsraw (TARGET_ALTIVEC)
#define HAVE_altivec_vsr (TARGET_ALTIVEC)
#define HAVE_altivec_vsro (TARGET_ALTIVEC)
#define HAVE_subv16qi3 (TARGET_ALTIVEC)
#define HAVE_subv8hi3 (TARGET_ALTIVEC)
#define HAVE_subv4si3 (TARGET_ALTIVEC)
#define HAVE_subv4sf3 (TARGET_ALTIVEC)
#define HAVE_altivec_vsubcuw (TARGET_ALTIVEC)
#define HAVE_altivec_vsububs (TARGET_ALTIVEC)
#define HAVE_altivec_vsubsbs (TARGET_ALTIVEC)
#define HAVE_altivec_vsubuhs (TARGET_ALTIVEC)
#define HAVE_altivec_vsubshs (TARGET_ALTIVEC)
#define HAVE_altivec_vsubuws (TARGET_ALTIVEC)
#define HAVE_altivec_vsubsws (TARGET_ALTIVEC)
#define HAVE_altivec_vsum4ubs (TARGET_ALTIVEC)
#define HAVE_altivec_vsum4sbs (TARGET_ALTIVEC)
#define HAVE_altivec_vsum4shs (TARGET_ALTIVEC)
#define HAVE_altivec_vsum2sws (TARGET_ALTIVEC)
#define HAVE_altivec_vsumsws (TARGET_ALTIVEC)
#define HAVE_xorv4si3 (TARGET_ALTIVEC)
#define HAVE_altivec_vspltb (TARGET_ALTIVEC)
#define HAVE_altivec_vsplth (TARGET_ALTIVEC)
#define HAVE_altivec_vspltw (TARGET_ALTIVEC)
#define HAVE_altivec_vspltisb (TARGET_ALTIVEC)
#define HAVE_altivec_vspltish (TARGET_ALTIVEC)
#define HAVE_altivec_vspltisw (TARGET_ALTIVEC)
#define HAVE_altivec_vspltisw_v4sf (TARGET_ALTIVEC)
#define HAVE_ftruncv4sf2 (TARGET_ALTIVEC)
#define HAVE_altivec_vperm_4si (TARGET_ALTIVEC)
#define HAVE_altivec_vperm_4sf (TARGET_ALTIVEC)
#define HAVE_altivec_vperm_8hi (TARGET_ALTIVEC)
#define HAVE_altivec_vperm_16qi (TARGET_ALTIVEC)
#define HAVE_altivec_vrfip (TARGET_ALTIVEC)
#define HAVE_altivec_vrfin (TARGET_ALTIVEC)
#define HAVE_altivec_vrfim (TARGET_ALTIVEC)
#define HAVE_altivec_vcfux (TARGET_ALTIVEC)
#define HAVE_altivec_vcfsx (TARGET_ALTIVEC)
#define HAVE_altivec_vctuxs (TARGET_ALTIVEC)
#define HAVE_altivec_vctsxs (TARGET_ALTIVEC)
#define HAVE_altivec_vlogefp (TARGET_ALTIVEC)
#define HAVE_altivec_vexptefp (TARGET_ALTIVEC)
#define HAVE_altivec_vrsqrtefp (TARGET_ALTIVEC)
#define HAVE_altivec_vrefp (TARGET_ALTIVEC)
#define HAVE_altivec_vsel_4si (TARGET_ALTIVEC)
#define HAVE_altivec_vsel_4sf (TARGET_ALTIVEC)
#define HAVE_altivec_vsel_8hi (TARGET_ALTIVEC)
#define HAVE_altivec_vsel_16qi (TARGET_ALTIVEC)
#define HAVE_altivec_vsldoi_4si (TARGET_ALTIVEC)
#define HAVE_altivec_vsldoi_4sf (TARGET_ALTIVEC)
#define HAVE_altivec_vsldoi_8hi (TARGET_ALTIVEC)
#define HAVE_altivec_vsldoi_16qi (TARGET_ALTIVEC)
#define HAVE_altivec_vupkhsb (TARGET_ALTIVEC)
#define HAVE_altivec_vupkhpx (TARGET_ALTIVEC)
#define HAVE_altivec_vupkhsh (TARGET_ALTIVEC)
#define HAVE_altivec_vupklsb (TARGET_ALTIVEC)
#define HAVE_altivec_vupklpx (TARGET_ALTIVEC)
#define HAVE_altivec_vupklsh (TARGET_ALTIVEC)
#define HAVE_altivec_predicate_v4si (TARGET_ALTIVEC)
#define HAVE_altivec_predicate_v4sf (TARGET_ALTIVEC)
#define HAVE_altivec_predicate_v8hi (TARGET_ALTIVEC)
#define HAVE_altivec_predicate_v16qi (TARGET_ALTIVEC)
#define HAVE_altivec_mtvscr (TARGET_ALTIVEC)
#define HAVE_altivec_mfvscr (TARGET_ALTIVEC)
#define HAVE_altivec_dssall (TARGET_ALTIVEC)
#define HAVE_altivec_dss (TARGET_ALTIVEC)
#define HAVE_altivec_dst (TARGET_ALTIVEC)
#define HAVE_altivec_dstt (TARGET_ALTIVEC)
#define HAVE_altivec_dstst (TARGET_ALTIVEC)
#define HAVE_altivec_dststt (TARGET_ALTIVEC)
#define HAVE_altivec_lvsl (TARGET_ALTIVEC)
#define HAVE_altivec_lvsr (TARGET_ALTIVEC)
#define HAVE_altivec_lvebx (TARGET_ALTIVEC)
#define HAVE_altivec_lvehx (TARGET_ALTIVEC)
#define HAVE_altivec_lvewx (TARGET_ALTIVEC)
#define HAVE_altivec_lvxl (TARGET_ALTIVEC)
#define HAVE_altivec_lvx (TARGET_ALTIVEC)
#define HAVE_altivec_stvx (TARGET_ALTIVEC)
#define HAVE_altivec_stvxl (TARGET_ALTIVEC)
#define HAVE_altivec_stvebx (TARGET_ALTIVEC)
#define HAVE_altivec_stvehx (TARGET_ALTIVEC)
#define HAVE_altivec_stvewx (TARGET_ALTIVEC)
#define HAVE_absv16qi2 (TARGET_ALTIVEC)
#define HAVE_absv8hi2 (TARGET_ALTIVEC)
#define HAVE_absv4si2 (TARGET_ALTIVEC)
#define HAVE_absv4sf2 (TARGET_ALTIVEC)
#define HAVE_altivec_abss_v16qi (TARGET_ALTIVEC)
#define HAVE_altivec_abss_v8hi (TARGET_ALTIVEC)
#define HAVE_altivec_abss_v4si (TARGET_ALTIVEC)
#define HAVE_zero_extendqidi2 (TARGET_POWERPC64)
#define HAVE_zero_extendhidi2 (TARGET_POWERPC64)
#define HAVE_extendhidi2 (TARGET_POWERPC64)
#define HAVE_zero_extendsidi2 (TARGET_POWERPC64)
#define HAVE_extendsidi2 (TARGET_POWERPC64)
#define HAVE_zero_extendqisi2 1
#define HAVE_extendqisi2 1
#define HAVE_extendqisi2_power (TARGET_POWER)
#define HAVE_extendqisi2_no_power (! TARGET_POWER && ! TARGET_POWERPC)
#define HAVE_zero_extendqihi2 1
#define HAVE_extendqihi2 1
#define HAVE_extendqihi2_power (TARGET_POWER)
#define HAVE_extendqihi2_no_power (! TARGET_POWER && ! TARGET_POWERPC)
#define HAVE_zero_extendhisi2 1
#define HAVE_extendhisi2 1
#define HAVE_addsi3 1
#define HAVE_subsi3 1
#define HAVE_sminsi3 (TARGET_POWER || TARGET_ISEL)
#define HAVE_smaxsi3 (TARGET_POWER || TARGET_ISEL)
#define HAVE_uminsi3 (TARGET_POWER || TARGET_ISEL)
#define HAVE_umaxsi3 (TARGET_POWER || TARGET_ISEL)
#define HAVE_abssi2 1
#define HAVE_mulsi3 1
#define HAVE_divmodsi4 (TARGET_POWER || (! TARGET_POWER && ! TARGET_POWERPC))
#define HAVE_udivsi3 (TARGET_POWERPC || (! TARGET_POWER && ! TARGET_POWERPC))
#define HAVE_divsi3 1
#define HAVE_modsi3 1
#define HAVE_udivmodsi4_normal (TARGET_POWER)
#define HAVE_udivmodsi4_tests (TARGET_POWER)
#define HAVE_udivmodsi4 1
#define HAVE_iorsi3 1
#define HAVE_xorsi3 1
#define HAVE_insv 1
#define HAVE_extzv 1
#define HAVE_ashlsi3 1
#define HAVE_lshrsi3 1
#define HAVE_ashrsi3 1
#define HAVE_negsf2 (TARGET_HARD_FLOAT)
#define HAVE_abssf2 (TARGET_HARD_FLOAT)
#define HAVE_addsf3 (TARGET_HARD_FLOAT)
#define HAVE_subsf3 (TARGET_HARD_FLOAT)
#define HAVE_mulsf3 (TARGET_HARD_FLOAT)
#define HAVE_divsf3 (TARGET_HARD_FLOAT)
#define HAVE_sqrtsf2 ((TARGET_PPC_GPOPT || TARGET_POWER2) && TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_maxsf3 (TARGET_PPC_GFXOPT && TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_minsf3 (TARGET_PPC_GFXOPT && TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_movsfcc (TARGET_PPC_GFXOPT && TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_maxdf3 (TARGET_PPC_GFXOPT && TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_mindf3 (TARGET_PPC_GFXOPT && TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_movdfcc (TARGET_PPC_GFXOPT && TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_floatsidf2 (TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_floatunssidf2 (TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_fix_truncdfsi2 ((TARGET_POWER2 || TARGET_POWERPC) && TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_floatdisf2 (TARGET_POWERPC64 && TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_floatdisf2_internal2 (TARGET_POWERPC64 && TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_mulsidi3 (! TARGET_POWERPC64)
#define HAVE_umulsidi3 (TARGET_POWERPC && ! TARGET_POWERPC64)
#define HAVE_smulsi3_highpart 1
#define HAVE_umulsi3_highpart (TARGET_POWERPC)
#define HAVE_adddi3 1
#define HAVE_subdi3 1
#define HAVE_negdi2 1
#define HAVE_divdi3 (TARGET_POWERPC64)
#define HAVE_moddi3 (TARGET_POWERPC64)
#define HAVE_ashldi3 (TARGET_POWERPC64 || TARGET_POWER)
#define HAVE_lshrdi3 (TARGET_POWERPC64 || TARGET_POWER)
#define HAVE_ashrdi3 1
#define HAVE_iordi3 (TARGET_POWERPC64)
#define HAVE_xordi3 (TARGET_POWERPC64)
#define HAVE_movsi_got (DEFAULT_ABI == ABI_V4 && flag_pic == 1)
#define HAVE_movsi 1
#define HAVE_movhi 1
#define HAVE_movqi 1
#define HAVE_movcc 1
#define HAVE_movsf 1
#define HAVE_movdf 1
#define HAVE_movtf (DEFAULT_ABI == ABI_AIX && TARGET_HARD_FLOAT && TARGET_FPRS \
   && TARGET_LONG_DOUBLE_128)
#define HAVE_movdi 1
#define HAVE_movti (TARGET_STRING || TARGET_POWERPC64)
#define HAVE_load_multiple (TARGET_STRING && !TARGET_POWERPC64)
#define HAVE_store_multiple (TARGET_STRING && !TARGET_POWERPC64)
#define HAVE_movstrsi 1
#define HAVE_movstrsi_8reg (TARGET_STRING)
#define HAVE_movstrsi_6reg (TARGET_STRING)
#define HAVE_movstrsi_4reg (TARGET_STRING)
#define HAVE_movstrsi_2reg (TARGET_STRING && ! TARGET_POWERPC64)
#define HAVE_movstrsi_1reg (TARGET_STRING)
#define HAVE_allocate_stack 1
#define HAVE_save_stack_function 1
#define HAVE_restore_stack_function 1
#define HAVE_restore_stack_block 1
#define HAVE_save_stack_nonlocal 1
#define HAVE_restore_stack_nonlocal 1
#define HAVE_builtin_setjmp_receiver ((DEFAULT_ABI == ABI_V4 && flag_pic == 1) \
   || (TARGET_TOC && TARGET_MINIMAL_TOC) \
   || (DEFAULT_ABI == ABI_DARWIN && flag_pic))
#define HAVE_call_indirect_aix32 (TARGET_32BIT)
#define HAVE_call_indirect_aix64 (TARGET_64BIT)
#define HAVE_call_value_indirect_aix32 (TARGET_32BIT)
#define HAVE_call_value_indirect_aix64 (TARGET_64BIT)
#define HAVE_call 1
#define HAVE_call_value 1
#define HAVE_untyped_call 1
#define HAVE_sibcall 1
#define HAVE_sibcall_value 1
#define HAVE_sibcall_epilogue (TARGET_SCHED_PROLOG)
#define HAVE_cmpsi 1
#define HAVE_cmpdi (TARGET_POWERPC64)
#define HAVE_cmpsf (TARGET_HARD_FLOAT)
#define HAVE_cmpdf (TARGET_HARD_FLOAT && TARGET_FPRS)
#define HAVE_cmptf (DEFAULT_ABI == ABI_AIX && TARGET_HARD_FLOAT \
   && TARGET_FPRS && TARGET_LONG_DOUBLE_128)
#define HAVE_beq 1
#define HAVE_bne 1
#define HAVE_bge 1
#define HAVE_bgt 1
#define HAVE_ble 1
#define HAVE_blt 1
#define HAVE_bgeu 1
#define HAVE_bgtu 1
#define HAVE_bleu 1
#define HAVE_bltu 1
#define HAVE_bunordered 1
#define HAVE_bordered 1
#define HAVE_buneq 1
#define HAVE_bunge 1
#define HAVE_bungt 1
#define HAVE_bunle 1
#define HAVE_bunlt 1
#define HAVE_bltgt 1
#define HAVE_seq 1
#define HAVE_sne 1
#define HAVE_sgt 1
#define HAVE_slt 1
#define HAVE_sge 1
#define HAVE_sle 1
#define HAVE_sgtu 1
#define HAVE_sltu 1
#define HAVE_sgeu 1
#define HAVE_sleu 1
#define HAVE_indirect_jump 1
#define HAVE_tablejump 1
#define HAVE_tablejumpsi (TARGET_32BIT)
#define HAVE_tablejumpdi (TARGET_64BIT)
#define HAVE_doloop_end 1
#define HAVE_ctrsi (! TARGET_POWERPC64)
#define HAVE_ctrdi (TARGET_POWERPC64)
#define HAVE_conditional_trap 1
#define HAVE_prologue (TARGET_SCHED_PROLOG)
#define HAVE_epilogue (TARGET_SCHED_PROLOG)
#define HAVE_movsi_to_cr_one 1
#define HAVE_eh_return 1
#define HAVE_movv4si (TARGET_ALTIVEC)
#define HAVE_movv8hi (TARGET_ALTIVEC)
#define HAVE_movv16qi (TARGET_ALTIVEC)
#define HAVE_movv4sf (TARGET_ALTIVEC)
#define HAVE_mulv4sf3 (TARGET_ALTIVEC && TARGET_FUSED_MADD)
#define HAVE_cr6_test_for_zero (TARGET_ALTIVEC)
#define HAVE_cr6_test_for_zero_reverse (TARGET_ALTIVEC)
#define HAVE_cr6_test_for_lt (TARGET_ALTIVEC)
#define HAVE_cr6_test_for_lt_reverse (TARGET_ALTIVEC)
extern rtx        gen_extendqidi2               PARAMS ((rtx, rtx));
extern rtx        gen_extendqisi2_ppc           PARAMS ((rtx, rtx));
extern rtx        gen_extendqihi2_ppc           PARAMS ((rtx, rtx));
static inline rtx gen_addsi3_high               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_addsi3_high(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
extern rtx        gen_one_cmplsi2               PARAMS ((rtx, rtx));
static inline rtx gen_abssi2_isel               PARAMS ((rtx, rtx));
static inline rtx
gen_abssi2_isel(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
extern rtx        gen_abssi2_nopower            PARAMS ((rtx, rtx));
extern rtx        gen_negsi2                    PARAMS ((rtx, rtx));
extern rtx        gen_ffssi2                    PARAMS ((rtx, rtx));
extern rtx        gen_mulsi3_mq                 PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mulsi3_no_mq              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_udivsi3_mq                PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divsi3_mq                 PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mulh_call                 PARAMS ((void));
extern rtx        gen_mull_call                 PARAMS ((void));
extern rtx        gen_divss_call                PARAMS ((void));
extern rtx        gen_divus_call                PARAMS ((void));
extern rtx        gen_quoss_call                PARAMS ((void));
extern rtx        gen_quous_call                PARAMS ((void));
extern rtx        gen_andsi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_insvsi                    PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_insvdi                    PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_extzvsi                   PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_extzvdi                   PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_rotlsi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashlsi3_power             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashlsi3_no_power          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_lshrsi3_power             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_lshrsi3_no_power          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashrsi3_power             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashrsi3_no_power          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_extendsfdf2               PARAMS ((rtx, rtx));
extern rtx        gen_truncdfsf2                PARAMS ((rtx, rtx));
extern rtx        gen_aux_truncdfsf2            PARAMS ((rtx, rtx));
static inline rtx gen_isel_signed               PARAMS ((rtx, rtx, rtx, rtx, rtx));
static inline rtx
gen_isel_signed(a, b, c, d, e)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
     rtx d ATTRIBUTE_UNUSED;
     rtx e ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_isel_unsigned             PARAMS ((rtx, rtx, rtx, rtx, rtx));
static inline rtx
gen_isel_unsigned(a, b, c, d, e)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
     rtx d ATTRIBUTE_UNUSED;
     rtx e ATTRIBUTE_UNUSED;
{
  return 0;
}
extern rtx        gen_negdf2                    PARAMS ((rtx, rtx));
extern rtx        gen_absdf2                    PARAMS ((rtx, rtx));
extern rtx        gen_adddf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subdf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_muldf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divdf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sqrtdf2                   PARAMS ((rtx, rtx));
extern rtx        gen_fctiwz                    PARAMS ((rtx, rtx));
extern rtx        gen_floatdidf2                PARAMS ((rtx, rtx));
extern rtx        gen_floatsidf_ppc64           PARAMS ((rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_floatunssidf_ppc64        PARAMS ((rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_fix_truncdfdi2            PARAMS ((rtx, rtx));
extern rtx        gen_floatdisf2_internal1      PARAMS ((rtx, rtx));
extern rtx        gen_mulsidi3_mq               PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umulsidi3_mq              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_smulsi3_highpart_mq       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umulsi3_highpart_mq       PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashldi3_power             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_lshrdi3_power             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashrdi3_power             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashrdi3_no_power          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_one_cmpldi2               PARAMS ((rtx, rtx));
extern rtx        gen_absdi2                    PARAMS ((rtx, rtx));
extern rtx        gen_ffsdi2                    PARAMS ((rtx, rtx));
extern rtx        gen_muldi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_smuldi3_highpart          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umuldi3_highpart          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_udivdi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_rotldi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashldi3_internal5         PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_ashldi3_internal8         PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_anddi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_elf_high                  PARAMS ((rtx, rtx));
extern rtx        gen_elf_low                   PARAMS ((rtx, rtx, rtx));
static inline rtx gen_macho_high                PARAMS ((rtx, rtx));
static inline rtx
gen_macho_high(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_macho_low                 PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_macho_low(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_movsi_low                 PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_movsi_low(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_movsi_low_st              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_movsi_low_st(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_movdf_low                 PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_movdf_low(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_movdf_low_st              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_movdf_low_st(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_movsf_low                 PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_movsf_low(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_movsf_low_st              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_movsf_low_st(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
extern rtx        gen_extenddftf2               PARAMS ((rtx, rtx));
extern rtx        gen_extendsftf2               PARAMS ((rtx, rtx));
extern rtx        gen_trunctfdf2                PARAMS ((rtx, rtx));
extern rtx        gen_trunctfsf2                PARAMS ((rtx, rtx));
extern rtx        gen_floatditf2                PARAMS ((rtx, rtx));
extern rtx        gen_floatsitf2                PARAMS ((rtx, rtx));
extern rtx        gen_fix_trunctfdi2            PARAMS ((rtx, rtx));
extern rtx        gen_fix_trunctfsi2            PARAMS ((rtx, rtx));
extern rtx        gen_negtf2                    PARAMS ((rtx, rtx));
extern rtx        gen_abstf2                    PARAMS ((rtx, rtx));
extern rtx        gen_movdi_update              PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_movsi_update              PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_load_toc_aix_si           PARAMS ((rtx));
extern rtx        gen_load_toc_aix_di           PARAMS ((rtx));
extern rtx        gen_load_toc_v4_pic_si        PARAMS ((rtx));
extern rtx        gen_load_toc_v4_PIC_1         PARAMS ((rtx, rtx));
extern rtx        gen_load_toc_v4_PIC_1b        PARAMS ((rtx, rtx, rtx));
extern rtx        gen_load_toc_v4_PIC_2         PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_load_macho_picbase        PARAMS ((rtx, rtx));
extern rtx        gen_macho_correct_pic         PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_blockage                  PARAMS ((void));
static inline rtx gen_move_from_CR_ov_bit       PARAMS ((rtx, rtx));
static inline rtx
gen_move_from_CR_ov_bit(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
extern rtx        gen_jump                      PARAMS ((rtx));
extern rtx        gen_return                    PARAMS ((void));
extern rtx        gen_indirect_jumpsi           PARAMS ((rtx));
extern rtx        gen_indirect_jumpdi           PARAMS ((rtx));
extern rtx        gen_nop                       PARAMS ((void));
extern rtx        gen_trap                      PARAMS ((void));
extern rtx        gen_movesi_from_cr            PARAMS ((rtx));
extern rtx        gen_stack_tie                 PARAMS ((rtx));
extern rtx        gen_eh_set_lr_si              PARAMS ((rtx));
extern rtx        gen_eh_set_lr_di              PARAMS ((rtx));
extern rtx        gen_prefetch                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_lvx_4si           PARAMS ((rtx, rtx));
extern rtx        gen_altivec_lvx_8hi           PARAMS ((rtx, rtx));
extern rtx        gen_altivec_lvx_16qi          PARAMS ((rtx, rtx));
extern rtx        gen_altivec_lvx_4sf           PARAMS ((rtx, rtx));
extern rtx        gen_altivec_stvx_4si          PARAMS ((rtx, rtx));
extern rtx        gen_altivec_stvx_8hi          PARAMS ((rtx, rtx));
extern rtx        gen_altivec_stvx_16qi         PARAMS ((rtx, rtx));
extern rtx        gen_altivec_stvx_4sf          PARAMS ((rtx, rtx));
extern rtx        gen_get_vrsave_internal       PARAMS ((rtx));
extern rtx        gen_addv16qi3                 PARAMS ((rtx, rtx, rtx));
extern rtx        gen_addv8hi3                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_addv4si3                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_addv4sf3                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vaddcuw           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vaddubs           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vaddsbs           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vadduhs           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vaddshs           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vadduws           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vaddsws           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_andv4si3                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vandc             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vavgub            PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vavgsb            PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vavguh            PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vavgsh            PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vavguw            PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vavgsw            PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vcmpbfp           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vcmpequb          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vcmpequh          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vcmpequw          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vcmpeqfp          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vcmpgefp          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vcmpgtub          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vcmpgtsb          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vcmpgtuh          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vcmpgtsh          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vcmpgtuw          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vcmpgtsw          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vcmpgtfp          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vmaddfp           PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_altivec_vnmsubfp          PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_altivec_vmsumubm          PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_altivec_vmsummbm          PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_altivec_vmsumuhm          PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_altivec_vmsumshm          PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_altivec_vmsumuhs          PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_altivec_vmsumshs          PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_umaxv16qi3                PARAMS ((rtx, rtx, rtx));
extern rtx        gen_smaxv16qi3                PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umaxv8hi3                 PARAMS ((rtx, rtx, rtx));
extern rtx        gen_smaxv8hi3                 PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umaxv4si3                 PARAMS ((rtx, rtx, rtx));
extern rtx        gen_smaxv4si3                 PARAMS ((rtx, rtx, rtx));
extern rtx        gen_smaxv4sf3                 PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vmhaddshs         PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_altivec_vmhraddshs        PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_altivec_vmladduhm         PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_altivec_vmrghb            PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vmrghh            PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vmrghw            PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vmrglb            PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vmrglh            PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vmrglw            PARAMS ((rtx, rtx, rtx));
extern rtx        gen_uminv16qi3                PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sminv16qi3                PARAMS ((rtx, rtx, rtx));
extern rtx        gen_uminv8hi3                 PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sminv8hi3                 PARAMS ((rtx, rtx, rtx));
extern rtx        gen_uminv4si3                 PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sminv4si3                 PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sminv4sf3                 PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vmuleub           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vmulesb           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vmuleuh           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vmulesh           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vmuloub           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vmulosb           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vmulouh           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vmulosh           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vnor              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_iorv4si3                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vpkuhum           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vpkuwum           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vpkpx             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vpkuhss           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vpkshss           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vpkuwss           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vpkswss           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vpkuhus           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vpkshus           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vpkuwus           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vpkswus           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vrlb              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vrlh              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vrlw              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vslb              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vslh              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vslw              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vslw_v4sf         PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vsl               PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vslo              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vsrb              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vsrh              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vsrw              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vsrab             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vsrah             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vsraw             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vsr               PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vsro              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subv16qi3                 PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subv8hi3                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subv4si3                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subv4sf3                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vsubcuw           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vsububs           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vsubsbs           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vsubuhs           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vsubshs           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vsubuws           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vsubsws           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vsum4ubs          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vsum4sbs          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vsum4shs          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vsum2sws          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vsumsws           PARAMS ((rtx, rtx, rtx));
extern rtx        gen_xorv4si3                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vspltb            PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vsplth            PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vspltw            PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vspltisb          PARAMS ((rtx, rtx));
extern rtx        gen_altivec_vspltish          PARAMS ((rtx, rtx));
extern rtx        gen_altivec_vspltisw          PARAMS ((rtx, rtx));
extern rtx        gen_altivec_vspltisw_v4sf     PARAMS ((rtx, rtx));
extern rtx        gen_ftruncv4sf2               PARAMS ((rtx, rtx));
extern rtx        gen_altivec_vperm_4si         PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_altivec_vperm_4sf         PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_altivec_vperm_8hi         PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_altivec_vperm_16qi        PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_altivec_vrfip             PARAMS ((rtx, rtx));
extern rtx        gen_altivec_vrfin             PARAMS ((rtx, rtx));
extern rtx        gen_altivec_vrfim             PARAMS ((rtx, rtx));
extern rtx        gen_altivec_vcfux             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vcfsx             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vctuxs            PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vctsxs            PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_vlogefp           PARAMS ((rtx, rtx));
extern rtx        gen_altivec_vexptefp          PARAMS ((rtx, rtx));
extern rtx        gen_altivec_vrsqrtefp         PARAMS ((rtx, rtx));
extern rtx        gen_altivec_vrefp             PARAMS ((rtx, rtx));
extern rtx        gen_altivec_vsel_4si          PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_altivec_vsel_4sf          PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_altivec_vsel_8hi          PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_altivec_vsel_16qi         PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_altivec_vsldoi_4si        PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_altivec_vsldoi_4sf        PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_altivec_vsldoi_8hi        PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_altivec_vsldoi_16qi       PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_altivec_vupkhsb           PARAMS ((rtx, rtx));
extern rtx        gen_altivec_vupkhpx           PARAMS ((rtx, rtx));
extern rtx        gen_altivec_vupkhsh           PARAMS ((rtx, rtx));
extern rtx        gen_altivec_vupklsb           PARAMS ((rtx, rtx));
extern rtx        gen_altivec_vupklpx           PARAMS ((rtx, rtx));
extern rtx        gen_altivec_vupklsh           PARAMS ((rtx, rtx));
extern rtx        gen_altivec_predicate_v4si    PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_altivec_predicate_v4sf    PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_altivec_predicate_v8hi    PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_altivec_predicate_v16qi   PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_altivec_mtvscr            PARAMS ((rtx));
extern rtx        gen_altivec_mfvscr            PARAMS ((rtx));
extern rtx        gen_altivec_dssall            PARAMS ((void));
extern rtx        gen_altivec_dss               PARAMS ((rtx));
extern rtx        gen_altivec_dst               PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_dstt              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_dstst             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_dststt            PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_lvsl              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_lvsr              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_lvebx             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_lvehx             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_lvewx             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_lvxl              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_lvx               PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_stvx              PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_stvxl             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_stvebx            PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_stvehx            PARAMS ((rtx, rtx, rtx));
extern rtx        gen_altivec_stvewx            PARAMS ((rtx, rtx, rtx));
extern rtx        gen_absv16qi2                 PARAMS ((rtx, rtx));
extern rtx        gen_absv8hi2                  PARAMS ((rtx, rtx));
extern rtx        gen_absv4si2                  PARAMS ((rtx, rtx));
extern rtx        gen_absv4sf2                  PARAMS ((rtx, rtx));
extern rtx        gen_altivec_abss_v16qi        PARAMS ((rtx, rtx));
extern rtx        gen_altivec_abss_v8hi         PARAMS ((rtx, rtx));
extern rtx        gen_altivec_abss_v4si         PARAMS ((rtx, rtx));
static inline rtx gen_spe_efsctuiz              PARAMS ((rtx, rtx));
static inline rtx
gen_spe_efsctuiz(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_fixunssfsi2           PARAMS ((rtx, rtx));
static inline rtx
gen_spe_fixunssfsi2(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_fix_truncsfsi2        PARAMS ((rtx, rtx));
static inline rtx
gen_spe_fix_truncsfsi2(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_floatunssisf2         PARAMS ((rtx, rtx));
static inline rtx
gen_spe_floatunssisf2(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_floatsisf2            PARAMS ((rtx, rtx));
static inline rtx
gen_spe_floatsisf2(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evabs                 PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evabs(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evandc                PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evandc(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evand                 PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evand(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evcmpeq               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evcmpeq(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evcmpgts              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evcmpgts(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evcmpgtu              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evcmpgtu(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evcmplts              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evcmplts(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evcmpltu              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evcmpltu(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evfscmpeq             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evfscmpeq(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evfscmpgt             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evfscmpgt(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evfscmplt             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evfscmplt(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evfststeq             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evfststeq(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evfststgt             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evfststgt(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evfststlt             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evfststlt(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evcntlsw              PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evcntlsw(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evcntlzw              PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evcntlzw(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_eveqv                 PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_eveqv(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evextsb               PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evextsb(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evextsh               PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evextsh(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evlhhesplat           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evlhhesplat(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evlhhesplatx          PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evlhhesplatx(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evlhhossplat          PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evlhhossplat(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evlhhossplatx         PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evlhhossplatx(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evlhhousplat          PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evlhhousplat(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evlhhousplatx         PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evlhhousplatx(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evlwhsplat            PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evlwhsplat(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evlwhsplatx           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evlwhsplatx(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evlwwsplat            PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evlwwsplat(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evlwwsplatx           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evlwwsplatx(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmergehi             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmergehi(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmergehilo           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmergehilo(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmergelo             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmergelo(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmergelohi           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmergelohi(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evnand                PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evnand(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evneg                 PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evneg(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evnor                 PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evnor(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evorc                 PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evorc(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evor                  PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evor(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evrlwi                PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evrlwi(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evrlw                 PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evrlw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evrndw                PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evrndw(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evsel                 PARAMS ((rtx, rtx, rtx, rtx));
static inline rtx
gen_spe_evsel(a, b, c, d)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
     rtx d ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evsel_fs              PARAMS ((rtx, rtx, rtx, rtx));
static inline rtx
gen_spe_evsel_fs(a, b, c, d)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
     rtx d ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evslwi                PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evslwi(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evslw                 PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evslw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evsrwis               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evsrwis(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evsrwiu               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evsrwiu(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evsrws                PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evsrws(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evsrwu                PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evsrwu(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evxor                 PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evxor(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evfsabs               PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evfsabs(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evfsadd               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evfsadd(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evfscfsf              PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evfscfsf(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evfscfsi              PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evfscfsi(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evfscfuf              PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evfscfuf(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evfscfui              PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evfscfui(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evfsctsf              PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evfsctsf(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evfsctsi              PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evfsctsi(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evfsctsiz             PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evfsctsiz(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evfsctuf              PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evfsctuf(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evfsctui              PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evfsctui(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evfsctuiz             PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evfsctuiz(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evfsdiv               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evfsdiv(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evfsmul               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evfsmul(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evfsnabs              PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evfsnabs(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evfsneg               PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evfsneg(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evfssub               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evfssub(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evldd                 PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evldd(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evlddx                PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evlddx(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evldh                 PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evldh(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evldhx                PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evldhx(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evldw                 PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evldw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evldwx                PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evldwx(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evlwhe                PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evlwhe(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evlwhex               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evlwhex(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evlwhos               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evlwhos(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evlwhosx              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evlwhosx(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evlwhou               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evlwhou(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evlwhoux              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evlwhoux(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_brinc                 PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_brinc(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhegsmfaa           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhegsmfaa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhegsmfan           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhegsmfan(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhegsmiaa           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhegsmiaa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhegsmian           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhegsmian(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhegumiaa           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhegumiaa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhegumian           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhegumian(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhesmfaaw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhesmfaaw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhesmfanw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhesmfanw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhesmfa             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhesmfa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhesmf              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhesmf(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhesmiaaw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhesmiaaw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhesmianw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhesmianw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhesmia             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhesmia(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhesmi              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhesmi(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhessfaaw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhessfaaw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhessfanw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhessfanw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhessfa             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhessfa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhessf              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhessf(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhessiaaw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhessiaaw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhessianw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhessianw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmheumiaaw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmheumiaaw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmheumianw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmheumianw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmheumia             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmheumia(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmheumi              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmheumi(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmheusiaaw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmheusiaaw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmheusianw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmheusianw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhogsmfaa           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhogsmfaa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhogsmfan           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhogsmfan(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhogsmiaa           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhogsmiaa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhogsmian           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhogsmian(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhogumiaa           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhogumiaa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhogumian           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhogumian(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhosmfaaw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhosmfaaw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhosmfanw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhosmfanw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhosmfa             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhosmfa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhosmf              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhosmf(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhosmiaaw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhosmiaaw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhosmianw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhosmianw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhosmia             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhosmia(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhosmi              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhosmi(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhossfaaw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhossfaaw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhossfanw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhossfanw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhossfa             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhossfa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhossf              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhossf(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhossiaaw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhossiaaw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhossianw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhossianw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhoumiaaw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhoumiaaw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhoumianw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhoumianw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhoumia             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhoumia(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhoumi              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhoumi(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhousiaaw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhousiaaw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmhousianw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmhousianw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmmlssfa             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmmlssfa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmmlssf              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmmlssf(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwhsmfa             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwhsmfa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwhsmf              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwhsmf(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwhsmia             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwhsmia(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwhsmi              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwhsmi(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwhssfa             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwhssfa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwhusian            PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwhusian(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwhssf              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwhssf(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwhumia             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwhumia(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwhumi              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwhumi(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwlsmiaaw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwlsmiaaw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwlsmianw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwlsmianw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwlssiaaw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwlssiaaw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwlssianw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwlssianw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwlumiaaw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwlumiaaw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwlumianw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwlumianw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwlumia             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwlumia(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwlumi              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwlumi(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwlusiaaw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwlusiaaw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwlusianw           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwlusianw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwsmfaa             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwsmfaa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwsmfan             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwsmfan(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwsmfa              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwsmfa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwsmf               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwsmf(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwsmiaa             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwsmiaa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwsmian             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwsmian(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwsmia              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwsmia(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwsmi               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwsmi(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwssfaa             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwssfaa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwssfan             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwssfan(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwssfa              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwssfa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwssf               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwssf(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwumiaa             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwumiaa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwumian             PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwumian(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwumia              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwumia(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwumi               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwumi(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evaddw                PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evaddw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evaddusiaaw           PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evaddusiaaw(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evaddumiaaw           PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evaddumiaaw(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evaddssiaaw           PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evaddssiaaw(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evaddsmiaaw           PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evaddsmiaaw(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evaddiw               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evaddiw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evsubifw              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evsubifw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evsubfw               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evsubfw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evsubfusiaaw          PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evsubfusiaaw(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evsubfumiaaw          PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evsubfumiaaw(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evsubfssiaaw          PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evsubfssiaaw(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evsubfsmiaaw          PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evsubfsmiaaw(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmra                 PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evmra(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evdivws               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evdivws(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evdivwu               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evdivwu(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evsplatfi             PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evsplatfi(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evsplati              PARAMS ((rtx, rtx));
static inline rtx
gen_spe_evsplati(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evstdd                PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evstdd(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evstddx               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evstddx(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evstdh                PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evstdh(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evstdhx               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evstdhx(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evstdw                PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evstdw(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evstdwx               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evstdwx(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evstwhe               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evstwhe(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evstwhex              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evstwhex(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evstwho               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evstwho(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evstwhox              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evstwhox(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evstwwe               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evstwwe(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evstwwex              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evstwwex(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evstwwo               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evstwwo(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evstwwox              PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evstwwox(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwhssfaa            PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwhssfaa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwhssmaa            PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwhssmaa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwhsmfaa            PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwhsmfaa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwhsmiaa            PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwhsmiaa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwhusiaa            PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwhusiaa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwhumiaa            PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwhumiaa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwhssfan            PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwhssfan(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwhssian            PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwhssian(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwhsmfan            PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwhsmfan(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwhsmian            PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwhsmian(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwhumian            PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwhumian(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwhgssfaa           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwhgssfaa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwhgsmfaa           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwhgsmfaa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwhgsmiaa           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwhgsmiaa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwhgumiaa           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwhgumiaa(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwhgssfan           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwhgssfan(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwhgsmfan           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwhgsmfan(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwhgsmian           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwhgsmian(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_evmwhgumian           PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_spe_evmwhgumian(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_mtspefscr             PARAMS ((rtx));
static inline rtx
gen_spe_mtspefscr(a)
     rtx a ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_spe_mfspefscr             PARAMS ((rtx));
static inline rtx
gen_spe_mfspefscr(a)
     rtx a ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_cmpsfeq_gpr               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_cmpsfeq_gpr(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_tstsfeq_gpr               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_tstsfeq_gpr(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_cmpsfgt_gpr               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_cmpsfgt_gpr(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_tstsfgt_gpr               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_tstsfgt_gpr(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_cmpsflt_gpr               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_cmpsflt_gpr(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_tstsflt_gpr               PARAMS ((rtx, rtx, rtx));
static inline rtx
gen_tstsflt_gpr(a, b, c)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
{
  return 0;
}
extern rtx        gen_zero_extendqidi2          PARAMS ((rtx, rtx));
extern rtx        gen_zero_extendhidi2          PARAMS ((rtx, rtx));
extern rtx        gen_extendhidi2               PARAMS ((rtx, rtx));
extern rtx        gen_zero_extendsidi2          PARAMS ((rtx, rtx));
extern rtx        gen_extendsidi2               PARAMS ((rtx, rtx));
extern rtx        gen_zero_extendqisi2          PARAMS ((rtx, rtx));
extern rtx        gen_extendqisi2               PARAMS ((rtx, rtx));
extern rtx        gen_extendqisi2_power         PARAMS ((rtx, rtx));
extern rtx        gen_extendqisi2_no_power      PARAMS ((rtx, rtx));
extern rtx        gen_zero_extendqihi2          PARAMS ((rtx, rtx));
extern rtx        gen_extendqihi2               PARAMS ((rtx, rtx));
extern rtx        gen_extendqihi2_power         PARAMS ((rtx, rtx));
extern rtx        gen_extendqihi2_no_power      PARAMS ((rtx, rtx));
extern rtx        gen_zero_extendhisi2          PARAMS ((rtx, rtx));
extern rtx        gen_extendhisi2               PARAMS ((rtx, rtx));
extern rtx        gen_addsi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subsi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sminsi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_smaxsi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_uminsi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umaxsi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_abssi2                    PARAMS ((rtx, rtx));
extern rtx        gen_mulsi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divmodsi4                 PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_udivsi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divsi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_modsi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_udivmodsi4_normal         PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_udivmodsi4_tests          PARAMS ((rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_udivmodsi4                PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_iorsi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_xorsi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_insv                      PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_extzv                     PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_ashlsi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_lshrsi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashrsi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_negsf2                    PARAMS ((rtx, rtx));
extern rtx        gen_abssf2                    PARAMS ((rtx, rtx));
extern rtx        gen_addsf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subsf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mulsf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_divsf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_sqrtsf2                   PARAMS ((rtx, rtx));
extern rtx        gen_maxsf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_minsf3                    PARAMS ((rtx, rtx, rtx));
static inline rtx gen_movsicc                   PARAMS ((rtx, rtx, rtx, rtx));
static inline rtx
gen_movsicc(a, b, c, d)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
     rtx c ATTRIBUTE_UNUSED;
     rtx d ATTRIBUTE_UNUSED;
{
  return 0;
}
extern rtx        gen_movsfcc                   PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_maxdf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_mindf3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_movdfcc                   PARAMS ((rtx, rtx, rtx, rtx));
static inline rtx gen_fixunssfsi2               PARAMS ((rtx, rtx));
static inline rtx
gen_fixunssfsi2(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_fix_truncsfsi2            PARAMS ((rtx, rtx));
static inline rtx
gen_fix_truncsfsi2(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
extern rtx        gen_floatsidf2                PARAMS ((rtx, rtx));
static inline rtx gen_floatunssisf2             PARAMS ((rtx, rtx));
static inline rtx
gen_floatunssisf2(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
extern rtx        gen_floatunssidf2             PARAMS ((rtx, rtx));
extern rtx        gen_fix_truncdfsi2            PARAMS ((rtx, rtx));
static inline rtx gen_floatsisf2                PARAMS ((rtx, rtx));
static inline rtx
gen_floatsisf2(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
extern rtx        gen_floatdisf2                PARAMS ((rtx, rtx));
extern rtx        gen_floatdisf2_internal2      PARAMS ((rtx, rtx));
extern rtx        gen_mulsidi3                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umulsidi3                 PARAMS ((rtx, rtx, rtx));
extern rtx        gen_smulsi3_highpart          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_umulsi3_highpart          PARAMS ((rtx, rtx, rtx));
extern rtx        gen_adddi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_subdi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_negdi2                    PARAMS ((rtx, rtx));
extern rtx        gen_divdi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_moddi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashldi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_lshrdi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_ashrdi3                   PARAMS ((rtx, rtx, rtx));
extern rtx        gen_iordi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_xordi3                    PARAMS ((rtx, rtx, rtx));
extern rtx        gen_movsi_got                 PARAMS ((rtx, rtx));
extern rtx        gen_movsi                     PARAMS ((rtx, rtx));
extern rtx        gen_movhi                     PARAMS ((rtx, rtx));
extern rtx        gen_movqi                     PARAMS ((rtx, rtx));
extern rtx        gen_movcc                     PARAMS ((rtx, rtx));
extern rtx        gen_movsf                     PARAMS ((rtx, rtx));
extern rtx        gen_movdf                     PARAMS ((rtx, rtx));
extern rtx        gen_movtf                     PARAMS ((rtx, rtx));
extern rtx        gen_movdi                     PARAMS ((rtx, rtx));
extern rtx        gen_movti                     PARAMS ((rtx, rtx));
extern rtx        gen_load_multiple             PARAMS ((rtx, rtx, rtx));
extern rtx        gen_store_multiple            PARAMS ((rtx, rtx, rtx));
extern rtx        gen_movstrsi                  PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_movstrsi_8reg             PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_movstrsi_6reg             PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_movstrsi_4reg             PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_movstrsi_2reg             PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_movstrsi_1reg             PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_allocate_stack            PARAMS ((rtx, rtx));
extern rtx        gen_save_stack_function       PARAMS ((rtx, rtx));
extern rtx        gen_restore_stack_function    PARAMS ((rtx, rtx));
extern rtx        gen_restore_stack_block       PARAMS ((rtx, rtx));
extern rtx        gen_save_stack_nonlocal       PARAMS ((rtx, rtx));
extern rtx        gen_restore_stack_nonlocal    PARAMS ((rtx, rtx));
extern rtx        gen_builtin_setjmp_receiver   PARAMS ((rtx));
extern rtx        gen_call_indirect_aix32       PARAMS ((rtx, rtx));
extern rtx        gen_call_indirect_aix64       PARAMS ((rtx, rtx));
extern rtx        gen_call_value_indirect_aix32 PARAMS ((rtx, rtx, rtx));
extern rtx        gen_call_value_indirect_aix64 PARAMS ((rtx, rtx, rtx));
#define GEN_CALL(A, B, C, D) gen_call ((A), (B), (C))
extern rtx        gen_call                      PARAMS ((rtx, rtx, rtx));
#define GEN_CALL_VALUE(A, B, C, D, E) gen_call_value ((A), (B), (C), (D))
extern rtx        gen_call_value                PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_untyped_call              PARAMS ((rtx, rtx, rtx));
#define GEN_SIBCALL(A, B, C, D) gen_sibcall ((A), (B), (C))
extern rtx        gen_sibcall                   PARAMS ((rtx, rtx, rtx));
#define GEN_SIBCALL_VALUE(A, B, C, D, E) gen_sibcall_value ((A), (B), (C), (D))
extern rtx        gen_sibcall_value             PARAMS ((rtx, rtx, rtx, rtx));
extern rtx        gen_sibcall_epilogue          PARAMS ((void));
extern rtx        gen_cmpsi                     PARAMS ((rtx, rtx));
extern rtx        gen_cmpdi                     PARAMS ((rtx, rtx));
extern rtx        gen_cmpsf                     PARAMS ((rtx, rtx));
extern rtx        gen_cmpdf                     PARAMS ((rtx, rtx));
extern rtx        gen_cmptf                     PARAMS ((rtx, rtx));
extern rtx        gen_beq                       PARAMS ((rtx));
extern rtx        gen_bne                       PARAMS ((rtx));
extern rtx        gen_bge                       PARAMS ((rtx));
extern rtx        gen_bgt                       PARAMS ((rtx));
extern rtx        gen_ble                       PARAMS ((rtx));
extern rtx        gen_blt                       PARAMS ((rtx));
extern rtx        gen_bgeu                      PARAMS ((rtx));
extern rtx        gen_bgtu                      PARAMS ((rtx));
extern rtx        gen_bleu                      PARAMS ((rtx));
extern rtx        gen_bltu                      PARAMS ((rtx));
extern rtx        gen_bunordered                PARAMS ((rtx));
extern rtx        gen_bordered                  PARAMS ((rtx));
extern rtx        gen_buneq                     PARAMS ((rtx));
extern rtx        gen_bunge                     PARAMS ((rtx));
extern rtx        gen_bungt                     PARAMS ((rtx));
extern rtx        gen_bunle                     PARAMS ((rtx));
extern rtx        gen_bunlt                     PARAMS ((rtx));
extern rtx        gen_bltgt                     PARAMS ((rtx));
extern rtx        gen_seq                       PARAMS ((rtx));
extern rtx        gen_sne                       PARAMS ((rtx));
extern rtx        gen_sgt                       PARAMS ((rtx));
extern rtx        gen_slt                       PARAMS ((rtx));
extern rtx        gen_sge                       PARAMS ((rtx));
extern rtx        gen_sle                       PARAMS ((rtx));
extern rtx        gen_sgtu                      PARAMS ((rtx));
extern rtx        gen_sltu                      PARAMS ((rtx));
extern rtx        gen_sgeu                      PARAMS ((rtx));
extern rtx        gen_sleu                      PARAMS ((rtx));
extern rtx        gen_indirect_jump             PARAMS ((rtx));
extern rtx        gen_tablejump                 PARAMS ((rtx, rtx));
extern rtx        gen_tablejumpsi               PARAMS ((rtx, rtx));
extern rtx        gen_tablejumpdi               PARAMS ((rtx, rtx));
extern rtx        gen_doloop_end                PARAMS ((rtx, rtx, rtx, rtx, rtx));
extern rtx        gen_ctrsi                     PARAMS ((rtx, rtx));
extern rtx        gen_ctrdi                     PARAMS ((rtx, rtx));
extern rtx        gen_conditional_trap          PARAMS ((rtx, rtx));
extern rtx        gen_prologue                  PARAMS ((void));
extern rtx        gen_epilogue                  PARAMS ((void));
extern rtx        gen_movsi_to_cr_one           PARAMS ((rtx, rtx));
extern rtx        gen_eh_return                 PARAMS ((rtx));
extern rtx        gen_movv4si                   PARAMS ((rtx, rtx));
extern rtx        gen_movv8hi                   PARAMS ((rtx, rtx));
extern rtx        gen_movv16qi                  PARAMS ((rtx, rtx));
extern rtx        gen_movv4sf                   PARAMS ((rtx, rtx));
extern rtx        gen_mulv4sf3                  PARAMS ((rtx, rtx, rtx));
extern rtx        gen_cr6_test_for_zero         PARAMS ((rtx));
extern rtx        gen_cr6_test_for_zero_reverse PARAMS ((rtx));
extern rtx        gen_cr6_test_for_lt           PARAMS ((rtx));
extern rtx        gen_cr6_test_for_lt_reverse   PARAMS ((rtx));
static inline rtx gen_movv2si                   PARAMS ((rtx, rtx));
static inline rtx
gen_movv2si(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_movv1di                   PARAMS ((rtx, rtx));
static inline rtx
gen_movv1di(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_movv4hi                   PARAMS ((rtx, rtx));
static inline rtx
gen_movv4hi(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}
static inline rtx gen_movv2sf                   PARAMS ((rtx, rtx));
static inline rtx
gen_movv2sf(a, b)
     rtx a ATTRIBUTE_UNUSED;
     rtx b ATTRIBUTE_UNUSED;
{
  return 0;
}

#endif /* GCC_INSN_FLAGS_H */
