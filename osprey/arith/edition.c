
/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/

static char ar_eval_lib_edition[] = "@(#)arith.a version 2." EDITION;

char AR_version [] = EDITION;		/* arith.a edition number */
char AR_libmv2[32] = "x.x.x.x (00000)";	/* libm[v2].a version and edition #s */
char AR_arith_data[2] = "\0";		/* arith.a data file edition number */

const char *arith_vers_name(void)   { return "arith";}

const char *arith_vers_ID(void)     { return "a";}

const char *arith_vers_number(void) { return EDITION;}


/* Change log for each edition:
Edition 241:
	1.	MIPS NaNs are the opposite of SPARC/T90-IEEE/Alpha NaNs:
		the latter has the high bit of the mantissa clear for
		signalling NaNs, and set for quiet ones, while the former
		is the other way around.

Edition 240:
	1.	Add status setting for AR_STAT_INEXACT in IEEE normalization.
	2.	Makefile cleanup -- don't execute "target" if not on a Cray
		machine, and in the "arith.a:" target, copy the arith.h file
		into a "common" subdirectory.  I had moved the common files
		into a "common" subdir in the USS directory in edition 234,
		and modified the get.obj.arith script accordingly, but I
		needed to do the same thing in the build directory structure,
		so that doing a "get.obj.arith -v xn ..." would work.
	3.	Remove the use of /cray/uss/compiler/cost/bin/validate from
		the get.obj.arith script.

Edition 239:
	1.	Add AR_ibits().

Edition 238:
	1.	Missed some modules that needed additions for 8-bit and
		16-bit integers.
	2.	Add full support for determining the host architecture.
	3.	Fix some minor divots in float-->string conversions.
	4.	Add AR_ishft() and AR_ishftc(), for the corresponding f90
		intrinsics, and add code to testint.c to test these.
	5.	Remove the use of <ctype.h> in strcvt.c.  This allows us
		to compile arith with 9.x #include files, and link it
		with 8.x libraries, and have everything work right.  It
		doesn't hurt wide-character compatibility, because there
		is quite a bit of code in strcvt.c that is incompatible
		with wide characters already.

Edition 237:
	1.	Add support for 8-bit and 16-bit integers.

Edition 236:
	1.	Bring in the MIPS quad changes from Rich Shapiro.
	2.	Fix 703309 -- non-base-10 int-to-str conversions yielded
		trash.
	3.	Add appropriate casts to cray1_sim.c and mpp_sim.c, to
		remove some warnings about mistyped assignments.

Edition 235:
	1.	Remove the MIPS hack in native.c for the INDEX(), SCAN(), and
		VERIFY() intrinsics.
	2.	Make negating IEEE float/complex 0.0 work right (it was yielding
		-0.0).
	3.	Undo #2.  The IEEE standard says that -x is x with its sign bit
		toggled.  That is, it is not necessarily the same as (0-x).

Edition 234:
	1.	Expand the simulated stack size for Cray-1 style simulation.
		As of the 3.0 arith data file, 2 kwords is not enough.  It's
		now 10 kwords.
	2.	Clean up the code in various spots so that the Solaris and IRIX
		compilers don't emit any warnings any more.
	3.	Really support the IRIX platform.
	4.	Allow re-updating of the current USS edition.
	5.	Move the USS arith.{doc,h,msg}.* files into a "common" subdirectory
		(aesthetics -- makes the directory easier to look at).
	6.	Make sure all the files installed on USS have group-write access.
	7.	Clean up get.obj.arith -- the indentation had gotten a little
		odd over the years.
	8.	Use click and/or clack for IRIX builds, instead of maestro, since
		those two cross-mount USS and the other CCN PVPs.
	9.	Retag the PL every time we do a revision.
   10.	Add a -n option to remote-builds.csh, to tell it not to do a
		"make delete" before proceeding with the builds.

Edition 233:
	1.	Make _numargs() work properly on T3E.  The SSIB/DSIB
		simulation was broken.

Edition 232:
	1.	Fix some problems with the preprocessing for MIPS.

Edition 231:
	1.	Simulate byte-manipulation instructions properly for a T3E.
	2.	Remove pdbgint support for MPP.
	3.	Add environment variable control of ar_disasm debugging for
		MPP.

Edition 230:
	1.	Allow for building on a TS (only building _on_ one, not
		_for_ one).

Edition 229:
	1.	Modifications to the self-tests to allow for correct results
		on TS-IEEE (they've never been right before this).
	2.	Continuing Makefile cleanup.
	3.	MIPS real*16 support from Rich Shapiro.
	4.	Added support for printing integers from MPP library code
		during simulation.

Edition 228:
	1.	More closely follow the IEEE rules regarding NaNs as operands
		and results.
	2.  Use the functional forms of isdigit() and isspace(), rather
		than the macro forms.  This allows us to compile with UNICOS
		9.x #include files, without forcing users of arith to link
		with UNICOS 9.x libs (specifically, optcg wants to be able
		to link with UNICOS 8.x libs).
	3.	Try again.  In the Makefile, #include the UNICOS 8.3 <ctype.h>
		instead of the 9.0 one.  We still link with 9.0 libs.  This
		should allow both nondebug and debug builds of things that
		link in arith to work.  (Fix #2, above, failed when linking
		debug compilers.)

Edition 227:
	1.	For version comparison between the arith data file and the
		libm.a when simulating, do a better job of figuring out the
		libm.a version.  Also did some prettification of the version
		checking code in mpp_sim.c, and a minor change to allow for
		creating the MPP simulation data file on a C-90.

Edition 226:
	1.	Oops.  Send store_mpp_word() the _address_ of the register you
		want to simulate storing, not the contents of it.  Sheesh.

Edition 225:
	1.	Large amounts of Makefile beautification, after Mark Cruciani
		had some targeting problems.

	2.	Add a -z switch to get.obj.arith, to return sources, and add
		a new target "install_source" to the Makefile, to install them
		where the -z logic is expecting to find them.  This new target
		should be made after "update" is made.

	3.	Take out the -X1 for T3E builds temporarily.  It's causing
		problems with the currently-available hardware.

Edition 224:
	1.	IRIX/MIPS changes.  Add fintrin_irix.f, fintrin_dc_irix.f,
		fintrin_irix_f77.f.  Update Makefile to understand about IRIX,
		and build the three new modules.  Update #ifdefs in confidence.c,
		miscmath.c, native.c, simulate.c, strcvt.c, and test_ar_intrin.c
		to do MIPS like we do Solaris.

	2.	Fix a problem in AR_make_complex (miscmath.c), where aliasing
		result and op2 led to wrong results.

	3.	Set up the simulated stack differently in mpp_sim.c.  For the
		T3E, we had the simulated DSIB between the simulated frames of
		the caller and callee.  The latter two need to abut, or arg
		addressing when simulating routines with more than 6 arguments
		won't work.

	4.	Replace a sequence of '|' expressions in ar_convert_str_to_float()
		with a sequence of '|=' assignments.  The former is subject to
		expression reordering which, although it apparently hasn't yet
		occurred, would get the arguments to _defgu2sd() in the wrong
		order if it happened.

Edition 223:
    1.  Fix for spr 100775.  Insure that 64 bit addressing is used
	in  Ar_put_real_address().  Previous cast to long it was 
	getting only the low order 32 bits.

    2.  Changed get.obj.arith to provide release 2 arith versions for  
	cft90 6.x or 6.X products.

Edition 222:
    1.	Changed header processing for cld a.out again.  Do move
	down over header after all segments are processed.  Insure
	that segment ids can't be redefined by an undefined segment.

    2.  Perform right shifts using macro with unsigned longs so no
	sign extension takes place, even for Cray systems that sign
	extend right shifts.  The SHRIGHT64X macro specifically does
	sign extension and will continue to sign extend.

    3.	Changed definitions of parcels to unsigned longs to insure
	that sign extension doesn't take place.

Edition 221:
    1.  Changed definitions for cld a.out header. Defined dynamically
	rather that static assignements.
    2.  Spr 100747: Mod by Greg Titus.
        arith.internal.h:   Add ar_crnd128() declaration.
        cvt.c:              Call ar_crnd128() for 128-bit Cray floating
                            point round_int_div.
        convert.c:          Add ar_crnd128() body.
    3.  SPR 101290: Mod by Greg Titus.
        mpp_sim.c:          Fix invalid type punning (IEEE complex to
                            IEEE float) in ar_pass_arg_value().  We
                            were passing a misaligned 32-bit IEEE
                            float to ar_convert_to_float() when we
                            were doing the real part of a 32-bit IEEE
                            complex.

Edition 220:
    1.	Changed Makefile to build Cray-t3d and Cray-t3e targets.

    2.	Changes for native versions of Cray-t3d and Cray-t3e that use
	Solaris simulation routines. 

Edition 219:
    1.  Changed mpp_sim.c to handle the segment identifier differences between
        the t3d and t3e.  open_intrinsics_file now looks at the segment
	type field of the file header instead of the id, and
	load/store_mpp_word now looks at variables corresponding to the
	new segment identifiers.  T3E data files built with mppldr should
	not be used.  Those produced by cld will be handled (hopefully) by
	this mod.

    2.  Changed arith_mpp.s to add the 32-bit SCANI, VERIFYI, and INDEXI
        intrinsics to the list of those needing to have their fcd arguments
	converted to byte-lengths.

Edition 218:
    1.  Triton-IEEE 0162 and 0163 divide instructions had the operands
        reversed for the divide calls.  This fixes SPRs: 95451, 96747,
	95582, 97309, 99176, 99366.

Edition 217:
    1.  MPP 0x18 instructions were being called illegal and arith was aborting.
        These are now disassembled and ignored.  For 0x18c (RPCC), the rtc
	is moved into ra.

    2.  The mpp intrinsic simulations for index, scan, verify, and selected_
        real_kind for 32-bit integers were not supported.  The appropriate
	entry points are now in place however.

Edition 216:
    1.  Fix for SPR 98417.  The 01xx40 instructions (loads and stores to
        and from S-regs with 3-parcel address expressions) were incorrectly
	simulated.  Only two parcels were ever being used.

Edition 215:

    1.  Changed get.obj.arith to use /bin/cp instead of whatever one's path
        pointed to, to prevent ownership problems, and now use chmod to
	force the correct permissions.
	
    2.  Added "bump" to the Makefile to allow moving the newest version
        to latest without creating a new newest.
	
    3.  Fixed SPR 96747.  This was a problem with the 174[1,2,3] instructions
        on the c90 and triton causing the reshape intrinsic to fail for 128-bit
	data items.  The loop variables and indices in the loops in ar_sim
	(cray1_sim.c) for these instructions were wrong.

Edition 214:

    1.  Fixed bug in ar_itoc128 in which the resulting exponent was 1 too big.
        (jhw) Altered this function to return the correct status.  This fixes
	SPR 90659.

    2.	Fixed bugs in ar_sim simulation of 026/027 instructions that were
	causing unsimulated instruction errors and not using new A-reg operand
	instructions.

    3.	Removed rounding in conversion of Cray 128-bit to 64-bit floating point
	since all compilers internally just truncate (keep the first 64-bits).

    4.  Added tbb and svc to the change list, made the default uss results
        repository to be in cray/uss/a6/jhw, and changed the c90 and triton
	cc and libs setup to the correct directories in Makefile.

Edition 213:

    1.	Added check for compatible floating point format between the requested
	resulttype and that specified by the arith data file.  Generate arith error
	message 2018 for a mismatch.

    2.	Added /opt/ctl/craylibs[_m]/craylibs[_m] as the first default location
	for the (native) arith data file.  Added check for libm.a in the .
	directory as well as the .. directory relative to the arith data file.

    3.  Relaxed the requirement for libm.a existing--cross-compilations do not
	require it.  However, if it does exist, it's version must match.

    4.  Added integer*8 versions of native power function evaluation.

    5.	Added test for IEEE attribute in get.obj.arith in determining the
	default target.

	6.  Added simulation for the remaining cray-ts[,ieee] instructions that
	may be encountered in future releases of libraries.

    7.  Modified arith_mpp.s to support T3D and T3E libraries and convert to
	the T3E argument/fcd interfaces if necessary.

    8.  Modified mpp_sim.c to interface to either T3D or T3E libraries.

Edition 212:

    1.	Added simulation of Triton instructions Vi Vj*[LU]Vk.

    2.  Fixed bug in simulation of Vi Vj,Vj<Ak instruction.  Fixed bug in
	store_pvp_word to mask the segment number.

Edition 211:

    1.  Added full 128-bit IEEE arithmetic everywhere.

    2.  Removed rounding argument from ar_cfadd/sub64 routines--wasn't used.

    3.  Removed all internal arith declaration stuff from arith.h.  In
	particular, removed the internal declarations for the AR_DATA union and
	moved them to an internal ar_data union in arith.internal.h.

    4.	Removed internal redundancy of rounding mode names.

    5.	Changed internal AR_FLOAT_NOT_COMPLEX to AR_FLOAT_SIMPLE.

    6.	Added ar_state_info struct and associated ar_state_register variable.
	Moved ar_CRAY_64_trunc_bits and ar_IEEE_denorm_behavior into this variable.

    7.	Added partial support for "multi-targetable" arith.  Intrinsic folding
	is multi-targetable via the arith data file pointed to by CRAYLIBS.  Basic
	arithmetic still depends upon input type arguments.

    8.	Added support for cray-ts and cray-ts,ieee in the Makefile and
	get.obj.arith.

Edition 210:

    1.  Fixed problem with 64-bit pointers being cleared to 24-bits in
	ar_clear_unused_bits.

    2.  Now allow uppercase, lowercase, and full names for new and latest
	version ids.

    3.  Switched to Craylibs 2.0 for arith testing.

    4.  Fixed problem in cray1_sim.c and mpp_sim.c that generated libm
	warning message on non-package versions of Craylibs.

Edition 209:

    1.	Modified Makefile support for updating arith data files to be able
	to create and update using the newest (possibly non-packaged) versions
	of Craylibs.  Also, modified *.o targets to be arith.a(*.o) to minimize
	the amount of recompilation after a make clobber.  The edition target
	copies an initial arith.a from the corresponding system directory.

    2.	Changed severity level of 2016 arith message to Logfile_Warning.

    3.	Added check for non-package version of arith data file (?.x.?.? in
	AR_libmv2) which skips libm[v2].a validation step when data file
	is opened.

	4.	Made modifications to accept 128-bit IEEE floating and 64-bit integer
	AR_TYPEs for Solaris.  Internally 128-bit IEEE floating values are converted
	to/from 64-bit values for the time being.  Also, the integer-to-integer
	power function uses the 32-bit intrinsic for the time being.

    5.	64-bit pointers no longer have the upper 8 bits cleared by
	ar_clear_unused_bits.

    6.	Allow 46-bit integer type in MPP simulation (since this is also used
	for Triton currently).  Eliminates AR_STAT_INVALID_TYPE for powri.

    7.	Added cray-ts as an allowable target in get.obj.arith.

    8.  Changed c++ product in get.obj.arith to return the same arith.a as scc
	for ccg and mppcg code generators but continue to return an arith.a with
	no intrinsic folding for rcg.  Also force new version for mppcg.

    9.	Removed extern of ARSQRTA from confidence.c and ctgen.c since Cray-2
	is no longer supported.  Added trapmathlibabort() to confidence.c.

   10.	Fix MPP simulation of defgu2sd returning 32-bit IEEE floats by moving
	the upper 32-bits returned by defgu2sd into the lower 32-bits.  Also, set
	the dynamic rounding mode variables to round-to-nearest defaults.

Edition 208:

    1.	Fixed bug in PVP simulation of Ai [PQZ]Sj instruction to test for j==0.

    2.	Changed DCMPLX in test_native.f, sim_c1_gen.f, and sim_mpp_sim.f to
	CMPLX since DCMPLX is no longer recognized.

Edition 207:

    1.	Allow more slop in stores also (see edition 205 notes).  Fixes problem
	in which a simulated ORE load is stored back to the same location with
	the same value.

    2.	Added this "allowable ORE" logic to mpp_sim.c but only up to 8 words.

Edition 206:

    1.	Added a bunch of new PVP instructions to be simulated.  Now the only
	PVP (YMP) instructions not simulated are ERR, EXIT, half-precision
	floating multiply, monitor instructions, and any instruction
	referencing shared registers.

Edition 205:

    1.	Add more slop for vector loads in simulated search functions such as
	in memchr.  Return bad data (0xf0f0...f0) for up to 64 words beyond
	known data length rather than trying to read the data and possibly
	getting an ORE.  Force internal error if any simulated store goes
	beyond known data length.

Edition 204:

    1.	Define EDITION string on cc command line now.

    2.	Added declarations for arith_vers_name, arith_vers_ID, and
	arith_vers_number in arith.h.

    3.	Changed back argument in calls to _F90_{INDEX,SCAN,VERIFY} to be
	a pointer rather than a value due to a library change.

    4.	Changed makefile to permit the PVP arith data file to be built on a
	C90 (but still in Y-mode and using YMP libraries though).  Also build
	the MPP arith data file on a C90.

Edition 203:

    1.  Bugfixes in get.obj.arith.

    2.	Added AR_cabs to no_intrin.c (to resolve external reference in
	miscmath.c)--returns AR_STAT_UNDEFINED.  Added ar_divide_complex
	to no_intrin.c (to resolve external reference in math.c)--computes
	"standard" algorithm.

    3.  Added 3 "" arguments to 2016 warning message call to PRINTMSG to
	support cft77's (Pascal) version of PRINTMSG.

    4.	Fixed bug to return AR_STAT_UNDERFLOW in ar_cvt_str_to_float.

    5.  Added what-line to arith_c1.s and arith_mpp.s so that a 'what -s arith'
	will display the libm version of the arith data file.  Also, cray1_sim.c
	and mpp_sim.c search for the what-line string and use it, if found, to
	obtain the libm version number.  The what-line does not contain the
	arith.a version number so tests using this (to generate internal error
	messages) were changed.

Edition 202:

    1.	Removed 'cray-2' from the list of MACHINES in Makefile.  Miscellaneous
	other improvements such as email to-list.

    2.  Fixed bug in get.obj.arith when -v la needed to back up to the previous
	release, the latest edition in that release wasn't being gotten.

    3.	Fixed bug in AR_power in native.c for 32-bit IEEE complex--ARPOWCR was
	being called even when the power was complex type.
 
Edition 201:

    1.	Removed cvs log entries in all files except edition.c.

    2.  Added test in 'make update' to NOT update get.obj.arith or result test
   	case files for release branch updates.  Added associated note to README.

    3.  Fixed bugs in get.obj.arith associated with getting development
   	editions.  Version string now contains the release number but only
   	if the release number is greater than 1.

    4.	Deleted support for cray-2 and cray-x4 everywhere.

    5.  Deleted calls to native PVP library/intrinsic functions.  All such
   	functions are now simulated.  Note that this implicitly eliminates
   	support for the cmcs code generator products.

    6.  Removed save/clear/restore of ar_CRAY_64_trunc_bits from intrin.c and
   	mpp_sim.c.  Save/clear/restore of ar_CRAY_64_trunc_bits is applied to
   	all PVP library/intrinsic function simulation (in cray1_sim.c).

    7.	Removed AR_IEEE_denorm_behavior routine.  Denorm behavior is now 
	specified inside corresponding intrinsic evaluation modules (e.g.,
	mpp_sim.c).  Added AR_Denorm_Operands_Trap flag and set it for MPP
	arithmetic evaluation (in mpp_sim.c).

	Moved all denorm declarations from arith.h to arith.internal.h to
	make it invisible to the outside world.

    8.	Defined AR_STAT_SEMIVALID error status bit.  Updated arith.doc to
	describe the situations under which this bit is set.

    9.  Restructured the intrinsic folding routines.  Now either intrin.c
	or no_intrin.c must be used with the latter causing load-time missing
	external messages if intrinsic evaluation routines are called.  Some
	intrinsic evaluation routines, such as those for string conversions,
	may exist in no_intrin.c.  In any case, these source files contain the
	high-level interface routines to intrinsic folding (AR_name routines).

	The high-level interface routines in turn call lower-level routines,
	named ar_name, in either native.c or simulate.c.  Those in native.c
	ultimately call native (non-Cray system) library routines.  Those in
	simulate.c ultimately cause a simulation of Cray system (PVP or MPP)
	library routines.

	The lower level ar_name routines in simulate.c now do generic argument
	set up via calls to support routines located in specific platform
	modules (cray1_sim.c or mpp_sim.c).  After arguments have been set up,
	routine ar_sim is called to simulate just the evaluation of the
	specific library function for the specific platform.  Finally, routine
	ar_get_function_value is called to get the function value from the
	appropriate register.

   10.	Fixed AR_convert_str_to_int to call ar_set_invalid_result for overflows
	rather than clearing the unused bits.  In some cases, AR_STAT_ZERO
	was being set in addition to AR_STAT_OVERFLOW.
 
   11.	Fixed bug in AR_convert which set AR_STAT_UNDERFLOW for the conversion 
	of 0.0 from 64 to 32-bit IEEE float.
 
   12.	Fixed bug in AR_i32/64norm which was not setting AR_STAT_UNDERFLOW when
	the operand bits were not zero, but the normalization process caused 
	all coefficient bits to become zero.  Also removed unnecessary test 
	for expo < 0 when preceding logic guaranteed it was always >= 0. 

   13.	Changed Makefile naming of arith cpio file in tlc_rel to use the last
	two fields in the libm.a what -s version number.

   14.	Changed names of internal non-static routines to always begin with 'ar_'
	from 'AR_'.  All user-callable routines should now consistently begin
	with 'AR_'.

   15.	Added a description to arith.doc for AR_one and AR_round_int_div.

   16.	Added version number proposal entry points into edition.c.

   17.	Fixed long standing bug in convert.c (ar_ifix64/32) in which zero was
	being incorrectly returned when the shift count was greater than the
	number of coefficient bits and the rounding mode was not AR_ROUND_ZERO.
	It is possible for the rounding to produce a value of 1 after all
	coefficient bits have been shifted off.

   18.	Fixed bug in mpp_sim.c in which the CMPULE/T instructions were not
	being evaluated correctly.

   19.	In simulate.c corrected the value stored into lcap1 for the call to
	defgu2sd to be a character address versus a word address.  This requires
	ar_pass_ext_address to pass back a word-to-character shift count if the
	external address descriptor is requested.

   20.	Added AR_index, AR_scan, AR_verify, AR_reshape, and AR_transfer as well
	as new logical type AR_Logical and associated constants AR_const_true
	and AR_const_false in arith.h.

   21.	Modified F90 routine ARSELRK to use PRESENT(P/R) and call the library
	routine with the correct argument/s.

Edition 54:

    1.	Changes in Makefile, get.obj.arith, and README to support the creation
	and use of release branches.  This includes new Makefile targets
	edition, update, revision, and new_release.  Also get.obj.arith now
	uses the -R option to translate a product version to an arith release
	number (default is still the development version except for cft77 which
	is forced to release 1).

    2.  Added 'cam' to the list of supported products in get.obj.arith.

    3.  Added logic to IEEE floating point arithmetic routines to return
	AR_STAT_UNDEFINED for denormalized operands and the denorm behavior
	flag is not AR_Denorms_Keep (i.e., an assumed MPP compiler).  This
	logic needs to exist in release 1 of arith to support updates to the
	"frozen" cft77_m compilers (and in particular fix SPR 79459).  This
	logic will be changed in release 2 of arith to more explicitly trap
	on denorm operands based on new	values for the denorm behavior flag.

Edition 53:

    1.  Support for the Fortran 90 modulo and selected_real_kind now exists for
	PVP, MPP, and Solaris compilers along with associated confidence tests.
	AR_Modulo is the name of the arith routine that supports the Fortran 90
	modulo intrinsic.  This distinguishes it from AR_modulo which was the
	Fortran 77 mod intrinsic which could return different results.

    2.	Added AR_mod to be the Fortran 77 mod intrinsic and deprecated AR_modulo
	to (hopefully) eliminate maintenance questions and user confusion.
	AR_mod is identical to the old AR_modulo for now.

    3.	Made changes to Makefile to build arith data files using modulo and
	selected_real_kind from libfi.a.

Edition 52:

    1.  Added support for floating modulus and selected_real_kind for cf90_m
	only.  This is a temporary edition to support MPP compilers only.

Edition 51:

    1.  Fixed LDS and STS MPP instruction simulation to NOT return
	AR_STAT_UNDEFINED (AR_convert returns this when converting a 64-bit NaN
	to 32-bit but this exception cannot occur on MPP hardware when simply
	loading/storing a NaN value).

    2.  Added characters to the internal ar_eval_lib_edition variable so that
	it can be printed by the what command.

Edition 50:

    1.	Changed AR_[ic]fcmp* routines to be statically declared inside compare.c
	and removed their interface description from arith.internal.h.

    2.	Changed the IEEE compare algorithms to not use subtract since they must
	be exact and not overflow or underflow.

    3.  Changed mpp_sim.c to use AR_compare for the CMPTxx instructions.

    4.  Upgraded sim_mpp_gen.f to compile and run with latest cft90 and
	generate correct data for/from the HLOG, HEXP, and HSQRT routines.
	Also added more test cases for string to float conversions.

    5.  Changed version numbering to drop the leading zero (version 50 will be
	stored as 50, not 050).

    6.  Changed packaging of arith data files.  The default location is now
	the same directory as libm[v2].a on PVP systems and NO default on nonPVP
	systems.  The CRAYLIBS environment variable is now used for both PVP
	and MPP simulation.  Created a lib subdirectory below arith and
	$(USS)/arith containing platform subdirectories with the associated
	data file in each of these.  Removed the .a files from the cpio files.
	Added arith_c1.mh and arith_mpp.mh to the corresponding cpio files.

Edition 49:

    1.	Force ar_IEEE_denorm_behavior default to be consistent with intrinsic
	simulation platform (Keep on sparc, +0.0 on MPP, illegal on C1).  Removed
	default declaration from miscmath.c.  Replaced logic inside
	AR_IEEE_denorm_behavior to simply validate the input value and return
	a nonzero value if not equal to the default.

    2.  Fixed constant value for 'A' in arith_mpp.s.

    3.	Fixed subscripting bugs in cray1_sim.c and mpp_sim.c where libm[v2]
	version is consistency checked against that in arith_c1/mpp.

    4.  Added support for 64-bit pointers in arith.h, cvt.c, math.c and
        miscmath.c.  These are required for Triton.

    5.  Made all calls to PRINTMSG have 8 arguments to support cft77 message
	routine.

    6.  Fixed end-case in mpp_sim.c because the CMPTxx instructions are exact
	and do not over/underflow.

    7.  Fixed spelling in arith.mk.

Edition 48:

    1.	Fixed bug in ar_power (in mpp_sim.c) in which switch statement was
	based on basetype rather than resulttype.

    2.  Modified get.obj.arith to return latest validated version for each
	platform rather than the global validated version.  Created 'latest'
	file in platform subdirectories in arith directory.

Edition 47:

    1.	Fixed floating input conversion bug in which an all zero mantissa
	produced an AR_STAT_UNDEFINED.

Edition 46:

    1.	Fixed floating input conversion bug in which a zero exponent still
	produces an AR_STAT_UNDEFINED if exponent is signed (e.g. 1.0e+0).

Edition 45:

    1.  Added simulation interface identifiers in case later changes are
	necessary.

    2.  Converted register numbers to mnemonic names in mpp_sim.c

    3.	Fixed floating input conversion bug in which a zero exponent (e.g. 1.0e0)
	produced an AR_STAT_UNDEFINED because leading zeroes are skipped.

    4.	Make sure that the libm[v2].a returned by get.obj.arith RW by owner.

    5.	Add AR_NOINTRIN_ERROR routine to ctgen.c to resolve fintrin.o external.

Edition 44:

    1.	get.obj.arith removes cray[12].o from arith.a when non-native intrinsic
	evaluation is used (eliminates trapmathlibabort missing external
	message).  Removes misc_sim.o for native intrinsic evaluation.
	Tests for version >= 43 when deleting dummy_dc.o (vs dc_dummy.o).

    2.  Added simulation support for MPP LDS, STS, and S8ADDQ instructions.

    3.  Began using actual 32-bit algorithms for HEXP, HLOG, and HSQRT.

    4.  Updated README file to eliminate the USM mod creation.

    5.  Made complex division a pseudo-intrinsic such that native version (in
	intrin.c) calls a locally compiled Fortran routine which yields whatever
	the native algorithm might be (except for sparc which uses the previous
	algorithm).  The simulation version (in misc_sim.c) currently uses the
	previous algorithm (originally in math.c).  This may need to be replaced
	in the future with a simulation of what F90 does for the targeted system.
	Fixes SPRs 78056 and 78123.

    6.  Added complex division confidence tests (in itgen.c and confidence.c,
        added itgen.c support file itgenf.f).

    7.  Added 'ar_' prefix to the shift_{left,right} functions in misc_sim.c.

Edition 43:

    1.	All known bugs are fixed (except complex divides for Solaris cft90).

    2.	Added support for -v ne and -v va to get.obj.arith.  (Also -v xn is
	used in Makefile to build confidence test executables.)

    3.	Split intrinsic evaluation into 4 basic modes with no cross-over
	permitted.  The modes are:

	a) No intrinsic function evaluation available	(no_intrin.o)
	b) Use native intrinsic library routines		(intrin.o)
	c) Simulate PVP intrinsic library routines	(cray1_sim.o)
	d) Simulate MPP intrinsic library routines	(mpp_sim.o)
	
	Get.obj.arith uses the code generator, language, and target options
	to construct an arith.a capable of folding in exactly one of these
	modes.  The names in parentheses are the modules in the constructed
	arith.a corresponding to these modes.

	Modes a, c, and d should not require libm.a (libmv2.a) to be loaded
	into the compiler (at least for folding).

    4.	Allow 32 and 64-bit non-complex floats to be passed as operands
	to AR_leadz, AR_popcnt, and AR_poppar.  The operand type is no
	longer required to match the result type.  Also, updated messages
	with changes suggested by Pubs.

    5.	Increased output precision for float_to_string.

    6.	Add support for base 2 string_to_int conversions

    7.	Cleared part1 and part2 for 32-bit integer results in bits.c and
        math.c (ar_add_integer, ar_subtract_integer, bitoper, ar_dblshift,
        AR_leadz, AR_popcnt, AR_poppar).

    8.	Added 'c++' to the list of supported languages in get.obj.arith.
	Currently, the arith.a constructed for c++ does not support any
	math library intrinsic function evaluation (mode 3a above).

    9.	Add minimal support for 16-bit ints.  Functions supported are
	AR_convert, AR_status, and all functions which permit operands
	or results of any size integer value.

   10.	Also, fixed AR_convert_str_to_int which claimed to work for any
	integer type, but really didn't.  Part of the problem with
	this function (the value of the ar_bits_used argument) was
	"fixed" by a change in documentation.

   11.	Eliminated simulation of strtod/strtold in favor of the lower level
	routine defgu2sd.  An interface routine, ar_unpack_float_str is
	called to unpack a string into simulated Cray words prior to
	simulating defgu2sd.

   12.	Made a small optimization by using hardware shift instructions if
	running on a Cray (in shift_left/right).

   13.	Added a cpio target in Makefile.  This creates the different cpio
	files necessary for packaging arith with Craylibs and copies them
	out to the TLC_REL directory on USS.  Added file arith.mk to be
	included in the cpio files.

   14.	Added the following simulation error messages (see arith.msg for
	details):

	2014 - simulated intrinsic interface error
	2015 - unsupported new intrinsic function
	2016 - potentially different intrinsic function values (warning only)
	2017 - intrinsic routine not loaded

	These are intended to support version consistency checks when arith
	is built into compilers in the field.

   15.	Changed a bunch of file and routine names from *_ieee* to *_mpp*
	since it appears likely that future IEEE Cray architectures may not
	exactly produce the same results as the MPP.  If they happen to
	always produce identical results, then internal links to MPP versions
	can be used.

   16.	Moved conv routines into the results directory.  The conv
	subdirectory is no longer needed.

   17.	Removed dc_dummy.c.  Added dummy_dc.f in order to more easily
	maintain the Fortran naming conventions on multiple platforms.

   18.	Added HSQRT, HLOG, and HEXP to arith_mpp.s.  These are still just
	interfaces to the 64-bit algorithms.  However, the next edition of
	arith should automatically start using the actual 32-bit algorithms
	when they are put in the next Craylibs_m package.

   19.	Added external char variables AR_libmv2 and AR_arith_data.  These
	contain a null string if simulation is not used.  Otherwise,
	AR_libmv2 contains the libmv2.a version information, including the
	edition number (e.g., 81002).  AR_arith_data contains the arith.a
	edition number of the arith_c1/mpp file (i.e., the arith.a edition
	number when the data file was built).

Edition 42:

    1.	Added PVP and MPP intrinsic simulation for libm and floating input
	conversion routines.

    2.	Modified get.obj.arith to turn on simulation for ccg and mppcg code
	generators but disable it for rcg and cmcs.

    3.	Extensive modifications were made to the makefile for consistent
	packaging, easier testing, and to provide debug versions of arith.
	Added make targets to generate the intrinsic simulation data files
	and to generate the test data for the simulation confidence tests.

    4.	Updated the README, arith.msg, and arith.doc files.

    5.	Modified existing confidence tests to work with/without simulation
	enabled.  Added simulation confidence tests.
*/


static char USMID [] = "\n%Z%%M%	%I%	%G% %U%\n";
static char rcsid [] = "$Id: edition.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
/*
 * Revision 2.46  1997/07/22 23:31:22  gbt
 * MIPS NaNs are the opposite of SPARC/T90-IEEE/Alpha NaNs: the
 * latter has the high bit of the mantissa clear for signalling
 * NaNs, and set for quiet ones; the former is the other way
 * around.
 *
 * Revision 2.45  1997/06/17  17:46:19  gbt
 * - Add status setting for AR_STAT_INEXACT in IEEE normalization.
 * - Makefile cleanup -- don't execute "target" if not on a Cray
 *   machine, and in the "arith.a:" target, copy the arith.h file
 *   into a "common" subdirectory.  I had moved the common files
 *   into a "common" subdir in the USS directory in edition 234,
 *   and modified the get.obj.arith script accordingly, but I
 *   needed to do the same thing in the build directory structure,
 *   so that doing a "get.obj.arith -v xn ..." would work.
 * - Remove the use of /cray/uss/compiler/cost/bin/validate from
 *   the get.obj.arith script.
 *
 * Revision 2.44  1997/06/02  19:14:16  gbt
 * Add AR_ibits(), and the corresponding internal tests.
 *
 * Revision 2.43  1997/05/01  15:25:18  gbt
 * Remove the use of <ctype.h> in strcvt.c.  This allows us
 * to compile arith with 9.x #include files, and link it
 * with 8.x libraries, and have everything work right.  It
 * doesn't hurt wide-character compatibility, because there
 * is quite a bit of code in strcvt.c that is incompatible
 * with wide characters already.
 *
 * Revision 2.42  1997/04/25  02:56:02  gbt
 * 1.  Missed some modules that needed additions for 8-bit and
 *     16-bit integers.
 * 2.  Add full support for determining the host architecture.
 * 3.  Fix some minor divots in float-->string conversions.
 * 4.  Add AR_ishft() and AR_ishftc(), for the corresponding f90
 *     intrinsics, and add code to testint.c to test these.
 *
 * Revision 2.41  1997/04/02  00:04:31  gbt
 * Add support for 8-bit and 16-bit integers.  Update testing programs
 * appropriately.
 *
 * Revision 2.40  1997/03/17  21:19:11  gbt
 * 1.  Bring in the MIPS quad changes from Rich Shapiro.
 * 2.  Fix 703309 -- non-base-10 int-to-str conversions yielded
 *     trash.
 * 3.  Add appropriate casts to cray1_sim.c and mpp_sim.c, to
 *     remove some warnings about mistyped assignments.
 *
 * Revision 2.39  1996/12/12  20:31:44  gbt
 * Undo the change that made negating IEEE 0.0 not yield -0.0 any more.  The
 * standard says that negating 0.0 is *supposed* to yield -0.0.  We hadn't
 * read the IEEE standard completely.
 *
 * Revision 2.38  1996/11/05  18:33:00  gbt
 * Remove the MIPS hack in native.c for the INDEX(), SCAN(), and VERIFY()
 * intrinsics.
 *
 * Revision 2.37  1996/10/30  21:01:21  gbt
 * Add a -n option to remote-builds.csh, to tell it not to do a
 * "make delete" before proceeding with the builds.  Also, update
 * edition.c -- I had forgotten to do so on the last few changes.
 *
 * Revision 2.36  1996/10/29  21:53:39  gbt
 * - Clean up the code in various spots so that the Solaris and IRIX
 *   compilers don't emit any warnings any more.
 * - Really support the IRIX platform.
 * - Allow re-updating of the current USS edition.
 * - Move the USS arith.{doc,h,msg}.* files into a "common" subdirectory
 *   (aesthetics -- makes the directory easier to look at).
 * - Make sure all the files installed on USS have group-write access.
 * - Clean up get.obj.arith -- the indentation had gotten a little
 *   odd over the years.
 *
 * Revision 2.35  1996/10/28  23:10:04  gbt
 * Expand the simulated stack size for Cray-1 style simulation, from 2 kwords
 * to 10 kwords.  It wasn't big enough for the needs of all the intrinsics in
 * the 3.0 arith data file.
 *
 * Revision 2.34  1996/10/11  22:20:34  gbt
 * Make _numargs() work properly on T3E.  The SSIB and DSIB weren't properly
 * set up before beginning simulation, nor was the CI register correct on
 * entry to the simulated intrinsic code.
 *
 * Revision 2.33  1996/10/03  19:24:26  gbt
 * Fix some preprocessing problems having to do with the _Solaris #if tests.
 *
 * Revision 2.32  1996/09/23  21:21:46  gbt
 * - Simulate byte-manipulation instructions properly for a T3E.
 * - Remove pdbgint support for MPP -- it couldn't have been made to
 *   work anyway.
 * - Add environment variable control of ar_disasm debugging for MPP.
 *
 * Revision 2.31  1996/09/10  21:46:11  gbt
 * Allow for building on a TS (only building _on_ one, not _for_ one).
 *
 * Revision 2.30  1996/08/30  19:39:50  gbt
 * - Modifications to the self-tests to allow for correct results on
 *   TS-IEEE (they've never been right before this).
 * - Continuing Makefile cleanup.
 * - MIPS real*16 support from Rich Shapiro.
 * - Added support for printing integers from MPP library code during
 *   simulation.
 *
 * Revision 2.29  1996/08/20  20:07:35  gbt
 * Go back to using the UNICOS 8.3 #include files for C-90 and Y-MP.  There's
 * no apparent way to safely use the 9.x ones, and still allow people to link
 * an arith.a thus compiled with 8.x libraries.
 *
 * Revision 2.28  1996/08/15  23:09:12  gbt
 * More closely follow the IEEE rules regarding NaNs as operands and results.
 * Also, do a little fixup in the Makefile.  The YMP tests all pass now.
 *
 * Revision 2.27  1996/07/03  23:14:26  gbt
 * For version comparison between the arith data file and the
 * libm.a when simulating, do a better job of figuring out the
 * libm.a version.  Also did some prettification of the version
 * checking code in mpp_sim.c, and a minor change to allow for
 * creating the MPP simulation data file on a C-90.
 *
 * Revision 2.26  1996/06/19  23:32:16  gbt
 * Oops.  Send store_mpp_word() the _address_ of the register you
 * want to simulate storing, not the contents of it.  Sheesh.
 *
 * Revision 2.25  1996/06/12  18:20:16  gbt
 * Large amounts of Makefile beautification, after Mark Cruciani
 * had some targeting problems.
 * Add a -z switch to get.obj.arith, to return sources, and add
 * a new target "install_source" to the Makefile, to install them
 * where the -z logic is expecting to find them.  This new target
 * should be made after "update" is made.
 * Take out the -X1 for T3E builds temporarily.  It's causing
 * problems with the currently-available hardware.
 *
 * Revision 2.24  1996/05/31  21:09:51  gbt
 * 1. IRIX/MIPS changes.  Add fintrin_irix.f, fintrin_dc_irix.f,
 *    fintrin_irix_f77.f.  Update Makefile to understand about IRIX,
 *    and build the three new modules.  Update #ifdefs in confidence.c,
 *    miscmath.c, native.c, simulate.c, strcvt.c, and test_ar_intrin.c
 *    to do MIPS like we do Solaris.
 *
 * 2. Fix a problem in AR_make_complex (miscmath.c), where aliasing
 *    result and op2 led to wrong results.
 *
 * 3. Set up the simulated stack differently in mpp_sim.c.  For the
 *    T3E, we had the simulated DSIB between the simulated frames of
 *    the caller and callee.  The latter two need to abut, or arg
 *    addressing when simulating routines with more than 6 arguments
 *    won't work.
 *
 * 4. Replace a sequence of '|' expressions in ar_convert_str_to_float()
 *    with a sequence of '|=' assignments.  The former is subject to
 *    expression reordering which, although it apparently hasn't yet
 *    occurred, would get the arguments to _defgu2sd() in the wrong
 *    order if it happened.
 *
 * Revision 2.23  1996/05/10  22:02:22  ghg
 * Edition 223:
 *     1.  Fix for spr 100775.  Insure that 64 bit addressing is used
 *         in  Ar_put_real_address().  Previous cast to long it was
 *         getting only the low order 32 bits.
 *
 *     2.  Changed get.obj.arith to provide release 2 arith versions for
 *         cft90 6.x or 6.X products.
 *
 * Revision 2.22  1996/05/01  22:00:10  ghg
 *     1.  Changed header processing for cld a.out again.  Do move
 *         down over header after all segments are processed.  Insure
 *         that segment ids can't be redefined by an undefined segment.
 *
 *     2.  Perform right shifts using macro with unsigned longs so no
 *         sign extension takes place, even for Cray systems that sign
 *         extend right shifts.  The SHRIGHT64X macro specifically does
 *         sign extension and will continue to sign extend.
 *
 *     3.  Changed definitions of parcels to unsigned longs to insure
 *         that sign extension doesn't take place.
 *
 * Revision 2.21  1996/04/23  23:42:44  ghg
 *  '********************************************************'
 *  Please send following email...
 *  To: krz, bcn, mwm, knaak, homer, rlf, srp, pmk, bhj, kik, tbb, jk, gbt, lew, svc, ghg, mac
 *
 *  Subject: New edition `cat release`.`cat edition` of arith
 *
 *  Edition `cat release`.`cat edition` of arith is now available in $(USSARITH)/R`cat release`.
 *  It can be accessed with get.obj.arith as follows:
 *
 *  "      get.obj.arith -v ne ..."
 *
 *  It contains the following enhancements:
 * Edition 221:
 *     1.  Changed segment definitions for cld a.out header. Defined dynamically
 *         rather than static assignements.
 *     2.  Spr 100747: Mod by Greg Titus.
 *               arith.internal.h:   Add ar_crnd128() declaration.
 *               cvt.c:              Call ar_crnd128() for 128-bit Cray floating
 *                                   point round_int_div.
 *               convert.c:          Add ar_crnd128() body.
 *     3.  SPR 101290: Mod by Greg Titus.
 *               mpp_sim.c:          Fix invalid type punning (IEEE complex to
 *                                   IEEE float) in ar_pass_arg_value().  We
 *                                   were passing a misaligned 32-bit IEEE
 *                                   float to ar_convert_to_float() when we
 *                                   were doing the real part of a 32-bit IEEE
 *                                   complex.
 *
 * Revision 2.20  1996/04/17  18:22:50  ghg
 * Edition 220:
 *     1.  Changed Makefile to build Cray-t3d and Cray-t3e targets.
 *
 *     2.  Changes for native versions of Cray-t3d and Cray-t3e that use
 *         Solaris simulation routines.
 *
 * Revision 2.19  1996/02/07  23:39:49  jhw
 * Edition 219:
 *     1.  Changed mpp_sim.c to handle the segment identifier differences between
 *         the t3d and t3e.  open_intrinsics_file now looks at the segment
 * 	type field of the file header instead of the id, and
 * 	load/store_mpp_word now looks at variables corresponding to the
 * 	new segment identifiers.  T3E data files built with mppldr should
 * 	not be used.  Those produced by cld will be handled (hopefully) by
 * 	this mod.
 *
 *     2.  Changed arith_mpp.s to add the 32-bit SCANI, VERIFYI, and INDEXI
 *         intrinsics to the list of those needing to have their fcd arguments
 * 	converted to byte-lengths.
 *
 * Revision 2.18  1996/01/03  22:15:46  jhw
 * Edition 218:
 *     1.  Triton-IEEE 0162 and 0163 divide instructions had the operands
 *         reversed for the divide calls.  This fixes SPRs: 95451, 96747,
 * 	95582, 97309, 99176, 99366.
 *
 * Revision 2.17  1995/12/19  16:51:03  jhw
 * 12/19/95 jhw:
 *     1.  MPP 0x18 instructions were being called illegal and arith was aborting.
 *         These are now disassembled and ignored.  For 0x18c (RPCC), the rtc
 * 	is moved into ra.  This was in mpp_sim.c.
 *
 *     2.  The mpp intrinsic simulations for index, scan, verify, and selected_
 *         real_kind for 32-bit integers were not supported.  The appropriate
 * 	entry points are now in place however.  This was in arith_mpp.s
 *         and simulate.c
 *
 * Revision 2.16  1995/12/11  15:56:51  jhw
 * 12/11/95 jhw:  This change fixes incorrect simulation of the 01xx4 (S-reg
 * load/store) instructions.  Only two-parcels of the 3-parcel address
 * expressions were being used.  This fixes SPR 98417.
 *
 * Revision 2.15  1995/11/30  21:34:37  jhw
 * 11/30/95 jhw:  Changed cray1_sim.c so that the 174[1,2,3] instructions were
 * being simulated correctly.  This fixes reshape sprs: 96747, 95628, 95627,
 * 97309, and 97842.  Also modified get.obj.arith to manually set permissions
 * when copying arith.a.  Makefile was modified to reference newer libraries
 * for building arith data files, and I added the capability of bumping a new
 * version to latest.
 *
 * Revision 2.14  1995/11/13  20:25:46  jhw
 * Edition 214:
 *
 *     1.  Fixed bug in ar_itoc128 in which the resulting exponent was 1 too big.
 *         (jhw) Altered this function to return the correct status.  This fixes
 * 	SPR 90659.
 *
 *     2.	Fixed bugs in ar_sim simulation of 026/027 instructions that were
 * 	causing unsimulated instruction errors and not using new A-reg operand
 * 	instructions.
 *
 *     3.	Removed rounding in conversion of Cray 128-bit to 64-bit floating point
 * 	since all compilers internally just truncate (keep the first 64-bits).
 *
 *     4.  Added tbb and svc to the change list, made the default uss results
 *         repository to be in cray/uss/a6/jhw, and changed the c90 and triton
 * 	cc and libs setup to the correct directories in Makefile.
 *
 * Revision 2.13  1995/09/26  20:40:19  jk
 * Added check for compatible floating point format between result type and
 * opened arith data file.  Made /opt/ctl/craylibs[_m]/craylibs[_m] first
 * default location for arith.  Removed libm.a existance requirement for
 * all cross compilations.  Added integer*8 versions of native power
 * functions.  Added simulation of remaining cray-ts[,ieee] instructions
 * (currently unused but lowers risk if future Craylibs uses them).  Changed
 * arith_mpp.s and mpp_sim.c to interface to either T3D or T3E libraries.
 *
 * Revision 2.12  1995/08/18  17:18:00  jk
 * Fixed some bugs in cray1_sim.c; added simulation for Vi Vj*[LU]Vk.
 *
 * Revision 2.11  1995/08/16  22:09:24  jk
 * Added full 128-bit IEEE support; removed all internal arith declarations
 * from arith.h; implemented partial support for multi-targetable arith
 * library; added support for native cray-ts, cray-ts-ieee.
 *
 * Revision 2.10  1995/04/18  23:17:52  jk
 * Fixed cast of int to 64-bit pointer problem; fixed problem with nonpackage
 * versions of Craylibs.  Begin using Craylibs 2.0 for arith testing.
 *
 * Revision 2.9  1995/02/08  19:55:22  jk
 * Added support for non-package versions of Craylibs and asynchronous
 * updating of the arith data file.  Changed severity of 2016 message to
 * Logfile_Warning.  Allow 128-bit IEEE float types and 64-bit integer
 * types on sparc systems but internally work in reduced precision.  Fixed
 * MPP simulation bugs--allow 46-bit integer type and set default dynamic
 * rounding modes and types.  Added cray-ts as target in get.obj.arith.
 * Changed c++ product in get.obj.arith to be same as scc except for rcg.
 * No longer clear upper 8 bits of 64-bit pointers.
 *
 * Revision 2.8  1995/01/04  16:06:20  jk
 * Test for j==0 in simulation of Ai [PQZ]Sj.  Changed DCMPLX to CMPLX where used
 * since cft90 no longer recognizes DCMPLX.
 *
 * Revision 2.7  1994/12/20  22:39:20  jk
 * Allow slop in stores also.  Copy allowable ORE logic to mpp_sim.c.
 *
 * Revision 2.6  1994/12/16  18:02:18  jk
 * Added simulation of remaining PVP instructions except for system calls,
 * monitor mode instructions, half-precision multiply, and shared register usage.
 * Fixes problem when reshape calls %LDSV which uses some previously unsimulated
 * vector instructions.
 *
 * Revision 2.5  1994/12/13  23:00:44  jk
 * Fixed problem with simulated vector load in memchr reaching outside fcd
 * memory area.
 *
 * Revision 2.4  1994/12/13  14:51:38  jk
 * Changed back arg in _F90_{INDEX,SCAN,VERIFY} calls to be a pointer rather
 * than a value.  Added declarations in arith.h for arith_vers_name,
 * arith_vers_ID, and arith_vers_number.  Moved definition of EDITION macro
 * from edition.c to cc command line.  Changed Makefile to build arith data
 * files on either YMP or C90 (in Y-mode).
 *
 * Revision 2.3  1994/11/29  23:37:14  jk
 * Added AR_cabs and ar_divide_complex to no_intrin.c.  Added 3 NULL args
 * to 2016 PRINTMSG call.  Fixed ar_cvt_str_to_float to return
 * AR_STAT_UNDERFLOW.  Added what-lines to arith_c1/mpp.s.
 *
 * Revision 2.2  1994/10/19  20:54:52  jk
 * Fix bug in native.c--x**C was calling ARPOWCR.
 *
 * Revision 2.1  1994/10/18  19:48:35  jk
 * Major restructuring of intrinsic evaluation.  Added AR_index, AR_scan,
 * AR_verify, AR_reshape, and AR_transfer.  Added type AR_Logical.  Added
 * AR_SEMI_VALID status value.  Removed AR_IEEE_denorm_behavior.  Removed
 * support for Cray-2 and Cray-XMP.  Changed internal names to consistently
 * begin with 'ar_' while all external user-callable names begin with 'AR_'.
 * See edition.c Edition 201 notes for a complete list of changes.
 *
 * Revision 2.0  1994/08/11  16:21:30  jk
 * Increment development version to 2.0
 *
 * Revision 1.53  1994/08/10  22:01:58  jk
 * Make changes to support release 1 branch; added logic to trap on denormalized
 * operands (for MPP only) in IEEE arithmetic routines; added product cam to
 * get.obj.arith.
 *
 * Revision 1.52  1994/08/05  22:27:03  jk
 * Add support for Fortran 90 modulo and selected_real_kind intrinsics.
 *
 * Revision 1.51  1994/06/16  18:06:58  jk
 * Fix bug in mpp_sim.c to prevent premature AR_STAT_UNDEFINED returned when
 * LDS instruction loads a NaN value.  Add what-line chars in edition.c.
 *
 * Revision 1.50  1994/06/09  14:17:15  jk
 * Bug fixes, made AR_[ic]fcmp* routines internal, avoid using subtract for
 * IEEE compares (they must be exact and not over/underflow), changed version
 * numbering to 2-digit number, changed packaging for simulation (arith.a will
 * go in the compiler packages, arith data file will go in craylibs package
 * and be installed in the same location as libm.a/libmv2.a).
 *
 * Revision 1.49  1994/06/01  21:57:25  jk
 * Fixed problems to correctly handle denorm behavior, correctly test for
 * version consistency with libm/libmv2, and interface with cft77's version
 * of PRINTMSG.
 *
 * Revision 1.48  1994/05/27  18:34:46  jk
 * Fix bug in AR_power in mpp_sim.c for 32-bit float base operand--a 64-bit
 * value was being returned.
 *
 * Revision 1.47  1994/05/23  21:57:18  jk
 * Fixed bug in ar_unpack_float_str in which 0e1 was failing.
 *
 * Revision 1.46  1994/05/19  22:34:21  jk
 * Fixed another bug str to float conversions with 0 exponents.
 *
 * Revision 1.45  1994/05/19  16:51:17  jk
 * Add simulation interface ids to support future mods.  Converted register
 * #s to mnemonics in mpp_sim.c.  Fixed '..E0' bug in AR_convert_str_to_float.
 * Misc Makefile, get.obj.arith, confidence test fixes.
 *
 * Revision 1.44  1994/05/17  15:13:22  jk
 * Added simulation for the LDS, STS, and S8ADDQ MPP instructions plus the
 * actual 32-bit algorithms for HEXP, HLOG, and HSQRT.  Made complex division
 * a pseudo-intrinsic (native version in intrin.c, simulation version in
 * misc_sim.c) to fix SPRs 78056 and 78123.  Misc cleanup and bug fixes.
 *
 * Revision 1.43  1994/05/04  14:50:13  jk
 * Split intrinsic folding into 4 non-overlapping modes--none, native, PVP
 * simulation, and MPP simulation.  Allow 32/64-bit non-complex floats in
 * AR_leadz, AR_popcnt, and AR_poppar.  Add base 2 string_to_int conversion.
 * Clear part1 and part2 in integer results in bits.c and math.c.  Added
 * minimal support for 16-bit integers.  Began edition log in edition.c
 * for editions with large numbers of changes (see edition.c).
 *
 * Revision 1.42  1994/04/07  18:41:18  jk
 * Added PVP and MPP intrinsic simulation for libm and floating input conversion
 * routines.  Modified get.obj.arith to turn on simulation for ccg and mppcg
 * code generators but disable it for rcg and cmcs.  Extensive modifications
 * were made to the makefile for consistent packaging, easier testing, and
 * to provide debug versions of arith.  Updated the README, arith.msg, and
 * arith.doc files.  Modified existing confidence tests to work with/without
 * simulation enabled.  Added simulation confidence tests.  Added make targets
 * to generate the intrinsic simulation data files and to generate the test
 * data for the simulation confidence tests.
 *
 * Revision 1.41  1994/02/10  17:10:42  pmk
 * Fix problems with 32-bit IEEE intrinsic folding feature begun with last
 * version. Should affect only SPARC.
 *
 * Revision 1.40  1994/02/04  03:18:27  pmk
 * Fix SPARC/Solaris intrinsic folding so that 32-bit IEEE Fortran intrinsics
 * are not folded by conversion to/from 64-bit.
 *
 * Revision 1.39  1994/02/02  20:17:02  krz
 * Fix conversion of unsigned int to IEEE (SPR 74885).  Incorrect member was
 * accessed when determining unsignedness of integer type.
 *
 * Revision 1.38  1993/12/01  01:39:01  pmk
 * Change complex division as requested by GSF to special-case zero
 * imaginary parts in denominators (only) -- SPR 70472.
 * Also fix conversions from 64-bit IEEE to 32-bit IEEE -- SPR 70042.
 *
 * Revision 1.37  1993/11/30  00:38:22  krz
 * Add support for Cray single-precision floating-point truncation.  (SPR
 * 70042)
 *
 * Allow user-specified behavior for IEEE denorms.  (SPR 66025)
 *
 * Provide functions to convert a host 64-bit int to an AR_DATA representation.
 *
 * Update arith.msg with changes made by pubs (Chris Brewster).
 *
 * Fix bug in AR_status for 32-bit IEEE floats (AR_STAT_ZERO was not reported
 * correctly).
 *
 * Fix AR_convert_float_to_str to correctly report Nan, +Inf, and -Inf for IEEE
 * floating-point numbers.
 *
 * Fix ar_convert_to_float (called by AR_convert) to allow conversions from
 * integral values to type AR_Float_Cray1_64_F.  Previously,
 * AR_STAT_INVALID_TYPE was returned for such conversions.
 *
 * Revision 1.36  1993/11/12  02:08:39  krz
 * 1.  Add a new function, AR_make_complex, which creates a complex number from
 * two floating-point numbers.
 *
 * 2.  Update the messages file (arith.msg) with changes made by Pubs.
 *
 * 3.  Fix the result from function AR_one for 32-bit ints.  The unused upper
 * 32 bits of the word are now zeroed.
 *
 * 4.  Fix the result from AR_convert_str_to_float for 32-bit floats when
 * executing on the Suns.  The result was previously put into the wrong half of
 * the 64-bit word.
 *
 * Revision 1.35  1993/10/14  01:49:56  krz
 * The directory for arith has moved on the file server.  Update references to
 * the arith directory in all files.
 *
 * Add support to build under the "solaris" environment.
 *
 * Revision 1.34  1993/09/23  00:57:35  krz
 * Add function AR_subtract_ptr_ptr to perform pointer minus pointer
 * evaluations.
 *
 * Change function AR_add_ptr_int to return an error status (it previously
 * always returned AR_STAT_OK).
 *
 * Don't make reference to ARERPFIX on platforms where it doesn't exist.
 *
 * Change uses of macros CRAY, CRAY1, and CRAY2 to the preferred
 * (standard-conforming) macros _CRAY, _CRAY1, and _CRAY2.
 *
 * Added a dummy AR_INTERNAL_ERROR function to ctgen.c to avoid the unsatisfied
 * external.  Also link in PRINTMSG_dummy.c.
 *
 * Revision 1.33  1993/09/03  20:18:57  pmk
 * Fix cross-compiled exponentiation folding (IEEE on Cray hardware).
 *
 * Revision 1.32  1993/09/01  23:29:31  pmk
 * Revamp intrinsic processing, especially exponentiation, to support double
 * precision complex and to use system math libraries. Also fix a rounding bug
 * in convert.c.
 *
 * Revision 1.31  1993/08/12  20:55:41  krz
 * Add support to issue internal error messages through the PRINTMSG interface to the message system.
 *
 * Revision 1.30  1993/08/05  17:46:00  krz
 * 1.  Added AR_convert_hex_str_to_float function.
 *
 * 2.  Updated build procedures in README file to use generation compiler, library, and header files as jointly agreed upon by the compiler integrators.  (Don't have to worry about signgam anymore!)
 *
 * 3.  Added copyright notices and USMIDs to source files.
 *
 * 4.  Added function prototypes for AR_c2fapprox in cray2_dummy.c and matherr in itgen.c.
 *
 * 5.  Added descriptions for AR_convert_str_to_int, AR_convert_int_to_str, AR_convert_str_to_float, and AR_convert_float_to_str to arith.doc.
 *
 * 6.  Clean up, standardize, and document the return values for AR_convert_str_to_int and AR_convert_str_to_float.
 *
 * Revision 1.29  1993/07/13  18:25:21  gbt
 * For reals raised to integer powers, AR_power() was checking the size of
 * the base, rather than the power, to determine whether the power was a
 * 32 bit int or a 64 bit one.  The bug was introduced when 32 bit integer
 * support was added.
 *
 * Revision 1.28  1993/07/01  23:41:52  krz
 * This is a small commit to test the automatic mail feature of CVS.
 *
 * Revision 1.27  1993/06/15  00:59:27  krz
 * Add a dummy entry for the Cray-2 floating-point approximation function (and
 * associated data) for use on non-Cray-2 machines (to save code/data space
 * when the library is linked into executables on those machines).  Add a
 * couple of workarounds for bugs found in the gcc compiler and library
 * headers.
 *
 * Revision 1.26  1993/05/20  02:59:28  krz
 * 1.  Fix return value for AR_modulo, which incorrectly reported overflow in
 *     cases where the numerator was negative and the result was zero.
 *
 * 2.  Fix sprintf() calls for floats on non-Cray hosts.  While the ar_ieee64
 *     union member is the same size as a double on a Sun, the parameter
 *     passing mechanism is different for structs and doubles.  Thus explicit
 *     casts are necessary.  Also, make all the format strings on those calls
 *     right.
 *
 * 3.  Comparisons of infinities with infinities are now fixed.
 *
 * Revision 1.25  1993/05/17  17:50:48  krz
 * Add a new function, AR_add_ptr_int to perform pointer arithmetic.
 * Fix some integral conversions that weren't sign extending correctly.
 * AR_status now allows and correctly handles 24 and 8 bit ints.
 *
 * Revision 1.24  1993/04/19  20:39:02  gbt
 * 1. Make sure all file errors increment the error counter in confidence.c.
 * 2. Add 32 bit signed and unsigned integer support.
 * 3. Modify testint.c to allow it to run on (SPARC) Suns.
 *
 * Revision 1.23  1993/04/09  22:36:35  pmk
 * Minor fixes to Cray double precision floating add and double precision
 * exponentiation, to yield proper results in end-case error situations.
 *
 * Revision 1.22  1993/04/09  03:51:04  krz
 * Change the data format for IEEE 32-bit floats so that the value is maintained in the lower 32 bits of a 64-bit word.  Change the data format for IEEE 64-bit complex (2 32-bit floats) to match the format used at runtime.  This change should be completely transparent since nobody is using these data types (yet).
 *
 * Revision 1.21  1993/03/11  23:29:15  pmk
 * Add new AR_round_int_div routine, and fix Cray-1 truncated multiplies
 * in the high-level interface.
 *
 * Revision 1.20  1993/02/09  21:07:19  pmk
 * Correct an abort in intrin.c when folding IEEE arithmetic on the Crays;
 * also, continue development and testing of 32-bit IEEE arithmetic.
 *
 * Revision 1.19  1993/02/04  23:40:25  pmk
 * Speed up integer multiplication, addition, and string conversion.
 *
 * Revision 1.18  1993/01/29  01:26:55  krz
 * Forgot to update the edition last time.  This is edition 17.
 *
 * Revision 1.17  1993/01/27  22:52:21  pmk
 * Add Cray 128-bit float -> IEEE 64-bit conversion.
 *
 * Revision 1.16  1992/12/29  03:47:04  krz
 * Move the removal of the overflow flag for unsigned integer multiplication from ar_multiply_integer to AR_multiply so that AR_convert_str_to_int can check for overflow during conversion.
 *
 * Revision 1.15  1992/12/12  04:43:20  pmk
 * Implement AR_exp, AR_log, and AR_power routines for the evaluation
 * of exponentiation in intrin.c.
 *
 * Revision 1.14  1992/12/03  23:21:24  pmk
 * Add new AR_cabs function for bitwise accuracy with library.
 *
 * Revision 1.13  1992/11/25  21:23:52  krz
 * 1.  Add tests for conversions from integral operands to integral results.
 * 2.  When converting to integral value, return overflow status bit when
 *     original operand value can not be represented in the result type.
 * 3.  Fix integral addition, subtraction, division, and modulus to return
 *     correct status when the result and an operand are the same object.
 *
 * Revision 1.12  1992/11/18  01:26:14  pmk
 * Fix complex absolute value and integer division overflow checking.
 *
 * Revision 1.11  1992/11/06  17:35:20  pmk
 * Break up bitopers.c, and support square root.
 *
 * Revision 1.10  1992/10/24  01:12:38  pmk
 * Compile with optimization enabled.
 *
 * Revision 1.9  1992/10/21  23:35:33  pmk
 * Correct status returns in complex operations.
 *
 * Revision 1.8  1992/10/19  19:52:20  pmk
 * Provide conversions between Cray and IEEE format floating-point numbers.
 *
 * Revision 1.7  1992/10/17  00:56:08  krz
 * Add arith.doc to repository.  Add AR_Int_46_U back to arith.h.  Fixed bugs in AR_convert_str_to_int.
 *
 * Revision 1.6  1992/10/16  23:08:34  krz
 * Finished testing of integral operations.  Changed return status bits from integral operations where necessary.
 *
 * Revision 1.5  1992/10/12  16:19:56  pmk
 * Version 004 - fixes from Kent to bitopers.c and testint.c.
 *
 * Revision 1.4  1992/10/08  01:17:35  pmk
 * Version 003 - a fix to conversion from Kent in bitopers.c.
 *
 * Revision 1.3  1992/09/23  21:55:54  pmk
 *
 * Revision 1.2  1992/09/23  21:53:59  pmk
 * Correct minor problems with packaging. This is version V002. All
 * problems reported with V001 should have been addressed.
 *
 * Revision 1.1  1992/09/22  20:01:10  pmk
 * Set up build procedures.
 *
 */
