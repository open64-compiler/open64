/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


/* USMID @(#) libu/util/targlib.h	92.2	11/24/99 14:11:55 */



#ifdef _UNICOS
  typedef long LL_int;
#else
  typedef long long LL_int;
#endif

#ifdef _UNICOS
  typedef unsigned long int ULL_int;
#else /* Sun */
  typedef unsigned long long int ULL_int;
#endif

/*      primary machine types:      */
/*	should match order in array pmt[] */
#define PM_HOST		0
#define PM_TARGET	1
#define PM_IOP		2
#define PM_CRAY_YMP	3
#define PM_CRAY_C90	4
#define PM_CRAY_T3D	5
#define PM_CRAY_EL	6
#define PM_CRAY_TS	7
#define PM_CRAY_T3E	8
#define PM_CRAY_J90	9
#define PM_CRAY_JSE	10
#define PM_CRAY_SV1	11
#define PM_CRAY_SV2	12

#ifndef _UNICOS
#define PM_SPARC	13
#endif

#define PM_STARHOST	(sizeof(pmt)/sizeof(struct pmttab) -2)
#define PM_STARTARGET	(sizeof(pmt)/sizeof(struct pmttab) -1)

#define PMTYPES		PM_STARHOST	/* Number of primary machine types (not 
				    counting *host and *target) */

#define	BLANK	((int) ' ')



/* Values for pmttab.mtype */
/* This is an internal value - it is used to determine */
/* whether a particular characteristic is valid on a machine type */
#define MPPTYPE      04		/* mpp */
#define IOPTYPE     010		/* iop */
#define YMPTYPE     020		/* cray-ymp */
#define C90TYPE     040		/* cray-c90 */
#define TSTYPE     0100		/* Cray-ts */
#define UNKNOWNTYP 0200		/* Unknown */
#define SPARCTYPE  0400		/* Sparc */
#define SV1TYPE   01000		/* cray-sv1 (primary type) */
#define SV2TYPE   02000		/* cray-sv2 */

/*         machine names:           */
struct pmttab {
	char *name;	/* machine name */
	long mtype;	/* machine type */
};

#ifdef _UNICOS

#ifdef	_CRAY1
#define HOSTTYPE UNKNOWNTYP
#endif

#ifdef	_CRAYMPP
#define HOSTTYPE MPPTYPE
#endif

#ifndef	HOSTTYPE
#error UNKNOWN TARGET MACHINE
#endif

#else /* Sun */
#define HOSTTYPE SPARCTYPE
#endif

/*   ASCII machine names for mctable  */
#define MAKE_NAME(C1, C2, C3, C4, C5, C6, C7, C8)	    \
	(((ULL_int)C1)<<56 | ((ULL_int)C2)<<48 | 	    \
	 ((ULL_int)C3)<<40 | ((ULL_int)C4)<<32 | 	    \
	 ((ULL_int)C5)<<24 | ((ULL_int)C6)<<16 | 	    \
	 ((ULL_int)C7)<<8  | ((ULL_int)C8))

#define HOST_NAME	MAKE_NAME('H','O','S','T','\0','\0','\0','\0')
#define TARGET_NAME     MAKE_NAME('T','A','R','G','E','T','\0','\0')
#define CRAY_YMP_NAME   MAKE_NAME('C','R','A','Y','-','Y','M','P')
#define CRAY_C90_NAME   MAKE_NAME('C','R','A','Y','-','C','9','0')
#define CRAY_T3D_NAME   MAKE_NAME('C','R','A','Y','-','T','3','D')
#define CRAY_TS_NAME    MAKE_NAME('C','R','A','Y','-','T','S','\0')
#define CRAY_T3E_NAME   MAKE_NAME('C','R','A','Y','-','T','3','E')
#define CRAY_SV1_NAME   MAKE_NAME('C','R','A','Y','-','S','V','1')
#define CRAY_SV2_NAME   MAKE_NAME('C','R','A','Y','-','S','V','2')
#define IOP_NAME	MAKE_NAME('I','O','P','\0','\0','\0','\0','\0')
#define SPARC_NAME	MAKE_NAME('S','P','A','R','C','\0','\0','\0')
#define CRAY_NAME	MAKE_NAME('C','R','A','Y','\0','\0','\0','\0')
#define CRAY_EL_SUBTYPE	MAKE_NAME('C','R','A','Y','-','E','L','\0')
#define CRAY_J90_SUBTYPE MAKE_NAME('C','R','A','Y','-','J','9','0')
#define CRAY_JSE_SUBTYPE MAKE_NAME('C','R','A','Y','-','J','S','E')
#define CRAY_SV1_SUBTYPE MAKE_NAME('C','R','A','Y','-','S','V','1')
#define CRAY_TSIEEE_SUBTYPE0 MAKE_NAME('C','R','A','Y','-','T','S','-')
#define CRAY_TSIEEE_SUBTYPE1 MAKE_NAME('I','E','E','E','\0','\0','\0','\0')

/*        machine characteristics:    */
/*	  should match mcoffs      */
#define PRIMARY         0
#define BANKS		1
#define NUMCPUS		2
#define IBUFSIZE	3
#define MEMSIZE		4
#define MEMSPEED	5
#define CLOCKTIM	6
#define NUMCLSTR	7
#define BANKBUSY	8
#define SUBTYPE         9
#define EMA		10
#define NOEMA		11
#define CIGS		12
#define NOCIGS		13
#define VPOP		14
#define NOVPOP		15
#define PC		16
#define NOPC		17
#define READVL		18
#define NOREADVL	19
#define VRECUR		20
#define NOVRECUR	21
#define AVL		22
#define NOAVL		23
#define HPM		24
#define NOHPM		25
#define BDM		26
#define NOBDM		27
#define STATRG		28
#define NOSTATRG	29
#define CORI		30
#define NOCORI		31
#define ADDR32		32
#define NOADDR32	33
#define XEA		34
#define NOXEA		35
#define BMM		36
#define NOBMM		37
#define AVPOP         	38
#define NOAVPOP         39
#define FULLSECT        40	/* Deprecated */
#define NOFULLSECT      41	/* Deprecated */
#define IEEE            42
#define NOIEEE          43
#define CMRREQ          44	/* Deprecated */
#define NOCMRREQ        45	/* Deprecated */
#define CACHE           46	/* Deprecated */
#define NOCACHE         47	/* Deprecated */
#define NUMMCHARS       44	/* Number of machine characteristics. Do not */
							/* count the deprecated characteristics that. */
							/* follow the last usable characteristic. */

#define T	((ULL_int) -1)
#define F	 0
struct mctable{              /* 128-word machine characteristics table */
	/* numeric machine characteristics */
	 ULL_int mcpmt;
	 ULL_int mcbank;
	 ULL_int mcncpu;
	 ULL_int mcibsz;
	 ULL_int mcmsz;
	 ULL_int mcmspd;
	 ULL_int mcclk;
	 ULL_int mcncl;
	 ULL_int mcbbsy;
         ULL_int mcclktck;
         ULL_int mcserial;
         ULL_int mcrls;
         ULL_int mc_c_option_rev;
         ULL_int mc_i_option_rev;
         ULL_int mc_r_option_rev;
         ULL_int mc_m_option_rev_0;
         ULL_int mc_m_option_rev_1;
         ULL_int mc_m_option_rev_2;
         ULL_int mc_m_option_rev_3;
	 ULL_int mcsubt[2];
	 ULL_int numeric_unused[43];
	/* logical machine characteristics */
	union {
		struct {
			 ULL_int mcema;
			 ULL_int mccigs;
			 ULL_int mcvpop;
			 ULL_int mcpc;
			 ULL_int mcrdvl;
			 ULL_int mcvrcr;
			 ULL_int mcavl;
			 ULL_int mchpm;
			 ULL_int mcbdm;
			 ULL_int mcstr;
			 ULL_int mccori;
			 ULL_int mcaddr32;
			 ULL_int mcxea;
			 ULL_int mcbmm;
			 ULL_int mcavpop;
			 ULL_int mcfullsect;	/* Deprecated. Do not reuse, as this */
			                    	/* may be set for Cray-ts by some older */
			                    	/* compilers. */
			 ULL_int mcieee;
			 ULL_int mccmrreq;		/* Deprecated. Do not reuse. */
			 ULL_int mccache;		/* Deprecated. Do not reuse. */
			 ULL_int logical_unused[45];
		} mctblxy;
	} mctlog;
};
#define MC500K 		524288
#define MC1MEG 		1048576
#define MC2MEG 		MC1MEG*2
#define MC4MEG 		MC1MEG*4
#define MC8MEG 		MC1MEG*8
#define MC16MEG 	MC1MEG*16
#define MC32MEG 	MC1MEG*32
#define MC64MEG 	MC1MEG*64
#define MC256MEG 	MC1MEG*256
#define MC1024MEG 	MC1MEG*1024

struct pdttable{
#ifdef _UNICOS
	unsigned	   : 48 ;   /* unused-reserved by CRI       */
#else
	unsigned           : 32 ;   /* unused-reserved by CRI       */
	unsigned           : 16 ;   /* unused-reserved by CRI       */
#endif
	unsigned length    : 16 ;   /* word size of machine type    */
	ULL_int machine        ;   /* basic machine type-ASCII     */
	union{
	    struct {
		  unsigned unused_1  : 15 ;   /* unused "and" bits - reserved */
					      /* by CRI */
		  unsigned bdmsafe   :  1 ;   /* bidirectional memory "safe"  */
					      /* Settable only by compilers   */
		  unsigned unused_2  : 16 ;   /* unused-reserved by CRI       */
		  unsigned unused_3  : 12 ;   /* unused-reserved by CRI       */
		  unsigned cache     :  1 ;   /* cache. Deprecated		      */
		  unsigned cmrreq    :  1 ;   /* CMR required. Deprecated      */
		  unsigned ieee	     :  1 ;   /* IEEE arithmetic	      */
		  unsigned fullsect  :  1 ;   /* full memory section. Deprecated     */
		  unsigned avpop     :  1 ;   /* additional vector pop count  */
		  unsigned bmm       :  1 ;   /* bit matrix multiply unit     */
		  unsigned addr32    :  1 ;   /* YMP 32-bit addressing require*/
		  unsigned cori      :  1 ;   /* control operand range intrpts*/
		  unsigned clsreq    :  1 ;   /* cluster registers required   */
		  unsigned str       :  1 ;   /* status register required     */
	     	  unsigned bdm       :  1 ;   /* bidirectional memory flag    */
		  unsigned hpm       :  1 ;   /* hardware performance flag    */
		  unsigned avl       :  1 ;   /* additional vector unit       */
		  unsigned nvrcr     :  1 ;   /* no vector recursion          */
		  unsigned vrcr      :  1 ;   /* vector recursion             */
		  unsigned rdvl      :  1 ;   /* can read vector length       */
		  unsigned pc        :  1 ;   /* programmable clock flag      */
		  unsigned cigs      :  1 ;   /* compress/index gather/scatter*/
		  unsigned ema       :  1 ;   /* extended addressing required */
		  unsigned vpop      :  1 ;   /* vector pop count required    */
	    } pdtxy;
	    struct {
		  unsigned andmask   : 16 ;
		  unsigned unused_1  : 16 ;   /* unused-reserved by CRI       */
		  unsigned mask	     : 32 ;
	    } pdtmask;
	} word2;
};
/* PDTORBITS indicates which bits in pdttable.pdtxy can be or'ed together */
/* in VALTMC */
#define PDTORBITS 03377637

struct mcoffs {
	int wrd;	/* word in pdt containing this field	*/
	int strt;	/* starting bit position of field in PDT*/
	int len;	/* length in bits in pdt		*/
	int indx;	/* offset to word in machine char. table*/
	int type;	/* logical or numeric (1 = logical)	*/
	int def;	/* default value			*/
	long mach;	/* bit mask defining where this is a valid char. */
	long canset;	/* bit mask defining where this can be set */
	char *mcname;	/* name of characteristic */
};

#define C1TYPE  (C90TYPE | YMPTYPE | SV1TYPE |  TSTYPE )
#define C90_XY_TYPE  (C90TYPE | YMPTYPE | SV1TYPE )
#define ALLTYPE (C1TYPE | MPPTYPE | IOPTYPE | SPARCTYPE | SV2TYPE)

void _setsubtype(int ipm, struct mctable *mctable);
void __setsv1type(struct mctable *mctable);
void _setsubtype2(int ipm, struct mctable *mctable);
