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


/* USMID:  "\n@(#)5.0_pl/macros/fold_drive.m	5.3	07/21/99 12:53:36\n" */


/*****************\
|* MISCELLANEOUS *|
\*****************/


# define EQUAL_OP       0
# define NOT_EQUAL      1
# define LESS_THAN      0
# define LESS_EQ        1
# define GREATER_THAN   2
# define GREATER_EQ     3


# if defined(_TARGET64) && defined(_HOST32)
#   define	INT_KIND_2_MAX	0177777
#   define	INT_KIND_4_MAX	037777777777
#   define	INT_KIND_8_MAX	0777777777777777777777LL

# elif defined(_TARGET64)
#   define	INT_KIND_2_MAX	0177777
#   define	INT_KIND_4_MAX	037777777777
#   define	INT_KIND_8_MAX	0777777777777777777777

# else
#   define	INT_KIND_2_MAX	0177777
#   define	INT_KIND_4_MAX	017777777777
#   define	INT_KIND_8_MAX	017777777777
# endif

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
# define        MASK_TEST(MASK)                                 \
                ((MASK & AR_STAT_OVERFLOW)  != 0 ||             \
                 (MASK & AR_STAT_UNDEFINED) != 0)
# else
# define        MASK_TEST(MASK)                                 \
                ((MASK & AR_STAT_OVERFLOW)  != 0 ||             \
                 (MASK & AR_STAT_SEMIVALID) != 0 ||             \
                 (MASK & AR_STAT_UNDEFINED) != 0)
# endif

# if !defined(_USE_FOLD_DOT_f)
# define        ARITH_ERROR_RESULT_TEST(MASK, TYPE_IDX, OK, LINE, COL)         \
                if (MASK_TEST(MASK)) {                                         \
                   if (TYP_TYPE(TYPE_IDX) == Integer ||                        \
                       TYP_TYPE(TYPE_IDX) == Typeless) {                       \
                      if (issue_overflow_msg_719) {			       \
                         PRINTMSG(LINE, 719, Error, COL);                      \
                      } else {                                                 \
                         need_to_issue_719 = TRUE;			       \
                      }                                                        \
                      OK = FALSE;                                              \
                   }                                                           \
                   else if (target_ieee) {                                     \
                      PRINTMSG(LINE, 1184, Warning, COL);                      \
                   } else {                                                    \
                      PRINTMSG(LINE, 720, Error, COL);                         \
                      OK = FALSE;                                              \
                   }                                                           \
                }                                                              \
                else if ((MASK & AR_STAT_INVALID_TYPE) != 0) {                 \
                   PRINTMSG(LINE, 1079, Internal, COL);                        \
                }
# else 
# define        ARITH_ERROR_RESULT_TEST(MASK, TYPE_IDX, OK, LINE, COL)
# endif


/**************************************\
|* CONSTANT FOLDER ENTRY POINTS       *|
\**************************************/

# if defined(_HOST_OS_UNICOS) || defined(_HOST_OS_SOLARIS) ||                  \
      defined(_HOST_OS_MAX)

# define	_CONVRI		CONVRI
# define	_CONVRDI	CONVRDI
# define	_CONVCI		CONVCI
# define	_CONVCDI	CONVCDI
# define	_CONVIR		CONVIR
# define	_CONVRDR	CONVRDR
# define	_CONVCR		CONVCR
# define	_CONVCDR	CONVCDR
# define	_CONVIRD	CONVIRD
# define	_CONVRRD	CONVRRD
# define	_CONVCRD	CONVCRD
# define	_CONVCDRD	CONVCDRD
# define	_CONVIC		CONVIC
# define	_CONVRC		CONVRC
# define	_CONVRDC	CONVRDC
# define	_CONVCDC	CONVCDC
# define	_CONVICD	CONVICD
# define	_CONVRCD	CONVRCD
# define	_CONVRDCD	CONVRDCD
# define	_CONVCCD	CONVCCD
# define	_UMII		UMII
# define	_UMIR		UMIR
# define	_UMIRD		UMIRD
# define	_UMIC		UMIC
# define	_UMICD		UMICD

# define	_ADDII		ADDII
# define	_ADDIR		ADDIR
# define	_ADDIRD		ADDIRD
# define	_ADDIC		ADDIC
# define	_ADDICD		ADDICD
# define	_ADDRR		ADDRR
# define	_ADDRRD		ADDRRD
# define	_ADDRC		ADDRC
# define	_ADDRCD		ADDRCD
# define	_ADDRDRD	ADDRDRD
# define	_ADDRDC		ADDRDC
# define	_ADDRDCD	ADDRDCD
# define	_ADDCC		ADDCC
# define	_ADDCCD		ADDCCD
# define	_ADDCDCD	ADDCDCD

# define	_SUBII		SUBII
# define	_SUBIR		SUBIR
# define	_SUBRI		SUBRI
# define	_SUBIRD		SUBIRD
# define	_SUBRDI		SUBRDI
# define	_SUBIC		SUBIC
# define	_SUBCI		SUBCI
# define	_SUBICD		SUBICD
# define	_SUBCDI		SUBCDI
# define	_SUBRR		SUBRR
# define	_SUBRRD		SUBRRD
# define	_SUBRDR		SUBRDR
# define	_SUBRC		SUBRC
# define	_SUBCR		SUBCR
# define	_SUBRCD		SUBRCD
# define	_SUBCDR		SUBCDR
# define	_SUBRDRD	SUBRDRD
# define	_SUBRDC		SUBRDC
# define	_SUBCRD		SUBCRD
# define	_SUBRDCD	SUBRDCD
# define	_SUBCDRD	SUBCDRD
# define	_SUBCC		SUBCC
# define	_SUBCCD		SUBCCD
# define	_SUBCDC		SUBCDC
# define	_SUBCDCD	SUBCDCD

# define	_MULII		MULII
# define	_MULIR		MULIR
# define	_MULIRD		MULIRD
# define	_MULIC		MULIC
# define	_MULICD		MULICD
# define	_MULRR		MULRR
# define	_MULRRD		MULRRD
# define	_MULRC		MULRC
# define	_MULRCD		MULRCD
# define	_MULRDRD	MULRDRD
# define	_MULRDC		MULRDC
# define	_MULRDCD	MULRDCD
# define	_MULCC		MULCC
# define	_MULCCD		MULCCD
# define	_MULCDCD	MULCDCD

# define	_DIVII		DIVII
# define	_DIVIR		DIVIR
# define	_DIVRI		DIVRI
# define	_DIVIRD		DIVIRD
# define	_DIVRDI		DIVRDI
# define	_DIVIC		DIVIC
# define	_DIVCI		DIVCI
# define	_DIVICD		DIVICD
# define	_DIVCDI		DIVCDI
# define	_DIVRR		DIVRR
# define	_DIVRRD		DIVRRD
# define	_DIVRDR		DIVRDR
# define	_DIVRC		DIVRC
# define	_DIVCR		DIVCR
# define	_DIVRCD		DIVRCD
# define	_DIVCDR		DIVCDR
# define	_DIVRDRD	DIVRDRD
# define	_DIVRDC		DIVRDC
# define	_DIVCRD		DIVCRD
# define	_DIVRDCD	DIVRDCD
# define	_DIVCDRD	DIVCDRD
# define	_DIVCC		DIVCC
# define	_DIVCCD		DIVCCD
# define	_DIVCDC		DIVCDC
# define	_DIVCDCD	DIVCDCD

# define	_EXPII		EXPII
# define	_EXPIR		EXPIR
# define	_EXPRI		EXPRI
# define	_EXPIRD		EXPIRD
# define	_EXPRDI		EXPRDI
# define	_EXPIC		EXPIC
# define	_EXPCI		EXPCI
# define	_EXPICD		EXPICD
# define	_EXPCDI		EXPCDI
# define	_EXPRR		EXPRR
# define	_EXPRRD		EXPRRD
# define	_EXPRDR		EXPRDR
# define	_EXPRC		EXPRC
# define	_EXPCR		EXPCR
# define	_EXPRCD		EXPRCD
# define	_EXPCDR		EXPCDR
# define	_EXPRDRD	EXPRDRD
# define	_EXPRDC		EXPRDC
# define	_EXPCRD		EXPCRD
# define	_EXPRDCD	EXPRDCD
# define	_EXPCDRD	EXPCDRD
# define	_EXPCC		EXPCC
# define	_EXPCCD		EXPCCD
# define	_EXPCDC		EXPCDC
# define	_EXPCDCD	EXPCDCD

# define	_LXII		LXII
# define	_LXIR		LXIR
# define	_LXRR		LXRR
# define	_LXIRD		LXIRD
# define	_LXRRD		LXRRD
# define	_LXRDRD		LXRDRD
# define	_LXRI		LXRI
# define	_LXRDI		LXRDI
# define	_LXRDR		LXRDR
# define	_LXCHCH		LXCHCH

# define	_EQII		EQII
# define	_EQIR		EQIR
# define	_EQRR		EQRR
# define	_EQIRD		EQIRD
# define	_EQRRD		EQRRD
# define	_EQRDRD		EQRDRD
# define	_EQIC		EQIC
# define	_EQRC		EQRC
# define	_EQRDC		EQRDC
# define	_EQCC		EQCC
# define	_EQICD		EQICD
# define	_EQRCD		EQRCD
# define	_EQRDCD		EQRDCD
# define	_EQCCD		EQCCD
# define	_EQCDCD		EQCDCD
# define	_EQCHCH		EQCHCH

# define	ABSCD		ABSCD
# define	ABSC		ABSC
# define	ABSD		ABSD
# define	ABSI		ABSI
# define	ABSR		ABSR
# define	DIMD		DIMD
# define	DIMI		DIMI
# define	DIMR		DIMR
# define	MASKI		MASKI
# define	MODD		MODD
# define	MODI		MODI
# define	MODR		MODR
# define	MODULOD		MODULOD
# define	MODULOI		MODULOI
# define	MODULOR		MODULOR
# define	NINTD		NINTD
# define	NINTR		NINTR
# define	SHIFTI		SHIFTI
# define	SHIFTLI		SHIFTLI
# define	SHIFTRI		SHIFTRI
# define	SIGND		SIGND
# define	SIGNI		SIGNI
# define	SIGNR		SIGNR

# elif (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN))

# define	_CONVRI		convri_
# define	_CONVRDI	convrdi_
# define	_CONVCI		convci_
# define	_CONVCDI	convcdi_
# define	_CONVIR		convir_
# define	_CONVRDR	convrdr_
# define	_CONVCR		convcr_
# define	_CONVCDR	convcdr_
# define	_CONVIRD	convird_
# define	_CONVRRD	convrrd_
# define	_CONVCRD	convcrd_
# define	_CONVCDRD	convcdrd_
# define	_CONVIC		convic_
# define	_CONVRC		convrc_
# define	_CONVRDC	convrdc_
# define	_CONVCDC	convcdc_
# define	_CONVICD	convicd_
# define	_CONVRCD	convrcd_
# define	_CONVRDCD	convrdcd_
# define	_CONVCCD	convccd_
# define	_UMII		umii_
# define	_UMIR		umir_
# define	_UMIRD		umird_
# define	_UMIC		umic_
# define	_UMICD		umicd_

# define	_ADDII		addii_
# define	_ADDIR		addir_
# define	_ADDIRD		addird_
# define	_ADDIC		addic_
# define	_ADDICD		addicd_
# define	_ADDRR		addrr_
# define	_ADDRRD		addrrd_
# define	_ADDRC		addrc_
# define	_ADDRCD		addrcd_
# define	_ADDRDRD	addrdrd_
# define	_ADDRDC		addrdc_
# define	_ADDRDCD	addrdcd_
# define	_ADDCC		addcc_
# define	_ADDCCD		addccd_
# define	_ADDCDCD	addcdcd_

# define	_SUBII		subii_
# define	_SUBIR		subir_
# define	_SUBRI		subri_
# define	_SUBIRD		subird_
# define	_SUBRDI		subrdi_
# define	_SUBIC		subic_
# define	_SUBCI		subci_
# define	_SUBICD		subicd_
# define	_SUBCDI		subcdi_
# define	_SUBRR		subrr_
# define	_SUBRRD		subrrd_
# define	_SUBRDR		subrdr_
# define	_SUBRC		subrc_
# define	_SUBCR		subcr_
# define	_SUBRCD		subrcd_
# define	_SUBCDR		subcdr_
# define	_SUBRDRD	subrdrd_
# define	_SUBRDC		subrdc_
# define	_SUBCRD		subcrd_
# define	_SUBRDCD	subrdcd_
# define	_SUBCDRD	subcdrd_
# define	_SUBCC		subcc_
# define	_SUBCCD		subccd_
# define	_SUBCDC		subcdc_
# define	_SUBCDCD	subcdcd_

# define	_MULII		mulii_
# define	_MULIR		mulir_
# define	_MULIRD		mulird_
# define	_MULIC		mulic_
# define	_MULICD		mulicd_
# define	_MULRR		mulrr_
# define	_MULRRD		mulrrd_
# define	_MULRC		mulrc_
# define	_MULRCD		mulrcd_
# define	_MULRDRD	mulrdrd_
# define	_MULRDC		mulrdc_
# define	_MULRDCD	mulrdcd_
# define	_MULCC		mulcc_
# define	_MULCCD		mulccd_
# define	_MULCDCD	mulcdcd_

# define	_DIVII		divii_
# define	_DIVIR		divir_
# define	_DIVRI		divri_
# define	_DIVIRD		divird_
# define	_DIVRDI		divrdi_
# define	_DIVIC		divic_
# define	_DIVCI		divci_
# define	_DIVICD		divicd_
# define	_DIVCDI		divcdi_
# define	_DIVRR		divrr_
# define	_DIVRRD		divrrd_
# define	_DIVRDR		divrdr_
# define	_DIVRC		divrc_
# define	_DIVCR		divcr_
# define	_DIVRCD		divrcd_
# define	_DIVCDR		divcdr_
# define	_DIVRDRD	divrdrd_
# define	_DIVRDC		divrdc_
# define	_DIVCRD		divcrd_
# define	_DIVRDCD	divrdcd_
# define	_DIVCDRD	divcdrd_
# define	_DIVCC		divcc_
# define	_DIVCCD		divccd_
# define	_DIVCDC		divcdc_
# define	_DIVCDCD	divcdcd_

# define	_EXPII		expii_
# define	_EXPIR		expir_
# define	_EXPRI		expri_
# define	_EXPIRD		expird_
# define	_EXPRDI		exprdi_
# define	_EXPIC		expic_
# define	_EXPCI		expci_
# define	_EXPICD		expicd_
# define	_EXPCDI		expcdi_
# define	_EXPRR		exprr_
# define	_EXPRRD		exprrd_
# define	_EXPRDR		exprdr_
# define	_EXPRC		exprc_
# define	_EXPCR		expcr_
# define	_EXPRCD		exprcd_
# define	_EXPCDR		expcdr_
# define	_EXPRDRD	exprdrd_
# define	_EXPRDC		exprdc_
# define	_EXPCRD		expcrd_
# define	_EXPRDCD	exprdcd_
# define	_EXPCDRD	expcdrd_
# define	_EXPCC		expcc_
# define	_EXPCCD		expccd_
# define	_EXPCDC		expcdc_
# define	_EXPCDCD	expcdcd_

# define	_LXII		lxii_
# define	_LXIR		lxir_
# define	_LXRR		lxrr_
# define	_LXIRD		lxird_
# define	_LXRRD		lxrrd_
# define	_LXRDRD		lxrdrd_
# define	_LXRI		lxri_
# define	_LXRDI		lxrdi_
# define	_LXRDR		lxrdr_
# define	_LXCHCH		lxchch_

# define	_EQII		eqii_
# define	_EQIR		eqir_
# define	_EQRR		eqrr_
# define	_EQIRD		eqird_
# define	_EQRRD		eqrrd_
# define	_EQRDRD		eqrdrd_
# define	_EQIC		eqic_
# define	_EQRC		eqrc_
# define	_EQRDC		eqrdc_
# define	_EQCC		eqcc_
# define	_EQICD		eqicd_
# define	_EQRCD		eqrcd_
# define	_EQRDCD		eqrdcd_
# define	_EQCCD		eqccd_
# define	_EQCDCD		eqcdcd_
# define	_EQCHCH		eqchch_

# define	ABSCD		abscd_
# define	ABSC		absc_
# define	ABSD		absd_
# define	ABSI		absi_
# define	ABSR		absr_
# define	DIMD		dimd_
# define	DIMI		dimi_
# define	DIMR		dimr_
# define	MASKI		maski_
# define	MODD		modd_
# define	MODI		modi_
# define	MODR		modr_
# define	MODULOD		modulod_
# define	MODULOI		moduloi_
# define	MODULOR		modulor_
# define	NINTD		nintd_
# define	NINTR		nintr_
# define	SHIFTI		shifti_
# define	SHIFTLI		shiftli_
# define	SHIFTRI		shiftri_
# define	SIGND		signd_
# define	SIGNI		signi_
# define	SIGNR		signr_

# else

# define	_CONVRI         CONVRI_
# define	_CONVRDI        CONVRDI_
# define	_CONVCI         CONVCI_
# define	_CONVCDI        CONVCDI_
# define	_CONVIR         CONVIR_
# define	_CONVRDR        CONVRDR_
# define	_CONVCR         CONVCR_
# define	_CONVCDR        CONVCDR_
# define	_CONVIRD        CONVIRD_
# define	_CONVRRD        CONVRRD_
# define	_CONVCRD        CONVCRD_
# define	_CONVCDRD       CONVCDRD_
# define	_CONVIC         CONVIC_
# define	_CONVRC         CONVRC_
# define	_CONVRDC        CONVRDC_
# define	_CONVCDC        CONVCDC_
# define	_CONVICD        CONVICD_
# define	_CONVRCD        CONVRCD_
# define	_CONVRDCD       CONVRDCD_
# define	_CONVCCD        CONVCCD_
# define	_UMII           UMII_
# define	_UMIR           UMIR_
# define	_UMIRD          UMIRD_
# define	_UMIC           UMIC_
# define	_UMICD          UMICD_

# define	_ADDII          ADDII_
# define	_ADDIR          ADDIR_
# define	_ADDIRD         ADDIRD_
# define	_ADDIC          ADDIC_
# define	_ADDICD         ADDICD_
# define	_ADDRR          ADDRR_
# define	_ADDRRD         ADDRRD_
# define	_ADDRC          ADDRC_
# define	_ADDRCD         ADDRCD_
# define	_ADDRDRD        ADDRDRD_
# define	_ADDRDC         ADDRDC_
# define	_ADDRDCD        ADDRDCD_
# define	_ADDCC          ADDCC_
# define	_ADDCCD         ADDCCD_
# define	_ADDCDCD        ADDCDCD_


# define	_SUBII          SUBII_
# define	_SUBIR          SUBIR_
# define	_SUBRI          SUBRI_
# define	_SUBIRD         SUBIRD_
# define	_SUBRDI         SUBRDI_
# define	_SUBIC          SUBIC_
# define	_SUBCI          SUBCI_
# define	_SUBICD         SUBICD_
# define	_SUBCDI         SUBCDI_
# define	_SUBRR          SUBRR_
# define	_SUBRRD         SUBRRD_
# define	_SUBRDR         SUBRDR_
# define	_SUBRC          SUBRC_
# define	_SUBCR          SUBCR_
# define	_SUBRCD         SUBRCD_
# define	_SUBCDR         SUBCDR_
# define	_SUBRDRD        SUBRDRD_
# define	_SUBRDC         SUBRDC_
# define	_SUBCRD         SUBCRD_
# define	_SUBRDCD        SUBRDCD_
# define	_SUBCDRD        SUBCDRD_
# define	_SUBCC          SUBCC_
# define	_SUBCCD         SUBCCD_
# define	_SUBCDC         SUBCDC_
# define	_SUBCDCD        SUBCDCD_


# define	_MULII          MULII_
# define	_MULIR          MULIR_
# define	_MULIRD         MULIRD_
# define	_MULIC          MULIC_
# define	_MULICD         MULICD_
# define	_MULRR          MULRR_
# define	_MULRRD         MULRRD_
# define	_MULRC          MULRC_
# define	_MULRCD         MULRCD_
# define	_MULRDRD        MULRDRD_
# define	_MULRDC         MULRDC_
# define	_MULRDCD        MULRDCD_
# define	_MULCC          MULCC_
# define	_MULCCD         MULCCD_
# define	_MULCDCD        MULCDCD_


# define	_DIVII          DIVII_
# define	_DIVIR          DIVIR_
# define	_DIVRI          DIVRI_
# define	_DIVIRD         DIVIRD_
# define	_DIVRDI         DIVRDI_
# define	_DIVIC          DIVIC_
# define	_DIVCI          DIVCI_
# define	_DIVICD         DIVICD_
# define	_DIVCDI         DIVCDI_
# define	_DIVRR          DIVRR_
# define	_DIVRRD         DIVRRD_
# define	_DIVRDR         DIVRDR_
# define	_DIVRC          DIVRC_
# define	_DIVCR          DIVCR_
# define	_DIVRCD         DIVRCD_
# define	_DIVCDR         DIVCDR_
# define	_DIVRDRD        DIVRDRD_
# define	_DIVRDC         DIVRDC_
# define	_DIVCRD         DIVCRD_
# define	_DIVRDCD        DIVRDCD_
# define	_DIVCDRD        DIVCDRD_
# define	_DIVCC          DIVCC_
# define	_DIVCCD         DIVCCD_
# define	_DIVCDC         DIVCDC_
# define	_DIVCDCD        DIVCDCD_


# define	_EXPII          EXPII_
# define	_EXPIR          EXPIR_
# define	_EXPRI          EXPRI_
# define	_EXPIRD         EXPIRD_
# define	_EXPRDI         EXPRDI_
# define	_EXPIC          EXPIC_
# define	_EXPCI          EXPCI_
# define	_EXPICD         EXPICD_
# define	_EXPCDI         EXPCDI_
# define	_EXPRR          EXPRR_
# define	_EXPRRD         EXPRRD_
# define	_EXPRDR         EXPRDR_
# define	_EXPRC          EXPRC_
# define	_EXPCR          EXPCR_
# define	_EXPRCD         EXPRCD_
# define	_EXPCDR         EXPCDR_
# define	_EXPRDRD        EXPRDRD_
# define	_EXPRDC         EXPRDC_
# define	_EXPCRD         EXPCRD_
# define	_EXPRDCD        EXPRDCD_
# define	_EXPCDRD        EXPCDRD_
# define	_EXPCC          EXPCC_
# define	_EXPCCD         EXPCCD_
# define	_EXPCDC         EXPCDC_
# define	_EXPCDCD	EXPCDCD_

# define        _LXII           LXII_
# define        _LXIR           LXIR_
# define        _LXRR           LXRR_
# define        _LXIRD          LXIRD_
# define        _LXRRD          LXRRD_
# define        _LXRDRD         LXRDRD_
# define        _LXRI           LXRI_
# define        _LXRDI          LXRDI_
# define        _LXRDR          LXRDR_
# define        _LXCHCH         LXCHCH_

# define        _EQII           EQII_
# define        _EQIR           EQIR_
# define        _EQRR           EQRR_
# define        _EQIRD          EQIRD_
# define        _EQRRD          EQRRD_
# define        _EQRDRD         EQRDRD_
# define        _EQIC           EQIC_
# define        _EQRC           EQRC_
# define        _EQRDC          EQRDC_
# define        _EQCC           EQCC_
# define        _EQICD          EQICD_
# define        _EQRCD          EQRCD_
# define        _EQRDCD         EQRDCD_
# define        _EQCCD          EQCCD_
# define        _EQCDCD         EQCDCD_
# define        _EQCHCH         EQCHCH_

# define	ABSCD		ABSCD_
# define	ABSC		ABSC_
# define	ABSD		ABSD_
# define	ABSI		ABSI_
# define	ABSR		ABSR_
# define	DIMD		DIMD_
# define	DIMI		DIMI_
# define	DIMR		DIMR_
# define	MASKI		MASKI_
# define	MODD		MODD_
# define	MODI		MODI_
# define	MODR		MODR_
# define	MODULOD		MODULOD_
# define	MODULOI		MODULOI_
# define	MODULOR		MODULOR_
# define	NINTD		NINTD_
# define	NINTR		NINTR_
# define	SHIFTI		SHIFTI_
# define	SHIFTLI		SHIFTLI_
# define	SHIFTRI		SHIFTRI_
# define	SIGND		SIGND_
# define	SIGNI		SIGNI_
# define	SIGNR		SIGNR_

# endif

