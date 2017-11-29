/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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


/* ====================================================================
 * ====================================================================
 *
 * Module: cwh_io.c
 * $Revision: 1.6 $
 * $Date: 05/11/10 13:51:51-08:00 $
 * $Author: scorrell@soapstone.internal.keyresearch.com $
 * $Source: crayf90/sgi/SCCS/s.cwh_io.cxx $
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description: Contains routines to convert IO statements. Entry points from
		the PDGCS layer are:
		fei_*_read, fei_*_write, fei_control_list, fei_IO_list,
		fei_implied_do and fei_iolength.
 *
 * ====================================================================
 * ====================================================================
 */

static const char *source_file = __FILE__;

#ifdef _KEEP_RCS_ID
  static char *rcs_id = " $Id: cwh_io.cxx 1.6 05/11/10 13:51:51-08:00 scorrell@soapstone.internal.keyresearch.com $ ";
#endif /* _KEEP_RCS_ID */

/* sgi includes */

#include "defs.h"
#include "glob.h"
#include "stab.h"
#include "strtab.h"
#include "errors.h"
#include "config.h"
#include "config_targ.h"
#include "wn.h"
#include "const.h"
#include "wio.h"
#include "ir_reader.h"
#include "wn_util.h"
#include "targ_const.h"
#include "targ_sim.h"

/* Cray includes */

#include "i_cvrt.h"


/* conversion includes */

#include "cwh_defines.h"
#include "cwh_addr.h"
#include "cwh_dope.h"
#include "cwh_stk.h"
#include "cwh_types.h"
#include "cwh_expr.h"
#include "cwh_block.h"
#include "sgi_cmd_line.h"
#include "cwh_preg.h"
#include "cwh_stab.h"
#include "cwh_auxst.h"

#include "cwh_io.i"
#include "cwh_io.h"

#define STACK_PUSH(x) \
if (x != NULL) \
  cwh_stk_push(x, WN_item);

#define IoItem_TY(wn) (WN_kid(wn,3))
#ifndef NIL
#define NIL 0
#endif

static INT32 num_list_items_last_processed;

typedef enum {
  CILIST_EDFLAG = 0,
  CILIST_EEEFLAG = 1,
  CILIST_FLFLAG = 2,
  CILIST_UNIT = 3,
  CILIST_IOSTAT = 4,
  CILIST_REC = 5,
  CILIST_PARSFMT = 6,
  CILIST_FMTSRC = 7,
  CILIST_ADVANCE = 8,
  CILIST_SIZE = 9,
  CILIST_ERR = 10,
  CILIST_END = 11,
  CILIST_EOR = 12
} CILIST_TABLE_ITEM;

typedef enum {
  OPEN_CALLNAME = 0,
  OPEN_VERSION = 1,
  OPEN_UNIT = 2,
  OPEN_IOSTAT = 3,
  OPEN_ERRFLAG = 4,
  OPEN_FILE = 5,
  OPEN_STATUS = 6,
  OPEN_ACCESS = 7,
  OPEN_FORM = 8,
  OPEN_RECL = 9,
  OPEN_BLANK = 10,
  OPEN_POSITION = 11,
  OPEN_ACTION = 12,
  OPEN_DELIM = 13,
  OPEN_PAD = 14,
  OPEN_ERR = 15
} OPEN_TABLE_ITEM;

typedef enum {
  CLOSE_CALLNAME = 0,
  CLOSE_VERSION = 1,
  CLOSE_UNIT = 2,
  CLOSE_IOSTAT = 3,
  CLOSE_ERRFLAG = 4,
  CLOSE_STATUS = 5,
  CLOSE_ERR = 6
} CLOSE_TABLE_ITEM;

typedef enum {
  INQ_CALLNAME = 0,
  INQ_VERSION = 1,
  INQ_UNIT = 2,
  INQ_FILE = 3,
  INQ_IOSTAT = 4,
  INQ_ERRFLAG = 5,
  INQ_EXIST = 6,
  INQ_OPENED = 7,
  INQ_NUMBER = 8,
  INQ_NAMED = 9,
  INQ_NAME = 10,
  INQ_ACCESS = 11,
  INQ_SEQUENTIAL = 12,
  INQ_DIRECT = 13,
  INQ_FORM = 14,
  INQ_FORMATTED = 15,
  INQ_UNFORMATTED = 16,
  INQ_RECL =17,
  INQ_NEXTREC = 18,
  INQ_BLANK = 19,
  INQ_POSITION = 20,
  INQ_ACTION = 21,
  INQ_READ = 22,
  INQ_WRITE = 23,
  INQ_READWRITE = 24,
  INQ_DELIM = 25,
  INQ_PAD = 26,
  INQ_ERR = 27
} INQ_TABLE_ITEM;

typedef enum {
  BIO_CALLNAME = 0,
  BIO_VERSION = 1,
  BIO_UNIT = 2,
  BIO_RECMODE = 3,
  BIO_BLOC = 4,
  BIO_ELOC = 5,
  BIO_TIPTR = 6
} BIO_TABLE_ITEM;

/* Rewind, backspace and endfile do not require the io descriptor setup
   and use the following enum */

typedef enum {
  NODESC_CALLNAME = 0,
  NODESC_UNIT = 1,
  NODESC_IOSTAT = 2,
  NODESC_ERRFLAG = 3,
  NODESC_ERR = 4
} NODESC_TABLE_ITEM;

#define WRITE_STMT 0
#define READ_STMT 1
#define NML_MASK 2
#define READ_WRITE_MASK 1
#define NAMELIST_MODE(x) ((x) & NML_MASK)
#define READ_MODE(x) ((x) & READ_WRITE_MASK)
#define WRITE_MODE(x) (!((x) & READ_WRITE_MASK))

static WN * cwh_io_ioitem(int mode, WN *craytype);
static WN * cwh_io_str_ioitem(IOITEM it, int mode, WN *craytype);
static WN * cwh_io_char_ioitem(IOITEM it, WN *len, int mode, WN *craytype);   

static INT32 eeeflag;
static MARKED_SET *marked_set;

static TY_IDX  
cwh_io_scalar_type(WN *wn) {
  TY_IDX ty = NIL;

  if (wn) {
    ty = cwh_types_WN_TY(wn,FALSE);
    ty = cwh_types_array_TY(ty);
    ty = cwh_types_scalar_TY(ty);
  } 
  return ty;
}

/*===============================================
 *
 * fei_control_list
 *
 * Create IO item nodes from the list items already pushed on the
 * stack. 
 * Ten list items are assumed to have already been pushed onto the stack
 * with #10 at the top of the stack.
 *   1. Encode/Decode Flag
 *   2. eeeflag value
 *         1 if ERR=specified
 *         2 if END=specified
 *         4 if EOR=specified
 *         8 if IOSTAT=specified
 *   3. flflag value -- indicates information on first/last flag
 *   4. Unit Specifier
 *   5. IOSTAT variable
 *   6. REC variable
 *   7. Preparsed format variable
 *   8. format source variable
 *   9. ADVANCE variable
 *  10. SIZE variable
 *===============================================
 */

extern void 
fei_control_list(int mode)
{
  INT item;
  WN *wn_eeeflag = NULL;
  WN *wn_unit = NULL;
  WN *wn_iostat = NULL;
  WN *wn_rec = NULL;
  WN *wn_parsfmt = NULL;
  WN *wn_fmtsrc = NULL;
  WN *wn_advance = NULL;
  WN *wn_size = NULL;
  WN *wn_flflag = NULL;
  WN *wn_edflag = NULL;
  WN *wn_end = NULL;
  WN *wn_err = NULL;
  WN *wn_eor = NULL;
  WN *wn1;
  WN *wn2;
  WN *unit_address = NULL;
  WN *ed_unit = NULL;  /* unit for encode decode */
  ST *st;
  TY_IDX ts = NIL;
  TY_IDX td = NIL;
  TY_IDX ty = NIL;
  WN *se;
  INT32 edflag = 0;

  eeeflag = 0;
  for (item=CILIST_EOR; item >= CILIST_EDFLAG ; item--) {

      switch (item) {

        case CILIST_SIZE:
	  ts = cwh_stk_get_TY();
	  wn1 = cwh_expr_address(f_NONE);

          if (wn1) { 
	    if (ts != NIL) {
	       ty = cwh_types_array_TY(ts);
	       ty = cwh_types_scalar_TY(ty);
            } else {
	       ty = cwh_io_scalar_type(wn1);
            }
            wn_size = WN_CreateIoItem1 ( IOC_SIZE, wn1, ty );   
          }
	  break;

        case CILIST_ADVANCE:
	  if (cwh_stk_get_class() == STR_item) {
	     cwh_stk_pop_STR();
	     wn2 = cwh_expr_operand(DELETE_ARRAYEXP_WN);
	     wn1 = cwh_expr_address(f_NONE);
	     wn_advance  = WN_CreateIoItem2 (IOC_ADVANCE, wn1, wn2, NIL);
          } else {
	     /* Assume there was a null wn for a place holder; just pop it */
	     wn1 = cwh_expr_operand(DELETE_ARRAYEXP_WN);
          }
	  break;

        case CILIST_FMTSRC:
	   if (target_io_library == IOLIB_MIPS) {
	     wn1 = cwh_expr_address(f_NONE);
	     if (wn1 == NULL)
	       wn_fmtsrc = WN_CreateIoItem0 ( IOF_LIST_DIRECTED, NIL );
             else
	       DevWarn(("Only List directed I/O supported now"));
           } else { /* Library = CRAY */
	     switch(cwh_stk_get_class()) {

             case STR_item :

                cwh_stk_pop_STR();
                wn2 = cwh_expr_operand(DELETE_ARRAYEXP_WN);
		ts  = cwh_stk_get_TY();
		wn1 = cwh_expr_address(f_NONE);
		se = cwh_addr_find_section(wn1, p_RETURN_SECTION);
		if (se) {
		   ty = cwh_types_array_TY(ts);
		   wn1 = cwh_dope_from_expression(wn1, NULL, wn2, ts, NULL );
		   ty  = cwh_types_scalar_TY(ty);
  		   wn_fmtsrc = WN_CreateIoItem2 (IOF_CR_FMTSRC_DOPE, wn1, 
		                 cwh_addr_find_address(se), ty);
                } else {
		  wn_fmtsrc = WN_CreateIoItem2 (IOF_CR_FMTSRC, wn1, wn2, NIL);
                }
                break ;

	     case ST_item :
	     case ST_item_whole_array:

		if (NAMELIST_MODE(mode)) {

		   ST *namelist_group;
		   ITEM* element;
		   NLIST *dummy, *nmlist;
		   INT32 count, i;

		   element = NULL;
		   namelist_group = cwh_stk_pop_ST();
		   wn1 = cwh_addr_address_ST(namelist_group, 0);
		   dummy = (NLIST *) malloc(sizeof (NLIST)) ;
		   Nlist_wn(dummy) = wn1;
		   Nlist_next(dummy) = NULL;
		   nmlist = dummy;
		   count = 1;
		   while ((element = cwh_auxst_next_element(
			    namelist_group,element,l_NAMELIST)) != NULL ) {
                      wn1 = cwh_addr_address_ST(I_element(element), 0 );  
		      dummy = (NLIST *) malloc(sizeof (NLIST)) ;
		      Nlist_wn(dummy) = wn1;
		      Nlist_next(dummy) = nmlist;
		      nmlist = dummy;
		      count++;
                   }
                   wn_fmtsrc = WN_CreateIoItemN(IOF_NAMELIST_DIRECTED, 
							count, NIL);
		   for(i=0; i<count; i++) {
		      WN_kid(wn_fmtsrc, i) = Nlist_wn(nmlist);
		      nmlist = Nlist_next(nmlist);
                   }
		   free (dummy);

               } else {

		  st = cwh_stk_pop_ST();
		  cwh_stk_push(st, ST_item);
		  ty = ST_type(st);

		  /* For ASSIGN 0012 TO I
                         WRITE (*,I) 
                     Cray FE puts the address of the format in I, and we 
                     need to use cwh_expr_operand to load I to get to the
                     address of the format. 
                     For write(*,100) i
                     100 format(I4)
                     the Cray FE provides a TEMP of KIND_ARRAY that holds the
                     format. For this we need to use cwh_expr_address.
                  */

                  if (TY_kind(ty) == KIND_ARRAY)
                    wn1 = cwh_expr_address(f_NONE);
                  else
                    wn1 = cwh_expr_operand(DELETE_ARRAYEXP_WN);
                  wn_fmtsrc = WN_CreateIoItem1 ( IOF_CR_FMTSRC, wn1, NIL );
                }
		break;

             default:
		 if (cwh_stk_get_class() == FLD_item) {
		    td = cwh_stk_get_FLD_TY();
                 }

		 wn1 = cwh_expr_address(f_NONE);
		 se = cwh_addr_find_section(wn1, p_RETURN_SECTION);
		 if (se) {
		     if (td != NIL) {
			ty = cwh_types_array_TY(td);
			ty = cwh_types_scalar_TY(ty);
                     } else {
		        ts = cwh_types_WN_TY(se,FALSE);
                        ts = cwh_types_array_TY(ts);
                        ty = cwh_types_scalar_TY(ts);
                     }
		     wn1 = cwh_dope_from_expression(wn1, NULL, NULL, ty,
		     NULL);
  		     wn_fmtsrc = WN_CreateIoItem2 (IOF_CR_FMTSRC_DOPE, wn1, 
				     cwh_addr_find_address(se), ty);
                 } else { 
		    if (wn1 != NULL)
		       wn_fmtsrc = WN_CreateIoItem1 (IOF_CR_FMTSRC, wn1, NIL);
                    else
		       wn_fmtsrc = WN_CreateIoItem0 ( IOF_NONE, NIL );
                 }
		 break;

             }
           }
           break;

        case CILIST_PARSFMT:
	  /* Preparsed format variable */
	  wn1 = cwh_expr_operand(DELETE_ARRAYEXP_WN);
          if (wn1)
            wn_parsfmt = WN_CreateIoItem1 ( IOF_CR_PARSFMT, wn1, NIL );
	  break;

        case CILIST_REC:
	  wn1 = cwh_expr_operand(DELETE_ARRAYEXP_WN);
          if (wn1)
	    wn_rec  = WN_CreateIoItem1 ( IOC_REC, wn1, NIL );
	  break;

        case CILIST_IOSTAT:
#ifdef KEY /* Bug 7874 */
	  ts = (FLD_item == cwh_stk_get_class()) ?
	    cwh_stk_get_FLD_TY() :
            cwh_stk_get_TY();
#else /* KEY Bug 7874 */
          ts = cwh_stk_get_TY();
#endif /* KEY Bug 7874 */
	  wn1 = cwh_expr_address(f_NONE);
	  if (wn1) {
            if (ts != NIL) {
               ty = cwh_types_array_TY(ts);
               ty = cwh_types_scalar_TY(ty);
            } else {
               ty = cwh_io_scalar_type(wn1);
            }
	    wn_iostat = WN_CreateIoItem1 ( IOC_IOSTAT, wn1, ty );
          }
	  break;
	case CILIST_END:
	case CILIST_ERR:
	case CILIST_EOR:

	  /* if label, mark assigned so not removed by optimizer */
          wn1 = cwh_io_cvt_tos_label_to_wn(TRUE);
	  if (wn1) {
            if (item == CILIST_END)
	      wn_end = WN_CreateIoItem1 ( IOC_END, wn1, NIL );
            else if (item == CILIST_ERR)
	      wn_err = WN_CreateIoItem1 ( IOC_ERR, wn1, NIL );
            else if (item == CILIST_EOR)
	      wn_eor = WN_CreateIoItem1 ( IOC_EOR, wn1, NIL );
          }
	  break;

        case CILIST_UNIT:
	  switch(cwh_stk_get_class()) {
	  case STR_item:

             cwh_stk_pop_STR();
             wn2 = cwh_expr_operand(DELETE_ARRAYEXP_WN);
	     ts  = cwh_stk_get_TY();
             wn1 = cwh_expr_address(f_NONE);
             se = cwh_addr_find_section(wn1, p_RETURN_SECTION);
	     if (se) {
	       wn1 = cwh_dope_from_expression(wn1, NULL, wn2, ts, NULL);
	       wn_unit = WN_CreateIoItem2(IOU_DOPE, wn1, 
				      cwh_addr_find_address(se), NIL);
             } else {
  	       wn_unit  = WN_CreateIoItem2 (IOU_INTERNAL, wn1, wn2, NIL);
             }
             break ;

          default:

             /* Push the top item again on the stack and extract it thru 
                cwh_expr_address. We save the resultant item, in case we
                need to use it later, if we happen to be in an encode/decode
                statement */
  
             cwh_stk_push_top_item_again();
             unit_address = cwh_expr_address(f_NONE);

             wn1 = cwh_expr_operand(DELETE_ARRAYEXP_WN);
	     if (target_io_library == IOLIB_MIPS) {
		if (wn1 == NULL) {
		  wn1 = WN_CreateIntconst ( OPC_I4INTCONST, 6);
		  wn_unit  = WN_CreateIoItem1 ( IOU_DEFAULT, wn1, NIL );
                } else {
		  /* Only handle external IO for now */
		  wn_unit  = WN_CreateIoItem1 ( IOU_EXTERNAL, wn1, NIL);
                }
              } else {   /* Target library is Cray */
		if (wn1 != NULL) {

		   /* DIMENSION IREC(2) 
		    * ENCODE(8,100,IREC) LREC(K),ICNT,ITAU
		    * 
		    * The above can cause the code to reach here, with wn1
		      as an ARRSECTION
		    */

                   se = cwh_addr_find_section(wn1, p_RETURN_SECTION);
		   if (se) {
		      ts = cwh_types_WN_TY(se,FALSE);
		      ts = cwh_types_array_TY(ts);
		      ty = cwh_types_scalar_TY(ts);
		      wn1 = cwh_dope_from_expression(wn1, NULL, NULL, ty,
		      NULL);
		      wn_unit = WN_CreateIoItem2(IOU_DOPE, wn1, 
				    cwh_addr_find_address(se), NIL);
                   } else {
		      wn_unit = WN_CreateIoItem1 ( IOU_EXTERNAL, wn1, NIL );
                      ed_unit = WN_CreateIoItem1 ( IOU_EXTERNAL, unit_address, NIL);
                   }
                } else {
		   wn_unit = WN_CreateIoItem0 ( IOU_NONE, NIL );
                }
              }
	      break;

          }
          break;

        case CILIST_FLFLAG:
	  wn1 = cwh_expr_operand(DELETE_ARRAYEXP_WN);
	  if (target_io_library == IOLIB_CRAY)
	    wn_flflag = WN_CreateIoItem1 ( IOC_CR_FLFLAG, wn1, NIL);
	  break;
	  
        case CILIST_EEEFLAG:
	  /* eeeflag value; 1 if err= specified, 2 if end= specified,
                            4 if eor= specified, 8 if iostat= specified 
          */
	  wn1 = cwh_expr_operand(DELETE_ARRAYEXP_WN);
	  eeeflag = WN_const_val(wn1);
	  if (target_io_library == IOLIB_CRAY)
	    wn_eeeflag = WN_CreateIoItem1 ( IOC_CR_EEEFLAG, wn1, NIL );  
	  break;

        case CILIST_EDFLAG:
	  /* Version Number */
	  wn1 = cwh_expr_operand(DELETE_ARRAYEXP_WN);
          edflag = WN_const_val(wn1);
	  wn_edflag = WN_CreateIoItem1(IOC_CR_EDFLAG, wn1, NIL);
	  /* Ignore */
	  break;

       } /* Switch */
   } /* for */

   if (edflag != 0 && ed_unit) {
      wn_unit  = WN_CreateIoItem2 (IOU_INTERNAL, WN_kid0(ed_unit), WN_kid0(wn_edflag), NIL);
      STACK_PUSH(wn_unit);
   } else {
      STACK_PUSH(wn_unit);
   }
   STACK_PUSH(wn_fmtsrc);
   STACK_PUSH(wn_flflag);
   STACK_PUSH(wn_parsfmt);
   STACK_PUSH(wn_eeeflag);
   STACK_PUSH(wn_iostat);
   STACK_PUSH(wn_rec);
   STACK_PUSH(wn_advance);
   STACK_PUSH(wn_size);
   STACK_PUSH(wn_edflag);
   STACK_PUSH(wn_end);
   STACK_PUSH(wn_err);
   STACK_PUSH(wn_eor);
} 

/*===================================================
 *
 * fei_IO_list
 *
 * Process the iolist items on the stack. For scalars
 * just load the value or pass the expression along
 * wrapped as an IO_item. For array sections a dope
 * vector supplants the OPC_ARRSECTION on the stack.
 * 
 *====================================================
*/
extern void 
fei_IO_list (int num_args, int mode )
{
  int i;
  WN *wn;
  WN **iolist;
  WN *cray_type_code = NULL;

  iolist = (WN **) malloc(sizeof (WN *) * num_args);
  
  for (i=0; i < num_args; i++) {
    
    if (!NAMELIST_MODE(mode)) {
       cray_type_code = cwh_expr_operand(DELETE_ARRAYEXP_WN);
   
       /* Implied do's that have already been processed by fei_implied_do don't
          have a type code over them; in this case, we need to push the 
          item back on the stack, because it is not a cray type code, and 
          continue with further processing */
   
       if ((WN_opcode(cray_type_code) == OPC_IO_ITEM) && 
          (WN_io_item(cray_type_code) == IOL_IMPLIED_DO))
          cwh_stk_push(cray_type_code,WN_item);
    }

    switch(cwh_stk_get_class()) {
    case STR_item :
      wn = cwh_io_str_ioitem(IOL_CHAR, mode, cray_type_code);
      break ;

    default:
      wn = cwh_io_ioitem(mode, cray_type_code);
      break;

    }
    iolist[i] = wn;
  }
  
  for (i=num_args-1; i >= 0; i--) {
    cwh_stk_push(iolist[i], WN_item);
  }
  
  num_list_items_last_processed = num_args;
  
  free(iolist);
}

/*===================================================
 *
 * fei_formatted_write
 *
 * Pops all the control items and the list items from 
 * stack and makes them kids of a IOS_CR_FWF node.
 *
 *====================================================
*/
  
extern void
fei_formatted_write( void ) 
{
  WN *wn;
  BOOL status;

  if (target_io_library == IOLIB_MIPS) 
    wn = cwh_stk_pop_iostmt (IOS_WRITE, eeeflag);
  else 
    wn = cwh_stk_pop_iostmt (IOS_CR_FWF, eeeflag);

  if (Use_Three_Call) {
     cwh_io_split_io_statement(wn);
  } else {
    marked_set = NULL;
    status = cwh_io_analyse_io_statement(wn, WRITE_STMT);
    cwh_io_unmark();
  
    if (status) {
       cwh_io_split_io_statement(wn);
    } else {
       cwh_io_create_dopes(wn);
       cwh_block_append(wn);
    }
  }
}

/*===================================================
 *
 * fei_formatted_read
 *
 * Pops all the control items and the list items from
 * stack and makes them kids of a IOS_CR_FRF node.
 *
 *====================================================
*/

extern void
fei_formatted_read(void) 
{
  WN *wn;
  BOOL status;

  wn = cwh_stk_pop_iostmt (IOS_CR_FRF, eeeflag);

  if (Use_Three_Call) {
     cwh_io_split_io_statement(wn);
  } else {
    marked_set = NULL;
    status = cwh_io_analyse_io_statement(wn, READ_STMT);
    cwh_io_unmark();
  
    if (status) {
       cwh_io_split_io_statement(wn);
    } else {
       cwh_io_create_dopes(wn);
       cwh_block_append(wn);
    }
  }
}

/*===================================================
 *
 * fei_unformatted_write
 *
 * Pops all the control items and the list items from
 * stack and makes them kids of a IOS_CR_FWU node.
 *
 *====================================================
*/

extern void
fei_unformatted_write(void) 
{
  WN *wn;
  BOOL status;

  wn = cwh_stk_pop_iostmt (IOS_CR_FWU, eeeflag);

  if (Use_Three_Call) {
     cwh_io_split_io_statement(wn);
  } else {
    marked_set = NULL;
    status = cwh_io_analyse_io_statement(wn, WRITE_STMT);
    cwh_io_unmark();
  
    if (status) {
       cwh_io_split_io_statement(wn);
    } else {
       cwh_io_create_dopes(wn);
       cwh_block_append(wn);
    }
  }
}

/*===================================================
 *
 * fei_unformatted_read
 *
 * Pops all the control items and the list items from
 * stack and makes them kids of a IOS_CR_FRU node.
 *
 *====================================================
*/

extern void
fei_unformatted_read(void)
{
  WN *wn;
  BOOL status;

  wn = cwh_stk_pop_iostmt (IOS_CR_FRU, eeeflag);

  if (Use_Three_Call) {
     cwh_io_split_io_statement(wn);
  } else {
    marked_set = NULL;
    status = cwh_io_analyse_io_statement(wn, READ_STMT);
    cwh_io_unmark();
  
    if (status) {
       cwh_io_split_io_statement(wn);
    } else {
       cwh_io_create_dopes(wn);
       cwh_block_append(wn);
    }
  }
}

/*===================================================
 *
 * fei_namelist_write
 *
 * Pops all the control items and the list items from
 * stack and makes them kids of a IOS_CR_FWN node.
 *
 *====================================================
*/

extern void
fei_namelist_write(void)
{
  WN *wn;

  wn = cwh_stk_pop_iostmt (IOS_CR_FWN, eeeflag);
  cwh_block_append(wn);
}

/*===================================================
 *
 * fei_namelist_read
 *
 * Pops all the control items and the list items from
 * stack and makes them kids of a IOS_CR_FRN node.
 *
 *====================================================
*/

extern void
fei_namelist_read(void)
{
  WN *wn;

  wn = cwh_stk_pop_iostmt (IOS_CR_FRN, eeeflag);
  cwh_block_append(wn);
}

/*===================================================
 *
 * fei_implied_do
 *
 * The following are already on the stack: implied_do_var,
 * start expr, end expr, incr expr, and the list of items 
 * to iterate over. It builds an entry for implied do
 * out of it. If any of the list items happen to be an
 * array element, a dope vector is created out of it, if
 * the implied_do_var happens to be part of the array index.
 *
 *====================================================
*/

extern void
fei_implied_do(void)
{
 WN **iolist;
 WN *incr;
 WN *stop;
 WN *start;
 WN *index;
 WN *wn;
 ST *st;
 INT32 num_list_items;
 INT32 i, j;

 num_list_items = num_list_items_last_processed;
 iolist = (WN **) malloc(sizeof (WN *) * num_list_items);

 for (i = 0; i < num_list_items; i++) 
   iolist[i] = cwh_expr_operand(DELETE_ARRAYEXP_WN);

 incr = cwh_expr_operand(DELETE_ARRAYEXP_WN);
 stop = cwh_expr_operand(DELETE_ARRAYEXP_WN);
 start = cwh_expr_operand(DELETE_ARRAYEXP_WN);
 st = cwh_stk_pop_ST();
 index = WN_CreateIdname(0, st);

 wn = WN_CreateIoItemN ( IOL_IMPLIED_DO, num_list_items + 4, NIL);
 WN_index(wn) = index;
 WN_start(wn) = start;
 WN_end(wn) = stop;
 WN_step(wn) = incr;

 for(i = num_list_items-1, j = 4; i >= 0 ; i--, j++) {
   WN_kid(wn,j) = iolist[i];
 }

 cwh_stk_push(wn, WN_item);
 free (iolist);
}

/*===================================================
 *
 * cwh_io_ioitem
 *
 * A utility routine for non-character scalar 
 * io items. Load the value TOS & return it as 
 * WN IO_item. 
 *
 * The only way to distinguish a logical constant
 * from an integer is to look a the TY that's pushed
 * on the stack (by fei_constant).

 * First pop the type code
 * 
 *====================================================
*/
static WN *
cwh_io_ioitem(int mode, WN *cray_type_code)
{
  WN * wn ;
  WN * wr ;
  TY_IDX  ty ;
  TY_IDX  ts = NIL ;
  TY_IDX cray_ptr_ty = NIL;
   
  if (cwh_stk_get_class() == FLD_item) {
     ts = cwh_stk_get_FLD_TY();
  } else if (cwh_stk_get_class() == DEREF_item) {
     ts = cwh_stk_get_TY();
     if (ts) {
       /* Get the type of the item stored from the dope vector */
       ts = TY_pointed(FLD_type(TY_fld(Ty_Table[ts])));
     }
  } else if ((cwh_stk_get_class() == ADDR_item) && !NAMELIST_MODE(mode)){
     ts = Be_Type_Tbl(Pointer_type);
     if (READ_MODE(mode))
       DevAssert((0),("Received an ADDR item in read mode"));
  } else if (cwh_stk_get_class() == ST_item) {
     ST *st;

     st = cwh_stk_pop_ST();

     if (ST_sclass(st) == SCLASS_FORMAL) {
	if ( !ST_is_value_parm(st) &&
		(TY_kind(TY_pointed(ST_type(st))) == KIND_POINTER)) {
	   /* This is a cray pointer */
	   DevAssert((!ST_is_temp_var(st)), ("Expecting a Cray Pointer"));
	   cray_ptr_ty = ST_type(st);
        } 
     } else if (TY_kind(ST_type(st)) == KIND_POINTER) {
	/* This is a cray pointer */
	DevAssert((!ST_is_temp_var(st)), ("Expecting a Cray Pointer"));
	cray_ptr_ty = ST_type(st);
     }
     cwh_stk_push(st,ST_item);
	   
  } else {
     ts = cwh_stk_get_TY();
  }

  if (NAMELIST_MODE(mode) || READ_MODE(mode)) {
     wn = cwh_expr_address(f_NONE);
  } else {
     wn = cwh_expr_operand(DELETE_ARRAYEXP_WN);
  }

  if (cray_ptr_ty != NIL) {
     ty = cray_ptr_ty;
  } else if (ts != NIL) {
     ty = cwh_types_array_TY(ts);
     ty = cwh_types_scalar_TY(ty);
  } else {
    ty = cwh_types_WN_TY(wn,FALSE);
    ty = cwh_types_array_TY(ty);
    ty = cwh_types_scalar_TY(ty);
  }

  if (cwh_addr_find_section(wn, p_RETURN_SECTION)) {
      wn = WN_CreateIoItem3 (IOL_DOPE, wn, cray_type_code, NULL, ty);
      return wn;
  }

  if ((WN_opcode(wn) == OPC_IO_ITEM) && (WN_io_item(wn) == IOL_IMPLIED_DO))
    return wn;

 
  if (NAMELIST_MODE(mode)) {
       wr = WN_CreateIoItem1 ( IOL_VAR, wn, ty);
  } else if (READ_MODE(mode)) {
       wr = WN_CreateIoItem2 ( IOL_VAR, wn, cray_type_code, ty);
  } else {
       wr = WN_CreateIoItem2 ( IOL_EXPR, wn, cray_type_code, ty);
  }
  return (wr);
}

/*===================================================
 *
 * cwh_io_str_ioitem
 *
 * A utility routine for character scalar 
 * io items. Load the value TOS & return it as 
 * WN IO_item. If it's an array section, it needs
 * a dope vector, otherwise it's just a 2 element item
 * 
 *====================================================
*/
static WN *
cwh_io_str_ioitem(IOITEM it, int mode, WN *craytype)
{
  WN * wn2 ;
  WN * wn  ;

  cwh_stk_pop_STR();

  wn2 = cwh_expr_operand(DELETE_ARRAYEXP_WN);
  wn = cwh_io_char_ioitem(it,wn2, mode, craytype);

  return(wn);
}
/*===================================================
 *
 * cwh_io_char_ioitem
 *
 * create an item of the given kind with the address 
 * of the string on TOS and its length passed.
 * 
 *====================================================
*/
static WN *
cwh_io_char_ioitem(IOITEM it, WN *len, int mode, WN *craytype)
{
  WN * wn ;
  TY_IDX  ty ;

  ty = cwh_stk_get_TY();

  wn = cwh_expr_address(f_NONE);

  if (cwh_addr_find_section(wn, p_RETURN_SECTION)) {
      wn = WN_CreateIoItem3 (IOL_DOPE, wn, craytype, len, ty );
      return wn;
  }

  wn = WN_CreateIoItem3 (it, wn, craytype, len, ty); 

  return (wn);
}

/*===================================================
 *
 * fei_iolength
 *
 * Handles inquire(iolength=len) <iolist>
 * 
 * The iolist is assumed to have gone through an fei_IO_list
 * interface. Also on the stack is a constant which represents
 * the first last flag.
 *
 *====================================================
*/

extern void
fei_iolength(void)
{
  INT32 num_items;
  INT32 i, j;
  WN *item;
  WN *wn;
  WN *wn1;
  ST *st;
  WN *temp;
  BOOL status;

  num_items = cwh_stk_get_num_inquire_items();
  wn = WN_CreateIo ( IOS_INQLENGTH, num_items + 4);

  /* Kids 1 and 2 are passed because for every IO sttaement
     the IO lowerer expects that the first kid is a UNIT,
     and the second a format; it just satisfies this requirement
     of the io lowerer and then ignored.
   */

  WN_kid0(wn) = WN_CreateIoItem0 ( IOU_NONE, NIL );
  WN_kid1(wn) = WN_CreateIoItem0 ( IOF_NONE, NIL );
  for (i=0, j = 4+num_items-1; i<num_items; i++, j--) {
     item = cwh_expr_operand(DELETE_ARRAYEXP_WN);
     WN_kid(wn,j) = item;
  }    
  wn1 = cwh_expr_operand(DELETE_ARRAYEXP_WN);
  WN_kid(wn,2) = WN_CreateIoItem1 ( IOC_CR_FLFLAG, wn1, NIL);
  
  st = cwh_stk_pop_ST();
  cwh_stk_push(st, ST_item);
  wn1 = cwh_expr_address(f_NONE);
  WN_kid(wn,3) = WN_CreateIoItem1(IOC_INQLENGTH_VAR, wn1, NIL);

     /* The following is just to fool the subsequent fei_store call;
	We make it store the inqlength var into itself.  The var has 
	already been passed to the io lowerer and it makes sure that 
	the value returned by _INQIL is stored into this var.
      */

  cwh_stk_push(st, ST_item);
  temp = cwh_expr_operand(DELETE_ARRAYEXP_WN);
  cwh_stk_push(st, ST_item);
  cwh_stk_push(temp,WN_item);

  if (Use_Three_Call) {
     cwh_io_split_io_statement(wn);
  } else {
    marked_set = NULL;
    status = cwh_io_analyse_io_statement(wn, WRITE_STMT);
    cwh_io_unmark();

    if (status) {
       cwh_io_split_io_statement(wn);
    } else {
       cwh_io_create_dopes(wn);
       cwh_block_append(wn);
    }
  }

}

/*===================================================
 *
 * fei_start_ioblock
 *
 * Mark the beginning of the translation for an IO statement with 
 * two comments: First, with the user's original IO statement, and 
 * the second with just "START_IO". Also add a pragma, to prevent
 * region processing from splitting IO statements.
 *
 *====================================================
*/

extern void
fei_start_ioblock(void) 
{
  char *str;
  TCON tc;
  ST *st;
  WN *wn;

  cwh_stk_pop_STR();
  cwh_stk_pop_whatever();
  st = cwh_stk_pop_ST();
  ++cwh_io_in_ioblock;

  if (IO_Comments) {
     tc = STC_val(st);
     str = Targ_String_Address(tc);
     wn = WN_CreateComment(str);
     cwh_auxst_clear(WN_st(wn));
     cwh_block_append(wn);
     str = (char *) malloc(9*sizeof(char));
     strcpy(str, "START_IO");
     wn = WN_CreateComment(str);
     cwh_auxst_clear(WN_st(wn));
     cwh_block_append(wn);
     free(str);

  }
  wn = WN_CreatePragma(WN_PRAGMA_START_STMT_CLUMP, (ST *) NIL, NIL,NIL);
  cwh_block_append(wn);

}

/*===================================================
 *
 * fei_start_ioblock
 *
 * Mark the end of the translation for this IO statement
 * with the comment "END_IO"
 *
 *====================================================
*/

extern void
fei_end_ioblock(void)
{
  char *str;
  WN *wn;

  wn = WN_CreatePragma(WN_PRAGMA_END_STMT_CLUMP, (ST *) NIL, NIL,NIL);
  cwh_block_append(wn);

  --cwh_io_in_ioblock;
  if (IO_Comments) {
     str = (char *) malloc(7*sizeof(char));
     strcpy(str, "END_IO");
     wn = WN_CreateComment(str);
     cwh_auxst_clear(WN_st(wn));
     cwh_block_append(wn);
     free(str);
  }
}

/*================================================================
 *
 * static BOOL cwh_io_null_address(WN * addr)
 *
 * return TRUE if the addr node is a NULL pointer (constant 0)
 * also deletes the pointer. 
 *
 *================================================================
*/
static BOOL cwh_io_null_address(WN *addr)
{
  if (WN_operator(addr) == OPR_INTCONST &&
      WN_const_val(addr) == 0) {
    WN_Delete(addr);
    return (TRUE);
  }

  return (FALSE);
}

/*===================================================
 *
 * fei_open
 *
 * Handles the F90 open statement
 *
 *====================================================
*/

void 
fei_open(void)
{
  INT item;
  WN **open_list;
#ifdef KEY /* Bug 10177 */
  WN *wn = 0;
#else /* KEY Bug 10177 */
  WN *wn;
#endif /* KEY Bug 10177 */
  WN *length;
  WN *addr;
  WN *unit= NULL;
  TY_IDX ty = NIL;
  INT32 num_items = 0;
  INT32 i,j;
  TY_IDX ts = NIL;

  open_list = (WN **) malloc(sizeof (WN *) * 15);

  for (item=OPEN_ERR; item >= OPEN_CALLNAME; item--) {
      switch (item) {

	case OPEN_ERR:
           addr = cwh_io_cvt_tos_label_to_wn(FALSE);
	   if (addr != NULL) {
	      wn = WN_CreateIoItem1(IOC_ERR, addr, NIL);
              open_list[num_items++] = wn;
           }
	   break;

	case OPEN_PAD:
	case OPEN_DELIM:
	case OPEN_ACTION:
	case OPEN_POSITION:
	case OPEN_BLANK:
	case OPEN_FORM:
	case OPEN_ACCESS:
	case OPEN_STATUS:
	case OPEN_FILE:
	   switch(cwh_stk_get_class()) {
	      case STR_item:
		  cwh_stk_pop_STR();
		  length = cwh_expr_operand(DELETE_ARRAYEXP_WN);
		  addr = cwh_expr_address(f_NONE);

		  if (item == OPEN_PAD)
		    wn = WN_CreateIoItem2 (IOC_PAD, addr, length, NIL);
                  else if (item == OPEN_DELIM)
		    wn = WN_CreateIoItem2 (IOC_DELIM, addr, length, NIL);
                  else if (item == OPEN_ACTION)
		    wn = WN_CreateIoItem2 (IOC_ACTION, addr, length, NIL);
                  else if (item == OPEN_POSITION)
		    wn = WN_CreateIoItem2 (IOC_POSITION, addr, length, NIL);
                  else if (item == OPEN_BLANK)
		    wn = WN_CreateIoItem2 (IOC_BLANK, addr, length, NIL);
                  else if (item == OPEN_FORM)
		    wn = WN_CreateIoItem2 (IOC_FORM, addr, length, NIL);
                  else if (item == OPEN_ACCESS)
		    wn = WN_CreateIoItem2 (IOC_ACCESS, addr, length, NIL);
                  else if (item == OPEN_STATUS)
		    wn = WN_CreateIoItem2 (IOC_STATUS, addr, length, NIL);
                  else if (item == OPEN_FILE)
		    wn = WN_CreateIoItem2 (IOC_FILE, addr, length, NIL);

		  open_list[num_items++] = wn;
		  break;
              default:
		  cwh_stk_pop_whatever();
		  break;
           }
	   break;

	case OPEN_RECL:
	case OPEN_IOSTAT:
	case OPEN_UNIT:

           ts = cwh_stk_get_TY();
	   switch(cwh_stk_get_class()) {
	     case ADDR_item:
	       
	       addr = cwh_expr_address(f_NONE);
	       if (cwh_io_null_address(addr)) break;

               if (ts != NIL) {
                 ty = cwh_types_array_TY(ts);
                 ty = cwh_types_scalar_TY(ty);
               } else {
                 ty = cwh_io_scalar_type(addr);
               }

	       if (item == OPEN_RECL)
	         wn = WN_CreateIoItem1(IOC_RECL, addr, ty);
               else if (item == OPEN_IOSTAT)
		 wn = WN_CreateIoItem1(IOC_IOSTAT, addr, ty);
               else if (item == OPEN_UNIT) 
                 unit = WN_CreateIoItem1(IOU_EXTERNAL, addr, NIL);

               if (item != OPEN_UNIT)
	         open_list[num_items++] = wn;
	       break;
             default:
	       cwh_stk_pop_whatever(); 
	       break;
           }
           break;

	case OPEN_ERRFLAG:
           switch(cwh_stk_get_class()) {
	     case ADDR_item:
	     case WN_item:
	     case WN_item_whole_array:
		wn = cwh_expr_operand(DELETE_ARRAYEXP_WN);
		wn = WN_CreateIoItem1(IOC_ERRFLAG, wn, NIL);
                open_list[num_items++] = wn;
		break;
             default:
		DevAssert((0),("Odd Open Item"));
            }
	    break;
	case OPEN_VERSION:
	case OPEN_CALLNAME:
            cwh_stk_pop_whatever(); /* ignore */
	    break;
     }
   }

   wn = WN_CreateIo (IOS_CR_OPEN, num_items + 2);

   if (unit)
      WN_kid0(wn) = unit;
   else
      WN_kid0(wn) = WN_CreateIoItem0 ( IOU_NONE, NIL );
    
   WN_kid1(wn) = WN_CreateIoItem0 ( IOF_NONE, NIL );
   for(i=0,j=2; i<num_items; i++,j++) 
      WN_kid(wn,j) = open_list[i];
   cwh_block_append(wn);

  free(open_list);
}  

/*===================================================
 *
 * fei_inquire
 *
 * Handles the F90 inquire statement
 *
 *====================================================
*/

void 
fei_inquire(void)
{
  INT item;
  WN **inq_list;
#ifdef KEY /* Bug 10177 */
  WN *wn = 0;
#else /* KEY Bug 10177 */
  WN *wn;
#endif /* KEY Bug 10177 */
  WN *length;
  WN *addr;
  WN *unit= NULL;
  INT32 num_items = 0;
  INT32 i,j;
  TY_IDX ts = NIL;

  inq_list = (WN **) malloc(sizeof (WN *) * 27);

  for (item=INQ_ERR; item >= INQ_CALLNAME; item--) {
      switch (item) {

        case INQ_ERR:

           addr = cwh_io_cvt_tos_label_to_wn(FALSE);

           if (addr != NULL) {
              wn = WN_CreateIoItem1(IOC_ERR, addr, NIL);
              inq_list[num_items++] = wn;
           }
           break;

	case INQ_PAD:
	case INQ_DELIM:
	case INQ_READWRITE:
	case INQ_WRITE:
	case INQ_READ:
	case INQ_ACTION:
	case INQ_POSITION:
	case INQ_BLANK:
	case INQ_UNFORMATTED:
	case INQ_FORMATTED:
	case INQ_FORM:
	case INQ_DIRECT:
	case INQ_SEQUENTIAL:
	case INQ_ACCESS:
	case INQ_NAME:
	case INQ_FILE:
	   switch(cwh_stk_get_class()) {
	      case STR_item:
		  cwh_stk_pop_STR();
		  length = cwh_expr_operand(DELETE_ARRAYEXP_WN);
		  addr = cwh_expr_address(f_NONE);

		  if (item == INQ_PAD)
		    wn = WN_CreateIoItem2 (IOC_PAD, addr, length, NIL);
                  else if (item == INQ_DELIM)
		    wn = WN_CreateIoItem2 (IOC_DELIM, addr, length, NIL);
                  else if (item == INQ_READWRITE)
		    wn = WN_CreateIoItem2 (IOC_READWRITE, addr, length, NIL);
                  else if (item == INQ_WRITE)
		    wn = WN_CreateIoItem2 (IOC_WRITE, addr, length, NIL);
                  else if (item == INQ_READ)
		    wn = WN_CreateIoItem2 (IOC_READ, addr, length, NIL);
                  else if (item == INQ_ACTION)
		    wn = WN_CreateIoItem2 (IOC_ACTION, addr, length, NIL);
                  else if (item == INQ_POSITION)
		    wn = WN_CreateIoItem2 (IOC_POSITION, addr, length, NIL);
                  else if (item == INQ_BLANK)
		    wn = WN_CreateIoItem2 (IOC_BLANK, addr, length, NIL);
                  else if (item == INQ_UNFORMATTED)
		    wn = WN_CreateIoItem2 (IOC_UNFORMATTED, addr, length, NIL);
                  else if (item == INQ_FORMATTED)
		    wn = WN_CreateIoItem2 (IOC_FORMATTED, addr, length, NIL);
                  else if (item == INQ_FORM)
		    wn = WN_CreateIoItem2 (IOC_FORM, addr, length, NIL);
                  else if (item == INQ_DIRECT)
		    wn = WN_CreateIoItem2 (IOC_DIRECT, addr, length, NIL);
                  else if (item == INQ_SEQUENTIAL)
		    wn = WN_CreateIoItem2 (IOC_SEQUENTIAL, addr, length, NIL);
                  else if (item == INQ_ACCESS)
		    wn = WN_CreateIoItem2 (IOC_ACCESS, addr, length, NIL);
                  else if (item == INQ_NAME)
		    wn = WN_CreateIoItem2 (IOC_NAME, addr, length, NIL);
                  else if (item == INQ_FILE)
		    wn = WN_CreateIoItem2 (IOC_FILE, addr, length, NIL);

		  inq_list[num_items++] = wn;
		  break;
              default:
		  cwh_stk_pop_whatever();
		  break;
           }
	   break;

	case INQ_NEXTREC: 
	case INQ_RECL:
	case INQ_NAMED:
	case INQ_NUMBER:
	case INQ_OPENED:
	case INQ_EXIST:
	case INQ_IOSTAT:
	case INQ_UNIT:

           ts = cwh_stk_get_TY();
	   switch(cwh_stk_get_class()) {
	     TY_IDX ty;

	     case ADDR_item:
	       addr = cwh_expr_address(f_NONE);
	       if (cwh_io_null_address(addr)) break;

               if (ts != NIL) {
                 ty = cwh_types_array_TY(ts);
                 ty = cwh_types_scalar_TY(ty);
               } else {
                 ty = cwh_io_scalar_type(addr);
               }

	       if (item == INQ_NEXTREC)
	         wn = WN_CreateIoItem1(IOC_NEXTREC, addr, ty);
	       else if (item == INQ_RECL)
	         wn = WN_CreateIoItem1(IOC_RECL, addr, ty);
	       else if (item == INQ_NAMED)
	         wn = WN_CreateIoItem1(IOC_NAMED, addr, ty);
	       else if (item == INQ_NUMBER)
	         wn = WN_CreateIoItem1(IOC_NUMBER, addr, ty);
	       else if (item == INQ_OPENED)
	         wn = WN_CreateIoItem1(IOC_OPENED, addr, ty);
	       else if (item == INQ_EXIST)
	         wn = WN_CreateIoItem1(IOC_EXIST, addr, ty);
               else if (item == INQ_IOSTAT)
		 wn = WN_CreateIoItem1(IOC_IOSTAT, addr, ty);
               else if (item == INQ_UNIT) 
                 unit = WN_CreateIoItem1(IOU_EXTERNAL, addr, NIL);

               if (item != INQ_UNIT)
	         inq_list[num_items++] = wn;
	       break;
             default:
	       cwh_stk_pop_whatever(); 
	       break;
           }
           break;

	case INQ_ERRFLAG:
           switch(cwh_stk_get_class()) {
	     case ADDR_item:
	     case WN_item:
	     case WN_item_whole_array:
		wn = cwh_expr_operand(DELETE_ARRAYEXP_WN);
		wn = WN_CreateIoItem1(IOC_ERRFLAG, wn, NIL);
                inq_list[num_items++] = wn;
		break;
             default:
		DevAssert((0),("Odd Inquire Item"));
            }
	    break;
	case INQ_VERSION:
	case INQ_CALLNAME:
            cwh_stk_pop_whatever(); /* ignore */
	    break;
     }
   }

   wn = WN_CreateIo (IOS_CR_INQUIRE, num_items + 2);

   if (unit)
      WN_kid0(wn) = unit;
   else
      WN_kid0(wn) = WN_CreateIoItem0 ( IOU_NONE, NIL );
    
   WN_kid1(wn) = WN_CreateIoItem0 ( IOF_NONE, NIL );
   for(i=0,j=2; i<num_items; i++,j++) 
      WN_kid(wn,j) = inq_list[i];
   cwh_block_append(wn);

  free(inq_list);
}  

/*===================================================
 *
 * fei_close
 *
 * Handles the F90 close statement
 *
 *====================================================
*/

void 
fei_close(void)
{
  INT item;
  WN **close_list;
#ifdef KEY /* Bug 10177 */
  WN *wn = 0;
#else /* KEY Bug 10177 */
  WN *wn;
#endif /* KEY Bug 10177 */
  WN *length;
  WN *addr;
  WN *unit= NULL;
  INT32 num_items = 0;
  INT32 i,j;
  TY_IDX ts = NIL;

  close_list = (WN **) malloc(sizeof (WN *) * 6);

  for (item=CLOSE_ERR; item >= CLOSE_CALLNAME; item--) {
      switch (item) {
       
        case CLOSE_ERR:

           addr = cwh_io_cvt_tos_label_to_wn(FALSE);

           if (addr != NULL) {
              wn = WN_CreateIoItem1(IOC_ERR, addr, NIL);
              close_list[num_items++] = wn;
           }
           break;
 
	case CLOSE_STATUS:
	   switch(cwh_stk_get_class()) {
	      case STR_item:
		  cwh_stk_pop_STR();
		  length = cwh_expr_operand(DELETE_ARRAYEXP_WN);
		  addr = cwh_expr_address(f_NONE);
		  wn = WN_CreateIoItem2 (IOC_STATUS, addr, length, NIL);
		  close_list[num_items++] = wn;
		  break;
              default:
		  cwh_stk_pop_whatever();
		  break;
           }
	   break;

	case CLOSE_IOSTAT:
	case CLOSE_UNIT:

           ts = cwh_stk_get_TY();

	   switch(cwh_stk_get_class()) {
	     TY_IDX ty;

	     case ADDR_item:

	       addr = cwh_expr_address(f_NONE);
	       if (cwh_io_null_address(addr)) break;

               if (ts != NIL) {
                 ty = cwh_types_array_TY(ts);
                 ty = cwh_types_scalar_TY(ty);
               } else {
                 ty = cwh_io_scalar_type(addr);
               }

               if (item == CLOSE_IOSTAT)
		 wn = WN_CreateIoItem1(IOC_IOSTAT, addr, ty);
               else if (item == CLOSE_UNIT) 
                 unit = WN_CreateIoItem1(IOU_EXTERNAL, addr, NIL);

               if (item != CLOSE_UNIT)
	         close_list[num_items++] = wn;
	       break;
             default:
	       cwh_stk_pop_whatever(); 
	       break;
           }
           break;

	case CLOSE_ERRFLAG:
           switch(cwh_stk_get_class()) {
	     case ADDR_item:
	     case WN_item:
	     case WN_item_whole_array:
		wn = cwh_expr_operand(DELETE_ARRAYEXP_WN);
		wn = WN_CreateIoItem1(IOC_ERRFLAG, wn, NIL);
                close_list[num_items++] = wn;
		break;
             default:
		DevAssert((0),("Odd Close Item"));
            }
	    break;
	case CLOSE_VERSION:
	case CLOSE_CALLNAME:
            cwh_stk_pop_whatever(); /* ignore */
	    break;
     }
   }

   wn = WN_CreateIo (IOS_CR_CLOSE, num_items + 2);

   if (unit)
      WN_kid0(wn) = unit;
   else
      WN_kid0(wn) = WN_CreateIoItem0 ( IOU_NONE, NIL );
    
   WN_kid1(wn) = WN_CreateIoItem0 ( IOF_NONE, NIL );
   for(i=0,j=2; i<num_items; i++,j++) 
      WN_kid(wn,j) = close_list[i];
   cwh_block_append(wn); 

  free(close_list);
}  


/*===================================================
 *
 * cwh_io_no_desc
 *
 * Handles rewind, backspace and endfile
 *
 *====================================================
*/

static void 
cwh_io_no_desc(IOSTATEMENT statement)
{
  INT item;
  WN **nodesc_list;
#ifdef KEY /* Bug 10177 */
  WN *wn = 0;
#else /* KEY Bug 10177 */
  WN *wn;
#endif /* KEY Bug 10177 */
  WN *addr;
  WN *unit= NULL;
  INT32 num_items = 0;
  INT32 i,j;
  TY_IDX ts = NIL;

  nodesc_list = (WN **) malloc(sizeof (WN *) * 6);

  for (item=NODESC_ERR; item >= NODESC_CALLNAME; item--) {
      switch (item) {

        case NODESC_ERR:

           addr = cwh_io_cvt_tos_label_to_wn(FALSE);
           if (addr != NULL) {
              wn = WN_CreateIoItem1(IOC_ERR, addr, NIL);
              nodesc_list[num_items++] = wn;
           }
           break;

	case NODESC_IOSTAT:
	case NODESC_UNIT:

           ts = cwh_stk_get_TY();
	   switch(cwh_stk_get_class()) {
	     TY_IDX ty;

	     case ADDR_item:

	       addr = cwh_expr_address(f_NONE);
	       if (cwh_io_null_address(addr)) break;

               if (ts != NIL) {
                 ty = cwh_types_array_TY(ts);
                 ty = cwh_types_scalar_TY(ty);
               } else {
                 ty = cwh_io_scalar_type(addr);
               }

               if (item == NODESC_IOSTAT)
		 wn = WN_CreateIoItem1(IOC_IOSTAT, addr, ty);
               else if (item == NODESC_UNIT) 
                 unit = WN_CreateIoItem1(IOU_EXTERNAL, addr, NIL);

               if (item != NODESC_UNIT)
	         nodesc_list[num_items++] = wn;
	       break;
             default:
	       cwh_stk_pop_whatever(); 
	       break;
           }
           break;

	case NODESC_ERRFLAG:
           switch(cwh_stk_get_class()) {
	     case ADDR_item:
	     case WN_item:
	     case WN_item_whole_array:
		wn = cwh_expr_operand(DELETE_ARRAYEXP_WN);
		wn = WN_CreateIoItem1(IOC_ERRFLAG, wn, NIL);
                nodesc_list[num_items++] = wn;
		break;
             default:
		DevAssert((0),("Odd Close Item"));
            }
	    break;

	case NODESC_CALLNAME:
            cwh_stk_pop_whatever(); /* ignore */
	    break;
     }
   }

   if (statement == IOS_CR_REWIND)
     wn = WN_CreateIo (IOS_CR_REWIND, num_items + 2);
   else if (statement == IOS_CR_BACKSPACE)
     wn = WN_CreateIo (IOS_CR_BACKSPACE, num_items + 2);
   else if (statement == IOS_CR_ENDFILE)
     wn = WN_CreateIo (IOS_CR_ENDFILE, num_items + 2);

   if (unit)
      WN_kid0(wn) = unit;
   else
      WN_kid0(wn) = WN_CreateIoItem0 ( IOU_NONE, NIL );
    
   WN_kid1(wn) = WN_CreateIoItem0 ( IOF_NONE, NIL );
   for(i=0,j=2; i<num_items; i++,j++) 
      WN_kid(wn,j) = nodesc_list[i];
   cwh_block_append(wn); 

   free(nodesc_list);
}  

/*===================================================
 *
 * fei_rewind
 *
 * Handles the F90 rewind statement
 *
 *====================================================
*/

void
fei_rewind(void)
{
  cwh_io_no_desc(IOS_CR_REWIND);
}

/*===================================================
 *
 * fei_backspace
 *
 * Handles the F90 backspace statement
 *
 *====================================================
*/

void
fei_backspace(void)
{
  cwh_io_no_desc(IOS_CR_BACKSPACE);
}

/*===================================================
 *
 * fei_endfile
 *
 * Handles the F90 endfile statement
 *
 *====================================================
*/

void
fei_endfile(void)
{
  cwh_io_no_desc(IOS_CR_ENDFILE);
}

/*===================================================
 *
 * fei_iotype
 *
 * Handles the io type code operator. They sit above each
 * io list item. The io type code constant is on top of the
 * stack, followed by the io list item.
 *
 *====================================================
*/

void
fei_iotype(void)
{

   WN *wn;

   wn = cwh_expr_operand(DELETE_ARRAYEXP_WN);
   cwh_stk_push(wn, WN_item);
}

/*===================================================
 *
 *
 * is_f90_pointer
 *
 * Called only when the OPR is an ILOAD, LDID or LDA.
 * Returns TRUE is the ST represents an f90 pointer.
 * 
 *
 *====================================================
*/

static BOOL 
is_f90_pointer(WN *addr)
{
   OPERATOR opr;
   opr = WN_operator(addr);
   if (opr == OPR_LDID || opr == OPR_LDA) {
      if (ST_class(WN_st(addr)) == CLASS_VAR) {

         return (ST_auxst_is_f90_pointer(WN_st(addr)));
      } else {
         return FALSE;
      }
   } else if (opr == OPR_ILOAD) {
      return (TY_is_f90_pointer(Ty_Table[TY_pointed(WN_load_addr_ty(addr))]));
   } else {
      return (FALSE);
   }
}

/*===================================================
 *
 * cwh_io_ST_base
 *
 * Input : an ST
 * Output:  Base of the ST
 *
 *====================================================
*/

static ST *
cwh_io_ST_base(ST *st)
{

  ST *base;

  /* cannot follow the base_st of a PREG!
   * or text
   * or preemptible symbols
   */

  if (ST_sclass(st) == SCLASS_REG || 
      ST_sclass(st) == SCLASS_TEXT ||
      ((Gen_PIC_Shared || Gen_PIC_Call_Shared) &&
       ST_export(st) == EXPORT_PREEMPTIBLE) )
  {
    return st;
  }

  /* SCLASS_BASED represents both Fortran pointers and also 
   * COMMON block spliting.  When the base_st is a KIND_POINTER,
   * it represents the former case.
   */

  if (Has_Base_Block(st)) {

    /* need to screen out the base_st that has pointer type */
//TODO ???
    TY_IDX ty = ST_type(ST_base(st));
    if (ty != NIL && TY_kind(ty) == KIND_POINTER)  {
      return st;
    }
  }

  base = st;

  while ( ST_base(base) != base  ) {

    base = ST_base(base); 
  }

  return base;
}

/*===================================================
 *
 * cwh_io_analyse_io_statement
 *
 * Returns TRUE is a single call will not suffice for this 
 * IO statement. If FALSE is returned then a single can 
 * safely be generated.
 *
 *====================================================
*/

static BOOL
cwh_io_analyse_io_statement(WN *tree, int mode)
{

  INT32 iolist;
  INT32 i;
  WN *wn;
  INT32 ioitem;

  for (iolist=0; iolist<WN_kid_count(tree); iolist++) { 
     wn = WN_kid(tree,iolist);
     ioitem = WN_io_item(wn);
     if (ioitem >= IOL_ARRAY)
        break;
  }

  for(i=iolist; i<WN_kid_count(tree); i++) {
    wn = WN_kid(tree,i);
    if (cwh_io_analyse_io_item(wn, NULL, mode))
       return TRUE;
  }
  return FALSE;
}


/*===================================================
 *
 * cwh_io_analyse_io_item
 *
 * Analyses one IO item to see if this item requires
 * that multiple calls be generated to the library. 
 * Returns FALSE if a single call will suffice; returns
 * TRUE otherwise. 
 * 
 * It uses the visited field in AUXST to analyse dependency
 * information.
 *
 *====================================================
*/

static BOOL
cwh_io_analyse_io_item(WN *tree, IMPDO_INFO *impdo_set, int mode)
{

   INT32 item;
   WN *kid0;
   OPERATOR opr;
   ST *index;
   IMPDO_INFO *new_impdo_set;
   BOOL visited;
   INT32 i;
   INT32 nd;

   item = WN_intrinsic(tree);
   
   switch(item) {

   case IOL_IMPLIED_DO:

       if (is_f90_pointer(WN_index(tree)))
          return TRUE;

       index = cwh_io_ST_base(WN_st(WN_index(tree)));
       new_impdo_set = (IMPDO_INFO *) malloc(sizeof(IMPDO_INFO));
       Impdo_index(new_impdo_set) = WN_st(WN_index(tree));
       Impdo_next(new_impdo_set) = impdo_set;

#ifdef KEY /* Bug 3279 */
       // Runtime library can interpret the implied-do loop internally only if
       // the indices are type integer*4 (see "struct dovarlist" in
       // dopexfer.c or "struct ioarray_entry" member "dovar" in f90io.h.)
       {
	 if (MTYPE_I4 != TY_mtype(WN_type(WN_index(tree))) ||
	     MTYPE_I4 != WN_rtype(WN_start(tree)) ||
	     MTYPE_I4 != WN_rtype(WN_end(tree)) ||
	     MTYPE_I4 != WN_rtype(WN_step(tree))) {
	   return TRUE;
	   }
       }
#endif /* KEY Bug 3279 */
       
       visited = ST_auxst_visited(index);
       if (visited) {
          return TRUE; 
       } else {
	 Set_ST_auxst_visited(index,TRUE);
	 cwh_io_add_st_to_marked_set(index);
       }

       if (cwh_io_analyse_expr(WN_start(tree), new_impdo_set, mode))
          return TRUE;
       else if (cwh_io_analyse_expr(WN_end(tree), new_impdo_set, mode))
          return TRUE;
       else if (cwh_io_analyse_expr(WN_step(tree), new_impdo_set, mode))
          return TRUE;
        
       for(i=4; i<WN_kid_count(tree); i++) {
          if (cwh_io_analyse_io_item(WN_kid(tree,i), new_impdo_set, mode))
             return TRUE;
       }
       free (new_impdo_set);
       break; 
       
   case IOL_EXPR:

       kid0 = WN_kid0(tree);
       opr = WNOPR(kid0);

       if ((opr == OPR_ILOAD) && (WNOPR(WN_kid0(kid0)) == OPR_ARRAY)) {
          if (cwh_io_analyse_arr(WN_kid0(kid0), impdo_set, mode))
             return TRUE;
       } else {
          if (cwh_io_analyse_expr(kid0, impdo_set, mode))
             return TRUE;
       }
       break;

   case IOL_VAR:

       kid0 = WN_kid0(tree);
       opr = WNOPR(kid0);

       if (opr == OPR_ARRAY) {
          if (cwh_io_analyse_arr(kid0, impdo_set, mode))
             return TRUE;
       } else {
          if (cwh_io_analyse_expr(kid0, impdo_set, mode))
             return TRUE;
       }
       break;

   case IOL_CHAR:
       kid0 = WN_kid0(tree);

       /* If the length is dependent on some variable, split */

       if (cwh_io_analyse_expr(WN_kid2(tree), impdo_set, mode))
	  return TRUE;

       opr = WNOPR(kid0);

       if (opr == OPR_ARRAY) {

          nd = WN_kid_count(kid0)/2;	

	  if (WNOPR(WN_kid0(kid0)) == OPR_LDA || WNOPR(WN_kid0(kid0)) == OPR_LDID) {
             if (cwh_io_analyse_arr(kid0, impdo_set, mode))
                return TRUE;
          } else if ((nd == 1) && (WNOPR(WN_kid0(kid0)) == OPR_ARRAY)) {
	     if (cwh_io_analyse_arr(WN_kid0(kid0), impdo_set, mode))
		return TRUE;
             for (i=2*nd; i > nd; i-- ) {
                if (cwh_io_analyse_index_expr(WN_kid(kid0,i), 
                                           impdo_set, mode) != 0)
                return TRUE;
             }
          } else {
             if (cwh_io_analyse_expr(kid0, impdo_set, mode))
                return TRUE;
          } 

       } else {
          if (cwh_io_analyse_expr(kid0, impdo_set, mode))
             return TRUE;
       }
       break;

   case IOL_DOPE:
       kid0 = WN_kid0(tree);
       opr = WNOPR(kid0);


       /* If the length is dependent on some variable, split */
       if (WN_kid2(tree) && 
              cwh_io_analyse_expr(WN_kid2(tree), impdo_set, mode))
          return TRUE;
      
       if (opr == OPR_ARRSECTION) {
	 if (cwh_io_analyse_arr(kid0, impdo_set, mode))
	    return TRUE;
       } else if ((opr == OPR_ILOAD) && 
		 (WNOPR(WN_kid0(kid0)) == OPR_ARRSECTION)) {
         if (cwh_io_analyse_arr(WN_kid0(kid0), impdo_set, mode))
	    return TRUE;
       } else if (opr == OPR_ARRAY) {
	    nd = WN_kid_count(kid0)/2;
	    if ((nd == 1) && (WNOPR(WN_kid0(kid0)) == OPR_ARRSECTION)) {
	       if (cwh_io_analyse_arr(WN_kid0(kid0), impdo_set, mode))
	          return TRUE;
               for (i=2*nd; i > nd; i-- ) {
                  if (cwh_io_analyse_index_expr(WN_kid(kid0,i), 
                                             impdo_set, mode) != 0)
                     return TRUE;
               }
            } else {
	       if (cwh_io_analyse_expr(kid0, impdo_set, mode))
		  return TRUE;
            }
       } else {
	 if (cwh_io_analyse_expr(kid0, impdo_set, mode))
	    return TRUE;
       }
       break;
	 
   default:
       DevAssert((0),("Odd iolist Item"));
   }

   return FALSE;
}
        

/*===================================================
 *
 * cwh_io_analyse_expr
 *
 * Analyse any expression to see if any ST in that
 * expression has the visited bit set, and if so,
 * return TRUE to indicate that the IO statement this
 * expression appears in, needs multiple calls.
 *
 * If an ST is encountered that does not have visited
 * bit set, the bit is now set so that dependency with
 * any subsequent expressions can be analyzed.
 *
 *====================================================
*/

static BOOL
cwh_io_analyse_expr(WN *tree, IMPDO_INFO *impdo_set, int mode)
{
   ST *st;
   BOOL visited;
   INT32 i;
  
   if ((WNOPR(tree) == OPR_ILOAD) || ( OPCODE_has_aux(WN_opcode(tree))))
      if (is_f90_pointer(tree))
         return TRUE;

   if ( OPCODE_has_aux(WN_opcode(tree))) {
     st = cwh_io_ST_base(WN_st(tree));
     visited = ST_auxst_visited(st);
     if (visited) {
        return TRUE;
     } else if (READ_MODE(mode)) {
       Set_ST_auxst_visited(st,TRUE);
       cwh_io_add_st_to_marked_set(st);
     }
   } else {
     for ( i = 0; i < WN_kid_count(tree); i++ )
       if (cwh_io_analyse_expr(WN_kid(tree,i), impdo_set, mode))
          return TRUE;
   }
   return FALSE;
}

/*===================================================
 *
 * cwh_io_analyse_arr
 *
 * Analyse an OPC_ARRAY node or an OPC_ARRSECTION node
 * to see if it has constructs that prevent a single call
 * from being generated. If kid0 is anything other than 
 * an LDA, a TRUE value is returned to indicate multiple
 * calls are required. The indexes along all dimensions 
 * are analyzed (see cwh_io_analyse_index_expr) and if
 * everything looks okay a value of FALSE is returned
 * to indicate that this node does not prevent a single call
 * from being generated.
 *
 *
 *====================================================
*/

static BOOL
cwh_io_analyse_arr(WN *tree, IMPDO_INFO *impdo_set, int mode)
{
  INT32 nd;
  WN *addr;
  ST *st;
  BOOL visited;
  INT32 i;

  nd = WN_kid_count(tree)/2;

  /* addr = cwh_addr_find_address(tree); */
  addr = WN_kid0(tree);

  if (WNOPR(addr) == OPR_LDA || WNOPR(addr) == OPR_LDID) {

     if (is_f90_pointer(addr))
        return TRUE;

     st = cwh_io_ST_base(WN_st(addr));
     visited = ST_auxst_visited(st);
     if (visited) {
        return TRUE;
     } else if (READ_MODE(mode)) {
        Set_ST_auxst_visited(st,TRUE);
        cwh_io_add_st_to_marked_set(st);
     }
     for (i=2*nd; i > nd; i-- ) {
         if (cwh_io_analyse_index_expr(WN_kid(tree,i), impdo_set, mode) == -1)
            return TRUE;
     }
  } else {
     if (cwh_io_analyse_expr(tree, impdo_set, mode))
        return TRUE;
  }

  return FALSE;
}

/*===================================================
 *
 * cwh_io_analyse_index_expr
 *
 * Analyze the index along a dimension of an ARRAY or
 * ARRSECTION node. If the index expr happens to be
 * an OPR_TRIPLET, then the kids representing the extent
 * and stride are analyzed by cwh_io_analyse_expr to make 
 * sure there is no dependency on an ST that has visited 
 * bit set. kid0 of the TRIPLET node is analyzed by this
 * same routine as would an index under an OPC_ARRAY node.
 *
 * If the index is of the form <i+<expr>> or <i-<expr>>
 * where i can possible be an implied do index, and expr
 * does not contain an implied do index, then it is a 
 * candidate for a single call. If anything in <expr> 
 * has the visited bit set, multiple calls are needed.
 * Any expressions which is not of the above type and 
 * is not a constant will cause a multiple call to be 
 * generated.
 *
 *====================================================
*/

static INT32
cwh_io_analyse_index_expr(WN *tree, IMPDO_INFO *impdo_set, int mode)
{
   INT32 kid0_status;
   INT32 kid1_status;
   INT32 i;
   INT32 pos;
   BOOL visited;
   ST *st;

   if (WNOPR(tree) == OPR_TRIPLET) {
     for (i=1; i<=2; i++) 
        if (cwh_io_analyse_expr(WN_kid(tree, i), impdo_set, mode))
           return -1;

     kid0_status = cwh_io_analyse_index_expr(WN_kid0(tree), impdo_set, 
                                             mode);
     return kid0_status;
     
   } else if (WNOPR(tree) == OPR_LDID) {

      if (is_f90_pointer(tree))
         return -1;

      if ( (pos = member (WN_st(tree), impdo_set)) != 0) {
         return pos;
      } else {
         st = cwh_io_ST_base(WN_st(tree));
         visited = ST_auxst_visited(st);
         if (visited)
           return -1;
         else
           return 0;
      }
   } else if ( WN_operator_is(tree,OPR_CONST) ||
               WN_operator_is(tree,OPR_INTCONST)) {
      return 0;
   } else if ( WN_operator_is(tree,OPR_ADD) ) { 
     kid0_status = cwh_io_analyse_index_expr(WN_kid0(tree), impdo_set, mode);
     kid1_status = cwh_io_analyse_index_expr(WN_kid1(tree), impdo_set, mode);
     switch(kid0_status) {
       case 0:
          return kid1_status;
       case -1:
          return -1;
       default: /* >= 1 */
          if (kid1_status == 0)
             return kid0_status;
          else
             return -1;
     }
   } else if (WN_operator_is(tree,OPR_SUB)) { 
     kid0_status = cwh_io_analyse_index_expr(WN_kid0(tree), impdo_set, mode);
     kid1_status = cwh_io_analyse_index_expr(WN_kid1(tree), impdo_set, mode);
     switch(kid0_status) {
       case 0:
	  if (kid1_status == 0)
	     return 0;
          else
	     return -1;
       case -1:
          return -1;
       default: /* >= 1 */
          if (kid1_status == 0)
             return kid0_status;
          else
             return -1;
     }
   }
   return -1;
}

/*===================================================
 *
 * member
 *
 * Checks if the given ST is a memeber of the ST's
 * contained in the implied do set. If it is a member,
 * the position of the ST is returned, else 0 is returned.
 *
 *
 *====================================================
*/

static mINT32
member(ST *st, IMPDO_INFO *impdo_set)
{
  mINT32 ret_val = 1;
  while (impdo_set) {
     if (st == Impdo_index(impdo_set))
        return ret_val;
     impdo_set = Impdo_next(impdo_set);
     ret_val++;
  }
  return 0;
}

/*===================================================
 *
 * cwh_io_create_dopes
 *
 * This routine is invoked only after it has been previously
 * determined that a single call will suffice for this IO
 * statement. This routine takes an IO statement as argument 
 * and converts:
 *
 * 1. All ARRSECTION nodes into dope vectors.
 * 2. OPC_ARRAY nodes under the control of an implied do
 *    are converted to dope vectors.
 *
 *
 *====================================================
*/

static void
cwh_io_create_dopes(WN *tree)
{
  INT32 iolist;
  INT32 i;
  WN *wn;
  INT32 ioitem;

  for (iolist=0; iolist<WN_kid_count(tree); iolist++) {
     wn = WN_kid(tree,iolist);
     ioitem = WN_io_item(wn);
     if (ioitem >= IOL_ARRAY)
        break;
  }

  for(i=iolist; i<WN_kid_count(tree); i++) {
    wn = WN_kid(tree,i);
    cwh_io_create_dope_from_item(tree, i, NULL);
  }
}

/*===================================================
 *
 * cwh_io_create_dope_from_item
 * 
 * Called by cwh_io_create_dopes to deal with a single
 * item when creating dope vectors.  For implied do's
 * the routine recursively calls itself. For other items,
 * it finds the appropriate array node and passes that to
 * cwh_io_conv_array_to_dope for conversion.
 *
 *
 *====================================================
*/

static void
cwh_io_create_dope_from_item(WN *parent, int kid_num, IMPDO_INFO *impdo_set)
{
   INT32 item;
   WN *kid0;
   OPERATOR opr;
   ST *index;
   IMPDO_INFO *new_impdo_set;
   INT32 i;
   WN *tree;

   tree = WN_kid(parent, kid_num);

   item = WN_intrinsic(tree);

   switch(item) {

   case IOL_IMPLIED_DO:

       index = WN_st(WN_index(tree));
       new_impdo_set = (IMPDO_INFO *) malloc(sizeof(IMPDO_INFO));
       Impdo_index(new_impdo_set) = index;
       Impdo_next(new_impdo_set) = impdo_set;

       for(i=4; i<WN_kid_count(tree); i++) 
          cwh_io_create_dope_from_item(tree, i, new_impdo_set);

       free(new_impdo_set);
       break;

   case IOL_EXPR:

       if (impdo_set == NULL)
	  return;

       kid0 = WN_kid0(tree);
       opr = WNOPR(kid0);

       if ((opr == OPR_ILOAD) && (WNOPR(WN_kid0(kid0)) == OPR_ARRAY)) {
          WN_kid(parent,kid_num) = cwh_io_conv_array_to_dope(kid0, 
                                                       impdo_set, tree, NULL, WN_ty(tree), WN_COPY_Tree(WN_kid1(tree)));
          WN_Delete(tree);
       }
       break;

   case IOL_VAR:

       if (impdo_set == NULL)
	  return;

       kid0 = WN_kid0(tree);
       opr = WNOPR(kid0);

       if (opr == OPR_ARRAY) {
          WN_kid(parent,kid_num) = cwh_io_conv_array_to_dope(kid0, 
                                                       impdo_set, tree, NULL,WN_ty(tree), WN_COPY_Tree(WN_kid1(tree)));
          WN_Delete(tree);
       }

       break;

   case IOL_CHAR:

       if (impdo_set == NULL)
	  return;

       kid0 = WN_kid0(tree);

       opr = WNOPR(kid0);

       if (opr == OPR_ARRAY) {

          if  ( (WNOPR(WN_kid0(kid0)) == OPR_LDA)  ||  
		(WNOPR(WN_kid0(kid0)) == OPR_LDID)  ||
		(WNOPR(WN_kid0(kid0)) == OPR_ARRAY)) {
              WN_kid(parent,kid_num) = cwh_io_conv_array_to_dope(kid0, 
                                              impdo_set, tree, WN_kid2(tree),
					      WN_ty(tree), WN_COPY_Tree(WN_kid1(tree)));
              WN_Delete(tree);
          }
       }

       break;


   case IOL_DOPE:

       kid0 = WN_kid0(tree);
       WN_kid(parent,kid_num) = cwh_io_conv_arrsection_to_dope(kid0, impdo_set, 
							  tree, WN_kid2(tree), WN_ty(tree), WN_kid1(tree));
       break;

   default:
       DevAssert((0),("Odd iolist Item"));
   }

}

/*===================================================
 *
 * cwh_io_conv_array_to_dope
 *
 * The routine receives an ARRAY or an ILOAD node as argument.
 * Thr routine first extracts the ARRAY node from the argument.
 * It then checks if any index is under the control of an implied
 * do; if yes, an IOL_DOPE item is created; the base address in the
 * dope vector may have some offset added to it. eg. for
 * a(i+5), i=1,n), the address stored in the dope vector is the address
 * of a(5).  kids 2 thru n+2 are added for the n dims of the array. The
 * 2+ith kid is set to the address of the corresponding implied do
 * var, if the ith dimension is under the control of an implied do index,
 * otherwise it contains 0. 
 *
 * If no index is under the control of an implied do, then 
 * a dope vector is not created.
 * The second kid on the IOL_DOPE is the base address of the arr node; this is 
 * passed as a dummy argument in the call generated in the IO lowerer. 
 *
 *====================================================
*/

static WN *
cwh_io_conv_array_to_dope(WN *tree, IMPDO_INFO *impdo_set, WN *old_item, WN *char_len,TY_IDX ty, WN *craytype) 
{
  INT32 nd;
  ST *st;
  WN *wn;
  INT32 pos;
  INT32 indflag = 0;
  IMPDO_INFO *impdo;
  INT32 i;
  INT32 j;
  INT32 k;
  WN *index;
  WN *address_fixup;
  WN *final_address = NULL;
  WN *offset = NULL;
  WN *new_index_expr;
  WN *kid;
  OPERATOR opr;
  WN *arr;

  opr = WNOPR(tree);

  switch(opr) {
    case OPR_ILOAD:
      kid = WN_kid0(tree);
      if (WNOPR(kid) == OPR_ARRAY) {
	 arr = kid;
	 offset =  WN_Intconst(Pointer_Mtype,WN_load_offset(tree));
      } else {
	 return (WN_COPY_Tree(old_item));
      }
      break;

    case OPR_ARRAY:
      kid = WN_kid0(tree);
      nd = WN_kid_count(tree)/2;
      if (WNOPR(kid) == OPR_LDA || WNOPR(kid) == OPR_LDID) {
	 arr = tree;
      } else if ((WNOPR(kid) == OPR_ARRAY) && (nd == 1) ) {
	 arr = kid;
         final_address = WN_Create(OPCODE_make_op(OPR_ARRAY,Pointer_Mtype,
                                   MTYPE_V), 3);
         WN_element_size(final_address) = WN_element_size(tree);
         WN_kid(final_address, 1) =  WN_COPY_Tree(WN_kid(tree,nd));
         WN_kid(final_address, 2) = WN_COPY_Tree(WN_kid(tree, 2*nd));
      } else {

         /* Previous dependence analysis guarantees that if we hit this else 
	    then the array is not dependent on an implied do; if it were,
	    dependence analysis would have asked us to convert the
	    implied do into a do loop and we wouldn't be here */

	 return (WN_COPY_Tree(old_item));
      } 
      break;

    default:
         /* Previous dependence analysis guarantees that if we hit this case
	    then the array is not dependent on an implied do; if it were,
	    dependence analysis would have asked us to convert the
	    implied do into a do loop and we wouldn't be here */

      return (WN_COPY_Tree(old_item));
  }
      
  nd = WN_kid_count(arr)/2;

  if (impdo_set != NULL) {
   for (i=2*nd; i > nd; i-- ) { 
       index = WN_kid(arr,i);
       if (WNOPR(index) == OPR_TRIPLET)
 	 pos = cwh_io_search_implied_do_index(WN_kid0(index), impdo_set);
       else
 	 pos = cwh_io_search_implied_do_index(index, impdo_set);
       if (pos >= 1 ) {
 	 indflag = 1;
 	 break;
       }
   }
  }
	
  if (indflag == 0) {
	return(WN_COPY_Tree(old_item));
  } else {
     wn = WN_CreateIoItemN (IOL_DOPE, nd+2, NIL);
     address_fixup = WN_Create(OPCODE_make_op(OPR_ARRAY,Pointer_Mtype,MTYPE_V),
			       WN_kid_count(arr));
     WN_kid0(address_fixup) = WN_kid0(arr);
     WN_element_size(address_fixup) = WN_element_size(arr);


     for (i=2*nd, k=2; i > nd; i--, k++ ) {
	 WN_kid(address_fixup,i-nd) = WN_COPY_Tree(WN_kid(arr,i-nd));
	 index = WN_kid(arr,i);
	 pos = cwh_io_search_implied_do_index(index, impdo_set);
         if (pos >= 1) {
            impdo = impdo_set;
	    for (j=1; j < pos; j++)
	       impdo = Impdo_next(impdo);
            WN_kid(wn,k) = WN_CreateLda (opc_lda, 0,
				 Make_Pointer_Type(ST_type(Impdo_index(impdo))),
                                 Impdo_index(impdo));
            new_index_expr = Substitute_1_For_Impdo_Index_Val(
				 WN_COPY_Tree(index), impdo);
            WN_kid(address_fixup,i) = new_index_expr;
         } else {
	    WN_kid(address_fixup,i) = WN_COPY_Tree(index);
	    WN_kid(wn,k) = WN_CreateIntconst ( OPC_I4INTCONST, 0);
         }
     }
     if (final_address != NULL) {
        WN_kid0(final_address) = address_fixup;
     } else if (offset != NULL) {
	final_address = WN_Add(Pointer_Mtype, address_fixup, offset);
     } else {
        final_address = address_fixup;
     }
     WN_kid0(wn) = cwh_dope_from_expression(arr, arr, char_len,ty, craytype);
     WN_kid1(wn) = cwh_addr_find_address(arr);
     st = WN_st(WN_kid0(wn));
     cwh_addr_store_ST(st, 0, Be_Type_Tbl(Pointer_type), final_address);
     return wn;
  }
}

/*===================================================
 *
 * cwh_io_conv_arrsection_to_dope
 *
 * The routine receives an expr as argument that has an ARRSECTION somewhere.
 * The routine first checks for an ILOAD over an ARRSECTION or an ARRAY
 * over an ARRSECTION or just an ARRSECTION at the top level. If any of
 * these is found, it extracts the ARRSECTION node and proceeds with
 * further processing, otherwise, the entire expr is just converted to
 * a dope vector.
 *
 * It first checks if any index is under the control of an implied
 * do; if yes, an IOL_DOPE item is created; the base address in the
 * dope vector may have some offset added to it. eg. for
 * a(i+5, :), i=1,n), the address stored in the dope vector is the address
 * of a(5,1).  kids 2 thru n+2 are added for the n dims of the array. The
 * 2+ith kid is set to the address of the corresponding implied do
 * var, if the ith dimension is under the control of an implied do index,
 * otherwise it contains 0. 
 *
 * If no index is under the control of an implied do, then f dope vector
 * is still created.
 * The second kid on the IOL_DOPE is the base address of the arr node; this is 
 * passed as a dummy argument in the call generated in the IO lowerer. 
 *
 *====================================================
*/

static WN *
cwh_io_conv_arrsection_to_dope(WN *tree, IMPDO_INFO *impdo_set, WN *old_item, 
                          WN *char_len, TY_IDX ty, WN *craytype)
{
  INT32 nd;
  ST *st;
  WN *wn;
  INT32 pos;
  INT32 indflag = 0;
  IMPDO_INFO *impdo;
  INT32 i;
  INT32 j;
  INT32 k;
  WN *index;
  WN *address_fixup;
  WN *final_address = NULL;
  WN *offset = NULL;
  WN *new_index_expr;
  WN *ad;
  WN *kid;
  OPERATOR opr;
  WN *arr;

  opr = WNOPR(tree);

  switch(opr) {
    case OPR_ILOAD:
      kid = WN_kid0(tree);
      if (WNOPR(kid) == OPR_ARRSECTION) {
	 arr = kid;
	 offset = WN_Intconst(Pointer_Mtype,WN_load_offset(tree));
      } else {
	 /* Previous dependence analysis guarantees us that if we reach
	    here, we can safely convert this to a dope vector; the ARRSECTION
	    hidden somewhere in this expression is not dependent on any
	    implied do, and we can avoid further analysis; if it were
	    dependent on an implied do index, then we would have decided
	    to split this statement, and we wouldn't be here */

         arr = cwh_addr_find_section(tree, p_RETURN_SECTION);
         ad = cwh_addr_find_address(arr);
         wn = cwh_dope_from_expression(tree, NULL, char_len, ty, craytype);	  
         wn = WN_CreateIoItem2(IOL_DOPE, wn, ad, NIL);
         return wn;
      }
      break;

    case OPR_ARRAY:
      kid = WN_kid0(tree);
      nd = WN_kid_count(tree)/2;
      if ((WNOPR(kid) == OPR_ARRSECTION) && (nd == 1) ) {
	 arr = kid;
         final_address = WN_Create(OPCODE_make_op(OPR_ARRAY,Pointer_Mtype,
                                   MTYPE_V), 3);
         WN_element_size(final_address) = WN_element_size(tree);
         WN_kid(final_address, 1) =  WN_COPY_Tree(WN_kid(tree,nd));
         WN_kid(final_address, 2) = WN_COPY_Tree(WN_kid(tree, 2*nd));
      } else {
         /* Previous dependence analysis guarantees us that if we reach
            here, we can safely convert this to a dope vector; the ARRSECTION
            hidden somewhere in this expression is not dependent on any
            implied do, and we can avoid further analysis; if it were
            dependent on an implied do index, then we would have decided
            to split this statement, and we wouldn't be here */

         arr = cwh_addr_find_section(tree, p_RETURN_SECTION);
         ad = cwh_addr_find_address(arr);
         wn = cwh_dope_from_expression(tree, NULL, char_len, ty, craytype);
         wn = WN_CreateIoItem2(IOL_DOPE, wn, ad, NIL);
         return wn;
      } 
      break;

    case OPR_ARRSECTION:
      arr = tree;
      break;

    default:
         /* Previous dependence analysis guarantees us that if we reach
            here, we can safely convert this to a dope vector; the ARRSECTION
            hidden somewhere in this expression is not dependent on any
            implied do, and we can avoid further analysis; if it were
            dependent on an implied do index, then we would have decided
            to split this statement, and we wouldn't be here */

      arr = cwh_addr_find_section(tree, p_RETURN_SECTION);
      ad = cwh_addr_find_address(arr);
      wn = cwh_dope_from_expression(tree, NULL, char_len, ty, craytype);
      wn = WN_CreateIoItem2(IOL_DOPE, wn, ad, NIL);
      return wn;
  }
      
  nd = WN_kid_count(arr)/2;

  if (impdo_set != NULL) {
    for (i=2*nd; i > nd; i-- ) { 
        index = WN_kid(arr,i);
        if (WNOPR(index) == OPR_TRIPLET)
  	 pos = cwh_io_search_implied_do_index(WN_kid0(index), impdo_set);
        else
  	 pos = cwh_io_search_implied_do_index(index, impdo_set);
        if (pos >= 1 ) {
  	 indflag = 1;
  	 break;
        }
    }
  }
	
  if (indflag == 0) {
      arr = cwh_addr_find_section(tree, p_RETURN_SECTION);
      ad = cwh_addr_find_address(arr);
      wn = cwh_dope_from_expression(tree, NULL, char_len, ty, craytype);
      wn = WN_CreateIoItem2(IOL_DOPE, wn, ad, NIL);
      return wn;
  } else {
     wn = WN_CreateIoItemN (IOL_DOPE, nd+2, NIL);
     address_fixup = WN_Create(OPCODE_make_op(OPR_ARRAY,Pointer_Mtype,MTYPE_V),
			       WN_kid_count(arr));
     WN_kid0(address_fixup) = WN_kid0(arr);
     WN_element_size(address_fixup) = WN_element_size(arr);


     for (i=2*nd, k=2; i > nd; i--, k++ ) {
	 WN_kid(address_fixup,i-nd) = WN_COPY_Tree(WN_kid(arr,i-nd));
	 index = WN_kid(arr,i);
	 if (WNOPR(index) == OPR_TRIPLET) 
	    index = WN_kid0(index);
	 pos = cwh_io_search_implied_do_index(index, impdo_set);
         if (pos >= 1) {
            impdo = impdo_set;
	    for (j=1; j < pos; j++)
	       impdo = Impdo_next(impdo);
            WN_kid(wn,k) = WN_CreateLda (opc_lda, 0,
					 Make_Pointer_Type(ST_type(Impdo_index(impdo))),
					 Impdo_index(impdo));
            new_index_expr = Substitute_1_For_Impdo_Index_Val(
				 WN_COPY_Tree(index), impdo);
            WN_kid(address_fixup,i) = new_index_expr;
         } else {
	    WN_kid(address_fixup,i) = WN_COPY_Tree(index);
	    WN_kid(wn,k) = WN_CreateIntconst ( OPC_I4INTCONST, 0);
         }
     }
     
     if (final_address != NULL) {
        WN_kid0(final_address) = address_fixup;
     } else if (offset != NULL) {
	final_address = WN_Add(Pointer_Mtype, address_fixup, offset);
     } else {
        final_address = address_fixup;
     }

     WN_kid0(wn) = cwh_dope_from_expression(arr, NULL, char_len, ty,
     craytype);
     WN_kid1(wn) = cwh_addr_find_address(arr);
     st = WN_st(WN_kid0(wn));
     cwh_addr_store_ST(st, 0, Be_Type_Tbl(Pointer_type), final_address);
     return wn;
  }
}

/*===================================================
 *
 * cwh_io_search_implied_do_index
 *
 * Checks if any ST in the array index <passed in tree>
 * is a member of the implied do index set. If yes, the 
 * position of the ST is returned, else, 0 is returned.
 *
 *====================================================
*/

static INT32 
cwh_io_search_implied_do_index(WN *tree, IMPDO_INFO *impdo_set)
{
  INT32 pos;
  ST *st;
  INT32 i;

  if (WNOPR(tree) == OPR_LDID) {
     st = WN_st(tree);
     if ( (pos = member (st, impdo_set)) != 0) 
        return pos;
  } else {
     for(i=0; i < WN_kid_count(tree); i++) {
        pos = cwh_io_search_implied_do_index(WN_kid(tree, i),impdo_set);
	if (pos != 0)
           return pos;
     }
  }
  return 0;
}

/*===================================================
 *
 * cwh_io_add_st_to_marked_set
 *
 * Adds a new ST to the visited set.
 *
 *====================================================
*/

static void
cwh_io_add_st_to_marked_set(ST *st) {
 
   MARKED_SET *new_marked_set;
 
   new_marked_set = (MARKED_SET *) malloc(sizeof(MARKED_SET));
   Marked_st(new_marked_set)  = st;
   Marked_next(new_marked_set) = marked_set;

   marked_set = new_marked_set; 
}

/*===================================================
 *
 * cwh_io_unmark
 *
 * Unmarks all ST's that were marked visited, since we
 * are done with this IO statement.
 *
 *====================================================
*/
 
static void 
cwh_io_unmark(void) {

   MARKED_SET *temp;

   while(marked_set)  {
      temp = marked_set;
      if (ST_auxst_visited(Marked_st(marked_set))) 
	Set_ST_auxst_visited(Marked_st(marked_set),FALSE);
      marked_set = Marked_next(marked_set);
      free(temp);
   }
}
      
/*===================================================
 *
 * Substitute_1_For_Impdo_Index_Val
 *
 * Searches for occurences of the implied do index in
 * the tree, and replaces the occurence by an INTCONST
 * node with val 1.
 *
 ====================================================
*/

static WN *
Substitute_1_For_Impdo_Index_Val(WN *tree, IMPDO_INFO *impdo)
{
  INT32 i;
#ifdef KEY /* Bug 10177 */
  OPCODE opc_intconst = OPC_I4INTCONST;
#else /* KEY Bug 10177 */
  OPCODE opc_intconst;
#endif /* KEY Bug 10177 */
  INT32 rtype;

  if (WN_operator_is(tree,OPR_LDID) && 
      (WN_st(tree) == Impdo_index(impdo)) ) {
     rtype = WN_rtype(tree);
     switch (rtype) {
       case MTYPE_I4:
         opc_intconst = OPC_I4INTCONST;
         break;
       case MTYPE_I8:
         opc_intconst = OPC_I8INTCONST;
         break;
       case MTYPE_U4:
         opc_intconst = OPC_U4INTCONST;
         break;
       case MTYPE_U8:
         opc_intconst = OPC_U8INTCONST;
         break;
       default:
          DevAssert((0),("Odd type"));
     }
     return (WN_CreateIntconst ( opc_intconst, 1));
  } else {
     for(i=0; i<WN_kid_count(tree); i++ ) {
       WN_kid(tree, i) = Substitute_1_For_Impdo_Index_Val(WN_kid(tree, i),
                                                          impdo);
     }
  }
  return tree;
}

/*===================================================
 *
 * cwh_io_split_io_statement
 *
 * Splits IO statement completely into a three call model.
 *
 *====================================================
*/
static void
cwh_io_split_io_statement(WN *tree) {
  WN **cilist;
  WN *wn_tmp;
  WN *item;
  WN *wn;
  INT32 ioitem_tmp;
  INT32 i;
  INT32 j;
  INT32 iolist_marker;
  INT32 num_cilist_items;
  INT32 iostatement;
  WN *kid0;
  INT32 flflag;
  INT32 new_flflag;

  iostatement = WN_io_statement(tree);

  for(i=0; i<WN_kid_count(tree); i++) {
     wn_tmp = WN_kid(tree,i);
     ioitem_tmp = WN_io_item(wn_tmp);
     if (ioitem_tmp >= IOL_ARRAY)
        break;
  }

  iolist_marker = i;

  num_cilist_items = i ;

  cilist = (WN **) malloc(sizeof (WN *) * num_cilist_items );

  for(i=0; i<iolist_marker; i++)
     cilist[i] = WN_kid(tree,i);

  wn = WN_CreateIo ( (IOSTATEMENT) iostatement, num_cilist_items);
  
  for(j=0; j<num_cilist_items; j++) {
     wn_tmp = cilist[j];
     ioitem_tmp =  WN_io_item(cilist[j]);
     if (ioitem_tmp == IOC_CR_FLFLAG) {
        kid0 = WN_kid0(wn_tmp);
        flflag = WN_const_val(kid0);
        new_flflag = flflag & 2;
        WN_kid(wn,j) = WN_CreateIoItem1 ( IOC_CR_FLFLAG,
                             WN_CreateIntconst ( OPC_I4INTCONST, new_flflag),
                             NIL);
     } else {
        WN_kid(wn,j) = WN_COPY_Tree(cilist[j]);
     } 
  } 
  cwh_block_append(wn);
    
     
  for(i=iolist_marker; i<WN_kid_count(tree); i++) {
     item = WN_kid(tree,i);
     cwh_io_split_io_items((IOSTATEMENT)iostatement, cilist, num_cilist_items, item);
  }

  wn = WN_CreateIo ( (IOSTATEMENT)iostatement, num_cilist_items);
  for(j=0; j<num_cilist_items; j++) {
     wn_tmp = cilist[j];
     ioitem_tmp =  WN_io_item(cilist[j]);
     if (ioitem_tmp == IOC_CR_FLFLAG) {
        kid0 = WN_kid0(wn_tmp);
        flflag = WN_const_val(kid0);
        new_flflag = flflag & 1;
        WN_kid(wn,j) = WN_CreateIoItem1 ( IOC_CR_FLFLAG,
                             WN_CreateIntconst ( OPC_I4INTCONST, new_flflag),
                             NIL);
     } else {
        WN_kid(wn,j) = WN_COPY_Tree(cilist[j]);
     }
  }
  cwh_block_append(wn);
  free(cilist);
}

/*===================================================
 *
 * cwh_io_split_io_items
 *
 * Called by cwh_io_split_iostatement to deal with an 
 * individual item. Recursively calls itself for implied 
 * do's.
 *
 *====================================================
*/

static void
cwh_io_split_io_items(IOSTATEMENT ios, WN **cilist,
                      INT32 num_cilist_items, WN *item) {
  WN *wn;
  INT32 ioitem_tmp;
  INT32 i;
  INT32 j;
  WN *top_label;
  WN *cont_label;
  TY_IDX ty;
  INT32 mtype;
  INT32 ntype;
  WN *load_index;
  WN *start;
  WN *step;
  WN *end;
  PREG_NUM pregnum;
  ST *pregst;
  WN *ad;
  WN *se;

  if (WN_io_item(item) == IOL_IMPLIED_DO) {
     top_label = cwh_io_create_new_label();
     cont_label = cwh_io_create_new_label();
     ty = ST_type(WN_st(WN_index(item)));
     if ( TY_kind(ty) != KIND_POINTER ) {
       ntype = mtype = TY_mtype(ty);
       if (ntype == MTYPE_I1 || ntype == MTYPE_I2)
         ntype = MTYPE_I4;
       load_index = WN_Ldid ( mtype, WN_idname_offset(WN_index(item)),
                              WN_st(WN_index(item)), ty );
       start = WN_Stid ( mtype, WN_idname_offset(WN_index(item)),
                         WN_st(WN_index(item)), ty, WN_start(item) );
       step = WN_Stid ( mtype, WN_idname_offset(WN_index(item)),
                        WN_st(WN_index(item)), ty,
                        WN_CreateExp2 ( OPCODE_make_op ( OPR_ADD, ntype,
                                                         MTYPE_V ),
                                        WN_COPY_Tree ( load_index ),
                                        WN_COPY_Tree(WN_step(item)) ));
     } else {
       ntype = mtype = TY_mtype(TY_pointed(ty));
       if (ntype == MTYPE_I1 || ntype == MTYPE_I2)
	  ntype = MTYPE_I4;
       load_index = WN_Iload ( mtype, 0, TY_pointed(ty),
                               WN_Ldid ( Pointer_type,
                                       WN_idname_offset(WN_index(item)),
                                         WN_st(WN_index(item)), ty ));
       start = WN_Istore ( mtype, 0, ty,
                           WN_Ldid ( Pointer_type,
                                     WN_idname_offset(WN_index(item)),
                                     WN_st(WN_index(item)), ty ),
                           WN_start(item) );
       step = WN_Istore ( mtype, 0, ty,
                          WN_Ldid ( Pointer_type,
                                    WN_idname_offset(WN_index(item)),
                                    WN_st(WN_index(item)), ty ),
                          WN_CreateExp2 ( OPCODE_make_op ( OPR_ADD,
								   ntype,
                                                           MTYPE_V ),
                                          WN_COPY_Tree ( load_index ),
                                          WN_COPY_Tree(WN_step(item)) ));
     }
     if ( WN_operator(WN_step(item)) == OPR_INTCONST ||
          WN_operator(WN_step(item)) == OPR_CONST ) {
       if ( ( WN_operator(WN_step(item)) == OPR_INTCONST &&
              WN_const_val(WN_step(item)) >= 0 ) ||
            ( WN_operator(WN_step(item)) == OPR_CONST &&
              STC_val(WN_st(WN_step(item))).vals.ival.v0 >= 0 ) )
         end = WN_LE ( ntype, load_index, WN_end(item) );
       else
         end = WN_GE ( ntype, load_index, WN_end(item) );
     } else {
       pregst = MTYPE_To_PREG ( Boolean_type );
       pregnum = Create_Preg ( Boolean_type, "stoptemp");
       cwh_block_append( WN_StidIntoPreg ( Boolean_type, pregnum,
                                               pregst,
                                       WN_GE ( ntype,
                                         WN_COPY_Tree ( WN_step(item) ),                                                 WN_Zerocon ( ntype ))));
       end = WN_Select ( Boolean_type,
                         WN_LdidPreg ( Boolean_type, pregnum ),
                         WN_LE ( ntype, load_index, WN_end(item) ),
                         WN_GE ( ntype, WN_COPY_Tree (load_index),
                                 WN_COPY_Tree (WN_end(item)) ));
     }
     cwh_block_append(start );
     
     cwh_block_append( WN_CreateGoto ( (ST_IDX) NULL,
						 WN_label_number(cont_label) ));
     cwh_block_append( top_label );
   
     for (i=4; i < WN_kid_count(item); i++)
        cwh_io_split_io_items(ios, cilist, num_cilist_items, 
                              WN_kid(item,i));

	cwh_block_append( step );
	cwh_block_append( cont_label );
	cwh_block_append( WN_CreateTruebr ( WN_label_number(top_label),
						     end ));

  } else {

     if (WN_io_item(item) == IOL_DOPE) {
	se = cwh_addr_find_section(WN_kid0(item), p_RETURN_SECTION);
	ad = cwh_addr_find_address(se);
	wn = cwh_dope_from_expression(WN_kid0(item), NULL, WN_kid2(item), WN_ty(item), WN_kid1(item));
	WN_Delete(item);
	item = WN_CreateIoItem2(IOL_DOPE, wn, ad, NIL);
     }

     wn = WN_CreateIo ( ios, num_cilist_items+1);
     for(j=0; j<num_cilist_items; j++) {
        ioitem_tmp = WN_io_item(cilist[j]);
        if (ioitem_tmp == IOC_CR_FLFLAG) {
          WN_kid(wn,j) = WN_CreateIoItem1 ( IOC_CR_FLFLAG,
                               WN_CreateIntconst ( OPC_I4INTCONST, 0),
                               NIL);
        } else {
          WN_kid(wn,j) = WN_COPY_Tree(cilist[j]);
        }
     }
    
      
     WN_kid(wn,num_cilist_items) = WN_COPY_Tree(item);
     cwh_block_append(wn);
  }
}

/*===================================================
 *
 * OPCODE_has_aux
 *
 * Return TRUE if operator is an LDID, or LDA or STID.
 *
 *====================================================
*/

static BOOL 
OPCODE_has_aux(const OPCODE opc)
{

  OPERATOR opr = OPCODE_operator(opc);
  return (opr == OPR_LDID || opr == OPR_STID || 
	  opr == OPR_LDA || opr == OPR_IDNAME);
}

/*===================================================
 *
 * cwh_io_create_new_label
 *
 * Create a new label and make a WN out of it.
 *
 *====================================================
*/

static WN *
cwh_io_create_new_label(void)
{
  LABEL_IDX label;

  (void) New_LABEL (CURRENT_SYMTAB, label);
  return WN_CreateLabel(NIL, label, 0, NIL);
}

/*===================================================
 *
 * cwh_io_cvt_tos_label_to_wn
 *
 * If TOS is a label, make a WN out of it.
 *
 * If flag is true, mark the label as LKIND_ASSIGNED.
 *
 *====================================================
*/
static WN *
cwh_io_cvt_tos_label_to_wn(BOOL flag)
{
  WN *wn;

  if (cwh_stk_get_class() == LB_item) {
    LABEL_IDX lbl;
    lbl = (LABEL_IDX) cwh_stk_pop_LB();
    if (flag)
       Set_LABEL_KIND(Label_Table[lbl], LKIND_ASSIGNED);
    wn = WN_CreateGoto (lbl);
  } else {
    cwh_stk_pop_whatever();
    wn = NULL;
  }
  return wn;
}
