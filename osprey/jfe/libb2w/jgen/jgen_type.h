#ifndef OSPREY_JGEN_TYPE_H
#define OSPREY_JGEN_TYPE_H

#include "json/json.h"
#include "jgen_include.h"
#include "json_reader.h"
#include "jgen_global.h"

#include <iostream>
#include <string>
#include <map>

using std::string;
using std::cerr;
using std::cout;
using std::endl;
using std::map;

namespace JGEN
{
    class JGEN_TY {
     private:
      static std::map<U32U, TY_IDX> generatedType;

     public:

      static JGEN_Typetree_Base * typetree;

      static JGEN_SymbolTree_Base * symtree;

      static TY_IDX getExistTypeIdx(U32U jIndex);

      static TY_IDX createFunction (U32U typenode);

      static TY_IDX Get_TY (U32U jIndex)
      {
        TY_IDX idx = 0;
        cout << " -- Getting TY for " << typetree->getNameString(jIndex) << endl;
        switch (typetree->getKind(jIndex))
        {
            case JGEN_TYPE_VOID:
            {
                idx = MTYPE_To_TY (MTYPE_V);
                break;
            }
            case JGEN_TYPE_BOOLEAN:
            case JGEN_TYPE_INTEGER:
            case JGEN_TYPE_OFFSET:
            {
                create_integer (jIndex);
                break;
            }
            case JGEN_TYPE_ENUMERATION:
                create_enumeration (jIndex);
                break;
            case JGEN_TYPE_CHAR:
            case JGEN_TYPE_BYTE:
            {
                create_char (jIndex);
                break;
            }
            case JGEN_TYPE_FLOAT:
            case JGEN_TYPE_DOUBLE:
            case JGEN_TYPE_NUMBER:
              {
                create_floating (jIndex);
                break;
              }
            case JGEN_TYPE_HANDLE:
            case JGEN_TYPE_POINTER:
            {
                idx = Be_Type_Tbl(Pointer_Size == 8 ? MTYPE_I8 : MTYPE_I4);
                break;
            }
            case JGEN_TYPE_ARRAY:
            {
                createArray (jIndex);
                break;
            }
            case JGEN_TYPE_RECORD:
            case JGEN_TYPE_UNION: {
              /** TOOD RECORD, UNION **/
              //cerr << "Unfinished Type : Union" << endl;
              createClassOrUnion(jIndex);
              break;
            }
            case JGEN_TYPE_METHOD:
            case JGEN_TYPE_FUNCTION: {
              createFunction(jIndex);
              break;
            }
            default:
              cerr << "Unexpected Type KIND : " << typetree->getKind(jIndex);

        }
        if (typetree->isConst (jIndex))
          Set_TY_is_const (idx);
        if (typetree->isVolatile (jIndex))
          Set_TY_is_volatile (idx);
        if (typetree->isRestrict (jIndex))
          Set_TY_is_restrict (idx);

      }

      static TY_IDX create_char (U32U jIndex)
      {
        MTYPE_t mtype;
        TY_IDX idx = 0;
        mtype = (typetree->isUnsigned (jIndex) ? MTYPE_U1 : MTYPE_I1);
        idx = MTYPE_To_TY (mtype);    // use predefined type
      }

      static TY_IDX create_floating (U32U jIndex)
      {
        MTYPE_t mtype;
        TY_IDX idx = 0;
        switch (typetree->get_type_size(jIndex))
          {
            case 4:
              mtype = MTYPE_F4;
            break;
            case 8:
              mtype = MTYPE_F8;
            break;
          }
        idx = MTYPE_To_TY (mtype);
      }

      static TY_IDX create_enumeration (U32U jIndex)
      {
        MTYPE_t mtype;
        TY_IDX idx = 0;
        switch (typetree->get_type_size(jIndex))
          {
            case 1: // bug 14445
              mtype = (typetree->isUnsigned (jIndex) ? MTYPE_U1 :
                       MTYPE_I1);
            break;
            case 2: // bug 14445
              mtype = (typetree->isUnsigned (jIndex) ? MTYPE_U2 :
                       MTYPE_I2);
            break;
            case 8: // bug 500
              mtype = (typetree->isUnsigned (jIndex) ? MTYPE_U8 :
                       MTYPE_I8);
            break;
            default:
              mtype = (typetree->isUnsigned (jIndex) ? MTYPE_U4 :
                       MTYPE_I4);
          }
        idx = MTYPE_To_TY (mtype);
        return idx;
      }

      static TY_IDX create_integer (U32U jIndex)
      {
        TY_IDX idx = 0;
        int  mtype;
        switch (typetree->get_type_size(jIndex))
        {
            case 1:
              mtype = MTYPE_I1;
            break;
            case 2:
              mtype = MTYPE_I2;
            break;
            case 4:
              mtype = MTYPE_I4;
            break;
            case 8:
              mtype = MTYPE_I8;
            break;
        }
        if (typetree->isUnsigned (jIndex))
        {
            mtype = MTYPE_complement(mtype);
        }
        idx = MTYPE_To_TY (mtype);
        if (TARGET_64BIT)
        {
            // TODO:Change back to align
            Set_TY_align (idx, 1);
            //    Set_TY_align(idx, align);
        }
      }

      static TY_IDX createArray (U32U jIndex)
      {
        TY_IDX idx = 0;
        TY &ty = (idx == TY_IDX_ZERO) ? New_TY (idx) : Ty_Table[idx];
        Clear_TY_is_incomplete (idx);
        TY_Init (ty, typetree->get_type_size(jIndex), KIND_ARRAY, MTYPE_M,
                 Save_Str (typetree->getNameString(jIndex).c_str ()));

        // for the anonymoust array
        if (typetree->getNameString(jIndex) == "NULL")
          Set_TY_anonymous (ty);

        Set_TY_etype (ty, Get_TY(typetree->get_element_type (jIndex)));
        Set_TY_align (idx, TY_align (TY_etype (ty)));

        // For GNU VLS (Variable length array in struct),
        // the size and upper boundary is expression.
        // If the TYPE_TY_IDX(type_tree) is not set, when
        // expanding the TY's size, it will fall into a infinite
        // recursion if the type_tree is referenced in the
        // size expression. So we set the TYPE_TY_IDX here.
        if (typetree->isReadonly (jIndex))
          Set_TY_is_const (idx);
        if (typetree->isVolatile (jIndex))
          Set_TY_is_volatile (idx);
        if (typetree->isRestrict (jIndex))
          Set_TY_is_restrict (idx);

        // assumes 1 dimension
        // nested arrays are treated as arrays of arrays
        ARB_HANDLE arb = New_ARB ();
        ARB_Init (arb, 0, 0, 0);
        Set_TY_arb (ty, arb);
        Set_ARB_first_dimen (arb);
        Set_ARB_last_dimen (arb);
        Set_ARB_dimension (arb, 1);

        if (typetree->get_type_size (typetree->get_element_type(jIndex)) == 0)
          return; // anomaly:  type will never be needed

        // =================== Array stride ======================
        if (!typetree->isVariableSize (jIndex))
          {
            Set_ARB_const_stride (arb);
            Set_ARB_stride_val (arb, typetree->get_element_size_unit (jIndex)/*gs_get_integer_value(gs_type_size_unit(gs_tree_type(type_tree)))*/);
          }
        else if (!JGEN::Config::expanding_function_definition &&
                 JGEN::Config::processing_function_prototypes)
          {
            Set_ARB_const_stride (arb);
            // dummy stride val 4
            Set_ARB_stride_val (arb, 4);
            Set_TY_is_incomplete (idx);
          }
        else
          {
            /* TODO:Make this available
            WN *swn;
            swn = WGEN_Expand_Expr(gs_type_size_unit(gs_tree_type(type_tree)));
            if (WN_opcode(swn) == OPC_U4I4CVT ||
                WN_opcode(swn) == OPC_U8I8CVT) {
                swn = WN_kid0(swn);
            }
            // In the event that swn operator is not
            // OPR_LDID, save expr node swn
            // and use LDID of that stored address as swn.
            // Copied from Wfe_Save_Expr in wfe_expr.cxx
            if (WN_operator(swn) != OPR_LDID) {
                TYPE_ID mtype = WN_rtype(swn);
                TY_IDX ty_idx = MTYPE_To_TY(mtype);
                ST *st;
                st = Gen_Temp_Symbol(ty_idx, "__save_expr");
                WGEN_add_pragma_to_enclosing_regions(WN_PRAGMA_LOCAL, st);
                WGEN_Set_ST_Addr_Saved(swn);
                swn = WN_Stid(mtype, 0, st, ty_idx, swn);
                WGEN_Stmt_Append(swn, Get_Srcpos());
                swn = WN_Ldid(mtype, 0, st, ty_idx);
            }
            FmtAssert (WN_operator(swn) == OPR_LDID,
                       ("stride operator for VLA not LDID"));
            ST *st = WN_st(swn);
            TY_IDX ty_idx = ST_type(st);
            WN *wn = WN_CreateXpragma (WN_PRAGMA_COPYIN_BOUND,
                                       (ST_IDX) NULL, 1);
            WN_kid0(wn) = WN_Ldid(TY_mtype(ty_idx), 0, st, ty_idx);
            WGEN_Stmt_Append(wn, Get_Srcpos());
            Clear_ARB_const_stride(arb);
            Set_ARB_stride_var(arb, (ST_IDX) ST_st_idx(st));
            Clear_TY_is_incomplete(idx);*/
          }

        // ================= Array lower bound =================
        Set_ARB_const_lbnd (arb);
        Set_ARB_lbnd_val (arb, 0);

        // ================= Array upper bound =================
        if (typetree->get_type_size (typetree->get_element_type(jIndex)) != 0)
          {
            // For Zero-length arrays, TYPE_MAX_VALUE tree is NULL
            if (!typetree->isZeroMaxValue (typetree->get_element_type (jIndex)))
              {
                Set_ARB_const_ubnd (arb);
                Set_ARB_ubnd_val (arb, 0xffffffff);
              }
            else if (typetree->isSizeMaxValueConstant (jIndex))
              {
                Set_ARB_const_ubnd (arb);
                Set_ARB_ubnd_val (arb, typetree->get_max_value (typetree->get_element_type(jIndex)));
              }
            else if (!JGEN::Config::expanding_function_definition &&
                     JGEN::Config::processing_function_prototypes)
              {
                Set_ARB_const_ubnd (arb);
                // dummy upper bound 8
                Set_ARB_ubnd_val (arb, 8);
                Set_TY_is_incomplete (idx);
              }
            else
              {
                // Get WN <- Expr
                /* TODO:Make this available
                WN *uwn = WGEN_Expand_Expr();
                if (WN_opcode(uwn) == OPC_U4I4CVT ||
                    WN_opcode(uwn) == OPC_U8I8CVT) {
                    uwn = WN_kid0(uwn);
                }
                ST *st;
                TY_IDX ty_idx;
                WN *wn;
                if (WN_operator(uwn) != OPR_LDID) {
                    ty_idx = MTYPE_To_TY(WN_rtype(uwn));
                    st = Gen_Temp_Symbol(ty_idx, "__vla_bound");
                    WGEN_add_pragma_to_enclosing_regions(WN_PRAGMA_LOCAL, st);
                    wn = WN_Stid(TY_mtype(ty_idx), 0, st, ty_idx, uwn);
                    WGEN_Stmt_Append(wn, Get_Srcpos());
                } else {
                    st = WN_st(uwn);
                    ty_idx = ST_type(st);
                }

                wn = WN_CreateXpragma (WN_PRAGMA_COPYIN_BOUND, (ST_IDX) NULL, 1);
                // Create An Ldid for the array bound to check for sizes.
                WN_kid0(wn) = WN_Ldid(TY_mtype(ty_idx), 0, st, ty_idx);
                WGEN_Stmt_Append(wn, Get_Srcpos());

                Clear_ARB_const_ubnd(arb);
                Set_ARB_ubnd_var(arb, ST_st_idx(st));
                Clear_TY_is_incomplete(idx);*/
              }
          }
        else
          { /* situation : type size == 0 */
            Clear_ARB_const_ubnd (arb);
            Set_ARB_ubnd_val (arb, 0);
          }

        /** TODO Array **/
        cerr << "Unfinished Type : Array";
      }

      static void Get_Fields_For_Class(U32U index);

      static TY_IDX createClassOrUnion(U32U jIndex);

    };

}

#endif

#ifdef USE_ME_NO

// idx is non-zero only for RECORD and UNION, when there is forward declaration
extern TY_IDX
Create_TY_For_Tree (gs_t type_tree, TY_IDX idx)
{

	if(gs_tree_code(type_tree) == GS_ERROR_MARK)
	   return idx;

	TY_IDX orig_idx = idx;
	if(gs_tree_code_class(type_tree) != GS_TCC_TYPE) {
	  DevWarn("Bad tree class passed to Create_TY_For_Tree %c",
		gs_tree_code_class(type_tree));
          return idx;
	}


#ifdef KEY
	UINT align = gs_type_align(type_tree) / BITSPERBYTE;
#endif
	// for typedefs get the information from the base type
	if (gs_type_name(type_tree) &&
	    idx == 0 &&
	    (gs_tree_code(type_tree) == GS_RECORD_TYPE ||
	     gs_tree_code(type_tree) == GS_UNION_TYPE) &&
	    gs_tree_code(gs_type_name(type_tree)) == GS_TYPE_DECL &&
	    gs_type_main_variant(type_tree) != type_tree) {
		idx = Get_TY (gs_type_main_variant(type_tree));
		if (gs_type_readonly(type_tree))
			Set_TY_is_const (idx);
		if (gs_type_volatile(type_tree))
			Set_TY_is_volatile (idx);
#ifdef KEY
		if (gs_type_restrict(type_tree))
			Set_TY_is_restrict (idx);
		Set_TY_align (idx, align); // bug 10533
#endif
		TYPE_TY_IDX(type_tree) = idx;
		if(Debug_Level >= 2) {
#ifdef KEY // bug 11782
		  defer_DST_type(type_tree, idx, orig_idx);
#else
		  DST_INFO_IDX dst = Create_DST_type_For_Tree(type_tree,
			idx,orig_idx);
		  TYPE_DST_IDX(type_tree) = dst;
#endif
	        }
		TYPE_FIELD_IDS_USED(type_tree) =
			TYPE_FIELD_IDS_USED(gs_type_main_variant(type_tree));
		return idx;
	}

	TYPE_ID mtype;
	INT64 tsize;
	BOOL variable_size = FALSE;
        
	gs_t type_size = gs_type_size(type_tree);
        gs_string_t type_mode  = gs_type_mode (type_tree);
#ifndef KEY
	UINT align = gs_type_align(type_tree) / BITSPERBYTE;
#endif
	if (type_size == NULL) {
		// incomplete structs have 0 size.  Similarly, 'void' is
                // an incomplete type that can never be completed.
		FmtAssert(gs_tree_code(type_tree) == GS_ARRAY_TYPE 
			|| gs_tree_code(type_tree) == GS_ENUMERAL_TYPE
			|| gs_tree_code(type_tree) == GS_UNION_TYPE
			|| gs_tree_code(type_tree) == GS_RECORD_TYPE
			|| gs_tree_code(type_tree) == GS_LANG_TYPE
			|| gs_tree_code(type_tree) == GS_FUNCTION_TYPE
			|| gs_tree_code(type_tree) == GS_VOID_TYPE,
			  ("Create_TY_For_Tree: type_size NULL for non ARRAY/RECORD/VOID, type is %d",
                           (int) gs_tree_code(type_tree)));
		tsize = 0;
	}
	else {
		if (gs_tree_code(type_size) != GS_INTEGER_CST) {
			if (gs_tree_code(type_tree) == GS_ARRAY_TYPE) {
				DevWarn ("Encountered VLA at line %d", lineno);
				tsize = 0;
			}
			else {
			// bugs 943, 11277, 10506
#if defined(TARG_SL)
				ErrMsg(EC_Unimplemented_Feature, "variable-length structure",
				  Orig_Src_File_Name?Orig_Src_File_Name:Src_File_Name, lineno);
#else
				DevWarn ("Encountered variable-length structure at line %d", lineno);
				tsize = -1;
#endif
			}
			variable_size = TRUE;
		}
		else
#ifdef KEY		// bug 3045
			tsize = (gs_get_integer_value(type_size) + BITSPERBYTE - 1)
				  / BITSPERBYTE;
#else
			tsize = gs_get_integer_value(type_size) / BITSPERBYTE;
#endif
	}
	switch (gs_tree_code(type_tree)) {
	case GS_VOID_TYPE:
	case GS_LANG_TYPE: // unknown type
		idx = MTYPE_To_TY (MTYPE_V);	// use predefined type
		break;
	case GS_BOOLEAN_TYPE:
	case GS_INTEGER_TYPE:
	case GS_OFFSET_TYPE:
		switch (tsize) {
		case 1:  mtype = MTYPE_I1;  break;
		case 2:  mtype = MTYPE_I2;  break;
		case 4:  mtype = MTYPE_I4;  break;
		case 8:  mtype = MTYPE_I8;  break;
#if !defined(TARG_X8664) && !defined(TARG_MIPS)  // Bug 12358
#ifdef _LP64
		case 16:  mtype = MTYPE_I8; break;
#endif /* _LP64 */
#else 
	        // needed for compiling variable length array
		// as in gcc.c-torture/execute/920929-1.c
		// we need to fix the rest of the compiler 
		// with _LP64 but seems to work fine without.	
		case 16:  mtype = MTYPE_I8; break;
#endif /* KEY */
		default:  FmtAssert(FALSE,
                                    ("Get_TY unexpected size %d", tsize));
		}
		if (gs_decl_unsigned(type_tree)) {
			mtype = MTYPE_complement(mtype);
		}
#ifdef KEY
		if (lookup_attribute("may_alias",gs_type_attributes(type_tree)))
		{
		  // bug 9975: Handle may_alias attribute, we need to create
		  // a new type to which we can attach the flag.
		  TY &ty = New_TY (idx);
		  TY_Init (ty, tsize, KIND_SCALAR, mtype, 
		           Save_Str(Get_Name(gs_type_name(type_tree))) );
		  Set_TY_no_ansi_alias (ty);
#if defined(TARG_SL)
		  // for -m32, it is not the predefined type, alignment shoule be set.
		  // Corresponding to following code about bug#2932.
		  if (!TARGET_64BIT)  
		    Set_TY_align (idx, align);
#endif
 		} else
#endif
		idx = MTYPE_To_TY (mtype);	// use predefined type
#if defined(TARG_X8664) || defined(TARG_SL)
		/* At least for -m32, the alignment is not the same as the data
		   type's natural size. (bug#2932)
		*/
		if( TARGET_64BIT )
#endif // TARG_X8664
		  Set_TY_align (idx, align);
		break;
	case GS_CHAR_TYPE:
		mtype = (gs_decl_unsigned(type_tree) ? MTYPE_U1 : MTYPE_I1);
		idx = MTYPE_To_TY (mtype);	// use predefined type
		break;
	case GS_ENUMERAL_TYPE:
#ifdef KEY
		switch (tsize) {
		  case 1: // bug 14445
		        mtype = (gs_decl_unsigned(type_tree) ? MTYPE_U1 :
		                                               MTYPE_I1);
		        break;
		  case 2: // bug 14445
		        mtype = (gs_decl_unsigned(type_tree) ? MTYPE_U2 :
		                                               MTYPE_I2);
		        break;
		  case 8: // bug 500
		        mtype = (gs_decl_unsigned(type_tree) ? MTYPE_U8 :
		                                               MTYPE_I8);
		        break;
		  default:
		        mtype = (gs_decl_unsigned(type_tree) ? MTYPE_U4 :
		                                               MTYPE_I4);
		}
#else
		mtype = (gs_decl_unsigned(type_tree) ? MTYPE_U4 : MTYPE_I4);
#endif
		idx = MTYPE_To_TY (mtype);	// use predefined type
		break;
	case GS_REAL_TYPE:
		switch (tsize) {
		case 4:  mtype = MTYPE_F4; break;
		case 8:  mtype = MTYPE_F8; break;
#if defined(TARG_IA64) || defined(TARG_X8664)
                // the correct way to get the type is from type mode
                // so it can support float_128
                case 12:
                  FmtAssert(!TARGET_64BIT, ("Get_TY unexpected size"));
                  // fall through
                case 16: 
                  {
#ifdef SUPPORT_FLOAT128
                     if (strcmp("XF", type_mode) == 0)
                       mtype = MTYPE_F10; 
                     else if (strcmp("TF",type_mode) == 0)
                       mtype = MTYPE_F16;
                     else 
                       FmtAssert(FALSE, ("Get_TY unexpected size"));
#else
                     mtype = MTYPE_F10;
#endif
                     break;
                  }
#elif defined(TARG_MIPS) || defined(TARG_IA32) 
		case 16: mtype = MTYPE_FQ; break;
#endif /* TARG_MIPS */
		default:  FmtAssert(FALSE, ("Get_TY unexpected size"));
		}
		idx = MTYPE_To_TY (mtype);	// use predefined type
		break;
	case GS_COMPLEX_TYPE:
		switch (tsize) {
		case 2: 
		case 4: ErrMsg (EC_Unsupported_Type, "Complex integer");
		case  8:  mtype = MTYPE_C4; break;
		case 16:  mtype = MTYPE_C8; break;
#if defined(TARG_IA64) || defined(TARG_X8664)
                // the correct way to get the type is from type mode
                // so it can support float_128
                case 24:
                  FmtAssert(!TARGET_64BIT, ("Get_TY unexpected size"));
                  // fall through
                case 32: 
                  {
#ifdef SUPPORT_FLOAT128
                     if (strcmp("XC", type_mode) == 0)
                       mtype = MTYPE_C10; 
                     else if (strcmp("TC",type_mode) == 0)
                       mtype = MTYPE_C16;
                     else 
                       FmtAssert(FALSE, ("Get_TY unexpected size"));
#else
                     mtype = MTYPE_C10;
#endif
                     break;
                  }
#elif defined(TARG_MIPS) || defined(TARG_IA32) 
		case 32: mtype = MTYPE_CQ; break;
#endif /* TARG_MIPS */
		default:  FmtAssert(FALSE, ("Get_TY unexpected size"));
		}
		idx = MTYPE_To_TY (mtype);	// use predefined type
		break;
	case GS_POINTER_TYPE:
		if (gs_type_ptrmem_p(type_tree)) {
			// pointer to member
			idx = Be_Type_Tbl(Pointer_Size == 8 ? MTYPE_I8 : MTYPE_I4);
			break;
		}
		/* FALLTHRU */
	case GS_REFERENCE_TYPE:
		idx = Make_Pointer_Type (Get_TY (gs_tree_type(type_tree)));
		Set_TY_align (idx, align);
		break;
	case GS_ARRAY_TYPE:
		{	// new scope for local vars
#ifdef KEY /* bug 8346 */
		TY &ty = (idx == TY_IDX_ZERO) ? New_TY(idx) : Ty_Table[idx];
		Clear_TY_is_incomplete (idx);
#else
		TY &ty = New_TY (idx);
#endif
		TY_Init (ty, tsize, KIND_ARRAY, MTYPE_M, 
			Save_Str(Get_Name(gs_type_name(type_tree))) );
                // for the anonymoust array
                if (gs_type_name(type_tree) == NULL)
                    Set_TY_anonymous(ty);
		Set_TY_etype (ty, Get_TY (gs_tree_type(type_tree)));
		Set_TY_align (idx, TY_align(TY_etype(ty)));

		// For GNU VLS (Variable length array in struct),
		// the size and upper boundary is expression.
		// If the TYPE_TY_IDX(type_tree) is not set, when
		// expanding the TY's size, it will fall into a infinite
		// recursion if the type_tree is referenced in the
		// size expression. So we set the TYPE_TY_IDX here.
		if (gs_type_readonly(type_tree))
                    Set_TY_is_const (idx);
		if (gs_type_volatile(type_tree))
                    Set_TY_is_volatile (idx);
		if (gs_type_restrict(type_tree))
                    Set_TY_is_restrict (idx);
	        TYPE_TY_IDX(type_tree) = idx;

		// assumes 1 dimension
		// nested arrays are treated as arrays of arrays
		ARB_HANDLE arb = New_ARB ();
		ARB_Init (arb, 0, 0, 0);
		Set_TY_arb (ty, arb);
		Set_ARB_first_dimen (arb);
		Set_ARB_last_dimen (arb);
		Set_ARB_dimension (arb, 1);
		if (gs_type_size(gs_tree_type(type_tree)) == 0)
			break; // anomaly:  type will never be needed

		// =================== Array stride ======================
		if (gs_tree_code(gs_type_size(gs_tree_type(type_tree))) == GS_INTEGER_CST) {
			Set_ARB_const_stride (arb);
			Set_ARB_stride_val (arb, 
				gs_get_integer_value (gs_type_size_unit(gs_tree_type(type_tree))));
		}
#ifdef KEY /* bug 8346 */
		else if (!expanding_function_definition &&
		         processing_function_prototype)
		{
			Set_ARB_const_stride (arb);
			// dummy stride val 4
			Set_ARB_stride_val (arb, 4);
			Set_TY_is_incomplete (idx);
		}
#endif
		else {
			WN *swn;
			swn = WGEN_Expand_Expr (gs_type_size_unit(gs_tree_type(type_tree)));
			if (WN_opcode (swn) == OPC_U4I4CVT ||
			    WN_opcode (swn) == OPC_U8I8CVT) {
				swn = WN_kid0 (swn);
			}
#ifdef KEY
			// In the event that swn operator is not 
			// OPR_LDID, save expr node swn 
			// and use LDID of that stored address as swn.
			// Copied from Wfe_Save_Expr in wfe_expr.cxx
			if (WN_operator (swn) != OPR_LDID) {

			  TYPE_ID   mtype   = WN_rtype(swn);
			  TY_IDX    ty_idx  = MTYPE_To_TY(mtype);
			  ST       *st;
			  st = Gen_Temp_Symbol (ty_idx, "__save_expr");
#ifdef FE_GNU_4_2_0
			  WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, st);
#endif
			  WGEN_Set_ST_Addr_Saved (swn);
			  swn = WN_Stid (mtype, 0, st, ty_idx, swn);
			  WGEN_Stmt_Append (swn, Get_Srcpos());
			  swn = WN_Ldid (mtype, 0, st, ty_idx);
			}
#endif /* KEY */
			FmtAssert (WN_operator (swn) == OPR_LDID,
				("stride operator for VLA not LDID"));
			ST *st = WN_st (swn);
			TY_IDX ty_idx = ST_type (st);
			WN *wn = WN_CreateXpragma (WN_PRAGMA_COPYIN_BOUND,
						   (ST_IDX) NULL, 1);
			WN_kid0 (wn) = WN_Ldid (TY_mtype (ty_idx), 0, st, ty_idx);
			WGEN_Stmt_Append (wn, Get_Srcpos());
			Clear_ARB_const_stride (arb);
			Set_ARB_stride_var (arb, (ST_IDX) ST_st_idx (st));
#ifdef KEY /* bug 8346 */
			Clear_TY_is_incomplete (idx);
#endif
		}

		// ================= Array lower bound =================
		Set_ARB_const_lbnd (arb);
		Set_ARB_lbnd_val (arb, 0);

		// ================= Array upper bound =================
		if (type_size) {
#ifdef KEY
		    // For Zero-length arrays, TYPE_MAX_VALUE tree is NULL
		    if (!gs_type_max_value (gs_type_domain (type_tree))) {
			Set_ARB_const_ubnd (arb);
			Set_ARB_ubnd_val (arb, 0xffffffff);
		    } else
#endif /* KEY */
		    if (gs_tree_code(gs_type_max_value (gs_type_domain (type_tree))) ==
			GS_INTEGER_CST) {
			Set_ARB_const_ubnd (arb);
			Set_ARB_ubnd_val (arb, gs_get_integer_value (
				gs_type_max_value (gs_type_domain (type_tree)) ));
		    }
#ifdef KEY /* bug 8346 */
		    else if (!expanding_function_definition &&
		             processing_function_prototype) {
			Set_ARB_const_ubnd (arb);
			// dummy upper bound 8
			Set_ARB_ubnd_val (arb, 8);
			Set_TY_is_incomplete (idx);
		    }
#endif
		    else {
			WN *uwn = WGEN_Expand_Expr (gs_type_max_value (gs_type_domain (type_tree)) );
			if (WN_opcode (uwn) == OPC_U4I4CVT ||
			    WN_opcode (uwn) == OPC_U8I8CVT) {
				uwn = WN_kid0 (uwn);
			}
			ST *st;
			TY_IDX ty_idx;
			WN *wn;
			if (WN_operator (uwn) != OPR_LDID) {
				ty_idx  = MTYPE_To_TY(WN_rtype(uwn));
				st = Gen_Temp_Symbol (ty_idx, "__vla_bound");
#ifdef FE_GNU_4_2_0
			  	WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, st);
#endif
				wn = WN_Stid (TY_mtype (ty_idx), 0, st, ty_idx, uwn);
				WGEN_Stmt_Append (wn, Get_Srcpos());
			}
			else {
				st = WN_st (uwn);
				ty_idx = ST_type (st);
			}
			wn = WN_CreateXpragma (WN_PRAGMA_COPYIN_BOUND, (ST_IDX) NULL, 1);
			WN_kid0 (wn) = WN_Ldid (TY_mtype (ty_idx), 0, st, ty_idx);
			WGEN_Stmt_Append (wn, Get_Srcpos());
			Clear_ARB_const_ubnd (arb);
			Set_ARB_ubnd_var (arb, ST_st_idx (st));
#ifdef KEY /* bug 8346 */
			Clear_TY_is_incomplete (idx);
#endif
		    }
		}
		else {
			Clear_ARB_const_ubnd (arb);
			Set_ARB_ubnd_val (arb, 0);
		}

		// ==================== Array size ====================
		if (variable_size) {
#ifdef KEY /* bug 8346 */
		   if (!expanding_function_definition &&
		       processing_function_prototype) {
		     Set_TY_is_incomplete (idx);
		   }
		   else
#endif
		   {
			WN *swn, *wn;
			swn = WGEN_Expand_Expr (gs_type_size_unit(type_tree));
			if (TY_size(TY_etype(ty))) {
				if (WN_opcode (swn) == OPC_U4I4CVT ||
				    WN_opcode (swn) == OPC_U8I8CVT) {
					swn = WN_kid0 (swn);
				}
#ifdef KEY
				// In the event that swn operator is not 
				// OPR_LDID, save expr node swn 
				// and use LDID of that stored address as swn.
				// Copied from Wfe_Save_Expr in wfe_expr.cxx
				if (WN_operator (swn) != OPR_LDID) {
				  TYPE_ID   mtype   = WN_rtype(swn);
				  TY_IDX    ty_idx  = MTYPE_To_TY(mtype);
				  ST       *st;
				  st = Gen_Temp_Symbol (ty_idx, "__save_expr");
#ifdef FE_GNU_4_2_0
			  	  WGEN_add_pragma_to_enclosing_regions (WN_PRAGMA_LOCAL, st);
#endif
				  WGEN_Set_ST_Addr_Saved (swn);
				  swn = WN_Stid (mtype, 0, st, ty_idx, swn);
				  WGEN_Stmt_Append (swn, Get_Srcpos());
				  swn = WN_Ldid (mtype, 0, st, ty_idx);
				}
#endif /* KEY */
				FmtAssert (WN_operator (swn) == OPR_LDID,
					("size operator for VLA not LDID"));
				ST *st = WN_st (swn);
				TY_IDX ty_idx = ST_type (st);
				TYPE_ID mtype = TY_mtype (ty_idx);

				wn = WN_Stid (mtype, 0, st, ty_idx, swn);
				WGEN_Stmt_Append (wn, Get_Srcpos());
			}
#ifdef KEY /* bug 8346 */
			Clear_TY_is_incomplete (idx);
#endif
		   }
		}
		} // end array scope
		break;
	case GS_RECORD_TYPE:
	case GS_UNION_TYPE:
		{	// new scope for local vars

		TY &ty = (idx == TY_IDX_ZERO) ? New_TY(idx) : Ty_Table[idx];
#ifdef KEY
		// Must create DSTs in the order that the records are declared,
		// in order to preserve their scope.  Bug 4168.
		if (Debug_Level >= 2)
		  defer_DST_type(type_tree, idx, orig_idx);

		// GCC 3.2 pads empty structures with a fake 1-byte field.
		// These structures should have tsize = 0.
		if (tsize != 0 &&
		    // is_empty_class assumes non-null CLASSTYPE_SIZE
		    // check if it has lang-specific data
		    gs_type_lang_specific(type_tree) &&
		    // check if it has its base version set
		    gs_classtype_as_base(type_tree) &&
		    gs_classtype_size(type_tree) &&
		    gs_is_empty_class(type_tree))
			tsize = 0;
#endif	// KEY
		TY_Init (ty, tsize, KIND_STRUCT, MTYPE_M, 
			Save_Str(Get_Name(gs_type_name(type_tree))) );

		if (gs_type_name(type_tree) == NULL || gs_type_anonymous_p(type_tree))
		    Set_TY_anonymous(ty);

		if (gs_tree_code(type_tree) == GS_UNION_TYPE) {
			Set_TY_is_union(idx);
		}
#ifdef KEY
                // gs_aggregate_value_p is only set for c++
		if (gs_aggregate_value_p(type_tree)) {
			Set_TY_return_in_mem(idx);
		}
#endif
		if (align == 0) align = 1;	// in case incomplete type
		Set_TY_align (idx, align);
		// set idx now in case recurse thru fields
		TYPE_TY_IDX(type_tree) = idx;
		Do_Base_Types (type_tree);

		// Process nested structs and static data members first

                for (gs_t field =  get_first_real_or_virtual_field (type_tree);
                          field;
                          field = next_real_field(type_tree, field)) {
		  	Set_TY_content_seen(idx); // bug 10851
                        if (gs_tree_code(field) == GS_TYPE_DECL ||
			    gs_tree_code(field) == GS_FIELD_DECL) {
                                gs_t field_type = gs_tree_type(field);
				if ((gs_tree_code(field_type) == GS_RECORD_TYPE ||
				     gs_tree_code(field_type) == GS_UNION_TYPE) &&
                                    field_type != type_tree) {
#ifdef KEY
					// Defer typedefs within class
					// declarations to avoid circular
					// declaration dependences.  See
					// example in bug 5134.
                                        if (gs_tree_code(field) == GS_TYPE_DECL)
					  defer_decl(field_type);
                                        else
#endif
                                        Get_TY(field_type);
				}
                        }
#ifdef KEY	// Defer expansion of static vars until all the fields in
		// _every_ struct are laid out.  Consider this code (see
		// bug 3044):
		//  struct A
		//    struct B *p
		//  struct B
		//    static struct A *q = ...	// static data member with
		//                              // initializer
		// We cannot expand static member vars while expanding the
		// enclosing stuct, for the following reason:  Expansion of
		// struct A leads to expansion of p, which leads to the
		// expansion of struct B, which leads to the expansion of q and
		// q's initializer.  The code that expands the initializer goes
		// through the fields of struct A, but these fields are not yet
		// completely defined, and this will cause kg++fe to die.
		//
		// The solution is the delay all static var expansions until
		// the very end.
			else if (gs_tree_code(field) == GS_VAR_DECL)
				defer_decl(field);
#else
			else if (gs_tree_code(field) == GS_VAR_DECL)
				WGEN_Expand_Decl(field, TRUE);
#endif
			else if (gs_tree_code(field) == GS_TEMPLATE_DECL)
				WGEN_Expand_Decl(field, TRUE);
	        }

  		Set_TY_fld (ty, FLD_HANDLE());
		FLD_IDX first_field_idx = Fld_Table.Size ();
		gs_t field;
		gs_t method = gs_type_methods(type_tree);
		FLD_HANDLE fld;
		INT32 next_field_id = 1;

#ifdef KEY
		// In GCC 4, the same tree node representing a vtable ptr field
		// can appear in different derived classes.  As a result,
		// DECL_FIELD_ID(field) can't be used to map its field ID.  As
		// a fix, always allocate field ID 1 to the vtable ptr field.
		// Do this before allocating IDs to any other field.
		gs_t vfield = get_virtual_field(type_tree); 
		if (vfield) {
		  Is_True(gs_tree_code(vfield) == GS_FIELD_DECL,
			  ("Create_TY_For_Tree: bad vfield code"));
		  Is_True(gs_decl_name(vfield) &&
			  !strncmp(Get_Name(gs_decl_name(vfield)),"_vptr", 5),
			  ("Create_TY_For_Tree: bad vfield name"));
		  // The vfield field ID is either not set, or was set to 1.
		  Is_True(DECL_FIELD_ID(vfield) <= 1,
			  ("Create_TY_For_Tree: invalid vfield field ID"));

		  DECL_FIELD_ID(vfield) = next_field_id;	// must be 1
		  next_field_id += TYPE_FIELD_IDS_USED(gs_tree_type(vfield)) +1;
		  fld = New_FLD ();
		  FLD_Init(fld, Save_Str(Get_Name(gs_decl_name(vfield))), 
			   0, // type
			   gs_get_integer_value(gs_decl_field_offset(vfield))
			    + gs_get_integer_value(gs_decl_field_bit_offset(vfield))
			    / BITSPERBYTE);
		}
#endif

		// Generate an anonymous field for every direct, nonempty,
		// nonvirtual base class.  

		INT32 offset = 0;
		INT32 anonymous_fields = 0;
#ifndef KEY	// g++'s class.c already laid out the base types.  Bug 11622.
		gs_t type_binfo, basetypes;
		if ((type_binfo = gs_type_binfo(type_tree)) != NULL &&
		    (basetypes = gs_binfo_base_binfos(type_binfo)) != NULL) {
		  gs_t list;
		  for (list = basetypes; gs_code(list) != EMPTY;
		       list = gs_operand(list, 1)) {
		    gs_t binfo = gs_operand(list, 0);
		    gs_t basetype = gs_binfo_type(binfo);
		    offset = Roundup (offset,
				    gs_type_align(basetype) / BITSPERBYTE);
		    if (!is_empty_base_class(basetype) || 
			!gs_binfo_virtual_p(binfo)) {
		      ++next_field_id;
		      ++anonymous_fields;
		      next_field_id += TYPE_FIELD_IDS_USED(basetype);
		      fld = New_FLD();
		      FLD_Init (fld, Save_Str(Get_Name(0)), 
				Get_TY(basetype), offset);
		      offset += Type_Size_Without_Vbases (basetype);
                      Set_FLD_is_anonymous(fld);
#ifdef KEY
// temporary hack for a bug in gcc
// Details: From layout_class_type(), it turns out that for this
// type, gcc is apparently sending wrong type info, they have 2 fields
// each 8 bytes in a 'record', with the type size == 8 bytes also!
// So we take care of it here...
		      if (offset > tsize)
			{
			    tsize = offset;
			    Set_TY_size (ty, tsize);
			}
#endif // KEY
		    }
		  }
		}
#endif // KEY

                hash_set <gs_t, void_ptr_hash> anonymous_base;
                hash_set <gs_t, void_ptr_hash> virtual_base;
                gs_t type_binfo, basetypes;

                // find all base classes
                if ((type_binfo = gs_type_binfo(type_tree)) != NULL &&
                    (basetypes = gs_binfo_base_binfos(type_binfo)) != NULL) {
                  gs_t list;
                  for (list = basetypes; gs_code(list) != EMPTY;
                       list = gs_operand(list, 1)) {
                    gs_t binfo = gs_operand(list, 0);
                    gs_t basetype = gs_binfo_type(binfo);
                    anonymous_base.insert(basetype);
                    if (gs_binfo_virtual_p(binfo))
                       virtual_base.insert(basetype);
                  } 
                } 

		// Assign IDs to real fields.  The vtable ptr field is already
		// assigned ID 1.
		for (field = get_first_real_field(type_tree); 
			field;
			field = next_real_field(type_tree, field) )
		{
			if (gs_tree_code(field) == GS_TYPE_DECL) {
				continue;
			}
			if (gs_tree_code(field) == GS_CONST_DECL) {
				DevWarn ("got CONST_DECL in field list");
				continue;
			}
			if (gs_tree_code(field) == GS_VAR_DECL) {
				continue;	
			}
			if (gs_tree_code(field) == GS_TEMPLATE_DECL) {
				continue;
			}

			// Either the DECL_FIELD_ID is not yet set, or is
			// already set to the same field ID.  The latter
			// happens when GCC 4 duplicates the type tree and the
			// same field node appears in both type nodes.
			Is_True(DECL_FIELD_ID(field) == 0 ||
				DECL_FIELD_ID(field) == next_field_id,
				("Create_TY_For_Tree: field ID already set"));

			DECL_FIELD_ID(field) = next_field_id;
			next_field_id += 
			  TYPE_FIELD_IDS_USED(gs_tree_type(field)) + 1;
			fld = New_FLD ();
			FLD_Init (fld, Save_Str(Get_Name(gs_decl_name(field))), 
				0, // type
				gs_get_integer_value(gs_decl_field_offset(field)) +
				gs_get_integer_value(gs_decl_field_bit_offset(field))
					/ BITSPERBYTE);
                        if (gs_decl_name(field) == NULL)
                            Set_FLD_is_anonymous(fld);
                        if (anonymous_base.find(gs_tree_type(field)) != anonymous_base.end())
                            Set_FLD_is_base_class(fld); 
                        if (virtual_base.find(gs_tree_type(field)) != virtual_base.end())
                            Set_FLD_is_virtual(fld); 
		}

		TYPE_FIELD_IDS_USED(type_tree) = next_field_id - 1;
  		FLD_IDX last_field_idx = Fld_Table.Size () - 1;
		if (last_field_idx >= first_field_idx) {
			Set_TY_fld (ty, FLD_HANDLE (first_field_idx));
			Set_FLD_last_field (FLD_HANDLE (last_field_idx));
		}

		// now set the fld types.
		fld = TY_fld(ty);
#ifdef KEY
		// Handle the vtable ptr field if it exists.
		if (vfield) {
		  Is_True(gs_tree_code(gs_tree_type(vfield)) == GS_POINTER_TYPE,
		  ("Create_TY_For_Tree: vtable ptr should be GS_POINTER_TYPE"));

		  // As mentioned below, don't expand pointer-type fields to
		  // avoid circular dependences.  Defer expanding the field
		  // type.
		  fld = TY_fld(ty);
		  TY_IDX p_idx = Make_Pointer_Type(MTYPE_To_TY(MTYPE_U8),FALSE);
		  Set_FLD_type(fld, p_idx);
		  defer_field(vfield, fld);
		  fld = FLD_next(fld);
		}
#endif
		// first skip the anonymous fields, whose types are already
		// set.
		while (anonymous_fields--)
		  fld = FLD_next(fld);

		for (field = get_first_real_field(type_tree);
		     /* ugly hack follows; traversing the fields isn't
                        the same from run-to-run. fwa? */
			field && fld.Entry();
			field = next_real_field(type_tree, field))
		{
#ifdef KEY
			const  int FLD_BIT_FIELD_SIZE   = 64;
#endif
			if (gs_tree_code(field) == GS_TYPE_DECL)
				continue;
			if (gs_tree_code(field) == GS_CONST_DECL)
				continue;
			if (gs_tree_code(field) == GS_VAR_DECL)
				continue;
			if (gs_tree_code(field) == GS_TEMPLATE_DECL)
				continue;
#ifdef KEY
			// Don't expand the field's type if it's a pointer
			// type, in order to avoid circular dependences
			// involving member object types and base types.  See
			// example in bug 4954.  
			if (gs_tree_code(gs_tree_type(field)) == GS_POINTER_TYPE) {
				// Defer expanding the field's type.  Put in a
				// generic pointer type for now.
				TY_IDX p_idx =
				  Make_Pointer_Type(MTYPE_To_TY(MTYPE_U8),
						    FALSE);
				Set_FLD_type(fld, p_idx);
				defer_field(field, fld);
				fld = FLD_next(fld);
				continue;
			}
#endif
			TY_IDX fty_idx = Get_TY(gs_tree_type(field));

			if ((TY_align (fty_idx) > align) || (TY_is_packed (fty_idx)))
				Set_TY_is_packed (ty);
			if (! gs_tree_this_volatile(field))
			  Clear_TY_is_volatile (fty_idx);
			Set_FLD_type(fld, fty_idx);

			if ( ! gs_decl_bit_field(field)
			  	&& gs_tree_code(gs_tree_type(field)) != GS_RECORD_TYPE
			  	&& gs_tree_code(gs_tree_type(field)) != GS_UNION_TYPE
			  	&& gs_decl_size(field) // bug 10305
				&& gs_get_integer_value(gs_decl_size(field)) > 0
#ifdef KEY
// We don't handle bit-fields > 64 bits. For an INT field of 128 bits, we
// make it 64 bits. But then don't set it as FLD_IS_BIT_FIELD.
				&& gs_get_integer_value(gs_decl_size(field)) <= 
				   FLD_BIT_FIELD_SIZE
				// bug 2401
				&& TY_size(Get_TY(gs_tree_type(field))) != 0
#endif
				&& gs_get_integer_value(gs_decl_size(field))
				  != (TY_size(Get_TY(gs_tree_type(field))) 
					* BITSPERBYTE) )
			{
#ifdef KEY
			        FmtAssert( gs_get_integer_value(gs_decl_size(field)) <=
					   FLD_BIT_FIELD_SIZE,
					   ("field size too big") );
#endif
				// for some reason gnu doesn't set bit field
				// when have bit-field of standard size
				// (e.g. int f: 16;).  But we need it set
				// so we know how to pack it, because 
				// otherwise the field type is wrong.
				DevWarn("field size %lld doesn't match type size %lld", 
					gs_get_integer_value(gs_decl_size(field)),
					TY_size(Get_TY(gs_tree_type(field)))
						* BITSPERBYTE );
				gs_set_decl_bit_field(field, 1);
			}
			if (gs_decl_bit_field(field)) {
				Set_FLD_is_bit_field (fld);
				// bofst is remaining bits from byte offset
				Set_FLD_bofst (fld, 
					gs_get_integer_value(
						gs_decl_field_bit_offset(field))
						% BITSPERBYTE);
				Set_FLD_bsize (fld, gs_get_integer_value(
                                                           gs_decl_size(field)));
			}
			fld = FLD_next(fld);
		}

#ifndef KEY	// Don't expand methods by going through TYPE_METHODS,
		// because:
		//   1) It is incorrect to translate all methods in
		//      TYPE_METHODS to WHIRL because some of the methods are
		//      never used, and generating the assembly code for them
		//      might lead to undefined symbol references.  Instead,
		//      consult the gxx_emitted_decls list, which has all the
		//      functions (including methods) that g++ has ever emitted
		//      to assembly.
		//   2) Expanding the methods here will cause error when the
		//      methods are for a class B that appears as a field in an
		//      enclosing class A.  When Get_TY is run for A, it will
		//      call Get_TY for B in order to calculate A's field ID's.
		//      (Need Get_TY to find B's TYPE_FIELD_IDS_USED.)  If
		//      Get_TY uses the code below to expand B's methods, it
		//      will lead to error because the expansion requires the
		//      field ID's of the enclosing record (A), and these field
		//      ID's are not yet defined.

		// process methods
		if (!Enable_WGEN_DFE) {
		if (cp_type_quals(type_tree) == TYPE_UNQUALIFIED) {
			while (method != NULL_TREE) {
				WGEN_Expand_Decl (method, TRUE);
				method = TREE_CHAIN(method);
			}
		}
		}
#endif	// KEY
		} //end record scope
		break;
	case GS_METHOD_TYPE:
		//DevWarn ("Encountered METHOD_TYPE at line %d", lineno);
	case GS_FUNCTION_TYPE:
		{	// new scope for local vars
		gs_t arg;
		INT32 num_args, i;
#ifdef KEY /* bug 8346 */
		TY &ty = (idx == TY_IDX_ZERO) ? New_TY(idx) : Ty_Table[idx];
		Clear_TY_is_incomplete (idx);
#else
		TY &ty = New_TY (idx);
#endif
		TY_Init (ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0); 
		Set_TY_align (idx, 1);
		TY_IDX ret_ty_idx;
		TY_IDX arg_ty_idx;
		TYLIST tylist_idx;

		// allocate TYs for return as well as parameters
		// this is needed to avoid mixing TYLISTs if one
		// of the parameters is a pointer to a function

		ret_ty_idx = Get_TY(gs_tree_type(type_tree));
		for (arg = gs_type_arg_types(type_tree);
		     arg;
		     arg = gs_tree_chain(arg))
		{
		  arg_ty_idx = Get_TY(gs_tree_value(arg));
#ifdef KEY /* bug 8346 */
		  if (TY_is_incomplete (arg_ty_idx) ||
		      (TY_kind(arg_ty_idx) == KIND_POINTER &&
		       TY_is_incomplete(TY_pointed(arg_ty_idx))))
		    Set_TY_is_incomplete (idx);
#endif
		}

		// if return type is pointer to a zero length struct
		// convert it to void
		if (!JGEN::Config::Keep_Zero_length_structs    &&
		    TY_mtype (ret_ty_idx) == MTYPE_M &&
		    TY_size (ret_ty_idx) == 0) {
			// zero length struct being returned
		  	DevWarn ("function returning zero length struct at line %d", lineno);
			ret_ty_idx = Be_Type_Tbl (MTYPE_V);
		}

#ifdef KEY
		// If the front-end adds the fake first param, then convert the
		// function to return void.
		if (TY_return_in_mem(ret_ty_idx)) {
		  ret_ty_idx = Be_Type_Tbl (MTYPE_V);
		  Set_TY_return_to_param(idx);		// bugs 2423 2424
		}
#endif
		Set_TYLIST_type (New_TYLIST (tylist_idx), ret_ty_idx);
		Set_TY_tylist (ty, tylist_idx);
		for (num_args = 0, arg = gs_type_arg_types(type_tree);
		     arg;
		     num_args++, arg = gs_tree_chain(arg))
		{
			arg_ty_idx = Get_TY(gs_tree_value(arg));
			Is_True (!TY_is_incomplete (arg_ty_idx) ||
			          TY_is_incomplete (idx),
				  ("Create_TY_For_Tree: unexpected TY flag"));
			if (!WGEN_Keep_Zero_Length_Structs    &&
			    TY_mtype (arg_ty_idx) == MTYPE_M &&
			    TY_size (arg_ty_idx) == 0) {
				// zero length struct passed as parameter
				DevWarn ("zero length struct encountered in function prototype at line %d", lineno);
			}
			else
				Set_TYLIST_type (New_TYLIST (tylist_idx), arg_ty_idx);
		}
		if (num_args)
		{
			Set_TY_has_prototype(idx);
			if (arg_ty_idx != Be_Type_Tbl(MTYPE_V))
			{
				Set_TYLIST_type (New_TYLIST (tylist_idx), 0);
				Set_TY_is_varargs(idx);
			}
			else
				Set_TYLIST_type (Tylist_Table [tylist_idx], 0);
		}
		else
			Set_TYLIST_type (New_TYLIST (tylist_idx), 0);
#ifdef TARG_X8664
		if (!TARGET_64BIT && !TY_is_varargs(idx))
		{
		  // Ignore m{sse}regparm and corresponding attributes at -m64.
		  // Ignore stdcall/fastcall attributes at -m64 and varargs.
		  if (SSE_Reg_Parm ||
		      lookup_attribute("sseregparm",
		                       gs_type_attributes(type_tree)))
		    Set_TY_has_sseregister_parm (idx);
		  if (gs_t attr = lookup_attribute("regparm",
		      gs_type_attributes(type_tree)))
		  {
		    gs_t value = gs_tree_value (attr);
		    Is_True (gs_tree_code(value) == GS_TREE_LIST,
		             ("Expected TREE_LIST"));
		    value = gs_tree_value (value);
		    if (gs_tree_code(value) == GS_INTEGER_CST)
		      Set_TY_register_parm (idx, gs_get_integer_value (value));
		  }
		  else if (Reg_Parm_Count)
		    Set_TY_register_parm (idx, Reg_Parm_Count);

                  if (gs_t attr = lookup_attribute("stdcall",
		      gs_type_attributes(type_tree)))
		  {
		    gs_t value = gs_tree_value (attr);
		    Set_TY_has_stdcall (idx);
		  }
                  else if (gs_t attr = lookup_attribute("fastcall",
		            gs_type_attributes(type_tree)))
		  {
		    gs_t value = gs_tree_value (attr);
		    Set_TY_has_fastcall (idx);
		  }
		}
#endif
		} // end FUNCTION_TYPE scope
		break;
#ifdef TARG_X8664
        // x86 gcc vector types
        case GS_VECTOR_TYPE:
                {
		char *p = gs_type_mode(type_tree);
		idx = 0;
		if (strcmp(p, "BLK") == 0) {
		  TY_IDX elem_ty = Get_TY(gs_tree_type(type_tree));
		  TYPE_ID elem_mtype = TY_mtype(elem_ty);
		  switch (gs_n(gs_type_precision(type_tree))) {
		    case 1: if (elem_mtype == MTYPE_I8)
		    	      idx = MTYPE_To_TY(MTYPE_V8I8);
			    break;
		    case 2: if (elem_mtype == MTYPE_I4)
		    	      idx = MTYPE_To_TY(MTYPE_M8I4);
			    else if (elem_mtype == MTYPE_F4)
		    	      idx = MTYPE_To_TY(MTYPE_V8F4);
		    	    else if (elem_mtype == MTYPE_I8)
		    	      idx = MTYPE_To_TY(MTYPE_V16I8);
			    else if (elem_mtype == MTYPE_F8)
		    	      idx = MTYPE_To_TY(MTYPE_V16F8);
			    break;
		    case 4: if (elem_mtype == MTYPE_I2)
		    	      idx = MTYPE_To_TY(MTYPE_M8I2);
		    	    else if (elem_mtype == MTYPE_I4)
		    	      idx = MTYPE_To_TY(MTYPE_V16I4);
                            else if (elem_mtype == MTYPE_I8)
                              idx = MTYPE_To_TY(MTYPE_V32I8);
			    else if (elem_mtype == MTYPE_F4)
		    	      idx = MTYPE_To_TY(MTYPE_V16F4);
                            else if (elem_mtype == MTYPE_F8)
                              idx = MTYPE_To_TY(MTYPE_V32F8);
			    break;
		    case 8: if (elem_mtype == MTYPE_I1)
		    	      idx = MTYPE_To_TY(MTYPE_M8I1);
		    	    else if (elem_mtype == MTYPE_I2)
		    	      idx = MTYPE_To_TY(MTYPE_V16I2);
                            else if (elem_mtype == MTYPE_I4)
                              idx = MTYPE_To_TY(MTYPE_V32I4);
                            else if (elem_mtype == MTYPE_F4)
                              idx = MTYPE_To_TY(MTYPE_V32F4);
			    break;
		    case 16: if (elem_mtype == MTYPE_I1)
		    	       idx = MTYPE_To_TY(MTYPE_V16I1);
                             else if (elem_mtype == MTYPE_I2)
                               idx = MTYPE_To_TY(MTYPE_V32I2);
			     break;
                    case 32: if (elem_mtype == MTYPE_I1)
                               idx = MTYPE_To_TY(MTYPE_V32I1);
                             break;
		    default:
		      Fail_FmtAssertion ("Get_TY: unexpected vector type element count");
		  }
		}
		else { // use string emcoded in TYPE_MODE
		  if (toupper(*p++) != 'V') {
		    if (gs_type_name(type_tree)) {
		      p = gs_identifier_pointer(gs_decl_name(gs_type_name(type_tree)));
		      if (toupper(*p++) != 'V') 
			Fail_FmtAssertion("Get_TY: NYI");
		    }
		    else Fail_FmtAssertion("Get_TY: NYI");
		  }
		  int num_elems = strtol(p, &p, 10);
                  if (strncasecmp(p, "DI", 2) == 0) {
                    if (num_elems == 1)
                      idx = MTYPE_To_TY(MTYPE_V8I8);
                    else if (num_elems == 2)
                      idx = MTYPE_To_TY(MTYPE_V16I8);
                    else if (num_elems == 4)
                      idx = MTYPE_To_TY(MTYPE_V32I8);
                  }
                  else if (strncasecmp(p, "DF", 2) == 0) {
                    if (num_elems == 2)
                      idx = MTYPE_To_TY(MTYPE_V16F8);
                    else if (num_elems == 4)
                      idx = MTYPE_To_TY(MTYPE_V32F8);
                  }
		  else if (strncasecmp(p, "SI", 2) == 0) {
		    if (num_elems == 2)
		      idx = MTYPE_To_TY(MTYPE_M8I4);
		    else if (num_elems == 4)
		      idx = MTYPE_To_TY(MTYPE_V16I4);
		    else if (num_elems == 8)
		      idx = MTYPE_To_TY(MTYPE_V32I4);
		  }
		  else if (strncasecmp(p, "SF", 2) == 0) {
		    if (num_elems == 2)
		      idx = MTYPE_To_TY(MTYPE_V8F4);
		    else if (num_elems == 4)
		      idx = MTYPE_To_TY(MTYPE_V16F4);
                    else if (num_elems == 8)
                      idx = MTYPE_To_TY(MTYPE_V32F4);
		  }
		  else if (strncasecmp(p, "HI", 2) == 0) {
		    if (num_elems == 4)
		      idx = MTYPE_To_TY(MTYPE_M8I2);
		    else if (num_elems == 8)
		      idx = MTYPE_To_TY(MTYPE_V16I2);
                    else if (num_elems == 16)
                      idx = MTYPE_To_TY(MTYPE_V32I2);
		  }
		  else if (strncasecmp(p, "QI", 2) == 0) {
		    if (num_elems == 8)
		      idx = MTYPE_To_TY(MTYPE_M8I1);
		    else if (num_elems == 16)
		      idx = MTYPE_To_TY(MTYPE_V16I1);
                    else if (num_elems == 32)
                      idx = MTYPE_To_TY(MTYPE_V32I1);
		  }
		}
		if (idx == 0)
		  Fail_FmtAssertion ("Get_TY: unexpected vector type");
                }
                break;
#endif // TARG_X8664
	default:
		FmtAssert(FALSE, ("Get_TY unexpected tree_type"));
	}
	if (gs_type_readonly(type_tree))
		Set_TY_is_const (idx);
	if (gs_type_volatile(type_tree))
		Set_TY_is_volatile (idx);
#ifdef KEY
	if (gs_type_restrict(type_tree))
		Set_TY_is_restrict (idx);
#endif
	TYPE_TY_IDX(type_tree) = idx;
        if(Debug_Level >= 2) {
#ifdef KEY
	  // DSTs for records were entered into the defer list in the order
	  // that the records are declared, in order to preserve their scope.
	  // Bug 4168.
	  if (gs_tree_code(type_tree) != GS_RECORD_TYPE &&
	      gs_tree_code(type_tree) != GS_UNION_TYPE &&
	      // Bugs 8346, 11819: Insert a TY for DST processing only
	      // when the TY is complete to ensure that when the DST info
	      // are created, the TY will be valid.
	      !TY_is_incomplete(idx) &&
	      !(TY_kind(idx) == KIND_POINTER &&
	        TY_is_incomplete(TY_pointed(idx)))) {
	    // Defer creating DST info until there are no partially constructed
	    // types, in order to prevent Create_DST_type_For_Tree from calling
	    // Get_TY, which in turn may use field IDs from partially created
	    // structs.  Such fields IDs are wrong.  Bug 5658.
	    defer_DST_type(type_tree, idx, orig_idx);
	  }
#else
          DST_INFO_IDX dst =
            Create_DST_type_For_Tree(type_tree,
              idx,orig_idx);
          TYPE_DST_IDX(type_tree) = dst;
#endif
        }

	return idx;
}

#endif
