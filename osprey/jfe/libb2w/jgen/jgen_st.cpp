/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

//
// Created by xc5 on 27/7/2018.
//

#include <string>
#include "jgen_st.h"
#include "jgen_include.h"
#include "jgen_global.h"
#include "jgen_exception.h"

namespace JGEN
{
    JGEN_SymbolTree_Base * JGEN_ST::symtree;
    TY_IDX JGEN_ST::ty_idx;
    ST *JGEN_ST::st;
    ST_SCLASS JGEN_ST::sclass = SCLASS_UGLOBAL;
    ST_EXPORT JGEN_ST::eclass = EXPORT_PREEMPTIBLE;
    SYMTAB_IDX JGEN_ST::level = GLOBAL_SYMTAB;

    ST_IDX JGEN_ST::Get_ST(U32U jIndex) {

      ST_IDX st = 0;

      if(jIndex < 0){
        throw jgen_exception("[Error] [getST] jIndex : " + int2str(jIndex) + " is negative, thus failed.");
      }

      U32U context = symtree->getParent(jIndex);
      U64U flag_ = symtree->getFlag(jIndex);
      U64U kind = symtree->getKind(jIndex);

      if ((kind == JGEN_ST_FUNC) || (kind == JGEN_ST_METHOD))
      {
        st = create_func (jIndex);
        logger("     --  Creating a ST of func " + symtree->getNameString(jIndex) + " , kind="+symtree->getKindName(jIndex)+"  --");
      }
      else if (kind == JGEN_ST_VAR)
      {
        st = createVar (jIndex);
        logger("     --  Creating a ST of var " + symtree->getNameString(jIndex) + " , kind="+symtree->getKindName(jIndex)+"  --");
      }
      else if (kind == JGEN_ST_CLASS)
      {
        st = createClass (jIndex);
        logger("     --  Creating a ST of class " + symtree->getNameString(jIndex) + " , kind="+symtree->getKindName(jIndex)+" --");
      }else{
        logger("     --  Unable to create ST (Unknown Kind), for  " + symtree->getNameString(jIndex) + " , kind="+symtree->getKindName(jIndex)+" --");
      }
    }

    ST_IDX JGEN_ST::create_func (U32U jIndex)
    {

      JGEN::Config::processing_function_prototypes = TRUE;
      TY_IDX func_ty_idx = get_related_TY (jIndex);
      JGEN::Config::processing_function_prototypes = FALSE;

      sclass = SCLASS_EXTERN;
      eclass = symtree->isPublic (jIndex) || symtree->isWeak (jIndex) ?
               EXPORT_PREEMPTIBLE :
               EXPORT_LOCAL;

      level = GLOBAL_SYMTAB + 1;

      PU_IDX pu_idx;
      PU &pu = New_PU (pu_idx);
      PU_Init (pu, func_ty_idx, level);

      st = New_ST (level - 1);
      ST_Init (st, Save_Str (symtree->getNameString(jIndex).c_str ()),
               CLASS_FUNC, sclass, eclass, TY_IDX (pu_idx));

      if (symtree->isConstructor (jIndex))
        {
          Set_PU_is_constructor (pu);
        }

      if (symtree->isPureVFunc (jIndex))
        {
          Set_ST_is_pure_vfunc (st);
        }

      if (symtree->isMethodOfClass (jIndex))
      {
          TY_IDX base = symtree->get_method_base_type (jIndex);
          Set_PU_base_class (pu, base);
      }

      if (symtree->getParent(jIndex) != 0 && symtree->isContextNamespace (symtree->getParent(jIndex)) &&
          eclass != EXPORT_LOCAL &&
          eclass != EXPORT_LOCAL_INTERNAL)
      {
          Set_ST_is_weak_symbol (st);
      }


      // process attributes for FUNCTION_DECL  TODO: Function Arguments
      /**
       * gs_t attr_list = gs_decl_attributes(decl_node);
      for ( ; attr_list != NULL; attr_list = gs_tree_chain(attr_list) ) {
                  Is_True(gs_tree_code(attr_list) == GS_TREE_LIST,
                          ("lookup_attributes: TREE_LIST node not found"));
          gs_t attr = gs_tree_purpose(attr_list);
          if ( is_attribute("noreturn", attr) ) // __attribute__((noreturn))
              Set_PU_has_attr_noreturn (pu);
      }
       */

      if (symtree->isNoThrow (jIndex))
      {
        Set_PU_nothrow (pu);
      }
    }

    ST_IDX JGEN_ST::createParam (U32U jIndex)
    {
      if (symtree->isExternal (jIndex))
        {
          sclass = SCLASS_EXTERN;
        }
      else if (symtree->isInitial (jIndex))
        {
          sclass = SCLASS_UGLOBAL;
        }
    }

    ST_IDX JGEN_ST::createVar (U32U jIndex)
    {

      if (symtree->getKind(jIndex) == JGEN_ST_PARM)
        {
          // wgen fix for C++ and also for C, as in bug 8346.
          /*if (decl_arguments) {
              st = Search_decl_arguments(gs_decl_name(decl_node) ? name : NULL);
              if (st) {
                  set_DECL_ST(decl_node, st); // created now
                  return st;
              }
          }
          */
          sclass = SCLASS_FORMAL;
          eclass = EXPORT_LOCAL;
          level = CURRENT_SYMTAB;
        }
      else
        {
          if (symtree->getParent(jIndex) == 0 || symtree->isContextNamespace (symtree->getParent(jIndex)) ||
              symtree->isContextRecord (symtree->getParent(jIndex)))
            {
              if (symtree->isPublic (jIndex))
                {

                  if (symtree->isExternal (jIndex) ||
                      (symtree->isLangSpecific (jIndex) &&
                       symtree->isReallyExtern (jIndex)))
                    sclass = SCLASS_EXTERN;
                  else if (symtree->isInitial (jIndex))
                    sclass = SCLASS_UGLOBAL;
                  else if (symtree->isStatic (jIndex))
                    {
                      if (JGEN::Config::do_not_parse_common || !symtree->isCommon (jIndex) ||
                          (!JGEN::Config::lang_oop && symtree->hasName (jIndex)))
                        sclass = SCLASS_UGLOBAL;
                      else
                        sclass = SCLASS_COMMON;
                    }
                  else
                    sclass = SCLASS_EXTERN;
                  eclass = EXPORT_PREEMPTIBLE;
                }
              else
                {
                  sclass = SCLASS_FSTATIC;
                  eclass = EXPORT_LOCAL;
                }
              level = GLOBAL_SYMTAB;
            }
          else
            {
              // .gnu.linkonce.b is .bss with DECL_ONE_ONLY set.  Bug 10876.
              std::string section_name = ".gnu";
              if (section_name.size () > 0 &&
                  symtree->getNameString(jIndex).substr(0,14) != ".gnu.linkonce.")
                {
                  if (symtree->getNameString(jIndex).substr(0,16) != ".gnu.linkonce.b."
                      // bug 13054
                      || symtree->getNameString(jIndex).substr(0,17) != ".gnu.linkonce.sb." )
                    {
                      sclass = SCLASS_UGLOBAL;
                      level = GLOBAL_SYMTAB;
                      eclass = EXPORT_PREEMPTIBLE;
                    }
                  else
                    {
                      // Add support as needed.
                      Fail_FmtAssertion ("Create_ST_For_Tree: %s section NYI", section_name.c_str ());
                    }
                }
                /*    // bug 13090 and 13245
                    // Bug 13047 shows that the gnu42 front-end (specifically
                    // the gcc/g++ part) behaves differently when built on a gnu3
                    // system, than when built on a gnu4 system. If the compiler
                    // is built on a gnu4 system, default_unique_section() in
                    // varasm.c will never generate a linkonce section because
                    // starting GNU42, this also depends on whether the host
                    // compiling system has COMDAT groups.
                else if (section_name &&
                         (!strncmp(gs_tree_string_pointer(section_name),
                                   ".sbss.", 6) ||
                          !strncmp(gs_tree_string_pointer(section_name),
                                   ".bss.", 5))) {
                    sclass = SCLASS_UGLOBAL;
                    level = GLOBAL_SYMTAB;
                    eclass = EXPORT_PREEMPTIBLE;
                }*/
              else if (symtree->isExternal (jIndex) || symtree->isWeak (jIndex))
                {
                  // OSP_255
                  // Not all weak symbols are EXTERN: COMMON&WEAK, STATIC&WEAK
                  if (!JGEN::Config::do_not_parse_common && symtree->isCommon (jIndex))
                    {
                      // COMMON & WEAK:
                      //   static vars in exported inline/template functions(IA64)
                      sclass = SCLASS_COMMON;
                    }
                  else if (symtree->isStatic (jIndex))
                    {
                      // STATIC & WEAK:
                      //   static vars in exported inline/template function(X8664)
                      sclass = SCLASS_UGLOBAL;
                    }
                  else
                    {
                      // OTHERS:
                      //   treat it EXTERN ( will not allocate space )
                      sclass = SCLASS_EXTERN;
                    }
                  level = GLOBAL_SYMTAB;
                  eclass = EXPORT_PREEMPTIBLE;
                }
                // Bug 8652: If GNU marks it as COMMON, we should the same.
              else if (!JGEN::Config::do_not_parse_common && symtree->isStatic (jIndex) &&
                  symtree->isCommon (jIndex) && symtree->isPublic (jIndex))
                {
                  sclass = SCLASS_COMMON;
                  level = GLOBAL_SYMTAB;
                  eclass = EXPORT_PREEMPTIBLE;
                }
              else
                {
                  if (symtree->isStatic (jIndex))
                    {
                      sclass = SCLASS_PSTATIC;
                      if (JGEN::Config::treat_static_as_global
                          && !(symtree->isInitial (jIndex) &&
                               !symtree->isExternal (jIndex) && symtree->isInitial (jIndex)))
                        level = GLOBAL_SYMTAB;
                      else
                        level = CURRENT_SYMTAB;
                    }
                  else
                    {
                      sclass = SCLASS_AUTO;
                      level = getSymtabLevel (jIndex) ? getSymtabLevel (jIndex) : CURRENT_SYMTAB;
                    }
                  eclass = EXPORT_LOCAL;
                }
            }
        }
      if (symtree->is_guard_var (jIndex))
        {
          // This is a guard variable created by the g++ front-end to protect
          // against multiple initializations (and destruction) of symbols
          // with static storage class. Make it local unless it's weak.
          level = GLOBAL_SYMTAB;
          if (symtree->isWeak (jIndex))
            {
              sclass = SCLASS_UGLOBAL;
              eclass = EXPORT_PREEMPTIBLE;
            }
          else
            {
              sclass = SCLASS_PSTATIC;
              eclass = EXPORT_LOCAL;
            }
        }
      else if (TRUE /* gv_cond_expr */) //TODO : Missing Statement
        {
          //Make guard variable for condtional expressions a local stack
          //variable to avoid being over-written when evaluating nested
          //conditional expressions.
          //See comments for WGEN_add_guard_var in wgen_expr.cxx
          //for information on conditional expressions.
          level = getSymtabLevel (jIndex) ?
                  getSymtabLevel (jIndex) : CURRENT_SYMTAB;
          sclass = SCLASS_AUTO;
          eclass = EXPORT_LOCAL;
        }

      sclass = SCLASS_FORMAL;
      eclass = EXPORT_LOCAL;
      level = CURRENT_SYMTAB;
      st = New_ST (level);

      ty_idx = get_related_TY (jIndex);

      // Set line number where define sym in source file
      //if (isO)
      //   Set_ST_Srcpos(*st, gs_decl_source_line(decl_node));
      //else  TODO: Set Line Number
      Set_ST_Srcpos (*st, symtree->getLineNum (jIndex));

      ST_Init (st, Save_Str (symtree->getNameString(jIndex).c_str()), CLASS_VAR, sclass, eclass, ty_idx);

      Set_ST_is_thread_private (st);

      if (symtree->getKind(jIndex) == JGEN_ST_VAR && sclass == SCLASS_AUTO)
        JGEN_add_pragma_to_location (WN_PRAGMA_LOCAL, st);

      /*
       * Variable Length Stuff
       *
      if (gs_decl_size_unit (decl_node) &&
          gs_tree_code (gs_decl_size_unit (decl_node)) != GS_INTEGER_CST)
      {
          // if this is the first alloca, save sp.
          int idx;
          if (!Set_Current_Scope_Has_Alloca (idx))
          {
              ST * save_st = WGEN_Alloca_0 ();
              Set_Current_Scope_Alloca_St (save_st, idx);
          }
          WN * size = WGEN_Expand_Expr (gs_decl_size_unit (decl_node));
          // mimic WGEN_Alloca_ST
          ST * alloca_st = New_ST (CURRENT_SYMTAB);
          ST_Init (alloca_st, Save_Str (name),
                   CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL,
                   Make_Pointer_Type (ty_idx, FALSE));
          Set_ST_is_temp_var (alloca_st);
          Set_ST_pt_to_unique_mem (alloca_st);
          Set_ST_base_idx (st, ST_st_idx (alloca_st));
          WN *wn  = WN_CreateAlloca (size);
          wn = WN_Stid (Pointer_Mtype, 0, alloca_st, ST_type (alloca_st), wn);
          WGEN_Stmt_Append (wn, Get_Srcpos());
          Set_PU_has_alloca (Get_Current_PU());
          // For kids 1..n of DEALLOCA
          Add_Current_Scope_Alloca_St (alloca_st, idx);
      }*/

      if (symtree->getKind(jIndex) == JGEN_ST_PARM)
        {
          Set_ST_is_value_parm (st);
        }
    }

    ST_IDX JGEN_ST::createNameSpace (U32U jIndex)
    {
      /* - local -->.      sclass = SCLASS_FSTATIC;
      - 		eclass = EXPORT_LOCAL;
      - initial --> global
      - static --> uglobal, common
      - else ---> SCLASS_EXTERN , EXPORT_PREEMPTIBLE*/
      // link once ?
      sclass = SCLASS_UGLOBAL;
      level = GLOBAL_SYMTAB;
      eclass = EXPORT_PREEMPTIBLE;
      logger("Still Got Stuff To DO -- JGEN_ST::createNameSpace");
    }

    ST_IDX JGEN_ST::createClass (U32U jIndex)
    {
      sclass = SCLASS_UGLOBAL;
      level = GLOBAL_SYMTAB;
      eclass = EXPORT_PREEMPTIBLE;
      st = New_ST (level);
      ty_idx = get_related_TY (jIndex);
      // Set line number where define sym in source file
      //if (isO)
      //   Set_ST_Srcpos(*st, gs_decl_source_line(decl_node));
      //else  TODO: Set Line Number
      Set_ST_Srcpos (*st, symtree->getLineNum(jIndex));
      ST_Init (st, Save_Str (symtree->getNameString(jIndex).c_str()), CLASS_VAR, sclass, eclass, ty_idx);
    }

    ST_IDX JGEN_ST::getSymtabLevel(U32U index) {
      return 0;
    }

}
