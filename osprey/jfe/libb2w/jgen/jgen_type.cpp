//
// Created by xc5 on 27/7/2018.
//

#include "jgen_type.h"
#include "jgen_st.h"
#include <string>
#include <map>
#include <vector>

using std::string;
using std::map;
using std::vector;

namespace JGEN
{

std::map<U32U, TY_IDX> JGEN_TY::generatedType;
JGEN_Typetree_Base * JGEN_TY::typetree;
JGEN_SymbolTree_Base * JGEN_TY::symtree;

TY_IDX JGEN_TY::getExistTypeIdx(U32U jTypeIndex) {
    if(generatedType.find(jTypeIndex) != generatedType.end()){
        return generatedType.find(jTypeIndex)->second;
    }
    return 0;
}

TY_IDX JGEN_TY::createFunction(U32U typenode) {


  FmtAssert(typetree != nullptr, ("-- [JGEN_TY] Reading from some nullptr (typetree)."));

  vector<U32U> args = typetree->getArgs(typenode);
  U32U retval = typetree->getRetVal(typenode);
  TY_IDX idx = getExistTypeIdx(typenode);

  TY &ty = (idx == TY_IDX_ZERO) ? New_TY (idx) : Ty_Table[idx];
  Clear_TY_is_incomplete (idx);
  TY_Init (ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0);
  Set_TY_align (idx, 1);

  TY_IDX ret_ty_idx;
  TY_IDX arg_ty_idx;
  TYLIST tylist_idx;

  // allocate TYs for return as well as parameters
  // this is needed to avoid mixing TYLISTs if one
  // of the parameters is a pointer to a function

  U32U ret_jIndex = typetree->getRetVal(typenode);
  if(ret_jIndex != 0){
    ret_ty_idx = Get_TY(ret_jIndex);
  }else{
    ret_ty_idx = MTYPE_To_TY (MTYPE_V);  // void;
  }

  for (vector<U32U>::iterator it = args.begin ();
       it != args.end ();
       it++)
  {
    arg_ty_idx = Get_TY(*it);
    if (TY_is_incomplete (arg_ty_idx) ||
        (TY_kind (arg_ty_idx) == KIND_POINTER &&
            TY_is_incomplete (TY_pointed (arg_ty_idx))))
      Set_TY_is_incomplete (idx);
  }

  // if return type is pointer to a zero length struct
  // convert it to void
  if (!JGEN::Config::Keep_Zero_length_structs &&
      TY_mtype (ret_ty_idx) == MTYPE_M &&
      TY_size (ret_ty_idx) == 0)
  {
    // zero length struct being returned
    DevWarn ("function returning zero length struct at line ([lineno])");
    ret_ty_idx = Be_Type_Tbl (MTYPE_V);
  }

  // If the front-end adds the fake first param, then convert the
  // function to return void.

  if (TY_return_in_mem (ret_ty_idx))
  {
    ret_ty_idx = Be_Type_Tbl (MTYPE_V);
    Set_TY_return_to_param (idx);        // bugs 2423 2424
  }

  Set_TYLIST_type (New_TYLIST (tylist_idx), ret_ty_idx);
  Set_TY_tylist (ty, tylist_idx); // Starting IDX

  for (vector<U32U>::iterator it = args.begin ();
       it != args.end ();
       it++)
  {
    arg_ty_idx = Get_TY(*it);
    Is_True (!TY_is_incomplete (arg_ty_idx) ||
        TY_is_incomplete (idx),
             ("Create_TY_For_Tree: unexpected TY flag"));
    if (!JGEN::Config::Keep_Zero_length_structs &&
        TY_mtype (arg_ty_idx) == MTYPE_M &&
        TY_size (arg_ty_idx) == 0)
    {
      // zero length struct passed as parameter
      DevWarn ("zero length struct encountered in function prototype at line (unknown)");
    }
    else
      Set_TYLIST_type (New_TYLIST (tylist_idx), arg_ty_idx);
  }
  if (args.size() > 0)
  {
    Set_TY_has_prototype (idx);
    if (arg_ty_idx != Be_Type_Tbl(MTYPE_V))
    {
      Set_TYLIST_type (New_TYLIST (tylist_idx), 0);
      Set_TY_is_varargs (idx); // End With Zero
    }
    else
      Set_TYLIST_type (Tylist_Table[tylist_idx], 0); // End With Zero
  }
  else
    Set_TYLIST_type (New_TYLIST (tylist_idx), 0);

  // TODO: TARGET X8664 SSE Specification Ignored

  return idx;
}

  void JGEN_TY::Get_Fields_For_Class(U32U jIndex) {

    TY_IDX idx = generatedType.at(jIndex);

    /***
     * Extracted Fields from Tsym, needed to use sym stuff from Now on
     */

    #if 0
    // Process nested structs and static data members first
    vector<U32U> flds = typetree->getFields(jIndex);
    for (vector<U32U>::iterator fild = flds.begin();
         fild != flds.end();
         fild++) {
        U32U field = *fild;
        Set_TY_content_seen(idx); // bug 10851

        ST_IDX idx = symtree->Get_ST(field);
        if ((typetree->getKind(field) == JGEN_TYPE_UNION ||
            typetree->getKind(field) == JGEN_TYPE_RECORD) &&
            field != jIndex) {
          // Defer typedefs within class
          // declarations to avoid circular
          // declaration dependences.  See
          // example in bug 5134.
          if (typetree->getKind(field) == JGEN_DECL_TYPE)
            defer_decl(field_type);
          else
            Get_TY(field_type);
        }
        // Defer expansion of static vars until all the fields in
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
      else if (typetree->getKind(field) == GS_VAR_DECL) //GS_VAR_DECL)
        //defer_decl(field);
        return;
      else if (typetree->getKind(field) == GS_TEMPLATE_DECL)
        //WGEN_Expand_Decl(field, TRUE);
        return;
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
      const  int FLD_BIT_FIELD_SIZE   = 64;
      if (gs_tree_code(field) == GS_TYPE_DECL)
        continue;
      if (gs_tree_code(field) == GS_CONST_DECL)
        continue;
      if (gs_tree_code(field) == GS_VAR_DECL)
        continue;
      if (gs_tree_code(field) == GS_TEMPLATE_DECL)
        continue;

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
        if (symtree->isFieldBitField(field)) {
          Set_FLD_is_bit_field (field);
          // bofst is remaining bits from byte offset
          Set_FLD_bofst (fld, symtree->getFieldBitOffset(field)) % BITSPERBYTE);
          Set_FLD_bsize (fld, symtree->getFieldSize(field));
        }

      fld = FLD_next(fld);

    }

    #endif

  }

TY_IDX JGEN_TY::createClassOrUnion(U32U jIndex) {
  {	// new scope for local vars

    TY_IDX idx = 0;
    TY &ty = (idx == TY_IDX_ZERO) ? New_TY(idx) : Ty_Table[idx];

    /**
     *
     *   TODO: DST Creation
     *
    // Must create DSTs in the order that the records are declared,
    // in order to preserve their scope.  Bug 4168.
    if (Debug_Level >= 2)
      defer_DST_type(type_tree, idx, orig_idx);

    **/

    /**
     *
     *  TODO : [Removable] because of GCC specification
     *
     *
    // GCC 3.2 pads empty structures with a fake 1-byte field.
    // These structures should have tsize = 0.
    if (typetree->get_type_size(jIndex) != 0 &&
        // is_empty_class assumes non-null CLASSTYPE_SIZE
        // check if it has lang-specific data
        typetree->isLangSpecific(jIndex) &&
        // check if it has its base version set
        gs_classtype_as_base(type_tree) &&
        gs_classtype_size(type_tree) &&
        gs_is_empty_class(type_tree))
      tsize = 0;
    **/

    TY_Init (ty, typetree->get_type_size(jIndex), KIND_STRUCT, MTYPE_M,
             Save_Str(typetree->getNameString(jIndex).c_str()) );

    if (typetree->getNameString(jIndex) == "" || typetree->isAnonymous(jIndex))
      Set_TY_anonymous(ty);

    if (typetree->getKind(jIndex) == JGEN_TYPE_UNION) {
      Set_TY_is_union(idx);
    }

    // gs_aggregate_value_p is only set for c++
    if (typetree->isAggregateValue(jIndex)) {
      Set_TY_return_in_mem(idx);
    }

    int align = typetree->getAlignWidth(jIndex);
    if (align == 0) align = 1;	// in case incomplete type
    Set_TY_align (idx, align);

    // set idx now in case recurse thru fields
    generatedType.insert(std::make_pair(jIndex, idx));

    // TODO: Get_Base_Type (jIndex);

    // Process nested structs and static data members first

    Get_Fields_For_Class(jIndex);

    Set_TY_fld (ty, FLD_HANDLE());
    FLD_IDX first_field_idx = Fld_Table.Size ();
    FLD_HANDLE fld;
    INT32 next_field_id = 1;

    #if 0

    gs_t field;
    gs_t method = gs_type_methods(type_tree);
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

/**
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
		    }
		  }
		}
#endif // KEY
          **/

    /**
     *
     *  TODO: Base CLass Parsing.
     *
    map <gs_t, void_ptr_hash> anonymous_base;
    map <gs_t, void_ptr_hash> virtual_base;
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
    */

    // TODO: Gen_Fields_For_Class_Type(jIndex);
    return idx;
  } //end record scope
}

}