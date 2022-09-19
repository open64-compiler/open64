/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

//
// Created by xc5 on 25/8/2018.
//

// TLS Choice Mechanism
		switch (tlsk) {
			case GS_TLS_MODEL_GLOBAL_DYNAMIC:
				tls_model = TLS_GLOBAL_DYNAMIC;
				break;
			case GS_TLS_MODEL_LOCAL_DYNAMIC:
				tls_model = TLS_LOCAL_DYNAMIC;
				break;
			case GS_TLS_MODEL_INITIAL_EXEC:
				tls_model = TLS_INITIAL_EXEC;
				break;
			case GS_TLS_MODEL_LOCAL_EXEC:
				tls_model = TLS_LOCAL_EXEC;
				break;
		}


// TODO: InitV Stuff

void WGEN_Add_Aggregate_Init_Integer(INT64 val, INT size);
void WGEN_Add_Init_Block(void);
void WGEN_Add_Aggregate_Init_Real(gs_t real, INT size);
void WGEN_Add_Aggregate_Init_Complex(gs_t rval, gs_t ival, INT size);
void WGEN_Add_Aggregate_Init_String(char *s, INT size);
void WGEN_Add_Aggregate_Init_Symbol(ST *st, WN_OFFSET offset = 0);
#ifdef TARG_IA64
void Add_Aggregate_Init_Symiplt(ST *st, WN_OFFSET offset = 0);
#endif
void WGEN_Add_Aggregate_Init_Label(LABEL_IDX lab);
void WGEN_Add_Aggregate_Init_Address(gs_t init);
void WGEN_Add_Aggregate_Init_Vector(gs_t init_list);
void Add_Init_For_WHIRL(WN *init_wn, UINT size, INT64 ofst);
void Add_Initv_For_Tree(gs_t val, UINT size);
void Add_Bitfield_Initv_For_Tree(gs_t val, FLD_HANDLE fld, INT &bytes);
void Traverse_Aggregate_Pad(ST *st, BOOL gen_initv, UINT pad,
			    UINT current_offset);

if (gs_type_name(type_tree) == NULL)
Set_TY_anonymous(ty);

if (gs_tree_code(type_tree) == GS_UNION_TYPE) {
Set_TY_is_union(idx);
}
#ifdef KEY
if (gs_aggregate_value_p(type_tree)) {
Set_TY_return_in_mem(idx);