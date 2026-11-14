/* Copyright (C) 2026 Open64 Project */
/* Q6b methods in the shared private VHO_TENSOR_HIGH builder. Logical work
 * ownership stays High; Q6c owns physical participants and instruction lowering.
 * Contract: doc/AI-COMPILER-MLA-Q6B-NATIVE-STAGING-v1.0.md. */
    VHO_Q6_MAPPING _q6;
    ST *_q6_tile,*_q6_slot,*_q6_first,*_q6_count,*_q6_offset,*_q6_physical;
    ST *_q6_raw[4];
    std::map<std::string,ST *> _q6_operations;
    /* Logical storage has an explicit byte extent, alignment and shared role.
     * The host review executes one complete logical work unit at a time. */
    WN *Q6_address(WN *byte) {
        WN *a=WN_Create(OPR_ARRAY,Pointer_Mtype,MTYPE_V,3);WN_element_size(a)=1;
        WN_kid0(a)=WN_Lda(Pointer_Mtype,0,_q6_storage);WN_kid1(a)=VHO_high_integer(_q6.shared_bytes);WN_kid2(a)=byte;return a;
    }
    /* TAS preserves the byte address while carrying the high-level pointer
     * type through PREOPT and source reconstruction (WHIRL.pdf 1.13-1.14). */
    ST *Q6_view(WN *body,const char *name,TYPE_ID scalar,UINT64 offset) {
        ST *view=New_ST(CURRENT_SYMTAB);TY_IDX type=Make_Pointer_Type(MTYPE_To_TY(scalar));
        ST_Init(view,Save_Str(name),CLASS_VAR,SCLASS_AUTO,EXPORT_LOCAL,type);Set_ST_Srcpos(*view,_source);
        WN_INSERT_BlockLast(body,VHO_high_store(view,WN_Tas(Pointer_Mtype,type,Q6_address(VHO_high_integer(offset)))));return view;
    }
    /* Async delivery uses its reserved eight-byte barrier identities. The
     * synchronous path has no physical barrier state; its raw base is a scope
     * identity only. Neither operation reads/writes bytes through that identity. */
    WN *Q6_identity(WN *slot) {
        WN *offset=_q6.control.delivery==VHO_Q6_BULK_TMA?
            WN_Add(MTYPE_I8,VHO_high_integer(_q6.barrier_offset),WN_Mpy(MTYPE_I8,slot,VHO_high_integer(8))):
            WN_Mpy(MTYPE_I8,slot,VHO_high_integer(_q6.raw_stride));return Q6_address(offset);
    }
    /* Every operation has a versioned exact typed interface and conservative
     * call effects. Existing globals with the same spelling must match. */
    ST *Q6_operation(const char *suffix,const std::vector<TY_IDX> &types) {
        std::string name=std::string("__open64_mla_stage_")+suffix;const ST *old;ST_IDX index;
        if(_q6_operations.count(name)) {
            old=_q6_operations[name];
            const char *tag=ST_tensor_metadata(ST_st_idx(old),"tensor.q6.primitive");
            if(ST_class(old)!=CLASS_FUNC || !tag || std::string(tag)!="q6b-byte-staging-v1") throw std::runtime_error("q6_primitive_collision");
            TY_IDX type=ST_pu_type(old);TYLIST_IDX a=TY_tylist(type);
            if(!TY_has_prototype(type) || TYLIST_type(Tylist_Table[a++])!=MTYPE_To_TY(MTYPE_V)) throw std::runtime_error("q6_primitive_result");
            for(unsigned i=0;i<types.size();++i) if(TYLIST_type(Tylist_Table[a++])!=types[i]) throw std::runtime_error("q6_primitive_argument");
            if(TYLIST_type(Tylist_Table[a])) throw std::runtime_error("q6_primitive_arity");return &St_Table[ST_st_idx(old)];
        }
        FOREACH_SYMBOL(GLOBAL_SYMTAB,old,index) if(name==ST_name(old)) throw std::runtime_error("q6_foreign_primitive_collision");
        TY_IDX ty;TY &t=New_TY(ty);TY_Init(t,0,KIND_FUNCTION,MTYPE_UNKNOWN,0);Set_TY_has_prototype(ty);TYLIST_IDX a;
        Set_TYLIST_type(New_TYLIST(a),MTYPE_To_TY(MTYPE_V));Set_TY_tylist(t,a);
        for(unsigned i=0;i<types.size();++i) Set_TYLIST_type(New_TYLIST(a),types[i]);Set_TYLIST_type(New_TYLIST(a),TY_IDX_ZERO);
        PU_IDX pi;PU &pu=New_PU(pi);PU_Init(pu,ty,GLOBAL_SYMTAB+1);Set_PU_c_lang(pu);
        ST *s=New_ST(GLOBAL_SYMTAB);ST_Init(s,Save_Str(name.c_str()),CLASS_FUNC,SCLASS_EXTERN,EXPORT_PREEMPTIBLE,ty);Set_ST_pu(s,pi);
        Set_ST_Srcpos(*s,_source);ST_tensor_bind_metadata(ST_st_idx(s),"tensor.q6.primitive","q6b-byte-staging-v1");_q6_operations[name]=s;return s;
    }
    /* Pointer arguments denote exact byte ranges or scope identities. Calls
     * stay statement boundaries; no hidden arithmetic or nested calls occur. */
    void Q6_call(WN *body,const char *name,std::vector<WN *> args,const char *kinds) {
        std::vector<TY_IDX> types;std::vector<TYPE_ID> mtypes;
        for(unsigned i=0;i<args.size();++i) {
            TYPE_ID type=kinds[i]=='p'?Pointer_Mtype:kinds[i]=='u'?MTYPE_U4:MTYPE_U8;mtypes.push_back(type);
            types.push_back(kinds[i]=='p'?Make_Pointer_Type(MTYPE_To_TY(MTYPE_V)):MTYPE_To_TY(type));
        }
        ST *operation=Q6_operation(name,types);WN *call=WN_Call(MTYPE_V,MTYPE_V,args.size(),ST_st_idx(operation));WN_Set_Call_Default_Flags(call);
        for(unsigned i=0;i<args.size();++i) WN_kid(call,i)=WN_CreateParm(mtypes[i],args[i],types[i],WN_PARM_BY_VALUE);
        WN_INSERT_BlockLast(body,call);
    }
    /* Create a one-operand scope operation without changing its semantics. */
    void Q6_scope(WN *body,const char *name,WN *slot) {
        std::vector<WN *> args;args.push_back(Q6_identity(slot));Q6_call(body,name,args,"p");
    }
    /* Explicit scope ranges let a consumer validate raw-slot separation and
     * the shared decoded lifetime without recovering layout from symbol names. */
    void Q6_initialize(WN *body,UINT32 slot) {
        std::vector<WN *> args;args.push_back(Q6_identity(VHO_high_integer(slot)));
        args.push_back(Q6_address(VHO_high_integer(slot*_q6.raw_stride)));args.push_back(WN_Intconst(MTYPE_U8,_q6.raw_stride));
        args.push_back(Q6_address(VHO_high_integer(_q6.decoded_offset)));args.push_back(WN_Intconst(MTYPE_U8,_q6.decoded_bytes));
        Q6_call(body,"init",args,"ppqpq");
    }
    /* One SSA-derived slot is shared by both payloads and both scale streams. */
    WN *Q6_physical(WN *token) {
        WN *coordinate=WN_Add(MTYPE_I8,WN_Mpy(MTYPE_I8,VHO_high_load(_indices[0]),VHO_high_integer(_q6.keys)),token);
        return _q5.schema.paged?Read(_q5.indices,coordinate):coordinate;
    }
    /* Emit one common contiguous run, retaining exact destinations/counts.
     * Q6c must partition actual device-aligned bulk middles and synchronous
     * fragments; this High copy operation never authorizes an overread. */
    WN *Q6_run() {
        WN *body=WN_CreateBlock();
        for(unsigned stream=0;stream<4;++stream) {
            UINT64 row=_q6.raw_row[stream],element=MTYPE_byte_size(Scalar_type(_program.values[_q5.encoded[stream]]));
            WN *destination=WN_Add(MTYPE_I8,WN_Mpy(MTYPE_I8,VHO_high_load(_q6_slot),VHO_high_integer(_q6.raw_stride)),
                WN_Add(MTYPE_I8,VHO_high_integer(_q6.raw_offset[stream]),WN_Mpy(MTYPE_I8,VHO_high_load(_q6_offset),VHO_high_integer(row))));
            std::vector<WN *> args;args.push_back(Q6_identity(VHO_high_load(_q6_slot)));args.push_back(Q6_address(destination));
            args.push_back(Address(_q5.encoded[stream],WN_Mpy(MTYPE_I8,VHO_high_load(_q6_first),VHO_high_integer(row/element))));
            args.push_back(WN_Cvt(MTYPE_I8,MTYPE_U8,WN_Mpy(MTYPE_I8,VHO_high_load(_q6_count),VHO_high_integer(row))));
            args.push_back(WN_Intconst(MTYPE_U4,_q6.control.delivery));Q6_call(body,"copy",args,"pppqu");
        }
        return body;
    }
    /* Acquire and issue a selected tile without modifying the current tile IV.
     * Coalesce only consecutive physical slots from the original source SSA;
     * page discontinuities split every stream at exactly the same boundary. */
    WN *Q6_stage(WN *tile,UINT32 pass) {
        WN *body=WN_CreateBlock();WN_INSERT_BlockLast(body,VHO_high_store(_q6_tile,tile));
        WN_INSERT_BlockLast(body,VHO_high_store(_q6_slot,WN_Binary(OPR_REM,MTYPE_I8,VHO_high_load(_q6_tile),VHO_high_integer(_q6.slots))));
        WN *start=WN_Mpy(MTYPE_I8,VHO_high_load(_q6_tile),VHO_high_integer(_q6.width));
        WN *remaining=WN_Sub(MTYPE_I8,VHO_high_integer(_q6.keys),WN_COPY_Tree(start));
        WN *valid=WN_Select(MTYPE_I8,WN_LT(MTYPE_I8,WN_COPY_Tree(remaining),VHO_high_integer(_q6.width)),remaining,VHO_high_integer(_q6.width));
        UINT64 rows=0;for(unsigned i=0;i<4;++i) rows+=_q6.raw_row[i];
        std::vector<WN *> args;args.push_back(Q6_identity(VHO_high_load(_q6_slot)));args.push_back(WN_Cvt(MTYPE_I8,MTYPE_U8,VHO_high_load(_q6_tile)));
        args.push_back(WN_Intconst(MTYPE_U4,pass));args.push_back(WN_Cvt(MTYPE_I8,MTYPE_U8,WN_Mpy(MTYPE_I8,valid,VHO_high_integer(rows))));Q6_call(body,"acquire",args,"pquq");
        WN_INSERT_BlockLast(body,VHO_high_store(_q6_first,Q6_physical(start)));WN_INSERT_BlockLast(body,VHO_high_store(_q6_count,VHO_high_integer(0)));
        WN_INSERT_BlockLast(body,VHO_high_store(_q6_offset,VHO_high_integer(0)));
        WN *token=WN_CreateBlock();WN *logical=WN_Add(MTYPE_I8,WN_Mpy(MTYPE_I8,VHO_high_load(_q6_tile),VHO_high_integer(_q6.width)),VHO_high_load(_indices[4]));
        WN_INSERT_BlockLast(token,VHO_high_store(_q6_physical,Q6_physical(WN_COPY_Tree(logical))));
        WN *flush=Q6_run();WN_INSERT_BlockLast(flush,VHO_high_store(_q6_first,VHO_high_load(_q6_physical)));
        WN_INSERT_BlockLast(flush,VHO_high_store(_q6_offset,VHO_high_load(_indices[4])));WN_INSERT_BlockLast(flush,VHO_high_store(_q6_count,VHO_high_integer(0)));
        WN *different=WN_LAND(WN_GT(MTYPE_I8,VHO_high_load(_q6_count),VHO_high_integer(0)),WN_NE(MTYPE_I8,VHO_high_load(_q6_physical),WN_Add(MTYPE_I8,VHO_high_load(_q6_first),VHO_high_load(_q6_count))));
        WN_INSERT_BlockLast(token,WN_CreateIf(different,flush,WN_CreateBlock()));WN_INSERT_BlockLast(token,VHO_high_store(_q6_count,WN_Add(MTYPE_I8,VHO_high_load(_q6_count),VHO_high_integer(1))));
        WN *guard=WN_CreateBlock();WN_INSERT_BlockLast(guard,WN_CreateIf(WN_LT(MTYPE_I8,logical,VHO_high_integer(_q6.keys)),token,WN_CreateBlock()));
        WN_INSERT_BlockLast(body,Loop(_indices[4],std::min(_q6.width,_q6.keys),guard));WN_INSERT_BlockLast(body,Q6_run());return body;
    }
    /* Read one staged scalar only after the explicit wait. Raw scale rows and
     * packed bytes retain the source's scalar load type and exact bit pattern. */
    WN *Q6_read(unsigned stream,WN *column) {
        UINT64 bytes=MTYPE_byte_size(Scalar_type(_program.values[_q5.encoded[stream]]));
        WN *index=WN_Add(MTYPE_I8,WN_Mpy(MTYPE_I8,WN_Binary(OPR_REM,MTYPE_I8,VHO_high_load(_indices[3]),VHO_high_integer(_q6.slots)),VHO_high_integer(_q6.raw_stride/bytes)),
            WN_Add(MTYPE_I8,WN_Mpy(MTYPE_I8,VHO_high_load(_indices[4]),VHO_high_integer(_q6.raw_row[stream]/bytes)),column));
        TYPE_ID type=Scalar_type(_program.values[_q5.encoded[stream]]);WN *a=WN_Create(OPR_ARRAY,Pointer_Mtype,MTYPE_V,3);WN_element_size(a)=bytes;
        WN_kid0(a)=VHO_high_load(_q6_raw[stream]);
        WN_kid1(a)=VHO_high_integer((_q6.decoded_offset-_q6.raw_offset[stream])/bytes);WN_kid2(a)=index;
        return WN_Iload(type,0,MTYPE_To_TY(type),a);
    }
    /* Decode native S8/U4 operations, sharing one F32 tile across logical heads.
     * Masked tokens are still decoded and multiplied, preserving 0*Inf/NaN. */
    WN *Q6_decode() {
        WN *token=WN_CreateBlock();bool u4=_q5.schema.encoding=="u4-offset8-f32.v1";
        for(unsigned pair=0;pair<4;pair+=2) {
            UINT64 dim=pair?_q6.rotary:_q6.latent;UINT32 value=pair?_q5.schema.geometry.rotary_value:_q5.schema.geometry.latent_value;
            WN_INSERT_BlockLast(token,VHO_high_store(_q5_scale,Q6_read(pair+1,VHO_high_integer(0))));WN *column=WN_CreateBlock();
            if(u4) WN_INSERT_BlockLast(column,VHO_high_store(_q5_byte,Q6_read(pair,VHO_high_load(_indices[6]))));
            for(unsigned half=0;half<(u4?2U:1U);++half) {
                WN *x=u4?WN_Band(MTYPE_U4,WN_Lshr(MTYPE_U4,VHO_high_load(_q5_byte),WN_Intconst(MTYPE_U4,half*4)),WN_Intconst(MTYPE_U4,15)):Q6_read(pair,VHO_high_load(_indices[6]));
                x=WN_Cvt(WN_rtype(x),MTYPE_F4,x);if(u4) x=WN_Sub(MTYPE_F4,x,Float(0x41000000));x=WN_Mpy(MTYPE_F4,x,VHO_high_load(_q5_scale));
                WN *coordinate=u4?WN_Add(MTYPE_I8,WN_Mpy(MTYPE_I8,VHO_high_load(_indices[6]),VHO_high_integer(2)),VHO_high_integer(half)):VHO_high_load(_indices[6]);
                Write(column,value,Q5_tile_index(dim,coordinate),x);
            }
            WN_INSERT_BlockLast(token,Loop(_indices[6],u4?dim/2:dim,column));
        }
        return Q5_tokens(token);
    }
    /* The selected pipeline stages one tile ahead only for two-slot delivery.
     * Explicit consumer completion precedes every release and slot overwrite. */
    WN *Q6_pass(UINT32 pass) {
        WN *body=Q6_stage(VHO_high_integer(0),pass),*tile=WN_CreateBlock();
        WN *slot=WN_Binary(OPR_REM,MTYPE_I8,VHO_high_load(_indices[3]),VHO_high_integer(_q6.slots));Q6_scope(tile,"wait",WN_COPY_Tree(slot));
        WN *next=WN_Add(MTYPE_I8,VHO_high_load(_indices[3]),VHO_high_integer(1));
        if(_q6.slots==2) WN_INSERT_BlockLast(tile,WN_CreateIf(WN_LT(MTYPE_I8,WN_COPY_Tree(next),VHO_high_integer(_q6.tiles)),Q6_stage(WN_COPY_Tree(next),pass),WN_CreateBlock()));
        WN_INSERT_BlockLast(tile,Q6_decode());Q6_scope(tile,"publish",WN_COPY_Tree(slot));
        WN_INSERT_BlockLast(tile,Q5_heads(Q5_tokens(Q5_compute(pass))));Q6_scope(tile,"consume",WN_COPY_Tree(slot));Q6_scope(tile,"release",slot);
        if(_q6.slots==1) WN_INSERT_BlockLast(tile,WN_CreateIf(WN_LT(MTYPE_I8,WN_COPY_Tree(next),VHO_high_integer(_q6.tiles)),Q6_stage(next,pass),WN_CreateBlock()));else WN_DELETE_Tree(next);
        WN_INSERT_BlockLast(body,Loop(_indices[3],_q6.tiles,tile));return body;
    }
    /* Construct one process-isolated High body. All possible source guards run
     * before scope initialization so early return leaves no outstanding copies. */
    WN *Q6_body(WN *scope,const VHO_Q5_PLAN &plan,const VHO_Q5_LAYOUT &layout,const VHO_Q6_MAPPING &mapping) {
        Q5_setup(scope,plan,layout);_q6=mapping;
        _q6_tile=VHO_high_local("q6_stage_tile",MTYPE_I8,_source);_q6_slot=VHO_high_local("q6_stage_slot",MTYPE_I8,_source);
        _q6_first=VHO_high_local("q6_run_first",MTYPE_I8,_source);_q6_count=VHO_high_local("q6_run_count",MTYPE_I8,_source);
        _q6_offset=VHO_high_local("q6_run_offset",MTYPE_I8,_source);_q6_physical=VHO_high_local("q6_physical",MTYPE_I8,_source);
        TY_IDX storage=Make_Array_Type(MTYPE_U1,1,_q6.shared_bytes);Set_TY_align(storage,16);Set_TY_is_user_align(storage);
        _q6_storage=New_ST(CURRENT_SYMTAB);ST_Init(_q6_storage,Save_Str("q6_shared_storage"),CLASS_VAR,SCLASS_AUTO,EXPORT_LOCAL,storage);
        Set_ST_device_symbol_kind(*_q6_storage,DEVICE_SYMBOL_SHARED);Set_ST_addr_passed(*_q6_storage);Set_ST_addr_saved(*_q6_storage);Set_ST_Srcpos(*_q6_storage,_source);
        WN *body=WN_CreateBlock();Status(body,0);std::set<UINT32> removed(_q5.removed.begin(),_q5.removed.end());
        for(unsigned i=0;i<=_q5.start;++i) if(!removed.count(i) && _program.operations[i].name!="common.reshape") Operation(body,i);
        if(_q5.schema.paged) {
            WN *check=WN_CreateBlock();const VHO_TENSOR_VALUE &pool=_program.values[_q5.encoded[0]];
            Checked_index(check,_q5.indices,VHO_high_load(_indices[7]),pool.shape[0]*pool.shape[1]);
            WN_INSERT_BlockLast(body,Loop(_indices[7],_q6.batch*_q6.keys,check));
        }
        for(unsigned i=0;i<4;++i) _q6_raw[i]=Q6_view(body,"q6_raw_view",Scalar_type(_program.values[_q5.encoded[i]]),_q6.raw_offset[i]);
        _q6_redirect[_program.values[_q5.schema.geometry.latent_value].storage]=Q6_view(body,"q6_latent_view",MTYPE_F4,_q6.decoded_offset);
        UINT64 latent_bytes=(_q6.width*_q6.latent*4+15)/16*16;
        _q6_redirect[_program.values[_q5.schema.geometry.rotary_value].storage]=Q6_view(body,"q6_rotary_view",MTYPE_F4,_q6.decoded_offset+latent_bytes);
        WN *group=WN_CreateBlock();for(unsigned slot=0;slot<_q6.slots;++slot) Q6_initialize(group,slot);
        WN *init=WN_CreateBlock();Write(init,Q5_value(13),Q5_row(),Float(0xff800000));Write(init,Q5_value(19),Q5_row(),Float(0));
        WN *zero=WN_CreateBlock();Write(zero,Q5_value(26),WN_Add(MTYPE_I8,WN_Mpy(MTYPE_I8,Q5_row(),VHO_high_integer(_q6.latent)),VHO_high_load(_indices[6])),Float(0));
        WN_INSERT_BlockLast(init,Loop(_indices[6],_q6.latent,zero));WN_INSERT_BlockLast(group,Q5_heads(init));WN_INSERT_BlockLast(group,Q6_pass(0));
        WN *safe=WN_CreateBlock(),*raw=Read(Q5_value(13),Q5_row());Write(safe,Q5_value(16),Q5_row(),Safe_max(raw));WN_INSERT_BlockLast(group,Q5_heads(safe));
        WN_INSERT_BlockLast(group,Q6_pass(1));WN *den=WN_CreateBlock(),*d=Read(Q5_value(19),Q5_row());
        Write(den,Q5_value(23),Q5_row(),WN_Select(MTYPE_F4,WN_GT(MTYPE_F4,WN_COPY_Tree(d),Float(0)),d,Float(0x3f800000)));WN_INSERT_BlockLast(group,Q5_heads(den));
        WN_INSERT_BlockLast(group,Q6_pass(2));for(unsigned slot=0;slot<_q6.slots;++slot) Q6_scope(group,"end",VHO_high_integer(slot));
        WN *query=WN_CreateBlock();WN_INSERT_BlockLast(query,Loop(_indices[2],_q6.queries,group));WN *heads=WN_CreateBlock();WN_INSERT_BlockLast(heads,Loop(_indices[1],(_q6.heads+_q6.group-1)/_q6.group,query));
        WN_INSERT_BlockLast(body,Loop(_indices[0],_q6.batch,heads));_q6_redirect.clear();
        for(unsigned i=_q5.start+27;i<_program.operations.size();++i) if(_program.operations[i].name!="common.reshape") Operation(body,i);
        WN_INSERT_BlockLast(body,WN_Create(OPR_RETURN,MTYPE_V,MTYPE_V,0));Position(body);return body;
    }
