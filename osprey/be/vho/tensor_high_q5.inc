/* Copyright (C) 2026 Open64 Project */
/* VHO_TENSOR_HIGH selected Q5 materializer. Exactly one strict ordered plan;
 * no cost search, page observation, CUDA policy or alternate implementation.
 * Design: doc/AI-COMPILER-MLA-Q5-SELECTED-PLANS-v1.0.md. */
    VHO_Q5_LAYOUT _q5;
    VHO_Q5_PLAN _q5_plan;
    ST *_q5_slot,*_q5_score,*_q5_dot,*_q5_rope,*_q5_weight,*_q5_scale,*_q5_byte;
    /* Row/column ownership is logical and independent of serving phase. */
    WN *Q5_head() {return WN_Add(MTYPE_I8,WN_Mpy(MTYPE_I8,VHO_high_load(_indices[1]),VHO_high_integer(_q5_plan.group)),VHO_high_load(_indices[5]));}
    /* Each head/query owns its ordered folds and latent output. */
    WN *Q5_row() {return WN_Add(MTYPE_I8,WN_Mpy(MTYPE_I8,WN_Add(MTYPE_I8,WN_Mpy(MTYPE_I8,VHO_high_load(_indices[0]),VHO_high_integer(_q5.schema.geometry.heads)),Q5_head()),VHO_high_integer(_q5.schema.geometry.queries)),VHO_high_load(_indices[2]));}
    /* Allocator P does not choose the compute tile W. */
    WN *Q5_token() {return WN_Add(MTYPE_I8,WN_Mpy(MTYPE_I8,VHO_high_load(_indices[3]),VHO_high_integer(_q5_plan.width)),VHO_high_load(_indices[4]));}
    /* Exact source SSA identities are established by detached preflight. */
    UINT32 Q5_value(unsigned relative) {return _program.operations[_q5.start+relative].output;}
    /* Dead decoded-array prefixes are private reusable tile storage. */
    WN *Q5_tile_index(UINT64 dimension,WN *column) {return WN_Add(MTYPE_I8,WN_Mpy(MTYPE_I8,VHO_high_load(_indices[4]),VHO_high_integer(dimension)),column);}
    /* Guard the tail before any dynamic index, packed byte or scale load. */
    WN *Q5_tokens(WN *work) {
        WN *guard=WN_CreateBlock();WN_INSERT_BlockLast(guard,WN_CreateIf(WN_LT(MTYPE_I8,Q5_token(),VHO_high_integer(_q5.schema.geometry.keys)),work,WN_CreateBlock()));
        WN *body=WN_CreateBlock();WN_INSERT_BlockLast(body,Loop(_indices[4],std::min((UINT64)_q5_plan.width,_q5.schema.geometry.keys),guard));return body;
    }
    /* Padded head slots reserve resources but never read padded query rows. */
    WN *Q5_heads(WN *work) {
        WN *guard=WN_CreateBlock();WN_INSERT_BlockLast(guard,WN_CreateIf(WN_LT(MTYPE_I8,Q5_head(),VHO_high_integer(_q5.schema.geometry.heads)),work,WN_CreateBlock()));
        WN *body=WN_CreateBlock();WN_INSERT_BlockLast(body,Loop(_indices[5],_q5_plan.group,guard));return body;
    }
    /* Decode both paired streams once per tile/query/head-group/pass. Signed
     * S8 extends before F32 conversion; U4 takes even-low nibbles, subtracts
     * offset eight in F32, then multiplies the corresponding source scale. */
    WN *Q5_decode() {
        const VHO_TENSOR_WORKLOAD_GEOMETRY &g=_q5.schema.geometry;WN *token=WN_CreateBlock();
        if(_q5.schema.paged) {
            const VHO_TENSOR_VALUE &pool=_program.values[_q5.encoded[0]];
            WN *idx=WN_Add(MTYPE_I8,WN_Mpy(MTYPE_I8,VHO_high_load(_indices[0]),VHO_high_integer(g.keys)),Q5_token());
            WN *slot=Checked_index(token,_q5.indices,idx,pool.shape[0]*pool.shape[1]);
            WN_INSERT_BlockLast(token,VHO_high_store(_q5_slot,slot));
        } else WN_INSERT_BlockLast(token,VHO_high_store(_q5_slot,WN_Add(MTYPE_I8,WN_Mpy(MTYPE_I8,VHO_high_load(_indices[0]),VHO_high_integer(g.keys)),Q5_token())));
        for(unsigned pair=0;pair<4;pair+=2) {
            UINT64 dim=pair?g.rotary:g.latent;UINT32 storage=pair?g.rotary_value:g.latent_value;bool u4=_q5.schema.encoding=="u4-offset8-f32.v1";
            WN_INSERT_BlockLast(token,VHO_high_store(_q5_scale,Read(_q5.encoded[pair+1],VHO_high_load(_q5_slot))));
            WN *column=WN_CreateBlock();
            WN *index=WN_Add(MTYPE_I8,WN_Mpy(MTYPE_I8,VHO_high_load(_q5_slot),VHO_high_integer(u4?dim/2:dim)),VHO_high_load(_indices[6]));
            if(u4) WN_INSERT_BlockLast(column,VHO_high_store(_q5_byte,Read(_q5.encoded[pair],index)));
            for(unsigned half=0;half<(u4?2U:1U);++half) {
                WN *value=u4?WN_Band(MTYPE_U4,WN_Lshr(MTYPE_U4,VHO_high_load(_q5_byte),WN_Intconst(MTYPE_U4,half*4)),WN_Intconst(MTYPE_U4,15)):Read(_q5.encoded[pair],index);
                value=WN_Cvt(WN_rtype(value),MTYPE_F4,value);if(u4) value=WN_Sub(MTYPE_F4,value,Float(0x41000000));
                value=WN_Mpy(MTYPE_F4,value,VHO_high_load(_q5_scale));
                WN *coordinate=u4?WN_Add(MTYPE_I8,WN_Mpy(MTYPE_I8,VHO_high_load(_indices[6]),VHO_high_integer(2)),VHO_high_integer(half)):VHO_high_load(_indices[6]);
                Write(column,storage,Q5_tile_index(dim,coordinate),value);
            }
            WN_INSERT_BlockLast(token,Loop(_indices[6],u4?dim/2:dim,column));
        }
        return Q5_tokens(token);
    }
    /* Recompute the same rounded score in each pass. Masked scores become
     * -infinity after both source dot products, without skipping cache decode. */
    void Q5_score(WN *body) {
        const VHO_TENSOR_WORKLOAD_GEOMETRY &g=_q5.schema.geometry;
        WN_INSERT_BlockLast(body,VHO_high_store(_q5_dot,Float(0)));WN_INSERT_BlockLast(body,VHO_high_store(_q5_rope,Float(0)));
        const UINT32 query[]={Q5_value(0),g.rotary_query_value},cache[]={g.latent_value,g.rotary_value};
        const UINT64 dims[]={g.latent,g.rotary};ST *acc[]={_q5_dot,_q5_rope};
        for(unsigned k=0;k<2;++k) {
            WN *fold=WN_CreateBlock();WN *qi=WN_Add(MTYPE_I8,WN_Mpy(MTYPE_I8,Q5_row(),VHO_high_integer(dims[k])),VHO_high_load(_reduction));
            WN *ci=Q5_tile_index(dims[k],VHO_high_load(_reduction));
            WN_INSERT_BlockLast(fold,VHO_high_store(acc[k],WN_Add(MTYPE_F4,VHO_high_load(acc[k]),WN_Mpy(MTYPE_F4,Read(query[k],qi),Read(cache[k],ci)))));
            WN_INSERT_BlockLast(body,Loop(_reduction,dims[k],fold));
        }
        UINT32 bits=strtoul(_program.operations[_q5.start+8].attributes.find("attr.value_bits")->second.c_str(),NULL,16);
        WN *score=WN_Mpy(MTYPE_F4,WN_Add(MTYPE_F4,VHO_high_load(_q5_dot),VHO_high_load(_q5_rope)),Float(bits));
        WN *mi=WN_Add(MTYPE_I8,WN_Mpy(MTYPE_I8,Q5_row(),VHO_high_integer(g.keys)),Q5_token());
        WN_INSERT_BlockLast(body,VHO_high_store(_q5_score,WN_Select(MTYPE_F4,WN_NE(MTYPE_U4,Read(g.mask_value,mi),WN_Intconst(MTYPE_U4,0)),score,Float(0xff800000))));
    }
    /* Strict max, denominator and weighted folds traverse tokens in increasing
     * order across tiles. No per-chunk normalization or reassociation occurs. */
    WN *Q5_compute(unsigned pass) {
        const VHO_TENSOR_WORKLOAD_GEOMETRY &g=_q5.schema.geometry;
        WN *token=WN_CreateBlock();Q5_score(token);
        if(!pass) {
            WN *old=Read(Q5_value(13),Q5_row()),*next=VHO_high_load(_q5_score);
            Write(token,Q5_value(13),Q5_row(),WN_Select(MTYPE_F4,WN_LIOR(WN_NE(MTYPE_F4,WN_COPY_Tree(next),WN_COPY_Tree(next)),WN_GT(MTYPE_F4,WN_COPY_Tree(next),WN_COPY_Tree(old))),next,old));
        } else {
            WN *arg=WN_CreateParm(MTYPE_F4,WN_Sub(MTYPE_F4,VHO_high_load(_q5_score),Read(Q5_value(16),Q5_row())),MTYPE_To_TY(MTYPE_F4),WN_PARM_BY_VALUE);
            WN *weight=WN_Create_Intrinsic(OPR_INTRINSIC_OP,MTYPE_F4,MTYPE_V,INTRN_F4EXP,1,&arg);
            if(pass==2) weight=WN_Div(MTYPE_F4,weight,Read(Q5_value(23),Q5_row()));
            WN_INSERT_BlockLast(token,VHO_high_store(_q5_weight,weight));
            if(pass==1) Write(token,Q5_value(19),Q5_row(),WN_Add(MTYPE_F4,Read(Q5_value(19),Q5_row()),VHO_high_load(_q5_weight)));
            else {
                WN *column=WN_CreateBlock();WN *index=WN_Add(MTYPE_I8,WN_Mpy(MTYPE_I8,Q5_row(),VHO_high_integer(g.latent)),VHO_high_load(_indices[6]));
                Write(column,Q5_value(26),WN_COPY_Tree(index),WN_Add(MTYPE_F4,Read(Q5_value(26),index),WN_Mpy(MTYPE_F4,VHO_high_load(_q5_weight),Read(g.latent_value,Q5_tile_index(g.latent,VHO_high_load(_indices[6]))))));
                WN_INSERT_BlockLast(token,Loop(_indices[6],g.latent,column));
            }
        }
        return token;
    }
    /* The baseline still constructs exactly the same ordered tile tree. */
    WN *Q5_pass(unsigned pass) {
        const VHO_TENSOR_WORKLOAD_GEOMETRY &g=_q5.schema.geometry;
        WN *token=Q5_compute(pass);
        WN *tile=Q5_decode();WN_INSERT_BlockLast(tile,Q5_heads(Q5_tokens(token)));
        WN *body=WN_CreateBlock();WN_INSERT_BlockLast(body,Loop(_indices[3],(g.keys+_q5_plan.width-1)/_q5_plan.width,tile));return body;
    }
    /* Build one plan against existing formals/constants. Every new local and
     * node belongs to the caller's journal; no root is installed here. */
    void Q5_setup(WN *scope,const VHO_Q5_PLAN &plan,const VHO_Q5_LAYOUT &layout) {
        _q5=layout;_q5_plan=plan;_selected=true;Constants(scope);unsigned formal=0;
        for(unsigned i=0;i<_program.values.size();++i) if(_program.values[i].storage==i) _buffers[i]=WN_st(WN_formal(scope,formal++));
        _status=WN_st(WN_formal(scope,formal));
        for(unsigned i=0;i<16;++i) _indices.push_back(VHO_high_local("q5_axis",MTYPE_I8,_source));
        _reduction=VHO_high_local("q5_reduction",MTYPE_I8,_source);_sum=VHO_high_local("q5_sum",MTYPE_F4,_source);_next=VHO_high_local("q5_next",MTYPE_F4,_source);
        _integer=VHO_high_local("q5_integer",MTYPE_I8,_source);_scan=VHO_high_local("q5_scan",MTYPE_I8,_source);_match=VHO_high_local("q5_match",MTYPE_I8,_source);_rounded=VHO_high_local("q5_rounded",MTYPE_F4,_source);
        _q5_slot=VHO_high_local("q5_slot",MTYPE_I8,_source);_q5_score=VHO_high_local("q5_score",MTYPE_F4,_source);_q5_dot=VHO_high_local("q5_dot",MTYPE_F4,_source);_q5_rope=VHO_high_local("q5_rope",MTYPE_F4,_source);_q5_weight=VHO_high_local("q5_weight",MTYPE_F4,_source);
        _q5_scale=VHO_high_local("q5_scale",MTYPE_F4,_source);_q5_byte=VHO_high_local("q5_packed_byte",MTYPE_U4,_source);
    }
    /* Keep native construction order unchanged for archived Q5 images. */
    WN *Q5_body(WN *scope,const VHO_Q5_PLAN &plan,const VHO_Q5_LAYOUT &layout) {
        Q5_setup(scope,plan,layout);
        WN *body=WN_CreateBlock();Status(body,0);std::set<UINT32> removed(_q5.removed.begin(),_q5.removed.end());
        for(unsigned i=0;i<=_q5.start;++i) if(!removed.count(i) && _program.operations[i].name!="common.reshape") Operation(body,i);
        WN *group=WN_CreateBlock(),*init=WN_CreateBlock();
        Write(init,Q5_value(13),Q5_row(),Float(0xff800000));Write(init,Q5_value(19),Q5_row(),Float(0));
        WN *zero=WN_CreateBlock();Write(zero,Q5_value(26),WN_Add(MTYPE_I8,WN_Mpy(MTYPE_I8,Q5_row(),VHO_high_integer(_q5.schema.geometry.latent)),VHO_high_load(_indices[6])),Float(0));
        WN_INSERT_BlockLast(init,Loop(_indices[6],_q5.schema.geometry.latent,zero));WN_INSERT_BlockLast(group,Q5_heads(init));
        WN_INSERT_BlockLast(group,Q5_pass(0));WN *safe=WN_CreateBlock();WN *raw=Read(Q5_value(13),Q5_row());
        Write(safe,Q5_value(16),Q5_row(),WN_Select(MTYPE_F4,WN_LE(MTYPE_F4,WN_Abs(MTYPE_F4,WN_COPY_Tree(raw)),Float(0x7f7fffff)),raw,Float(0)));
        WN_INSERT_BlockLast(group,Q5_heads(safe));WN_INSERT_BlockLast(group,Q5_pass(1));WN *den=WN_CreateBlock();WN *d=Read(Q5_value(19),Q5_row());
        Write(den,Q5_value(23),Q5_row(),WN_Select(MTYPE_F4,WN_GT(MTYPE_F4,WN_COPY_Tree(d),Float(0)),d,Float(0x3f800000)));
        WN_INSERT_BlockLast(group,Q5_heads(den));WN_INSERT_BlockLast(group,Q5_pass(2));
        WN *query=WN_CreateBlock();WN_INSERT_BlockLast(query,Loop(_indices[2],_q5.schema.geometry.queries,group));
        WN *heads=WN_CreateBlock();WN_INSERT_BlockLast(heads,Loop(_indices[1],(_q5.schema.geometry.heads+plan.group-1)/plan.group,query));
        WN_INSERT_BlockLast(body,Loop(_indices[0],_q5.schema.geometry.batch,heads));
        for(unsigned i=_q5.start+27;i<_program.operations.size();++i) if(_program.operations[i].name!="common.reshape") Operation(body,i);
        WN_INSERT_BlockBefore(body,WN_first(body),WN_CreateComment(VHO_q5_explanation(plan,&_program).c_str()));
        WN_INSERT_BlockLast(body,WN_Create(OPR_RETURN,MTYPE_V,MTYPE_V,0));Position(body);return body;
    }
