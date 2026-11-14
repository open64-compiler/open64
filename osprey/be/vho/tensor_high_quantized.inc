/* Copyright (C) 2026 Open64 Project */
/* VHO_TENSOR_HIGH's opt-in Q1 semantic expansion helpers, PU-local and O0.
 * Design: doc/AI-COMPILER-MLA-Q1-HIGH-OWNED-FACTS-v1.0.md. Reuses the legacy
 * ordered loop/ARRAY builder. No target policy, search or runtime dependency. */
    /* Storage descriptors use bytes; WHIRL ILOAD promotes I1/U1 to I4/U4. */
    TYPE_ID Scalar_type(const VHO_TENSOR_VALUE &v) {
        if(v.dtype=="int8") return MTYPE_I1;
        if(v.dtype=="uint8" || v.dtype=="bool") return MTYPE_U1;
        if(v.dtype=="int64") return MTYPE_I8;
        return MTYPE_F4;
    }
    /* Q1 status is a separate caller-owned disjoint scalar, never a call. */
    void Status(WN *body,INT32 code) {
        WN_INSERT_BlockLast(body,WN_Istore(MTYPE_I4,0,ST_type(_status),
            VHO_high_load(_status),WN_Intconst(MTYPE_I4,code)));
    }
    /* Fail before any unsafe arithmetic/load/conversion. Scratch may be partial;
     * the caller must discard the result when status is nonzero. */
    void Guard(WN *body,WN *valid) {
        WN *failed=WN_CreateBlock();Status(failed,1);
        WN_INSERT_BlockLast(failed,WN_Create(OPR_RETURN,MTYPE_V,MTYPE_V,0));
        WN_INSERT_BlockLast(body,WN_CreateIf(valid,WN_CreateBlock(),failed));
    }
    /* Only explicitly supported new contracts enter the Q1 expression path. */
    bool Is_quantized(const VHO_TENSOR_OPERATION &op) {
        return op.name=="common.abs" || op.name=="common.round_even" || op.name=="common.clamp" ||
            op.name=="common.convert" || op.name=="common.pack_u4" || op.name=="common.unpack_u4" ||
            op.name=="common.gather" || op.name=="common.scatter" ||
            (op.version==2 && (op.name=="common.fill_like" || op.name=="common.add" ||
             op.name=="common.mul" || op.name=="common.div" || op.name=="common.rem"));
    }
    /* Linearize explicit coordinates, retaining true indirect page operands. */
    WN *Coordinates(const std::vector<UINT64> &shape,std::vector<WN *> coordinates) {
        WN *index=VHO_high_integer(0);
        for(unsigned i=0;i<shape.size();++i)
            index=WN_Add(MTYPE_I8,WN_Mpy(MTYPE_I8,index,VHO_high_integer(shape[i])),coordinates[i]);
        return index;
    }
    /* Read a one-dimensional index vector, checking before use as an address. */
    WN *Checked_index(WN *body,UINT32 indices,WN *coordinate,UINT64 limit) {
        WN_INSERT_BlockLast(body,VHO_high_store(_integer,Read(indices,coordinate)));
        Guard(body,WN_LAND(WN_GE(MTYPE_I8,VHO_high_load(_integer),VHO_high_integer(0)),
            WN_LT(MTYPE_I8,VHO_high_load(_integer),VHO_high_integer(limit))));
        return VHO_high_load(_integer);
    }
    /* RNE is independent of host rounding mode. Large binary32 values are
     * already integral; only |x|<2^23 enters the safe integer conversion. */
    WN *Round_even(WN *body,WN *x) {
        WN_INSERT_BlockLast(body,VHO_high_store(_rounded,x));
        Guard(body,WN_LE(MTYPE_F4,WN_Abs(MTYPE_F4,VHO_high_load(_rounded)),Float(0x7f7fffff)));
        WN *small=WN_CreateBlock();
        WN_INSERT_BlockLast(small,VHO_high_store(_integer,WN_Trunc(MTYPE_F4,MTYPE_I8,VHO_high_load(_rounded))));
        WN *fraction=WN_Abs(MTYPE_F4,WN_Sub(MTYPE_F4,VHO_high_load(_rounded),
            WN_Cvt(MTYPE_I8,MTYPE_F4,VHO_high_load(_integer))));
        WN *adjust=WN_LIOR(WN_GT(MTYPE_F4,WN_COPY_Tree(fraction),Float(0x3f000000)),
            WN_LAND(WN_EQ(MTYPE_F4,fraction,Float(0x3f000000)),
                WN_NE(MTYPE_I8,WN_Band(MTYPE_I8,VHO_high_load(_integer),VHO_high_integer(1)),VHO_high_integer(0))));
        WN *increment=WN_Select(MTYPE_I8,WN_LT(MTYPE_F4,VHO_high_load(_rounded),Float(0)),
            VHO_high_integer(-1),VHO_high_integer(1));
        WN *change=WN_CreateBlock();
        WN_INSERT_BlockLast(change,VHO_high_store(_integer,WN_Add(MTYPE_I8,VHO_high_load(_integer),increment)));
        WN_INSERT_BlockLast(small,WN_CreateIf(adjust,change,WN_CreateBlock()));
        /* Preserve the source sign of zero by returning the original zero. */
        WN_INSERT_BlockLast(small,VHO_high_store(_rounded,WN_Select(MTYPE_F4,
            WN_EQ(MTYPE_I8,VHO_high_load(_integer),VHO_high_integer(0)),
            WN_Mpy(MTYPE_F4,VHO_high_load(_rounded),Float(0)),WN_Cvt(MTYPE_I8,MTYPE_F4,VHO_high_load(_integer)))));
        WN_INSERT_BlockLast(body,WN_CreateIf(WN_LT(MTYPE_F4,WN_Abs(MTYPE_F4,VHO_high_load(_rounded)),Float(0x4b000000)),
            small,WN_CreateBlock()));
        return VHO_high_load(_rounded);
    }
    /* Faithful scalar expansion, with checks inside the same canonical loops.
     * Functional scatter scans updates and copies untouched rows to fresh
     * storage; it never mutates input pools or publishes serving state. */
    WN *Quantized_expression(const VHO_TENSOR_OPERATION &op,WN *body) {
        const VHO_TENSOR_VALUE &o=_program.values[op.output],&a=_program.values[op.inputs[0]];
        UINT32 input=op.inputs[0];const std::string &name=op.name;
        if(name=="common.fill_like") return VHO_high_integer(strtoll(op.attributes.find("attr.value_i64")->second.c_str(),NULL,10));
        if(name=="common.gather" || name=="common.scatter") {
            unsigned axis=atoi(op.attributes.find("attr.axis")->second.c_str());
            std::vector<WN *> coordinates;for(unsigned i=0;i<a.shape.size();++i) coordinates.push_back(VHO_high_load(_indices[i]));
            if(name=="common.gather") {
                coordinates[axis]=Checked_index(body,op.inputs[1],VHO_high_load(_indices[axis]),a.shape[axis]);
                return Read(input,Coordinates(a.shape,coordinates));
            }
            WN_INSERT_BlockLast(body,VHO_high_store(_match,VHO_high_integer(-1)));
            WN *scan=WN_CreateBlock();
            WN *index=Checked_index(scan,op.inputs[1],VHO_high_load(_scan),a.shape[0]);
            WN *hit=WN_CreateBlock();
            Guard(hit,WN_EQ(MTYPE_I8,VHO_high_load(_match),VHO_high_integer(-1)));
            WN_INSERT_BlockLast(hit,VHO_high_store(_match,VHO_high_load(_scan)));
            WN_INSERT_BlockLast(scan,WN_CreateIf(WN_EQ(MTYPE_I8,index,VHO_high_load(_indices[0])),hit,WN_CreateBlock()));
            WN_INSERT_BlockLast(body,Loop(_scan,_program.values[op.inputs[1]].shape[0],scan));
            /* IF, rather than speculative SELECT, protects match=-1 loads. */
            ST *result=Scalar_type(o)==MTYPE_F4?_rounded:_integer;
            WN *updated=WN_CreateBlock(),*unchanged=WN_CreateBlock();
            coordinates[0]=VHO_high_load(_match);
            WN *value=Read(op.inputs[2],Coordinates(_program.values[op.inputs[2]].shape,coordinates));
            if(Scalar_type(o)!=MTYPE_F4 && WN_rtype(value)!=MTYPE_I8) value=WN_Cvt(WN_rtype(value),MTYPE_I8,value);
            WN_INSERT_BlockLast(updated,VHO_high_store(result,value));
            value=Read(input,Output_index(o));
            if(Scalar_type(o)!=MTYPE_F4 && WN_rtype(value)!=MTYPE_I8) value=WN_Cvt(WN_rtype(value),MTYPE_I8,value);
            WN_INSERT_BlockLast(unchanged,VHO_high_store(result,value));
            WN_INSERT_BlockLast(body,WN_CreateIf(WN_GE(MTYPE_I8,VHO_high_load(_match),VHO_high_integer(0)),updated,unchanged));
            WN *loaded=VHO_high_load(result);
            if(Scalar_type(o)==MTYPE_I1) return WN_Cvt(MTYPE_I8,MTYPE_I4,loaded);
            if(Scalar_type(o)==MTYPE_U1) return WN_Cvt(MTYPE_I8,MTYPE_U4,loaded);
            return loaded;
        }
        if(name=="common.pack_u4" || name=="common.unpack_u4") {
            WN *idx=Output_index(o);
            if(name=="common.pack_u4") {
                WN *lo=Read(input,WN_Mpy(MTYPE_I8,WN_COPY_Tree(idx),VHO_high_integer(2)));
                WN *hi=Read(input,WN_Add(MTYPE_I8,WN_Mpy(MTYPE_I8,idx,VHO_high_integer(2)),VHO_high_integer(1)));
                Guard(body,WN_LAND(WN_LE(MTYPE_U4,WN_COPY_Tree(lo),WN_Intconst(MTYPE_U4,15)),
                    WN_LE(MTYPE_U4,WN_COPY_Tree(hi),WN_Intconst(MTYPE_U4,15))));
                return WN_Bior(MTYPE_U4,lo,WN_Shl(MTYPE_U4,hi,WN_Intconst(MTYPE_U4,4)));
            }
            WN *byte=Read(input,WN_Div(MTYPE_I8,WN_COPY_Tree(idx),VHO_high_integer(2)));
            WN *shift=WN_Cvt(MTYPE_I8,MTYPE_U4,WN_Mpy(MTYPE_I8,
                WN_Band(MTYPE_I8,idx,VHO_high_integer(1)),VHO_high_integer(4)));
            return WN_Band(MTYPE_U4,WN_Lshr(MTYPE_U4,byte,shift),WN_Intconst(MTYPE_U4,15));
        }
        WN *x=Read(input,Broadcast_index(input,o));
        if(name=="common.abs") return WN_Abs(MTYPE_F4,x);
        if(name=="common.round_even") return Round_even(body,x);
        if(name=="common.clamp") {
            const char *sides[]={"min","max"};
            for(unsigned k=0;k<2;++k) {
                if(op.attributes.find(std::string("attr.has_")+sides[k])->second!="true") continue;
                UINT32 bits=strtoul(op.attributes.find(std::string("attr.")+sides[k]+"_bits")->second.c_str(),NULL,16);
                x=WN_Select(MTYPE_F4,k?WN_GT(MTYPE_F4,WN_COPY_Tree(x),Float(bits)):WN_LT(MTYPE_F4,WN_COPY_Tree(x),Float(bits)),Float(bits),x);
            }
            return x;
        }
        if(name=="common.convert") {
            if(o.dtype=="float32") return WN_Cvt(WN_rtype(x),MTYPE_F4,x);
            UINT32 low=o.dtype=="int8"?0xc3000000:0,high=o.dtype=="int8"?0x42fe0000:0x437f0000;
            Guard(body,WN_LAND(WN_GE(MTYPE_F4,WN_COPY_Tree(x),Float(low)),
                WN_LE(MTYPE_F4,WN_COPY_Tree(x),Float(high))));
            WN *integer=WN_Trunc(MTYPE_F4,MTYPE_I4,WN_COPY_Tree(x));
            Guard(body,WN_EQ(MTYPE_F4,x,WN_Cvt(MTYPE_I4,MTYPE_F4,WN_COPY_Tree(integer))));
            return integer;
        }
        WN *y=Read(op.inputs[1],Broadcast_index(op.inputs[1],o));
        Guard(body,WN_LAND(WN_GE(MTYPE_I8,WN_COPY_Tree(x),VHO_high_integer(0)),
            WN_GE(MTYPE_I8,WN_COPY_Tree(y),VHO_high_integer(0))));
        if(name=="common.div" || name=="common.rem") {
            Guard(body,WN_GT(MTYPE_I8,WN_COPY_Tree(y),VHO_high_integer(0)));
            if(name=="common.div") return WN_Div(MTYPE_I8,x,y);
            return WN_Sub(MTYPE_I8,WN_COPY_Tree(x),WN_Mpy(MTYPE_I8,
                WN_Div(MTYPE_I8,x,WN_COPY_Tree(y)),y));
        }
        if(name=="common.add") {
            Guard(body,WN_LE(MTYPE_I8,WN_COPY_Tree(x),WN_Sub(MTYPE_I8,VHO_high_integer(INT64_MAX),WN_COPY_Tree(y))));
            return WN_Add(MTYPE_I8,x,y);
        }
        WN *nonzero=WN_CreateBlock();
        Guard(nonzero,WN_LE(MTYPE_I8,WN_COPY_Tree(x),WN_Div(MTYPE_I8,VHO_high_integer(INT64_MAX),WN_COPY_Tree(y))));
        WN_INSERT_BlockLast(body,WN_CreateIf(WN_GT(MTYPE_I8,WN_COPY_Tree(y),VHO_high_integer(0)),nonzero,WN_CreateBlock()));
        return WN_Mpy(MTYPE_I8,x,y);
    }
