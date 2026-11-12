/* Copyright (C) 2026 Open64 Project */
/* Closed semantic admission for the absorbed decode-attention DAG.
 * Included by the owning PU-local lowering, after its namespace. This does
 * not recognize model names, allocate WN/ST/TY, or invent tensor operations.
 * Design: doc/AI-COMPILER-FUSED-MLA-v0.1.md. */
/* Derive all operands/dimensions and reject any unproved graph before mutation.
 * This initial contract admits the exact reviewed capture topology with
 * arbitrary compatible static B/H/L/R/Dr/Dn/Dv, not arbitrary attention DAGs. */
VHO_TENSOR_ATTENTION VHO_tensor_attention(const VHO_TENSOR_PROGRAM &p) {
    VHO_TENSOR_ATTENTION a;if(!p.fuse_attention) return a;
    need(p.fuse && p.cooperative && p.opt_level==3,"attention fusion requires cooperative fused O3");
    need(p.operations.size()==48,"attention fusion requires the closed absorbed DAG");
    std::vector<UINT32> in,out;
    for(UINT32 i=0;i<p.values.size();++i) {if(p.values[i].role==1) in.push_back(i);if(p.values[i].role==2) out.push_back(i);}
    need(in.size()==7 && out.size()==2,"attention interface mismatch");
    for(UINT32 i=0;i<7;++i) need(p.values[in[i]].shape.size()==3,"attention requires rank-three inputs");
    a.batch=p.values[in[0]].shape[0];a.heads=p.values[in[0]].shape[1];
    UINT64 dn=p.values[in[0]].shape[2],dv=p.values[in[5]].shape[1];
    a.length=p.values[in[2]].shape[1];a.rank=p.values[in[2]].shape[2];a.rope=p.values[in[1]].shape[2];
    const UINT64 ishapes[][3]={
        {a.batch,a.heads,dn},{a.batch,a.heads,a.rope},{a.batch,a.length,a.rank},
        {a.batch,a.length,a.rope},{a.heads,dn,a.rank},{a.heads,dv,a.rank},{a.batch,a.heads,a.length}};
    for(UINT32 i=0;i<7;++i) need(p.values[in[i]].shape==std::vector<UINT64>(ishapes[i],ishapes[i]+3) &&
        p.values[in[i]].dtype==(i==6?"bool":"float32"),"attention input shape/dtype mismatch");
    const char *names[]={
        "common.reshape",
        "common.matmul",
        "common.reshape",
        "common.reshape",
        "common.reshape",
        "common.transpose",
        "common.matmul",
        "common.reshape",
        "common.reshape",
        "common.reshape",
        "common.transpose",
        "common.matmul",
        "common.reshape",
        "common.add",
        "common.fill_like",
        "common.mul",
        "common.logical_not",
        "common.fill_like",
        "common.select",
        "common.reduce_max",
        "common.isfinite",
        "common.fill_like",
        "common.select",
        "common.sub",
        "common.exp",
        "common.reduce_sum",
        "common.fill_like",
        "common.greater",
        "common.fill_like",
        "common.select",
        "common.div",
        "common.reshape",
        "common.reshape",
        "common.matmul",
        "common.reshape",
        "common.reshape",
        "common.transpose",
        "common.matmul",
        "common.reshape",
        "common.fill_like",
        "common.greater",
        "common.log",
        "common.add",
        "common.fill_like",
        "common.select",
        "common.reshape",
        "copy",
        "copy"};
    const INT refs[][3]={
        {-1,-99,-99},
        {0,-5,-99},
        {1,-99,-99},
        {2,-99,-99},
        {-3,-99,-99},
        {4,-99,-99},
        {3,5,-99},
        {6,-99,-99},
        {-2,-99,-99},
        {-4,-99,-99},
        {9,-99,-99},
        {8,10,-99},
        {11,-99,-99},
        {7,12,-99},
        {13,-99,-99},
        {13,14,-99},
        {-7,-99,-99},
        {15,-99,-99},
        {16,17,15},
        {18,-99,-99},
        {19,-99,-99},
        {19,-99,-99},
        {20,19,21},
        {18,22,-99},
        {23,-99,-99},
        {24,-99,-99},
        {25,-99,-99},
        {25,26,-99},
        {25,-99,-99},
        {27,25,28},
        {24,29,-99},
        {30,-99,-99},
        {-3,-99,-99},
        {31,32,-99},
        {33,-99,-99},
        {34,-99,-99},
        {-6,-99,-99},
        {35,36,-99},
        {37,-99,-99},
        {25,-99,-99},
        {25,39,-99},
        {29,-99,-99},
        {22,41,-99},
        {22,-99,-99},
        {40,42,43},
        {44,-99,-99},
        {38,-99,-99},
        {45,-99,-99}};
    const UINT64 shapes[][4]={
        {a.batch,a.heads,1,dn},{a.batch,a.heads,1,a.rank},{a.batch,a.heads,a.rank},
        {a.batch,1,a.length,a.rank},{a.batch,1,a.rank,a.length},{a.batch,a.heads,1,a.length},
        {a.batch,a.heads,a.length},{a.batch,a.heads,1,a.rope},{a.batch,1,a.length,a.rope},
        {a.batch,1,a.rope,a.length},{a.batch,a.heads,1},{a.heads,a.rank,dv},
        {a.batch,a.heads,1,dv},{a.batch,a.heads,dv},{a.batch,a.heads}};
    const UINT32 shape_id[]={0,1,2,1,3,4,5,6,7,8,9,5,6,6,6,6,6,6,6,10,10,10,10,6,6,10,10,10,10,10,6,5,3,1,2,1,11,12,13,10,10,10,10,10,10,14,13,14};
    for(UINT32 i=0;i<48;++i) {
        const VHO_TENSOR_OPERATION &op=p.operations[i];need(op.name==names[i],"attention operator topology mismatch");
        UINT32 count=0;while(count<3 && refs[i][count]!=-99) ++count;
        need(op.inputs.size()==count,"attention arity mismatch");
        for(UINT32 k=0;k<count;++k) {
            INT ref=refs[i][k];UINT32 id=ref<0?in[-ref-1]:p.operations[ref].output;
            need(op.inputs[k]==id,"attention dataflow mismatch");
        }
        const VHO_TENSOR_VALUE &v=p.values[op.output];
        const UINT64 *dims=shapes[shape_id[i]];UINT32 rank=0;while(rank<4 && dims[rank]) ++rank;
        need(v.shape==std::vector<UINT64>(dims,dims+rank) && v.dtype==((i==16 || i==20 || i==27 || i==40)?"bool":"float32"),"attention intermediate shape/dtype mismatch");
        if(op.name=="common.transpose") need(op.attributes.find("attr.permutation")->second==(i==36?"0,2,1":"0,1,3,2"),"attention transpose mismatch");
        if(op.name=="common.reduce_max" || op.name=="common.reduce_sum")
            need(op.attributes.find("attr.axis")->second=="2" && op.attributes.find("attr.keepdim")->second=="true","attention reduction axis mismatch");
        if(op.name=="common.fill_like" && i!=14) {
            const char *bits=(i==17 || i==43)?"ff800000":i==28?"3f800000":"00000000";
            need(op.attributes.find("attr.value_bits")->second==bits,"attention numerical guard mismatch");
        }
    }
    need(p.operations[46].output==out[0] && p.operations[47].output==out[1],"attention result bindings mismatch");
    a.scale_bits=strtoul(p.operations[14].attributes.find("attr.value_bits")->second.c_str(),NULL,16);
    need((a.scale_bits&0x7f800000)!=0x7f800000,"attention requires a finite scale");
    need(p.split_width || a.length<=4096,"unsplit attention exceeds shared token capacity");
    a.q=p.operations[1].output;a.qr=in[1];a.cache=in[2];a.rope_cache=in[3];a.mask=in[6];
    a.latent=p.operations[33].output;a.lse=p.operations[44].output;
    UINT32 last=0;for(size_t i=0;i<p.operations.size();++i) {
        if(p.operations[i].output>last) last=p.operations[i].output;
        for(size_t k=0;k<p.operations[i].inputs.size();++k) if(p.operations[i].inputs[k]>last) last=p.operations[i].inputs[k];
    }
    a.bundle=last+1;a.partial=last+2;
    UINT64 chunks=p.split_width?(a.length+p.split_width-1)/p.split_width:1;
    need(chunks<=4096,"attention merge exceeds shared chunk capacity");
    a.enabled=true;return a;
}
