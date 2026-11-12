/* Copyright (C) 2026 Open64 Project */
/* Included by the existing pure serving-source schema service. Q5 qualification
 * owns exact source topology and storage legality; no native/runtime callbacks.
 * Design: doc/AI-COMPILER-MLA-Q5-SELECTED-PLANS-v1.0.md. */
namespace {
class Q5_SOURCE {
    const VHO_TENSOR_PROGRAM &_p;
    std::map<UINT32,unsigned> _defs;
    std::set<unsigned> _removed;
public:
    /* Borrow detached validated SSA only. */
    explicit Q5_SOURCE(const VHO_TENSOR_PROGRAM &p):_p(p) {for(unsigned i=0;i<p.operations.size();++i) _defs[p.operations[i].output]=i;}
    /* Demand the actual producer and optionally mark its eliminated storage. */
    const VHO_TENSOR_OPERATION &Op(UINT32 value,const char *name,bool remove=true) {
        require(_defs.count(value),"q5_missing_producer");unsigned i=_defs[value];
        require(_p.operations[i].name==name,"q5_decode_topology");if(remove) _removed.insert(i);return _p.operations[i];
    }
    /* Strip only existing storage-preserving reshape views. */
    UINT32 Unshape(UINT32 value) {
        while(_defs.count(value) && _p.operations[_defs[value]].name=="common.reshape") value=Op(value,"common.reshape").inputs[0];return value;
    }
    /* Encoded/scale roots must be the same SSA storage read by the source.
     * The four gathers must share the exact index SSA; no page-name heuristic. */
    void Stream(UINT32 value,UINT32 encoded,VHO_Q5_LAYOUT &l) {
        value=Unshape(value);
        if(l.schema.paged) {
            const VHO_TENSOR_OPERATION &gather=Op(value,"common.gather");
            require(gather.attributes.find("attr.axis")->second=="0","q5_gather_axis");
            const VHO_TENSOR_VALUE &pool=_p.values[encoded],&flat=_p.values[gather.inputs[0]],&index=_p.values[gather.inputs[1]];
            require(pool.shape.size()==3 && flat.shape.size()==2 && flat.shape[0]==pool.shape[0]*pool.shape[1] && flat.shape[1]==pool.shape[2],"q5_flat_pool_shape");
            require(index.shape.size()==1 && index.shape[0]==l.schema.geometry.batch*l.schema.geometry.keys,"q5_index_extent");
            UINT32 indices=gather.inputs[1];if(l.indices==~0U) l.indices=indices;
            require(l.indices==indices && _p.values[gather.inputs[0]].storage==_p.values[encoded].storage,"q5_stream_join");
        } else require(_p.values[value].storage==_p.values[encoded].storage,"q5_dense_stream_join");
    }
    /* Decode exactly source conversion, offset-U4 unpack and paired F32 scale. */
    void Decode(UINT32 value,unsigned pair,VHO_Q5_LAYOUT &l) {
        const VHO_TENSOR_OPERATION &product=Op(value,"common.mul");UINT32 payload=product.inputs[0];
        const VHO_TENSOR_WORKLOAD_GEOMETRY &g=l.schema.geometry;
        UINT64 decoded[]={g.batch,g.keys,pair?g.rotary:g.latent},scales[]={g.batch,g.keys,1};
        require(_p.values[value].shape==std::vector<UINT64>(decoded,decoded+3) &&
            _p.values[product.inputs[1]].shape==std::vector<UINT64>(scales,scales+3),"q5_decode_broadcast_axes");
        Stream(product.inputs[1],l.encoded[pair+1],l);
        if(l.schema.encoding=="u4-offset8-f32.v1") {
            const VHO_TENSOR_OPERATION &sub=Op(payload,"common.sub");
            const VHO_TENSOR_OPERATION &fill=Op(sub.inputs[1],"common.fill_like");
            require(fill.attributes.find("attr.value_bits")->second=="41000000","q5_u4_offset");payload=sub.inputs[0];
        }
        const VHO_TENSOR_OPERATION &convert=Op(payload,"common.convert");
        require(_p.values[convert.output].dtype=="float32","q5_conversion");payload=convert.inputs[0];
        if(l.schema.encoding=="u4-offset8-f32.v1") payload=Op(payload,"common.unpack_u4").inputs[0];
        Stream(payload,l.encoded[pair],l);
    }
    /* Refuse extra consumers of eliminated prefix values. The closed attention
     * domain is replaced as a whole; cache maintenance/result copies survive. */
    void Finish(VHO_Q5_LAYOUT &l) {
        for(unsigned i=0;i<_p.operations.size();++i) {
            const VHO_TENSOR_OPERATION &op=_p.operations[i];
            for(unsigned k=0;k<op.inputs.size();++k) if(_defs.count(op.inputs[k]) && _removed.count(_defs[op.inputs[k]]))
                require(_removed.count(i) || (i>=l.start && i<l.start+38),"q5_removed_value_escapes");
        }
        for(std::set<unsigned>::const_iterator i=_removed.begin();i!=_removed.end();++i) {
            require(*i<l.start && _p.values[_p.operations[*i].output].role==0,"q5_removal_scope");l.removed.push_back(*i);
        }
    }
};
}
/* Whole-source admission is independent of any current serving phase label. */
bool VHO_q5_layout(const VHO_TENSOR_PROGRAM &p,VHO_Q5_LAYOUT &out,std::string &why) {
    try {
        VHO_Q5_LAYOUT l;require(VHO_tensor_serving_schema(p,l.schema,why),"q5_source_schema");
        require(l.schema.encoding=="s8-token-scaled-f32.v1" || l.schema.encoding=="u4-offset8-f32.v1","q5_encoding_not_implemented");
        const VHO_TENSOR_WORKLOAD_GEOMETRY &g=l.schema.geometry;unsigned matches=0;
        for(unsigned i=0;i+38<=p.operations.size();++i) if(p.operations[i].name=="common.matmul" &&
            p.operations[i].inputs[0]==g.query_value && p.operations[i+1].inputs[0]==g.latent_value) {l.start=i;++matches;}
        require(matches==1 && l.start+42==p.operations.size(),"q5_complete_source_scope");
        for(unsigned k=0;k<2;++k) {
            require(p.operations[l.start+(k?5:2)].attributes.find("attr.permutation")->second=="0,1,3,2","q5_cache_transpose_axes");
            UINT64 shape[]={g.batch,1,g.keys,k?g.rotary:g.latent};
            require(p.values[p.operations[l.start+(k?4:1)].output].shape==std::vector<UINT64>(shape,shape+4),"q5_cache_reshape_axes");
        }
        for(unsigned k=0;k<4;++k) {
            const VHO_TENSOR_OPERATION &copy=p.operations[l.start+38+k];
            require(copy.name=="copy" && copy.output==l.schema.returned[k],"q5_return_order");l.encoded[k]=copy.inputs[0];
        }
        Q5_SOURCE source(p);source.Decode(g.latent_value,0,l);source.Decode(g.rotary_value,2,l);source.Finish(l);
        // Reuse the first tile-sized prefixes of the now-dead decoded arrays.
        require(p.values[g.latent_value].storage==g.latent_value && p.values[g.rotary_value].storage==g.rotary_value,"q5_tile_storage");
        out=l;why.clear();return true;
    } catch(const std::exception &e) {why=e.what();return false;}
}
