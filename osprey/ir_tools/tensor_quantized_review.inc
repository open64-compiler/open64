/* Copyright (C) 2026 Open64 Project */
/* Q1 independent mapped-source observation, never an optimization/search path.
 * The standalone driver owns file/PU scope and JSON. No JSON/native frontend
 * dependency is added to be.so. Actual native kids and mapped attributes are
 * inspected after gatekeeper verification, then evaluated by the test oracle.
 * Design: doc/AI-COMPILER-MLA-Q1-NATIVE-INGESTION-v1.0.md. */
Json::Value q1_source_review(PU_Info *pu)
{
    DSL_GATEKEEPER_RESULT checked;
    need(DSL_Gatekeeper_Verify_PU(pu,stderr,&checked),"Q1 mapped source gatekeeper refused");
    UINT32 symbols=ST_Table_Size(CURRENT_SYMTAB),types=TY_Table_Size();
    WN *entry=PU_Info_tree_ptr(pu);
    std::vector<open64_q1::DSC_QUANTIZED_SOURCE_VALUE> graph;
    std::map<ST_IDX,size_t> ids;
    std::vector<ST_IDX> results;
    std::map<ST_IDX,ST_IDX> returned;
    Json::Value report;report["schema"]="open64.quantized-source-review.v1";
    report["values"]=Json::Value(Json::arrayValue);report["results"]=Json::Value(Json::arrayValue);
    struct Q1_SOURCE_CAPTURE {
        /* Copy descriptor/metadata facts from the driver-selected PU; no intern. */
        static Json::Value Copy_value(ST_IDX st,open64_q1::DSC_QUANTIZED_SOURCE_VALUE *v) {
            TY_IDX ty=ST_type(St_Table[st]);
            need(TY_is_tensor_extension(ty),"Q1 non-tensor value");
            v->name=ST_name(St_Table[st]);
            const char *dtype=TY_tensor_attribute(ty,TY_TENSOR_SCHEMA_DTYPE);
            need(dtype!=NULL,"Q1 missing dtype");v->dtype=dtype;
            UINT64 dimensions[16];UINT32 rank=0;
            need(DSL_Shape_Parse_Static_Dimensions(TY_tensor_attribute(ty,TY_TENSOR_SCHEMA_SHAPE),
                 dimensions,16,&rank),"Q1 non-static shape");
            std::vector<UINT64> shape(dimensions,dimensions+rank);
            v->shape.assign(shape.begin(),shape.end());
            Json::Value j;j["name"]=v->name;j["dtype"]=v->dtype;j["shape"]=Json::Value(Json::arrayValue);
            for(size_t d=0;d<shape.size();++d) j["shape"].append(Json::UInt64(shape[d]));
            const char *role=TY_tensor_attribute(ty,TY_TENSOR_SCHEMA_TRAITS);
            const char *encoding=TY_tensor_attribute(ty,TY_TENSOR_SCHEMA_QUANTIZATION);
            j["trait"]=role?role:"";j["encoding"]=encoding?encoding:"";
            const char *scale=ST_tensor_metadata(st,"cache.inference.scale");
            const char *evidence=ST_tensor_metadata(st,"cache.inference.evidence");
            j["scale"]=scale?scale:"";j["evidence"]=evidence?evidence:"";
            return j;
        }
        /* Observe source-order definitions and returned values, refusing effects
         * or unsupported control before any observation can be published. */
        static void Capture_block(PU_Info *pu,WN *tree,std::vector<open64_q1::DSC_QUANTIZED_SOURCE_VALUE> *g,
                          std::map<ST_IDX,size_t> *ids,std::map<ST_IDX,ST_IDX> *returned,
                          Json::Value *records) {
            need(WN_operator(tree)==OPR_BLOCK,"Q1 expected source block");
            for(WN *s=WN_first(tree);s;s=WN_next(s)) {
                if(WN_operator(s)==OPR_BLOCK) {Capture_block(pu,s,g,ids,returned,records);continue;}
                if(WN_operator(s)==OPR_RETURN || WN_operator(s)==OPR_COMMENT) continue;
                need(WN_operator(s)==OPR_STID && !WN_offset(s),"Q1 unsupported source statement");
                WN *rhs=WN_kid0(s);ST_IDX st=WN_st_idx(s);
                if(WN_operator(rhs)==OPR_LDID) {
                    need(ST_tensor_metadata(st,"dsl.pu.result_role") && ids->count(WN_st_idx(rhs)),"Q1 unsupported source copy");
                    need(returned->insert(std::make_pair(st,WN_st_idx(rhs))).second,"Q1 duplicate result write");continue;
                }
                need(DSL_WN_Is_Native(rhs) && !ids->count(st),"Q1 unsupported native definition");
                DSL_IR_VALUE_RECORD value_record;DSL_IR_NODE_RECORD node;DSL_IR_OPCODE_DESCRIPTOR_RECORD descriptor;
                need(DSL_IR_Image_Find_Definition_Value(pu,s,&value_record) &&
                     DSL_IR_Image_Get_Node(value_record.producer_node_id,&node) &&
                     DSL_IR_Image_Get_Opcode_Descriptor(node.opcode_descriptor_id,&descriptor),"Q1 missing managed definition");
                open64_q1::DSC_QUANTIZED_SOURCE_VALUE v;Json::Value j=Copy_value(st,&v);
                v.op=Index_To_Str(descriptor.stable_name);v.version=descriptor.version;
                j["operands"]=Json::Value(Json::arrayValue);j["attrs"]=Json::Value(Json::objectValue);
                for(INT k=0;k<WN_kid_count(rhs);++k) {
                    WN *kid=WN_kid(rhs,k);
                    need(WN_operator(kid)==OPR_LDID && !WN_offset(kid) && ids->count(WN_st_idx(kid)),"Q1 undefined/non-native operand");
                    size_t id=(*ids)[WN_st_idx(kid)];v.kids.push_back(id);j["operands"].append((*g)[id].name);
                }
                for(UINT32 a=0;a<node.attribute_count;++a) {
                    DSL_IR_ATTRIBUTE_RECORD attribute;
                    need(DSL_IR_Image_Get_Attribute(node.first_attribute_id+a,&attribute),"Q1 missing attribute");
                    std::string key=Index_To_Str(attribute.name),text=Index_To_Str(attribute.value);
                    need(v.attrs.insert(std::make_pair(key,text)).second,"Q1 duplicate attribute");j["attrs"][key]=text;
                }
                j["opcode"]=v.op;j["version"]=v.version;
                (*ids)[st]=g->size();g->push_back(v);records->append(j);
            }
        }
    };
    for(INT i=0;i<WN_num_formals(entry);++i) {
        ST_IDX st=WN_st_idx(WN_formal(entry,i));
        if(ST_tensor_metadata(st,"dsl.pu.result_role")) {results.push_back(st);continue;}
        open64_q1::DSC_QUANTIZED_SOURCE_VALUE v;Json::Value j=Q1_SOURCE_CAPTURE::Copy_value(st,&v);v.op="formal";v.version=0;
        j["opcode"]="formal";j["version"]=0;j["attrs"]=Json::Value(Json::objectValue);j["operands"]=Json::Value(Json::arrayValue);
        need(ids.insert(std::make_pair(st,graph.size())).second,"Q1 duplicate formal");graph.push_back(v);report["values"].append(j);
    }
    Q1_SOURCE_CAPTURE::Capture_block(pu,WN_func_body(entry),&graph,&ids,&returned,&report["values"]);
    for(size_t i=0;i<results.size();++i) {
        need(returned.count(results[i]),"Q1 unwritten output");report["results"].append(graph[ids[returned[results[i]]]].name);
    }
    open64_q1::DSC_QUANTIZED_SOURCE_FACTS facts=open64_q1::DSC_QUANTIZED_SOURCE_INFERENCE(graph).Infer();
    for(open64_q1::DSC_QUANTIZED_SOURCE_FACTS::const_iterator f=facts.begin();f!=facts.end();++f) {
        const Json::Value &record=report["values"][(Json::ArrayIndex)f->first];
        need(record["trait"].asString()==f->second.role && record["scale"].asString()==f->second.scale &&
             record["evidence"].asString()==f->second.evidence,"Q1 inferred cache facts were lost or contradicted");
        if(graph[f->first].dtype=="int8" || graph[f->first].dtype=="uint8")
            need(record["encoding"].asString()==f->second.encoding,"Q1 encoded storage descriptor mismatch");
    }
    need(symbols==ST_Table_Size(CURRENT_SYMTAB) && types==TY_Table_Size(),"Q1 observation mutated symbol/type tables");
    report["native_nodes"]=checked.native_node_count;report["inferred_values"]=Json::UInt64(facts.size());
    report["native_state_unchanged"]=true;report["search_invoked"]=false;report["executable_lowering"]=false;
    return report;
}

/* Q1 handoff is separately versioned: old kernel recipes remain byte-identical
 * and keep refusing quantized input. No physical candidate or schedule fields
 * are accepted through this semantic-only route. */
Json::Value q1_encode(const VHO_TENSOR_PROGRAM &p) {
    Json::Value j=encode(p);j["schema"]="open64.quantized-common-high.recipe.v1";j.removeMember("recipe_fingerprint");
    for(unsigned i=0;i<p.values.size();++i) {
        j["values"][i]["cache_trait"]=p.values[i].cache_trait;
        j["values"][i]["cache_encoding"]=p.values[i].cache_encoding;
        j["values"][i]["cache_scale"]=p.values[i].cache_scale;
        j["values"][i]["cache_evidence"]=p.values[i].cache_evidence;
    }
    for(unsigned i=0;i<j["operations"].size();++i) {
        j["operations"][i].removeMember("kernel");j["operations"][i].removeMember("block_x");j["operations"][i].removeMember("grid_x");
    }
    return j;
}
/* Parse bounded copied data, then revalidate all schemas before native init. */
VHO_TENSOR_PROGRAM q1_decode(const Json::Value &j) {
    need(j["schema"].asString()=="open64.quantized-common-high.recipe.v1" &&
         j["values"].isArray() && j["values"].size()<=4096 &&
         j["operations"].isArray() && j["operations"].size()<=4096,"Q1 recipe header");
    VHO_TENSOR_PROGRAM p;p.source_pu=j["source_pu"].asString();p.source_file=j["source_file"].asString();
    for(unsigned i=0;i<j["values"].size();++i) {
        const Json::Value &v=j["values"][i];VHO_TENSOR_VALUE value;
        need(v.isObject() && v["shape"].isArray() && v["shape"].size()<=16 &&
             v["role"].isUInt() && v["ordinal"].isUInt() && v["storage"].isUInt(),"Q1 recipe value");
        value.name=v["name"].asString();value.dtype=v["dtype"].asString();
        value.role=v["role"].asUInt();value.ordinal=v["ordinal"].asUInt();value.storage=v["storage"].asUInt();
        value.cache_trait=v["cache_trait"].asString();value.cache_encoding=v["cache_encoding"].asString();
        value.cache_scale=v["cache_scale"].asString();value.cache_evidence=v["cache_evidence"].asString();
        for(unsigned d=0;d<v["shape"].size();++d) {need(v["shape"][d].isUInt64(),"Q1 extent type");value.shape.push_back(v["shape"][d].asUInt64());}
        p.values.push_back(value);
    }
    for(unsigned i=0;i<j["operations"].size();++i) {
        const Json::Value &o=j["operations"][i];VHO_TENSOR_OPERATION op;
        need(o["version"].isUInt() && o["output"].isUInt() && o["source_node"].isUInt() && o["source_line"].isUInt() &&
             o["inputs"].isArray() && o["inputs"].size()<=3 && o["attributes"].isObject(),"Q1 recipe operation");
        op.name=o["name"].asString();op.version=o["version"].asUInt();op.output=o["output"].asUInt();
        op.source_node=o["source_node"].asUInt();op.source_line=o["source_line"].asUInt();
        for(unsigned k=0;k<o["inputs"].size();++k) {need(o["inputs"][k].isUInt(),"Q1 operand type");op.inputs.push_back(o["inputs"][k].asUInt());}
        std::vector<std::string> keys=o["attributes"].getMemberNames();
        for(unsigned k=0;k<keys.size();++k) {need(o["attributes"][keys[k]].isString(),"Q1 attribute type");op.attributes[keys[k]]=o["attributes"][keys[k]].asString();}
        p.operations.push_back(op);
    }
    need(VHO_tensor_validate_quantized_high(p,stderr),"Q1 recipe preflight failed");
    need(search_text(q1_encode(p))==search_text(j),"Q1 noncanonical recipe");return p;
}
