/* Copyright (C) 2026 Open64 Project */
/* Harvest live active-PU LNO facts into detached immutable values. Design:
 * doc/AI-COMPILER-SEARCH-M2B-OWNED-SNAPSHOT-v1.0.md. Native pointers exist only
 * in this synchronous builder; it never normalizes or transforms WHIRL. */
#include "dsl_high_snapshot.h"
#include "dsl_high_admission.h"
#include "pu_info.h"
#include "wn_util.h"
#include "wn_pragmas.h"
#include "lnopt_main.h"
#include "access_vector.h"
#include "dep_graph.h"
#include "dep.h"
#include "opt_du.h"
#include "opt_alias_interface.h"
#include "opt_alias_mgr.h"
#include "targ_const.h"
#include "config.h"
#include "config_opt.h"
#include "config_targ.h"
#include <algorithm>
#include <map>
#include <sstream>
#include <set>
#include <cstdlib>

#ifdef LNO_DSL_HIGH_SNAPSHOT_TEST
/* Test-build callback only; the production component has no mutation hook. */
static void DSL_high_native_test(WN *, DU_MANAGER *, ALIAS_MANAGER *, ARRAY_DIRECTED_GRAPH16 *,
    const DSL_HIGH_EPOCH &, const DSL_HIGH_BINDINGS &, const DSL_HIGH_SNAPSHOT &);
#endif

namespace {
/* Format owned scalar fields without pointer/table-index spellings. */
template<class T> std::string DSL_high_number(T value) {
    /* TYPE_ID is a byte, so promote it rather than writing a control byte. */
    std::ostringstream text; text << +value; return text.str();
}
/* Length framing makes concatenated names and values unambiguous. */
void DSL_high_field(std::string &key, const std::string &value) {
    key += DSL_high_number(value.size()) + ":" + value;
}
/* Compact display namespace only; full keys are used for every comparison. */
std::string DSL_high_digest(const std::string &key) {
    uint64_t hash = UINT64_C(14695981039346656037);
    for (size_t i = 0; i < key.size(); ++i) {
        hash ^= (unsigned char)key[i]; hash *= UINT64_C(1099511628211);
    }
    std::ostringstream text; text << std::hex << hash; return text.str();
}
/* Comments/preamble markers have no executable identity or ordinal. */
bool DSL_high_ignored(WN *node) {
    return WN_operator(node) == OPR_COMMENT || (WN_operator(node) == OPR_PRAGMA &&
        WN_pragma(node) == WN_PRAGMA_PREAMBLE_END);
}
}

class DSL_HIGH_CAPTURE {
    DSL_HIGH_SNAPSHOT _result;
    std::map<WN *, std::string> _paths;
    std::vector<WN *> _nodes, _memory;
    std::map<ST *, unsigned> _formals;
    std::set<WN *> _array_bases;
    std::map<ST *, unsigned> _symbols;
    DU_MANAGER *_du;
    ALIAS_MANAGER *_alias;
    ARRAY_DIRECTED_GRAPH16 *_graph;
    std::string _reason;
    /* Refuse missing coverage instead of returning a partially useful object. */
    bool Refuse(const char *reason) { _reason = reason; return false; }
    /* Structural scalar/pointer type identity, never process-local TY_IDX.
     * Aggregate/recursive types are outside this admitted snapshot subset. */
    std::string Type(TY_IDX type, unsigned depth = 0) {
        if (!type || depth > 8) { Refuse("unsupported_type"); return "unknown"; }
        TY_KIND kind = TY_kind(type);
        if (kind != KIND_SCALAR && kind != KIND_POINTER && kind != KIND_VOID) {
            Refuse("unsupported_type"); return "unknown";
        }
        std::string value = DSL_high_number(kind) + "/" + DSL_high_number(TY_mtype(type)) +
            "/" + DSL_high_number(TY_size(type)) + "/" + DSL_high_number(TY_align(type)) +
            "/" + DSL_high_number(TY_is_volatile(type)) + "/" + DSL_high_number(TY_is_const(type)) +
            "/" + DSL_high_number(TY_is_restrict(type));
        if (kind == KIND_POINTER) value += "->" + Type(TY_pointed(type), depth + 1);
        return value;
    }
    /* First semantic occurrence disambiguates local symbols with equal names.
     * PREG offsets are semantic locations within the qualified source image. */
    std::string Symbol(ST *symbol) {
        if (!symbol) { Refuse("missing_symbol"); return "unknown"; }
        if (!_symbols.count(symbol)) {
            unsigned ordinal = _symbols.size(); _symbols[symbol] = ordinal;
        }
        std::string value = "S" + DSL_high_number(_symbols[symbol]) + "/" +
            DSL_high_number(ST_class(symbol)) + "/" + DSL_high_number(ST_sclass(symbol)) + "/";
        DSL_high_field(value, ST_name(symbol));
        if (ST_class(symbol) == CLASS_CONST) {
            const TCON &constant = STC_val(symbol);
            TYPE_ID type = TCON_ty(constant);
            if (type == MTYPE_F4) value += DSL_high_number(TCON_uval(constant));
            else if (type == MTYPE_F8 || type == MTYPE_I8 || type == MTYPE_U8)
                value += DSL_high_number(TCON_k0(constant));
            else if (MTYPE_is_integral(type)) value += DSL_high_number(TCON_uval(constant));
            else Refuse("unsupported_constant");
            value += "/" + DSL_high_number(type);
        } else {
            value += Type(ST_type(symbol)) + "/" + DSL_high_number(ST_ofst(symbol)) + "/" +
                DSL_high_number(ST_pt_to_unique_mem(symbol)) + "/" + DSL_high_number(ST_is_value_parm(symbol)) + "/" +
                DSL_high_number(ST_addr_saved(symbol)) + "/" + DSL_high_number(ST_addr_passed(symbol));
            const char *trait=ST_tensor_metadata(ST_st_idx(symbol),"tensor.cache.trait");
            if(trait) { DSL_high_field(value,"cache-trait-v1"); DSL_high_field(value,trait); }
        }
        return value;
    }
    /* Index executable semantic paths and serialize source fields. Unsupported
     * payloads fail closed; source positions and native map IDs are excluded. */
    bool Walk(WN *node, const std::string &path, const std::string &parent_loop, unsigned depth,
              bool conditional = false) {
        if (DSL_high_ignored(node)) return true;
        if (depth > 256 || _nodes.size() >= 200000) return Refuse("source_budget");
        _paths[node] = "N@" + path; _nodes.push_back(node);
        OPCODE opcode = WN_opcode(node); OPERATOR op = WN_operator(node);
        std::string payload = OPCODE_name(opcode);
        if (OPCODE_has_sym(opcode)) DSL_high_field(payload, Symbol(WN_st(node)));
        if (OPCODE_has_offset(opcode)) payload += "/offset=" + DSL_high_number(WN_offset(node));
        if (OPCODE_has_1ty(opcode)) DSL_high_field(payload, Type(WN_ty(node)));
        if (OPCODE_has_2ty(opcode)) DSL_high_field(payload, Type(WN_load_addr_ty(node)));
        if (op == OPR_INTCONST) payload += "/value=" + DSL_high_number(WN_const_val(node));
        if (op == OPR_ARRAY) payload += "/element=" + DSL_high_number(WN_element_size(node));
        if (op == OPR_CVTL) payload += "/bits=" + DSL_high_number(WN_cvtl_bits(node));
        if (op == OPR_INTRINSIC_OP) payload += "/intrinsic=" + DSL_high_number(WN_intrinsic(node));
        if (op == OPR_LDID || op == OPR_STID || op == OPR_ILOAD || op == OPR_ISTORE)
            payload += "/field=" + DSL_high_number(WN_field_id(node));
        if (OPCODE_has_flags(opcode)) payload += "/flags=" + DSL_high_number(WN_flag(node));
        if (OPCODE_has_label(opcode) ||
            op == OPR_EXTRACT_BITS || op == OPR_COMPOSE_BITS || op == OPR_ALLOCA)
            return _reason = "unsupported_semantic_payload/" + std::string(OPCODE_name(opcode)), false;
        DSL_high_field(_result._source_key, path); DSL_high_field(_result._source_key, payload);
        std::string loop = parent_loop;
        if (op == OPR_DO_LOOP) {
            OPCODE comparison; BOOL increasing;
            DSL_HIGH_LOOP record;
            record.id = "L@" + path; record.parent = parent_loop;
            record.induction = Symbol(WN_st(WN_index(node))) + "/" + DSL_high_number(WN_offset(WN_index(node)));
            record.lower = WN_const_val(WN_LOOP_LowerBound(node));
            record.upper = WN_const_val(WN_LOOP_UpperBound(node, &comparison));
            record.step = WN_const_val(WN_LOOP_Increment(node, &increasing));
            record.comparison = OPCODE_name(comparison);
            _result._loops.push_back(record); loop = record.id;
        }
        if (op == OPR_ILOAD || op == OPR_ISTORE) {
            if (_memory.size() >= 512) return Refuse("alias_pair_budget");
            _memory.push_back(node);
            DSL_HIGH_ACCESS record;
            record.id = "A@" + path; record.node = _paths[node]; record.enclosing_loop = loop;
            record.write = op == OPR_ISTORE;
            WN *array = record.write ? WN_kid1(node) : WN_kid0(node);
            record.element_bytes = WN_element_size(array); record.element_type = Type(WN_ty(node));
            record.element_encoding=MTYPE_name(WN_desc(node));
            record.byte_offset = WN_offset(node); record.conditional = conditional;
            std::string group = path.substr(0,path.find('.',5));
            record.execution_group = "N@" + group;
            record.execution_order = (unsigned)strtoul(group.c_str()+6,NULL,10);
            WN *base = WN_kid0(array); _array_bases.insert(base);
            record.direct_formal = WN_operator(base)==OPR_LDID && !WN_offset(base) && _formals.count(WN_st(base));
            if(record.direct_formal) record.storage = "F/" + DSL_high_number(_formals[WN_st(base)]);
            if(WN_kid_count(array)==3 && WN_operator(WN_kid1(array))==OPR_INTCONST)
                record.array_extent = WN_const_val(WN_kid1(array));
            std::string owner=parent_loop;
            while(owner!="none") {
                bool found=false;
                for(size_t k=0;k<_result._loops.size();++k) if(_result._loops[k].id==owner) {
                    record.loop_ids.insert(record.loop_ids.begin(),owner);
                    owner=_result._loops[k].parent;found=true;break;
                }
                if(!found) return Refuse("unrepresented_loop_owner");
            }
            /* Base expression is joined to its copied source subtree. */
            record.base = "N@" + path + (record.write ? ".1.0" : ".0.0");
            ACCESS_ARRAY *access = (ACCESS_ARRAY *)WN_MAP_Get(LNO_Info_Map, array);
            for (INT i = 0; i < access->Num_Vec(); ++i) {
                ACCESS_VECTOR *vector = access->Dim(i);
                if (vector->Contains_Non_Lin_Symb() || vector->Delinearized_Symbol)
                    return Refuse("nonlinear_or_delinearized_access");
                DSL_HIGH_DIM dimension;
                dimension.constant = vector->Const_Offset; dimension.nonconstant_loops = vector->Non_Const_Loops();
                for (unsigned j = 0; j < vector->Nest_Depth(); ++j)
                    dimension.loop_coefficients.push_back(vector->Loop_Coeff(j));
                if (vector->Lin_Symb) {
                    INTSYMB_CONST_ITER iter(vector->Lin_Symb);
                    for (const INTSYMB_NODE *term = iter.First(); !iter.Is_Empty(); term = iter.Next()) {
                        std::string symbol = term->Symbol.Is_Formal() ?
                            "formal/" + DSL_high_number(term->Symbol.Formal_Number()) : Symbol(term->Symbol.St());
                        symbol += "/" + DSL_high_number(term->Symbol.WN_Offset()) + "/" +
                            DSL_high_number(term->Symbol.Type) + "/" + DSL_high_number(term->Coeff);
                        dimension.linear_symbols.push_back(symbol);
                    }
                    std::sort(dimension.linear_symbols.begin(), dimension.linear_symbols.end());
                }
                record.dimensions.push_back(dimension);
            }
            _result._accesses.push_back(record);
        }
        if (op == OPR_BLOCK) {
            unsigned ordinal = 0;
            for (WN *statement = WN_first(node); statement; statement = WN_next(statement)) {
                if (DSL_high_ignored(statement)) continue;
                if (!Walk(statement, path + ".s" + DSL_high_number(ordinal++), loop, depth + 1,conditional)) return false;
            }
        } else for (INT i = 0; i < WN_kid_count(node); ++i)
            if (WN_kid(node, i) && !Walk(WN_kid(node, i), path + "." + DSL_high_number(i), loop, depth + 1,
                conditional || op==OPR_IF || op==OPR_SELECT || op==OPR_CSELECT || op==OPR_CAND || op==OPR_CIOR)) return false;
        return _reason.empty();
    }
    /* External/missing DU endpoints remain explicit unknowns, never independence. */
    std::string Endpoint(WN *node) const {
        std::map<WN *, std::string>::const_iterator found = _paths.find(node);
        return found == _paths.end() ? "external_or_unrepresented" : found->second;
    }
    /* Copy complete graph vertices/edges including scalar dependences; stable
     * ordering is independent of native graph allocation/iteration order. */
    bool Graph() {
        std::vector<std::string> vertices, edges;
        for (VINDEX16 vertex = _graph->Get_Vertex(); vertex; vertex = _graph->Get_Next_Vertex(vertex)) {
            std::string path = Endpoint(_graph->Get_Wn(vertex));
            if (path == "external_or_unrepresented") return Refuse("unrepresented_graph_vertex");
            vertices.push_back(path);
        }
        std::sort(vertices.begin(), vertices.end());
        for (size_t i = 0; i < vertices.size(); ++i) DSL_high_field(_result._facts_key, "vertex/" + vertices[i]);
        std::map<std::string, DSL_HIGH_DEPENDENCE> ordered;
        for (EINDEX16 edge = _graph->Get_Edge(); edge; edge = _graph->Get_Next_Edge(edge)) {
            if (ordered.size() >= 100000) return Refuse("dependence_budget");
            DEPV_ARRAY *array = _graph->Depv_Array(edge);
            if (!array) return Refuse("missing_dependence_vectors");
            DSL_HIGH_DEPENDENCE record;
            record.source = Endpoint(_graph->Get_Wn(_graph->Get_Source(edge)));
            record.sink = Endpoint(_graph->Get_Wn(_graph->Get_Sink(edge)));
            record.unused_dimensions = array->Num_Unused_Dim();
            std::string key = record.source + "->" + record.sink + "/unused=" + DSL_high_number(record.unused_dimensions);
            for (unsigned i = 0; i < array->Num_Vec(); ++i) {
                std::vector<DSL_HIGH_DEP_COMPONENT> vector;
                key += "/v";
                for (unsigned j = 0; j < array->Num_Dim(); ++j) {
                    DEP dep = DEPV_Dep(array->Depv(i), j); DSL_HIGH_DEP_COMPONENT component;
                    component.is_distance = DEP_IsDistance(dep);
                    component.value = component.is_distance ? DEP_Distance(dep) : DEP_Direction(dep);
                    component.direction = DEP_Direction(dep); component.distance_bound = DEP_DistanceBound(dep);
                    key += component.is_distance ? "/distance=" : "/direction=";
                    key += DSL_high_number(component.value) + "/dir=" + DSL_high_number(component.direction) +
                        "/bound=" + DSL_high_number(component.distance_bound);
                    vector.push_back(component);
                }
                record.vectors.push_back(vector);
            }
            ordered[key] = record;
        }
        for (std::map<std::string, DSL_HIGH_DEPENDENCE>::const_iterator i = ordered.begin(); i != ordered.end(); ++i) {
            DSL_high_field(_result._facts_key, i->first); _result._dependences.push_back(i->second);
        }
        return true;
    }
    /* Preserve both directions of scalar DU with explicit completeness. */
    void Copy_DU() {
        for (size_t i = 0; i < _nodes.size(); ++i) {
            WN *node = _nodes[i]; OPERATOR op = WN_operator(node);
            if (op != OPR_LDID && op != OPR_STID) continue;
            DSL_HIGH_DU record; record.node = _paths[node]; record.definitions = op == OPR_LDID;
            record.available = false; record.incomplete = true; record.loop = "unavailable";
            if (record.definitions) {
                DEF_LIST *list = _du->Ud_Get_Def(node);
                if (list) {
                    record.available = true; record.incomplete = list->Incomplete();
                    record.loop = list->Loop_stmt() ? Endpoint(list->Loop_stmt()) : "none";
                    DEF_LIST_ITER iter(list);
                    for (DU_NODE *entry = iter.First(); !iter.Is_Empty(); entry = iter.Next())
                        record.endpoints.push_back(Endpoint(entry->Wn()));
                }
            } else {
                USE_LIST *list = _du->Du_Get_Use(node);
                if (list) {
                    record.available = true; record.incomplete = list->Incomplete(); record.loop = "not_applicable";
                    USE_LIST_ITER iter(list);
                    for (DU_NODE *entry = iter.First(); !iter.Is_Empty(); entry = iter.Next())
                        record.endpoints.push_back(Endpoint(entry->Wn()));
                }
            }
            std::sort(record.endpoints.begin(), record.endpoints.end());
            std::string key = record.node + "/DU=" + DSL_high_number(record.definitions) + "/available=" +
                DSL_high_number(record.available) + "/incomplete=" + DSL_high_number(record.incomplete) + "/loop=" + record.loop;
            for (size_t j = 0; j < record.endpoints.size(); ++j) DSL_high_field(key, record.endpoints[j]);
            DSL_high_field(_result._facts_key, key); _result._du.push_back(record);
        }
    }
    /* Encode typed loops/accesses and all pairwise alias answers into an exact
     * equality key. This makes analysis-only edits detectable on revalidation. */
    void Copy_memory_facts() {
        DSL_high_field(_result._facts_key, "alias_context=" + DSL_high_number(_alias->Pu_context()));
        for (size_t i = 0; i < _result._loops.size(); ++i) {
            const DSL_HIGH_LOOP &loop = _result._loops[i];
            std::string key = loop.id + "/parent=" + loop.parent;
            DSL_high_field(key, loop.induction); DSL_high_field(key, loop.comparison);
            key += "/" + DSL_high_number(loop.lower) + "/" + DSL_high_number(loop.upper) + "/" + DSL_high_number(loop.step);
            DSL_high_field(_result._facts_key, key);
        }
        for (size_t i = 0; i < _result._accesses.size(); ++i) {
            const DSL_HIGH_ACCESS &access = _result._accesses[i];
            std::string key = access.id + "/" + access.node + "/" + access.enclosing_loop + "/" + access.base;
            DSL_high_field(key, access.element_type);
            key += "/write=" + DSL_high_number(access.write) + "/bytes=" + DSL_high_number(access.element_bytes);
            for (size_t j = 0; j < access.dimensions.size(); ++j) {
                const DSL_HIGH_DIM &dimension = access.dimensions[j];
                key += "/dim=" + DSL_high_number(dimension.constant) + "/nonconstant=" + DSL_high_number(dimension.nonconstant_loops);
                for (size_t k = 0; k < dimension.loop_coefficients.size(); ++k)
                    key += "/coeff=" + DSL_high_number(dimension.loop_coefficients[k]);
                for (size_t k = 0; k < dimension.linear_symbols.size(); ++k) DSL_high_field(key, dimension.linear_symbols[k]);
            }
            DSL_high_field(_result._facts_key, key);
        }
        for (size_t i = 0; i < _memory.size(); ++i) for (size_t j = i; j < _memory.size(); ++j) {
            DSL_HIGH_ALIAS record;
            record.first = _result._accesses[i].id; record.second = _result._accesses[j].id;
            record.result = Aliased(_alias, _memory[i], _memory[j]);
            record.disjoint = record.result==NOT_ALIASED;
            DSL_high_field(_result._facts_key, record.first + "/" + record.second + "/alias=" + DSL_high_number(record.result));
            _result._aliases.push_back(record);
        }
    }
    /* Copy formal identity and pointer uses without retaining ST/WN addresses.
     * This extension is independently sealed; no legacy binding key changes. */
    void Copy_storage_facts(WN *scope) {
        const char *recipe=ST_tensor_metadata(WN_st_idx(scope),"tensor.high.recipe_key");
        const char *schema=ST_tensor_metadata(WN_st_idx(scope),"tensor.high.schema");
        const char *contract=ST_tensor_metadata(WN_st_idx(scope),"tensor.high.buffer_contract");
        _result._producer_recipe=recipe?recipe:"";_result._producer_schema=schema?schema:"";
        _result._buffer_contract=contract?contract:"";
        DSL_high_field(_result._owned_memory_key,_result._producer_recipe);
        DSL_high_field(_result._owned_memory_key,_result._producer_schema);
        DSL_high_field(_result._owned_memory_key,_result._buffer_contract);
        for(size_t i=0;i<_memory.size();++i) if(_result._accesses[i].write) {
            WN *rhs=WN_kid0(_memory[i]);
            if(WN_operator(rhs)==OPR_ILOAD && _paths.count(rhs))
                _result._accesses[i].copied_from_access="A@"+_paths[rhs].substr(2);
        }
        for(INT i=0;i<WN_num_formals(scope);++i) {
            ST *formal=WN_st(WN_formal(scope,i));TY_IDX ty=ST_type(formal);
            DSL_HIGH_STORAGE record;record.id="F/"+DSL_high_number(i);record.formal=i;
            record.symbol=Symbol(formal);
            record.pointer_formal=TY_kind(ty)==KIND_POINTER && ST_is_value_parm(formal);
            record.restrict_qualified=record.pointer_formal && TY_is_restrict(ty);
            if(record.pointer_formal) {
                record.element_type=Type(TY_pointed(ty));record.element_bytes=TY_size(TY_pointed(ty));
                record.element_encoding=MTYPE_name(TY_mtype(TY_pointed(ty)));
            }
            const char *trait=ST_tensor_metadata(ST_st_idx(formal),"tensor.cache.trait");
            record.cache_trait=trait?trait:"";
            for(size_t j=0;j<_nodes.size();++j) {
                WN *node=_nodes[j];OPERATOR op=WN_operator(node);
                if((op==OPR_STID || op==OPR_LDID) && WN_st(node)==formal) {
                    if(op==OPR_STID) record.base_stable=false;
                    else if(!_array_bases.count(node)) record.uses_only_as_array_base=false;
                }
            }
            _result._storages.push_back(record);
            std::string key=record.id+"/"+record.element_type+"/"+DSL_high_number(record.element_bytes)+"/"+
                DSL_high_number(record.pointer_formal)+"/"+DSL_high_number(record.restrict_qualified)+"/"+
                DSL_high_number(record.base_stable)+"/"+DSL_high_number(record.uses_only_as_array_base);
            DSL_high_field(key,record.symbol);DSL_high_field(key,record.cache_trait);
            DSL_high_field(key,record.element_encoding);
            DSL_high_field(_result._owned_memory_key,key);
        }
        for(size_t i=0;i<_result._accesses.size();++i) {
            const DSL_HIGH_ACCESS &a=_result._accesses[i];
            std::string key=a.id+"/"+a.storage+"/"+a.execution_group+"/"+DSL_high_number(a.execution_order)+"/"+
                DSL_high_number(a.array_extent)+"/"+DSL_high_number(a.byte_offset)+"/"+
                DSL_high_number(a.direct_formal)+"/"+DSL_high_number(a.conditional);
            for(size_t j=0;j<a.loop_ids.size();++j) DSL_high_field(key,a.loop_ids[j]);
            DSL_high_field(key,a.copied_from_access);
            DSL_high_field(key,a.element_encoding);
            DSL_high_field(_result._owned_memory_key,key);
        }
        for(size_t i=0;i<_result._aliases.size();++i)
            DSL_high_field(_result._owned_memory_key,DSL_high_number(_result._aliases[i].disjoint));
    }
public:
    /* Borrow native services only until Build returns; result owns all data. */
    DSL_HIGH_CAPTURE(DU_MANAGER *du, ALIAS_MANAGER *alias, ARRAY_DIRECTED_GRAPH16 *graph) :
        _du(du), _alias(alias), _graph(graph) {}
    /* Preflight active scope and context, build privately, publish only on full
     * coverage. Any refusal clears the caller's previous result. */
    bool Build(WN *scope, const DSL_HIGH_EPOCH &epoch, const DSL_HIGH_BINDINGS &bindings,
        DSL_HIGH_SNAPSHOT &output, std::string &reason) {
        output = DSL_HIGH_SNAPSHOT();
        if (bindings.unit.empty() || bindings.numeric.empty() || bindings.target.empty() ||
            bindings.manifest.empty() || bindings.model.empty() || !epoch.Matches(epoch.Stamp()))
            return reason = "context_or_epoch_unavailable", false;
        if (!LNO_DSL_admit_common_high(scope, _du, _alias, _graph, NULL))
            return reason = "form_or_analysis_refused", false;
        _result._stamp = epoch.Stamp(); _result._bindings = bindings;
        DSL_high_field(_result._source_key, bindings.unit);
        DSL_high_field(_result._source_key, ST_name(WN_st(scope)));
        /* Formals in ABI order matter even if a buffer is not used in the body. */
        for (INT i = 0; i < WN_num_formals(scope); ++i)
            { _formals[WN_st(WN_formal(scope,i))]=i;
              DSL_high_field(_result._source_key, Symbol(WN_st(WN_formal(scope, i)))); }
        if (!Walk(WN_func_body(scope), "body", "none", 0) || !_reason.empty() || !Graph())
            return reason = _reason, false;
        Copy_DU(); Copy_memory_facts();
        Copy_storage_facts(scope);
        _result._identity = "high-v1/" + DSL_high_digest(_result._source_key);
        _result._valid = true; output = _result; reason = "captured"; return true;
    }
};

/* Record effective controls without granting reassociation or claiming a GPU
 * model. Profile/manifest handoff is a later explicit driver obligation. */
DSL_HIGH_BINDINGS LNO_DSL_high_bindings(const char *unit) {
    DSL_HIGH_BINDINGS bindings;
    bindings.unit = unit ? unit : "";
    bindings.numeric = "ieee=" + DSL_high_number(IEEE_Arithmetic) + "/roundoff=" + DSL_high_number(Roundoff_Level) +
        "/fast_math=" + DSL_high_number(OPT_Fast_Math);
    bindings.target = "abi=" + DSL_high_number(Target_ABI) + "/isa=" + DSL_high_number(Target_ISA) + "/gpu_resources=unbound";
    bindings.manifest = "unbound"; bindings.model = "unbound";
    return bindings;
}

/* Keep the native builder scoped to this synchronous capture call. */
bool LNO_DSL_capture_high(WN *scope, DU_MANAGER *du, ALIAS_MANAGER *alias,
    ARRAY_DIRECTED_GRAPH16 *graph, const DSL_HIGH_EPOCH &epoch, const DSL_HIGH_BINDINGS &bindings,
    DSL_HIGH_SNAPSHOT &output, std::string &reason) {
    DSL_HIGH_CAPTURE capture(du, alias, graph);
    bool captured = capture.Build(scope, epoch, bindings, output, reason);
#ifdef LNO_DSL_HIGH_SNAPSHOT_TEST
    static bool inside_test = false;
    if (captured && !inside_test) {
        inside_test = true;
        DSL_high_native_test(scope, du, alias, graph, epoch, bindings, output);
        inside_test = false;
    }
#endif
    return captured;
}
/* Reject stale tokens before native pointer access; current tokens additionally
 * require equality of complete freshly captured source and analysis facts. */
bool LNO_DSL_revalidate_high(WN *scope, DU_MANAGER *du, ALIAS_MANAGER *alias,
    ARRAY_DIRECTED_GRAPH16 *graph, const DSL_HIGH_EPOCH &epoch, const DSL_HIGH_BINDINGS &bindings,
    const DSL_HIGH_SNAPSHOT &snapshot, std::string &reason) {
    if (!snapshot.Is_current(epoch, bindings)) return reason = "stale_epoch_or_context", false;
    DSL_HIGH_SNAPSHOT current;
    if (!LNO_DSL_capture_high(scope, du, alias, graph, epoch, bindings, current, reason)) return false;
    if (!snapshot.Same_content(current)) return reason = "source_or_analysis_changed", false;
    reason = "current"; return true;
}

#ifdef LNO_DSL_HIGH_SNAPSHOT_TEST
#include "tests/dsl_high_snapshot_native_test.inc"
#endif
