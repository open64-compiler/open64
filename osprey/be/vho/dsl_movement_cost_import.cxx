/* Copyright (C) 2026 Open64 Project */
/* Driver-owned H3 supplement import before source processing. Existing
 * RapidJSON and SHA byte service only; exact device/profile binding, no GPU
 * observation or reinterpretation of old timing fields. No native IR changes.
 * Design: doc/AI-COMPILER-H3-MOVEMENT-COSTING-v1.0.md. */
#include "dsl_movement_cost.h"
#include "rapidjson/document.h"
#include <set>
#include <stdexcept>
#include <cstring>
namespace {
/* Release paths retain diagnostic reasons without exposing profile payloads. */
void VHO_h3_import_need(bool ok,const char *why) {if(!ok) throw std::runtime_error(why);}
/* Copy bounded printable tokens, rejecting embedded NUL and control bytes. */
std::string VHO_h3_import_text(const rapidjson::Value &v) {
    VHO_h3_import_need(v.IsString() && v.GetStringLength() && v.GetStringLength()<=1024,"h3_model_string");
    std::string out(v.GetString(),v.GetStringLength());
    for(size_t i=0;i<out.size();++i) VHO_h3_import_need((unsigned char)out[i]>=33 && (unsigned char)out[i]<127,"h3_model_token");
    return out;
}
/* Integer control/count fields never admit FP coercion or negative values. */
uint64_t VHO_h3_import_uint(const rapidjson::Value &v) {
    VHO_h3_import_need(v.IsUint64(),"h3_model_unsigned");return v.GetUint64();
}
/* Physical event inputs keep native JSON precision; range checks follow. */
double VHO_h3_import_number(const rapidjson::Value &v) {
    VHO_h3_import_need(v.IsNumber(),"h3_model_number");return v.GetDouble();
}
}
/* Read bounded bytes once, authenticate before parsing, then configure a fresh
 * owner atomically. Duplicate/extra/missing fields and wrong bindings refuse. */
bool VHO_h3_import(const char *path,const char *digest,const DSL_GPU_PLAN_SNAPSHOT &gpu,DSL_H3_SEARCH &out,std::string &why) {
    try {
        VHO_h3_import_need(path && digest && std::strlen(digest)==64 && gpu.Bound(),"h3_requires_pinned_model_and_imported_device");
        FILE *file=fopen(path,"rb");VHO_h3_import_need(file!=NULL,"h3_model_open");
        std::string bytes;char buffer[4096];size_t count;
        while((count=fread(buffer,1,sizeof(buffer),file))!=0) {
            if(bytes.size()+count>65536) {fclose(file);throw std::runtime_error("h3_model_size");}
            bytes.append(buffer,count);
        }
        bool read_ok=!ferror(file);fclose(file);VHO_h3_import_need(read_ok && DSL_movement_digest(bytes)==digest,"h3_model_digest");
        rapidjson::Document j;j.Parse<rapidjson::kParseIterativeFlag|rapidjson::kParseValidateEncodingFlag|rapidjson::kParseFullPrecisionFlag>(bytes.data(),bytes.size());
        const char *names[]={"schema","id","device_binding","provenance","objective","transaction_bytes","queue_per_sm","warm_hit_ppm","contention_ppm","issue_per_sm","barrier_ns","shuffle_ns","fence_ns","site_limit","request_limit","memory_limit","deadline_us","rsqrt_ns","spill_request_bytes_per_cta"};
        VHO_h3_import_need(!j.HasParseError() && j.IsObject() && j.MemberCount()==19,"h3_model_schema");
        std::set<std::string> expected(names,names+19),seen;
        for(rapidjson::Value::ConstMemberIterator i=j.MemberBegin();i!=j.MemberEnd();++i) {
            std::string name(i->name.GetString(),i->name.GetStringLength());VHO_h3_import_need(expected.count(name) && seen.insert(name).second,"h3_model_members");
        }
        VHO_h3_import_need(VHO_h3_import_text(j["schema"])=="open64.movement-cost-model.v1","h3_model_version");
        DSL_H3_MODEL model;model.id=VHO_h3_import_text(j["id"]);model.binding=VHO_h3_import_text(j["device_binding"]);
        model.provenance=VHO_h3_import_text(j["provenance"]);model.objective=VHO_h3_import_text(j["objective"]);model.key=digest;
        uint64_t *ints[]={&model.transaction_bytes,&model.queue_per_sm,&model.warm_hit_ppm,&model.contention_ppm,&model.site_limit,&model.request_limit,&model.memory_limit,&model.deadline_us};
        const char *integer_names[]={"transaction_bytes","queue_per_sm","warm_hit_ppm","contention_ppm","site_limit","request_limit","memory_limit","deadline_us"};
        for(unsigned i=0;i<8;++i) *ints[i]=VHO_h3_import_uint(j[integer_names[i]]);
        model.issue_per_sm=VHO_h3_import_number(j["issue_per_sm"]);model.barrier_ns=VHO_h3_import_number(j["barrier_ns"]);
        model.shuffle_ns=VHO_h3_import_number(j["shuffle_ns"]);model.fence_ns=VHO_h3_import_number(j["fence_ns"]);model.rsqrt_ns=VHO_h3_import_number(j["rsqrt_ns"]);
        if(j["spill_request_bytes_per_cta"].IsUint64()) model.spill_per_cta=DSL_MOVEMENT_BYTES(j["spill_request_bytes_per_cta"].GetUint64());
        return out.Configure(model,gpu,why);
    } catch(const std::exception &error) {why=error.what();return false;}
}
