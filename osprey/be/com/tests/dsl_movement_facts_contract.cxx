/* Copyright (C) 2026 Open64 Project */
/* H2 actual structural service and immutable profile/lifetime oracle. Fixtures
 * are synthetic, not native/GPU performance claims; no candidate is lowered.
 * Design: doc/AI-COMPILER-H2-OWNED-MOVEMENT-FACTS-v1.0.md. */
#include "dsl_movement_facts.h"
#include "dsl_gpu_profile.h"
#include <stdlib.h>

/* Fail independently of development assertion compilation. */
static void H2_require(bool value,const char *why) {if(!value) {fprintf(stderr,"H2: %s\n",why);exit(1);}}
/* Form an owned source projection with explicit disjointness and generation. */
static DSL_MOVEMENT_CAPTURE H2_source(const DSL_GPU_DEVICE_CONTEXT *context) {
    DSL_MOVEMENT_CAPTURE c;c.binding.family="fixture";c.binding.owner="native-PU";
    c.binding.source_key="full-native-source";c.binding.proof_key="owned-alias-and-access-proof";
    c.binding.controls="ordered-F32";c.binding.device=DSL_GPU_PLAN_SNAPSHOT::Capture(context);c.binding.device_key=c.binding.device.Cache_key();
    c.binding.activation=1;c.binding.mutation=0;c.binding.epoch_known=true;
    c.work.owner="row-owner";c.work.domain="row,feature";c.work.instances=4;c.work.width=17;c.work.active_last=17;
    c.work.axes.push_back("row");c.work.extents.push_back(4);c.work.axes.push_back("feature");c.work.extents.push_back(17);
    c.work.tail_policy="valid-before-address";c.work.numerical="ascending F32";
    std::vector<uint64_t> shape;shape.push_back(4);shape.push_back(17);
    c.storages.push_back(DSL_movement_dense_storage("X","allocation-1","g0","latent-cache",shape,4,true));
    DSL_MOVEMENT_ACCESS a;a.id="read-X";a.storage="X";a.version="g0";a.extent=272;a.read_bytes=816;
    a.multiplicity=3;a.reuse_proved=a.counts_known=true;a.reuse_scope="invocation";a.relation="X[row,k]";c.accesses.push_back(a);
    c.ledger.useful_reads=DSL_MOVEMENT_BYTES(272);c.ledger.source_reads=DSL_MOVEMENT_BYTES(816);
    DSL_movement_source_lifetime(c,"X","g0","invocation","owned-source-proof");return c;
}
/* Build separate raw/decode/consumer/drain/next-generation events. */
static DSL_MOVEMENT_CAPTURE H2_pipeline(const DSL_GPU_DEVICE_CONTEXT *context) {
    DSL_MOVEMENT_CAPTURE c=H2_source(context);c.events.clear();c.edges.clear();c.lifetimes.clear();
    c.storages.push_back(DSL_movement_dense_storage("decoded","allocation-2","g0","decoded",std::vector<uint64_t>(1,17),4,true));
    const char *kinds[]={"raw-ready","decoded-ready","consumer","readers-complete","reuse-ready"};
    for(unsigned i=0;i<5;++i) {DSL_MOVEMENT_EVENT e;e.id=kinds[i];e.kind=kinds[i];e.scope="CTA";e.evidence="existing-verified-pipeline";
        e.storage=i?"decoded":"X";e.version=i==4?"g1":"g0";c.events.push_back(e);if(i) c.edges.push_back(DSL_MOVEMENT_EDGE(i-1,i));}
    DSL_MOVEMENT_LIFETIME l;l.storage="decoded";l.raw_storage="X";l.version="g0";l.next_version="g1";
    l.raw_ready=0;l.decoded_ready=1;l.consumer=2;l.readers_done=3;l.reuse_ready=4;l.decoded=l.has_reuse=true;c.lifetimes.push_back(l);return c;
}
/* Production Create must refuse and clear a previously successful output. */
static void H2_refuse(const DSL_MOVEMENT_CAPTURE &c) {
    DSL_MOVEMENT_FACTS out;std::string reason;H2_require(DSL_MOVEMENT_FACTS::Create(H2_source(NULL),out,reason),"positive restore");
    H2_require(!DSL_MOVEMENT_FACTS::Create(c,out,reason) && !out.Available() && !reason.empty(),"negative retained stale facts");
}
/* Exercise profile binding, full identity, epoch invalidation and event gates. */
int main(int argc,char **argv) {
    H2_require(argc==4,"DEVICE PROFILE SHA");DSL_HARDWARE_TARGET_SELECTION target;DSL_GPU_DEVICE_CONTEXT_OWNER owner;
    H2_require(DSL_hardware_target_resolve(NULL,argv[1],&target,stderr) &&
        DSL_gpu_profile_resolve(target,argv[2],argv[3],"fixture-toolchain-v1",&owner,stderr),"production profile import");
    DSL_MOVEMENT_CAPTURE c=H2_source(owner.Context());DSL_MOVEMENT_FACTS base;std::string why;
    H2_require(DSL_MOVEMENT_FACTS::Create(c,base,why) && base.Is_current(c.binding,true),"positive source capture");
    H2_require(!base.Is_current(c.binding,false),"closed lifetime");
    for(unsigned i=0;i<13;++i) {
        DSL_MOVEMENT_CAPTURE changed=c;
        switch(i) {
        case 0:changed.storages[0].allocation="same-shape-other-storage";break;
        case 1:changed.storages[0].version=changed.accesses[0].version="g1";for(unsigned j=0;j<changed.events.size();++j) changed.events[j].version="g1";changed.lifetimes[0].version="g1";break;
        case 2:changed.storages[0].offset=4;changed.storages[0].capacity+=4;changed.accesses[0].offset=4;break;
        case 3:changed.binding.source_key+="changed";break;
        case 4:changed.binding.proof_key+="changed-view-stride";break;
        case 5:changed.binding.controls+="changed";break;
        case 6:changed.binding.serving_key="cache-generation-2/scale-generation-3/page-generation-4/publication-5";break;
        case 7:changed.binding.activation++;break;
        case 8:changed.binding.mutation++;break;
        case 9:changed.work.inactive_last=1;changed.work.active_last=16;break;
        case 10:changed.accesses[0].relation+="different-affine-map";break;
        case 11:changed.storages[0].alignment=16;break;
        case 12:changed.binding.epoch_known=false;break;
        }
        DSL_MOVEMENT_FACTS other;H2_require(DSL_MOVEMENT_FACTS::Create(changed,other,why) && !base.Same_content(other) && base.Key()!=other.Key(),"complete dependency key");
        if(i>=3 && i<=8) H2_require(!base.Is_current(changed.binding,true),"stale binding accepted");
    }
    for(unsigned i=0;i<12;++i) {
        DSL_MOVEMENT_CAPTURE changed=c;
        switch(i) {
        case 0:changed.storages[0].alias_proved=false;break;
        case 1:changed.storages[0].strides_known=false;break;
        case 2:changed.accesses[0].version="stale";break;
        case 3:changed.accesses[0].extent++;break;
        case 4:changed.storages[0].capacity--;break;
        case 5:changed.storages[0].strides[0]=~uint64_t(0);break;
        case 6:changed.storages.push_back(changed.storages[0]);break;
        case 7:changed.accesses.push_back(changed.accesses[0]);break;
        case 8:changed.binding.device_key="wrong-H20-profile";break;
        case 9:changed.ledger.hbm=DSL_MOVEMENT_BYTES(0);break;
        case 10:changed.ledger.spills=DSL_MOVEMENT_BYTES(0);break;
        case 11:changed.ledger.source_reads=DSL_MOVEMENT_BYTES(~uint64_t(0));break;
        }
        H2_refuse(changed);
    }
    DSL_MOVEMENT_CAPTURE unknown=c;unknown.storages[0].alias_proved=false;unknown.accesses[0].reuse_proved=false;
    DSL_MOVEMENT_FACTS unproved;H2_require(DSL_MOVEMENT_FACTS::Create(unknown,unproved,why),"unknown alias remains diagnostic without reuse permission");
    DSL_MOVEMENT_CAPTURE pipeline=H2_pipeline(owner.Context());DSL_MOVEMENT_FACTS stages;
    H2_require(DSL_MOVEMENT_FACTS::Create(pipeline,stages,why),"separate raw/decoded/reader generations");
    for(unsigned i=0;i<8;++i) {
        DSL_MOVEMENT_CAPTURE changed=pipeline;
        switch(i) {
        case 0:changed.edges.erase(changed.edges.begin());break;
        case 1:changed.edges.pop_back();break;
        case 2:changed.events[1].kind="raw-ready";break;
        case 3:changed.events[4].version="g0";break;
        case 4:changed.events[2].scope="other-CTA";break;
        case 5:changed.events[0].storage="decoded";break;
        case 6:changed.edges.push_back(DSL_MOVEMENT_EDGE(4,0));break;
        case 7:changed.lifetimes[0].readers_done=1;break;
        }
        H2_refuse(changed);
    }
    uint64_t value=7;H2_require(!DSL_movement_mul(~uint64_t(0),0,value) && value==7,"unknown product");
    H2_require(!DSL_movement_mul(uint64_t(1)<<63,2,value) && !DSL_movement_add(~uint64_t(0)-1,1,value),"byte overflow/sentinel");
    owner.Reset();H2_require(base.Available() && base.Capture().binding.device.Device()==argv[1] && stages.Available(),"owned data survives context destruction");
    base.Print(stdout,"AFTER_NATIVE_CLEANUP");printf("PASS H2 source-key changes=13 structural-refusals=20 device=%s GPU=false\n",argv[1]);return 0;
}
