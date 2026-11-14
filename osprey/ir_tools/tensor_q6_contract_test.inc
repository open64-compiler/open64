/* Copyright (C) 2026 Open64 Project */
/* Test-only Q6 detached admission/transfer/lifetime negatives, linked only in
 * tensor_selected_bridge_test. No serving observation or native mutation.
 * Design: doc/AI-COMPILER-MLA-Q6-GPU-STAGING-v1.0.md. */
/* Check a genuine selected source plus exhaustive small transfer residues and
 * corrupted schedules. The independent byte oracle never calls the producer. */
unsigned q6_contract_test(const VHO_TENSOR_PROGRAM &p,const VHO_Q5_PLAN &plan,const std::string &native,
    const VHO_Q6_CONTROL &c,const VHO_Q6_MAPPING &m,const std::vector<VHO_Q6_ACTION> &trace) {
    unsigned checks=0;std::string why;VHO_Q6_MAPPING unchanged=m;
    // H2 consumes the selected contract, with no native allocation or timing.
    DSL_MOVEMENT_BINDING binding;binding.owner="Q6-test-owner";binding.source_key=native;
    binding.serving_key=m.binding_key;binding.device=DSL_GPU_PLAN_SNAPSHOT::Capture(VHO_DSL_GPU_Context);
    binding.device_key=binding.device.Cache_key();DSL_MOVEMENT_FACTS facts,again;
    need(VHO_q6_capture_movement(m,trace,binding,facts,why) && VHO_q6_capture_movement(m,trace,binding,again,why) &&
        facts.Same_content(again) && facts.Capture().lifetimes.size()==2*std::min(UINT64(3),m.tiles),"H2_Q6_owned_selected_capture");++checks;
    need(!facts.Capture().ledger.hbm.known && !facts.Capture().ledger.spills.known &&
        facts.Capture().work.active_last+facts.Capture().work.inactive_last==m.width,"H2_Q6_unknown_service_exact_tail");++checks;
    for(unsigned mutation=0;mutation<4;++mutation) {
        DSL_MOVEMENT_BINDING bad=binding;std::vector<VHO_Q6_ACTION> actions=trace;
        if(mutation==0) bad.source_key+="stale";
        if(mutation==1) bad.serving_key+="stale";
        if(mutation==2) actions.erase(actions.begin()+2); // raw wait
        if(mutation==3) for(unsigned i=0;i<actions.size();++i) if(actions[i].kind==VHO_Q6_RELEASE) {actions.erase(actions.begin()+i);break;}
        again=facts;need(!VHO_q6_capture_movement(m,actions,bad,again,why) && !again.Available(),"H2_Q6_stale_source_Q3_or_readiness");++checks;
    }
    DSL_MOVEMENT_BINDING changed_binding=binding;changed_binding.device_key+="other-profile";
    need(!VHO_q6_capture_movement(m,trace,changed_binding,again,why) || !facts.Same_content(again),"H2_Q6_profile_binding");++checks;

    for(unsigned mutation=0;mutation<6;++mutation) {
        VHO_Q6_CONTROL bad=c;VHO_Q5_PLAN stale=plan;
        if(mutation==0) bad.shared_limit=1;
        if(mutation==1) bad.max_threads=1;
        if(mutation==2) bad.arch=80;
        if(mutation==3) bad.delivery=3;
        if(mutation==4) stale.source_key+="stale";
        if(mutation==5) stale.native_key.clear();
        need(!VHO_q6_mapping(p,stale,native,bad,unchanged,why) && unchanged.key==m.key,"q6_bad_mapping_changed_output");++checks;
    }
    if(c.delivery==VHO_Q6_BULK_TMA) {
        VHO_Q6_CONTROL bad=c;bad.ptx=805;
        need(!VHO_q6_mapping(p,plan,native,bad,unchanged,why) && unchanged.key==m.key,"q6_old_ptx_admitted");++checks;
        bad=c;bad.toolkit=12070;need(!VHO_q6_mapping(p,plan,native,bad,unchanged,why),"q6_old_toolkit_admitted");++checks;
    }
    VHO_Q6_MAPPING changed=m;++changed.shared_bytes;std::vector<VHO_Q6_ACTION> untouched=trace;
    need(!VHO_q6_pipeline(changed,untouched,why) && untouched.size()==trace.size(),"q6_stale_mapping_trace");++checks;
    changed=m;changed.control.delivery=3;changed.key=VHO_q6_mapping_key(changed);
    need(!VHO_q6_verify_pipeline(changed,trace,why),"q6_unknown_delivery_protocol");++checks;
    for(unsigned i=0;i<trace.size();++i) {
        std::vector<VHO_Q6_ACTION> bad=trace;bad.erase(bad.begin()+i);
        need(!VHO_q6_verify_pipeline(m,bad,why),"q6_missing_action_admitted");++checks;
        bad=trace;++bad[i].pass;need(!VHO_q6_verify_pipeline(m,bad,why),"q6_cross_pass_action_admitted");++checks;
        bad=trace;bad[i].slot=2;need(!VHO_q6_verify_pipeline(m,bad,why),"q6_foreign_slot_admitted");++checks;
        bad=trace;bad[i].kind=8;need(!VHO_q6_verify_pipeline(m,bad,why),"q6_unknown_action_admitted");++checks;
        bad=trace;bad[i].tile=m.tiles;need(!VHO_q6_verify_pipeline(m,bad,why),"q6_out_of_range_tile_admitted");++checks;
    }
    for(unsigned source=0;source<16;++source) for(unsigned destination=0;destination<16;++destination)
        for(unsigned size=1;size<=65;++size) for(unsigned async=0;async<2;++async) {
            std::vector<VHO_Q6_COPY> parts;UINT64 tx=~UINT64(0);
            need(VHO_q6_partition(3,5,size,3+size,5+size,source,destination,async!=0,parts,tx,why),"q6_partition_refused_legal_run");
            UINT64 count=0,bulk=0;
            for(unsigned k=0;k<parts.size();++k) {
                const VHO_Q6_COPY &part=parts[k];
                need(part.bytes && part.source==3+count && part.destination==5+count && part.bytes<=size-count,"q6_partition_gap_overlap_or_overrun");
                if(part.bulk) {need(async && !((part.source+source)%16) && !((part.destination+destination)%16) && !(part.bytes%16),"q6_misaligned_bulk");bulk+=part.bytes;}
                count+=part.bytes;
            }
            unsigned sm=(3+source)%16,dm=(5+destination)%16;
            UINT64 prefix=std::min(UINT64(size),UINT64((16-sm)%16));
            UINT64 expected=async && sm==dm?(size-prefix)/16*16:0;
            need(count==size && bulk==tx && tx==expected,"q6_partition_byte_or_transaction_oracle");++checks;
        }
    // Failure cannot leak a partial range/transaction count to a caller.
    for(unsigned mutation=0;mutation<5;++mutation) {
        std::vector<VHO_Q6_COPY> parts(1,VHO_Q6_COPY(7,9,11,true));UINT64 tx=23;
        bool ok=mutation==0?VHO_q6_partition(0,0,0,1,1,0,0,true,parts,tx,why):
            mutation==1?VHO_q6_partition(1,0,16,16,16,0,0,true,parts,tx,why):
            mutation==2?VHO_q6_partition(0,1,16,16,16,0,0,true,parts,tx,why):
            mutation==3?VHO_q6_partition(~UINT64(0)-7,0,16,~UINT64(0),16,0,0,true,parts,tx,why):
            VHO_q6_partition(0,0,16,16,16,16,0,true,parts,tx,why);
        need(!ok && parts.size()==1 && parts[0].source==7 && parts[0].destination==9 && parts[0].bytes==11 && parts[0].bulk && tx==23,"q6_refusal_changed_partition_output");++checks;
    }
    for(unsigned tail=0;tail<2;++tail) for(unsigned residue=0;residue<16;++residue) {
        UINT64 tile=tail?m.tiles-1:0,tokens=std::min(m.width,m.keys-tile*m.width);UINT32 slot=tile%m.slots;
        std::vector<VHO_Q6_PHYSICAL_RUN> runs;
        if(tokens>1) runs.push_back(VHO_Q6_PHYSICAL_RUN(3,tokens/2));
        runs.push_back(VHO_Q6_PHYSICAL_RUN(m.width+5,tokens-(tokens>1?tokens/2:0)));
        UINT64 sizes[4];UINT32 residues[4];std::vector<unsigned char> source[4];
        for(unsigned stream=0;stream<4;++stream) {
            sizes[stream]=(3*m.width+33)*m.raw_row[stream];residues[stream]=(residue+stream*3)%16;
            source[stream].resize(sizes[stream]);
            for(unsigned i=0;i<source[stream].size();++i) source[stream][i]=(i*17+stream*31)%256;
        }
        std::vector<VHO_Q6_TRANSFER> transfers;UINT64 tx=~UINT64(0);
        need(VHO_q6_transfers(m,tile,slot,runs,sizes,residues,transfers,tx,why),"q6_joined_stream_transfers");
        std::vector<unsigned char> raw(m.decoded_offset+16,0xcd),expected=raw;UINT64 counted=0,position=0;
        for(unsigned r=0;r<runs.size();++r) {
            for(unsigned stream=0;stream<4;++stream) for(UINT64 byte=0;byte<runs[r].tokens*m.raw_row[stream];++byte)
                expected[slot*m.raw_stride+m.raw_offset[stream]+position*m.raw_row[stream]+byte]=source[stream][runs[r].first_slot*m.raw_row[stream]+byte];
            position+=runs[r].tokens;
        }
        for(unsigned i=0;i<transfers.size();++i) {
            const VHO_Q6_TRANSFER &t=transfers[i];need(t.stream<4,"q6_foreign_transfer_stream");
            memcpy(&raw[t.copy.destination],&source[t.stream][t.copy.source],t.copy.bytes);
            if(t.copy.bulk) counted+=t.copy.bytes;
        }
        need(raw==expected && counted==tx && (!tx || c.delivery==VHO_Q6_BULK_TMA),"q6_four_stream_byte_oracle_and_canaries");++checks;
        // No first fragment may escape on a late stream/range refusal.
        for(unsigned mutation=0;mutation<4;++mutation) {
            std::vector<VHO_Q6_PHYSICAL_RUN> bad=runs;UINT64 broken[4];memcpy(broken,sizes,sizeof(broken));
            UINT32 bad_residues[4];memcpy(bad_residues,residues,sizeof(bad_residues));
            if(mutation==0) ++bad.back().tokens;
            if(mutation==1) bad[0].first_slot=~UINT64(0);
            if(mutation==2) broken[3]=0;
            if(mutation==3) bad_residues[3]=16;
            transfers.assign(1,VHO_Q6_TRANSFER(2,VHO_Q6_COPY(7,9,11,true)));tx=23;
            need(!VHO_q6_transfers(m,tile,slot,bad,broken,bad_residues,transfers,tx,why) && transfers.size()==1 &&
                transfers[0].stream==2 && transfers[0].copy.source==7 && transfers[0].copy.bytes==11 && tx==23,"q6_bad_join_changed_transfer_output");++checks;
        }
    }
    return checks;
}
