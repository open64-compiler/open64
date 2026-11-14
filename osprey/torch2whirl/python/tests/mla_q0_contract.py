"""Q0 review-only serving ABI constructor/validator, not native admission.

No compiler hook imports this module. Exact versions make fixture obligations
reviewable while Q1-Q3 implement the corresponding native/engine boundaries.
"""
import copy
import json
import math

SCHEMA = "open64.mla-serving-cache.v1"
ENCODINGS = {"s8-token": "s8-token-rne127-f32.v1", "u4-token": "u4-offset8-rne7-f32.v1"}
TOP = {"schema","encoding","shape","paging","workload","state","alignment","numerics","capabilities"}

def require(ok, message):
    if not ok:
        raise ValueError("mla-serving-v1: " + message)

def exact(value, names):
    require(type(value) is dict and set(value)==set(names), "unknown/missing fields")

def uint(value, positive=False):
    return type(value) is int and (0 < value if positive else 0 <= value) and value <= 16777216

def make_contract(meta, inputs):
    paged = meta["storage"] == "paged"
    append = meta["append"]
    value = dict(schema=SCHEMA,encoding=ENCODINGS[meta["format"]],
        shape=dict(batch=meta["batch"],heads=meta["heads"],queries=meta["queries"],
                   length=meta["length"],latent=meta["latent"],rotary=meta["rotary"]),
        paging=dict(kind=meta["storage"],page_tokens=meta["page_tokens"],
                    physical_pages=meta.get("physical_pages",0),
                    table=inputs[6].tolist() if paged else [],
                    tokens=inputs[13].tolist() if paged else list(range(meta["length"])),
                    write_slots=inputs[12].tolist() if paged and append else [],
                    shared_readonly_prefix_tokens=meta["page_tokens"] if paged else 0),
        workload=dict(phase=[meta["phase"]]*meta["batch"],phase_evidence=meta["phase_evidence"],
                      query_positions=[list(range(meta["length"]-meta["queries"],meta["length"])) for _ in range(meta["batch"])],
                      history_lengths=[meta["length"]-append]*meta["batch"],
                      valid_lengths=[meta["length"]]*meta["batch"]),
        state=dict(update="functional-append" if append else "read-only" if paged else "construction",
                   append_tokens=append,incoming_version=1,result_version=2 if append or not paged else 1,
                   allocation_generation=1,publication="after-all-results" if append or not paged else "already-ready",
                   release="after-all-readers"),
        alignment=dict(global_guarantee_bytes=0,tensor_map="not-created",gpu_alignment="unestablished"),
        numerics=dict(scale_dtype="float32",compute_dtype="float32",accum_dtype="float32",
                      scale_floor=2.0**-12,rounding="ties-to-even",empty_output="zero",empty_lse="-inf"),
        capabilities=dict(native="unsupported-Q0",tma="unsupported-Q0",inference="not-implemented"))
    # Fake quantization has no packed serving representation and no byte credit.
    if meta["fake"]:
        value["encoding"]="fake-s8-float32.v1"
    validate_contract(value)
    return value

def validate_contract(value):
    """Validate structural/range/cross-field obligations; no phase inference."""
    exact(value,TOP)
    require(value["schema"]==SCHEMA,"schema")
    require(value["encoding"] in set(ENCODINGS.values())|{"fake-s8-float32.v1"},"encoding")
    shape=value["shape"];exact(shape,("batch","heads","queries","length","latent","rotary"))
    require(all(uint(x,True) for x in shape.values()),"shape")
    b,h,q,l,c,r=(shape[x] for x in ("batch","heads","queries","length","latent","rotary"))
    require(b<=16 and h<=128 and q<=l and c<=4096 and r<=4096,"bounded Q0 shape")
    if value["encoding"]=="u4-offset8-rne7-f32.v1":require(c%2==r%2==0,"U4 even logical columns")
    p=value["paging"];exact(p,("kind","page_tokens","physical_pages","table","tokens","write_slots","shared_readonly_prefix_tokens"))
    require(p["kind"] in ("dense","paged"),"layout")
    require(type(p["tokens"]) is list and all(uint(x) for x in p["tokens"]) and p["tokens"]==list(range(l)),"token-span contract")
    require(all(uint(p[x]) for x in ("page_tokens","physical_pages","shared_readonly_prefix_tokens")),"page integers")
    s=value["state"];exact(s,("update","append_tokens","incoming_version","result_version","allocation_generation","publication","release"))
    require(s["update"] in ("functional-append","read-only","construction") and uint(s["append_tokens"]),"update")
    require(all(uint(s[x],True) for x in ("incoming_version","result_version","allocation_generation")),"versions")
    require(s["release"]=="after-all-readers","release")
    updated=s["update"]!="read-only"
    require(s["result_version"]==s["incoming_version"]+int(updated),"result version")
    require(s["publication"]==("after-all-results" if updated else "already-ready"),"bundle publication")
    require((s["update"]=="functional-append" and 0<s["append_tokens"]<=l) or
            (s["update"]!="functional-append" and s["append_tokens"]==0),"append extent")
    w=value["workload"];exact(w,("phase","phase_evidence","query_positions","history_lengths","valid_lengths"))
    require(type(w["phase"]) is list and len(w["phase"])==b and
            all(x in ("prefill","decode","speculative-verify","unknown") for x in w["phase"]),"per-request phase")
    require(w["phase_evidence"]=="fixture-intent-not-inferred-from-graph","Q0 provenance")
    require(all(type(w[x]) is list and len(w[x])==b and all(uint(n) for n in w[x]) for x in ("valid_lengths","history_lengths")),"length integers")
    require(w["valid_lengths"]==[l]*b and w["history_lengths"]==[l-s["append_tokens"]]*b,"length/update join")
    require(type(w["query_positions"]) is list and all(type(row) is list and all(uint(x) for x in row) for row in w["query_positions"]) and w["query_positions"]==[list(range(l-q,l)) for _ in range(b)],"query positions")
    # No Q=1=>decode or Q>1=>prefill rule exists here.
    if p["kind"]=="dense":
        require(p["page_tokens"]==p["physical_pages"]==0 and p["table"]==[] and p["write_slots"]==[] and
                p["shared_readonly_prefix_tokens"]==0 and s["update"]=="construction","dense construction")
    else:
        require(uint(p["page_tokens"],True) and uint(p["physical_pages"],True),"page geometry")
        page,n=p["page_tokens"],p["physical_pages"]
        require(p["shared_readonly_prefix_tokens"]<=l,"prefix bounds")
        require(type(p["table"]) is list and len(p["table"])==b,"page table batch")
        pages=(l+page-1)//page
        require(all(type(row) is list and len(row)==pages and all(uint(x) and x<n for x in row)
                    for row in p["table"]),"physical page table")
        require(value["encoding"]!="fake-s8-float32.v1","fake paged profile not admitted")
        if s["update"]=="functional-append":
            require(type(p["write_slots"]) is list and len(p["write_slots"])==b,"write batches")
            slots=[]
            for batch,row in enumerate(p["write_slots"]):
                require(type(row) is list and len(row)==s["append_tokens"],"write count")
                expected=[p["table"][batch][t//page]*page+t%page for t in range(l-s["append_tokens"],l)]
                require(row==expected,"append/page mapping")
                require(l-s["append_tokens"]>=p["shared_readonly_prefix_tokens"],"shared-prefix overwrite")
                slots.extend(row)
            require(len(set(slots))==len(slots),"duplicate writes")
            # Every physical target must be private to its logical token, also
            # against other read-only logical aliases in this invocation.
            for batch,row in enumerate(p["table"]):
                for token in range(l):
                    slot=row[token//page]*page+token%page
                    if slot in slots:
                        require(p["write_slots"][batch].count(slot)==1 and token>=l-s["append_tokens"],
                                "write aliases retained cache reads")
        else:
            require(s["update"]=="read-only" and p["write_slots"]==[],"read-only pages")
    exact(value["alignment"],("global_guarantee_bytes","tensor_map","gpu_alignment"))
    require(uint(value["alignment"]["global_guarantee_bytes"]),"alignment integer")
    require(value["alignment"]==dict(global_guarantee_bytes=0,tensor_map="not-created",gpu_alignment="unestablished"),
            "CPU fixture cannot grant GPU alignment")
    exact(value["numerics"],("scale_dtype","compute_dtype","accum_dtype","scale_floor","rounding","empty_output","empty_lse"))
    require(value["numerics"]==dict(scale_dtype="float32",compute_dtype="float32",accum_dtype="float32",
        scale_floor=2.0**-12,rounding="ties-to-even",empty_output="zero",empty_lse="-inf"),"numerical profile")
    require(value["capabilities"]==dict(native="unsupported-Q0",tma="unsupported-Q0",inference="not-implemented"),
            "unimplemented capabilities")
    return copy.deepcopy(value)

def loads(text):
    """Strict review parser; rejects duplicates/NUL/nonfinite JSON."""
    require(type(text) is str and "\x00" not in text and len(text)<=1024*1024,"document bounds")
    def pairs(items):
        result={}
        for key,value in items:
            require(key not in result,"duplicate field")
            result[key]=value
        return result
    def constant(_):
        raise ValueError("mla-serving-v1: nonfinite JSON")
    return validate_contract(json.loads(text,object_pairs_hook=pairs,parse_constant=constant))
