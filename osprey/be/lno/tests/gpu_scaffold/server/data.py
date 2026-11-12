"""Deterministic, dependency-free BF16 corpus and FP64 oracle generation."""
import hashlib
import json
import math
import random
import struct
from pathlib import Path

REGISTRY = {
    "gemm.dense.bf16-fp32.bf16.v1": ("gemm", "gemm.geometry.v1"),
    "mla.decode.paged.bf16-fp32.v1": ("mla", "mla.decode.context.v1"),
}
ROOT = Path(__file__).resolve().parents[1]
ATOL, RTOL = 0.003, 0.02


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def bf16_bits(value):
    bits = struct.unpack("<I", struct.pack("<f", value))[0]
    return ((bits + 0x7FFF + ((bits >> 16) & 1)) >> 16) & 0xFFFF


def bf16_value(bits):
    return struct.unpack("<f", struct.pack("<I", bits << 16))[0]


def write_values(path, fmt, values):
    values = list(values)
    path.write_bytes(struct.pack("<" + fmt * len(values), *values))


def tensor(path, count, rng):
    bits = [bf16_bits(rng.uniform(-0.5, 0.5)) for _ in range(count)]
    write_values(path, "H", bits)
    return [bf16_value(x) for x in bits]


def gemm_reference(p, a, b, c):
    m, n, k = p["m"], p["n"], p["k"]
    out = []
    for i in range(m):
        for j in range(n):
            products = (
                a[t*p["lda"]+i if p["trans_a"] else i*p["lda"]+t]
                * b[j*p["ldb"]+t if p["trans_b"] else t*p["ldb"]+j]
                for t in range(k)
            )
            out.append(p["alpha"]*math.fsum(products) + p["beta"]*c[i*p["ldc"]+j])
    return out


def mla_reference(p, q, kv, table, lengths):
    width = p["latent_dim"] + p["rope_dim"]
    out = []
    for b, length in enumerate(lengths):
        rows = []
        for t in range(length):
            page = table[b*p["max_pages"] + t//p["page_size"]]
            base = (page*p["page_size"] + t%p["page_size"])*width
            rows.append(kv[base:base+width])
        for h in range(p["heads"]):
            query = q[(b*p["heads"]+h)*width:(b*p["heads"]+h+1)*width]
            scores = [math.fsum(x*y for x,y in zip(query,row))*p["scale"] for row in rows]
            mx = max(scores)
            weights = [math.exp(x-mx) for x in scores]
            den = math.fsum(weights)
            for d in range(p["latent_dim"]):
                out.append(math.fsum(w*row[d] for w,row in zip(weights,rows))/den)
    return out


def generate(kind, root=None, force=False):
    root = Path(root or ROOT / "data") / kind
    if (root / "index.json").exists() and not force:
        verify(root)
        return root
    root.mkdir(parents=True, exist_ok=True)
    cases = []
    if kind == "gemm":
        specs = [("small",16,16,16,0,0,0),
                 ("larger",128,128,128,0,0,0),
                 ("boundary-transposed",33,47,29,1,1,3),
                 ("boundary-transpose-a",17,19,23,1,0,2),
                 ("boundary-transpose-b",19,17,23,0,1,2)]
    elif kind == "mla":
        specs = [("short-context",[32]), ("long-context",[2048]),
                 ("ragged-boundary",[17,65])]
    else:
        raise ValueError("unknown workload kind")
    for ordinal, spec in enumerate(specs):
        name = spec[0]
        folder = root / name
        folder.mkdir(exist_ok=True)
        seed = 7100 + ordinal
        rng = random.Random(seed)
        if kind == "gemm":
            _,m,n,k,ta,tb,pad = spec
            p = dict(m=m,n=n,k=k,trans_a=ta,trans_b=tb,
                     lda=(m if ta else k)+pad, ldb=(k if tb else n)+pad,
                     ldc=n+pad, ldd=n+pad, alpha=1.0, beta=0.5)
            a=tensor(folder/"a.bin",(k if ta else m)*p["lda"],rng)
            b=tensor(folder/"b.bin",(n if tb else k)*p["ldb"],rng)
            c=tensor(folder/"c.bin",m*p["ldc"],rng)
            golden=gemm_reference(p,a,b,c)
            fields=[p[x] for x in ("m","n","k","lda","ldb","ldc","ldd","trans_a","trans_b","alpha","beta")]
            flops=2*m*n*k
        else:
            lengths=spec[1]
            page_size=16
            max_pages=(max(lengths)+page_size-1)//page_size
            p=dict(batch=len(lengths),heads=4,latent_dim=32,rope_dim=8,
                   page_size=page_size,max_pages=max_pages,physical_pages=len(lengths)*max_pages,
                   scale=struct.unpack("<f",struct.pack("<f",1/math.sqrt(40)))[0])
            width=p["latent_dim"]+p["rope_dim"]
            q=tensor(folder/"q.bin",p["batch"]*p["heads"]*width,rng)
            kv=tensor(folder/"kv.bin",p["physical_pages"]*page_size*width,rng)
            table=list(range(p["physical_pages"]))
            rng.shuffle(table)
            # Unused page entries deliberately invalid; a correct kernel ignores them.
            for b,length in enumerate(lengths):
                for page in range((length+page_size-1)//page_size,max_pages):
                    table[b*max_pages+page]=-1
            write_values(folder/"page_table.bin","i",table)
            write_values(folder/"lengths.bin","i",lengths)
            golden=mla_reference(p,q,kv,table,lengths)
            fields=[p[x] for x in ("batch","heads","latent_dim","rope_dim","page_size","max_pages","scale")]
            flops=2*p["heads"]*sum(lengths)*(width+p["latent_dim"])
        write_values(folder/"golden.bin","d",golden)
        (folder/"params.txt").write_text(" ".join(map(str,fields))+"\n")
        metadata=dict(case_id=name,parameters=p,useful_flops=flops,seed=seed,
                      atol=ATOL,rtol=RTOL,reference="stdlib-fp64-fsum-v1",
                      output_elements=len(golden))
        (folder/"case.json").write_text(json.dumps(metadata,indent=2)+"\n")
        hashes={x.name:digest(x) for x in sorted(folder.iterdir()) if x.is_file()}
        cases.append(dict(case_id=name,files=hashes))
    workload_id=next(k for k,v in REGISTRY.items() if v[0]==kind)
    index=dict(schema_version=1,workload_id=workload_id,suite_id=REGISTRY[workload_id][1],
               flops="matmul-only; FMA=2; GEMM scaling/add and MLA softmax excluded",
               cases=cases)
    (root/"index.json").write_text(json.dumps(index,indent=2)+"\n")
    verify(root)
    return root


def verify(root):
    root=Path(root)
    index=json.loads((root/"index.json").read_text())
    for case in index["cases"]:
        for name,expected in case["files"].items():
            path=root/case["case_id"]/name
            if digest(path)!=expected:
                raise ValueError(f"corpus integrity failure: {path}")
    return index


if __name__ == "__main__":
    import argparse
    parser=argparse.ArgumentParser()
    parser.add_argument("kind",choices=["gemm","mla","all"])
    parser.add_argument("--force",action="store_true")
    args=parser.parse_args()
    for kind in (["gemm","mla"] if args.kind=="all" else [args.kind]):
        print(generate(kind,force=args.force))
