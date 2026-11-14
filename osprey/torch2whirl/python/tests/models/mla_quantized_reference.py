"""Independent scalar quantization/page/expanded-attention oracle for Q0.

No torch or numpy. F32 steps model the source quantizer; attention reconstructs
expanded K/V per token using the existing independent FP64 oracle.
"""
import math
import struct
from .mla_reference import expanded_reference

def f32(value):
    return struct.unpack("<f",struct.pack("<f",value))[0]

def encode_row(row, kind):
    scale=f32(max(max(abs(x) for x in row),2.0**-12)/(127.0 if kind=="s8-token" else 7.0))
    codes=[max(-127,min(127,round(f32(x/scale)))) if kind=="s8-token" else
           max(0,min(15,round(f32(x/scale))+8)) for x in row]
    packed=codes if kind=="s8-token" else [codes[i] | (codes[i+1]<<4) for i in range(0,len(codes),2)]
    return packed,[scale]

def decode_row(row, scale, kind):
    codes=row if kind=="s8-token" else [x for byte in row for x in (byte&15,byte>>4)]
    return [f32((x if kind=="s8-token" else x-8)*scale[0]) for x in codes]

def encode(values, kind):
    data,scales=[],[]
    for rows in values:
        pair=[encode_row(row,kind) for row in rows]
        data.append([p[0] for p in pair]);scales.append([p[1] for p in pair])
    return data,scales

def reference(inputs, meta):
    """Return expanded-attention output/LSE and exact source successor storage."""
    import copy
    kind=meta["format"]
    if meta["storage"]=="dense":
        q,qr,raw,rope,uk,uv,allowed=inputs
        cache,cs=encode(raw,kind);rotary,rs=encode(rope,kind)
        c=[[decode_row(row,scale,kind) for row,scale in zip(rows,scales)] for rows,scales in zip(cache,cs)]
        r=[[decode_row(row,scale,kind) for row,scale in zip(rows,scales)] for rows,scales in zip(rotary,rs)]
        state=(c,cs,r,rs) if meta["fake"] else (cache,cs,rotary,rs)
    else:
        q,qr,cache,cs,rotary,rs,table,uk,uv,allowed,newc,newr,writes,tokens=copy.deepcopy(inputs)
        if meta["append"]:
            nq,ns=encode(newc,kind);nr,nrs=encode(newr,kind)
            for b in range(meta["batch"]):
                for i,slot in enumerate(writes[b]):
                    page,offset=divmod(slot,meta["page_tokens"])
                    cache[page][offset]=nq[b][i];cs[page][offset]=ns[b][i]
                    rotary[page][offset]=nr[b][i];rs[page][offset]=nrs[b][i]
        c,r=[],[]
        for b in range(meta["batch"]):
            bc,br=[],[]
            for token in tokens:
                logical,offset=divmod(token,meta["page_tokens"]);page=table[b][logical]
                bc.append(decode_row(cache[page][offset],cs[page][offset],kind))
                br.append(decode_row(rotary[page][offset],rs[page][offset],kind))
            c.append(bc);r.append(br)
        state=cache,cs,rotary,rs
    output=[[[None for _ in range(meta["queries"])] for _ in range(meta["heads"])] for _ in range(meta["batch"])]
    lse=copy.deepcopy(output)
    for b in range(meta["batch"]):
        for query in range(meta["queries"]):
            result,normalizer=expanded_reference(
                [[q[b][h][query] for h in range(meta["heads"])]],
                [[qr[b][h][query] for h in range(meta["heads"])]],[c[b]],[r[b]],uk,uv,
                [[allowed[b][h][query] for h in range(meta["heads"])]],meta["alpha"])
            for h in range(meta["heads"]):
                output[b][h][query]=result[0][h];lse[b][h][query]=normalizer[0][h]
    return (output,lse)+tuple(state)
