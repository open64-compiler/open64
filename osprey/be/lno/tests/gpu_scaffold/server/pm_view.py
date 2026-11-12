"""Dependency-free standalone HTML/SVG display of actual PM sample points."""
import html
import json
from pathlib import Path


def esc(value):
    return html.escape(str(value),quote=True)


def chart(kernel):
    start,end=kernel["workload_start_ns"],kernel["workload_end_ns"]
    duration=(end-start)/1000
    parts=[f'<svg viewBox="0 0 820 250" role="img" aria-label="Memory and compute sample timelines">']
    for label,top,color in [("memory",30,"#07866d"),("compute",140,"#2563eb")]:
        channel=kernel["channels"][label]
        points=[s for s in channel["samples"] if start<=s["timestamp_ns"]<=end]
        ymax=max([0]+[s["value"] for s in points])
        ymax=100 if channel["unit"] in {"%","pct","percent"} and ymax<=100 else max(1,ymax)
        parts.append(f'<text x="10" y="{top}" font-size="13">{label.title()} ({esc(channel["unit"])})</text>')
        parts.append(f'<text x="10" y="{top+22}" font-size="11">0–{ymax:.3g}</text>')
        parts.append(f'<path d="M100 {top+5} V{top+75} H800" fill="none" stroke="#cbd5e1"/>')
        for s in points:
            x=100+700*(s["timestamp_ns"]-start)/(end-start)
            y=top+75-65*max(0,s["value"])/ymax
            title=f'{(s["timestamp_ns"]-start)/1000:.3f} us: {s["value"]:.5g} {channel["unit"]}'
            parts.append(f'<circle cx="{x:.3f}" cy="{y:.3f}" r="2.4" fill="{color}"><title>{esc(title)}</title></circle>')
        if not points:
            parts.append(f'<text x="125" y="{top+45}" fill="#92400e">No in-kernel samples</text>')
    for i in range(5):
        x=100+175*i
        parts.append(f'<text x="{x}" y="238" text-anchor="middle" font-size="11">{duration*i/4:.3f}</text>')
    parts.append('<text x="450" y="250" text-anchor="middle" font-size="11">Microseconds from this kernel’s measured start</text></svg>')
    return "".join(parts)


def render(case_id,roles,output,fixture=False):
    panels=[]
    for role in ["baseline","candidate"]:
        record=roles.get(role,{"status":"failed","reason":"not collected"})
        pieces=[f'<section><h2>{role.title()}</h2>']
        if record["status"]!="collected":
            pieces.append(f'<p>{esc(record.get("reason",record["status"]))}</p>')
        else:
            for k in record["data"]["kernels"]:
                pieces.append(f'<article><h3>{esc(k["kernel"])}</h3>')
                pieces.append(f'<p>One collection pass · actual interval: {k["interval_ns"]} ns / {k["interval_cycles"]} cycles</p>')
                pieces.append(chart(k))
                for label in ["memory","compute"]:
                    c=k["channels"][label]
                    pieces.append(f'<p class="metric">{label}: {esc(c["metric"])}<br>{esc(c["description"])}</p>')
                for warning in k["warnings"]:
                    pieces.append(f'<p class="warning">{esc(warning)}</p>')
                pieces.append('</article>')
        pieces.append('</section>')
        panels.append("".join(pieces))
    banner='<p class="warning">SYNTHETIC TEST FIXTURE — not measured GPU data.</p>' if fixture else ''
    document='''<!doctype html><html lang="en"><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Memory / compute PM timeline</title><style>
body{font:15px system-ui,sans-serif;background:#f4f7fb;color:#152238;margin:24px;line-height:1.45}
main{max-width:1500px;margin:auto}.panels{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:20px}
section{background:white;border:1px solid #dbe3ee;border-radius:10px;padding:18px;min-width:0}
article{border-top:1px solid #dbe3ee;margin-top:18px}svg{width:100%;height:auto}.metric{font:12px monospace;overflow-wrap:anywhere}
.warning{color:#92400e;background:#fffbeb;padding:8px}p{max-width:1100px}h1{font-size:25px}h3{overflow-wrap:anywhere}
@media(max-width:850px){.panels{grid-template-columns:1fr}body{margin:12px}}
</style><main><h1>Memory / compute PM timeline: '''+esc(case_id)+'''</h1>'''+banner+'''
<p>Dots are sampled device activity, not transfer or compute stage boundaries. Memory and compute use the same time axis within each kernel and independent vertical scales. Hover a dot for its value.</p>
<p>Each kernel is shown separately: replay timestamps do not reconstruct a continuous multi-kernel execution. Samples are clipped to measured kernel bounds; raw device samples are not context-filtered. Use an isolated GPU worker.</p>
<p>Coactivity does not establish per-tile overlap. Benchmark wall-clock ratios come exclusively from the separate unprofiled run.</p>
<div class="panels">'''+"".join(panels)+'''</div></main></html>'''
    Path(output).write_text(document)


if __name__=="__main__":
    import argparse
    parser=argparse.ArgumentParser()
    parser.add_argument("index",type=Path)
    parser.add_argument("output",type=Path)
    args=parser.parse_args()
    index=json.loads(args.index.read_text())
    roles={}
    for role,record in index["implementations"].items():
        roles[role]=dict(record)
        if record["status"]=="collected":
            roles[role]["data"]=json.loads((args.index.parent/record["samples"]).read_text())
    render(index["case_id"],roles,args.output,fixture=index.get("synthetic_fixture",False))
