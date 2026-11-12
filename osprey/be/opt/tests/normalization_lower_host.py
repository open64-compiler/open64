"""Test-only execution adapter for actual WHIRL-derived scalar CUDA.

Design: doc/AI-COMPILER-LAYERNORM-EXECUTABLE-LOWERING-v0.1.md.
Only CTA execution mechanics and exact PTX scalar leaves are substituted.
Generated kernel control, memory accesses, indices and constants are retained.
This neither generates compiler kernels nor asserts GPU behavior/performance.
"""
import random
import re
import struct
from normalization_lower_trace_report import cuda_structure
from normalization_plan_trace_report import require
from normalization_oracle import reference_row, equivalent


def adapter(cuda,record):
    """Execute each uniform phase for every lane while preserving private state."""
    body=cuda_structure(cuda,record);g=record['guard']
    start=re.search(r'\n  ln_tid =',body);require(start is not None,'missing explicit coordinate initialization')
    declarations=body[:start.start()];code=body[start.start():]
    shared=re.findall(r'\s*__shared__ __align__\(16\) _IEEE32 ln_shared\[\d+LL\];',declarations)
    private=declarations
    for declaration in shared:private=private.replace(declaration,'')
    private_names=set(re.findall(r'\bln_\w+\b',private))
    require('ln_shared' not in private_names,'shared storage accidentally private')

    def scalar(match):
        """Replace only an already structurally audited exact scalar ASM leaf."""
        text=match[1];operation=re.findall(r'\b(add|sub|mul|div|sqrt)\.rn\.f32\b',text)[0]
        return f'ln_host_scalar({("add sub mul div sqrt".split().index(operation)+1)}, ln_out_ptr, ln_left_ptr, '+('nullptr' if operation=='sqrt' else 'ln_right_ptr')+');'

    code=re.sub(r'__asm__ volatile \((.*?)\);',scalar,code,flags=re.S)
    require(code.count('return;')==1 and code.rstrip().endswith('return;'),'unsupported nonuniform return')
    code=code.rsplit('return;',1)[0]
    phases=code.split('__syncthreads();')
    code_phases=[]
    for phase in phases:
        phase=re.sub(r'\bln_\w+\b',lambda m:'p.'+m[0] if m[0] in private_names else m[0],phase)
        code_phases.append('for (unsigned lane=0; lane<'+str(g['threads'])+'; ++lane) { State &p=states[lane]; Dim threadIdx={lane}, blockIdx={row};\n'+phase+'\n}\n')
    aliases=cuda[cuda.index('typedef char'):cuda.index('extern "C"')]
    shared_text='\n'.join(s.replace('__shared__ __align__(16)','alignas(16)') for s in shared)
    source=r'''#include <cfenv>
#include <cmath>
#include <cstdint>
#include <cstring>
#include <iomanip>
#include <iostream>
#include <stdexcept>
#include <vector>
#include <xmmintrin.h>
'''+aliases+r'''
static void ln_host_scalar(unsigned op,float *out,float *left,float *right) {
  volatile float a=*left,b=right?*right:0,value;
  switch(op) {case 1:value=a+b;break;case 2:value=a-b;break;case 3:value=a*b;break;
    case 4:value=a/b;break;case 5:value=std::sqrt(a);break;default:throw std::runtime_error("op");}
  *out=value;
}
struct Dim {unsigned x;};
struct State {
'''+private+'\n};\nstatic void kernel(float *x,float *gamma,float *beta,float *y,unsigned row) {\n'+shared_text+'\nState states['+str(g['threads'])+'];\nstd::memset(states,0xa5,sizeof(states));\n'+('std::memset(ln_shared,0x6d,sizeof(ln_shared));\n' if shared else '')+''.join(code_phases)+r'''
}
static uint32_t bits(float x) {uint32_t b;std::memcpy(&b,&x,4);return b;}
static void assign(float &x,uint32_t b) {std::memcpy(&x,&b,4);}
int main() {
  if(std::fesetround(FE_TONEAREST)) return 2;
  _mm_setcsr(_mm_getcsr() & ~((1u<<15)|(1u<<6)));
  unsigned cases;if(!(std::cin>>cases))return 2;
  for(unsigned c=0;c<cases;++c) {
    std::vector<float> x(ROWS*WIDTH+32),g(WIDTH+32),b(WIDTH+32),y(ROWS*WIDTH+32);
    for(auto *v:{&x,&g,&b,&y})for(float &f:*v)assign(f,0xdeadbeef);
    for(auto *v:{&x,&g,&b}) for(unsigned i=16;i<v->size()-16;++i) {uint32_t word;if(!(std::cin>>std::hex>>word>>std::dec))return 2;assign((*v)[i],word);}
    auto oldx=x,oldg=g,oldb=b;
    for(unsigned row=0;row<ROWS;++row)kernel(x.data()+16,g.data()+16,ALIAS?g.data()+16:b.data()+16,y.data()+16,row);
    for(unsigned i=0;i<x.size();++i)if(bits(x[i])!=bits(oldx[i]))return 3;
    for(unsigned i=0;i<g.size();++i)if(bits(g[i])!=bits(oldg[i])||bits(b[i])!=bits(oldb[i]))return 3;
    for(unsigned i=0;i<16;++i)if(bits(y[i])!=0xdeadbeef||bits(y[y.size()-1-i])!=0xdeadbeef)return 4;
    for(unsigned i=16;i<y.size()-16;++i)std::cout<<std::hex<<std::setw(8)<<std::setfill('0')<<bits(y[i])<<' ';
    std::cout<<std::dec<<'\n';
  }
}
'''
    alias=g['gamma']==g['beta']
    return f'#define ROWS {g["rows"]}u\n#define WIDTH {g["width"]}u\n#define ALIAS {int(alias)}\n'+source


def datasets(record):
    """Deterministic exceptional, subnormal, cancellation and random witnesses."""
    g=record['guard'];r,n=g['rows'],g['width'];rng=random.Random(0x4c4e34)
    pools=([0,0x80000000,0x3f800000,0xbf800000],
           [1,2,0x80000001,0x007fffff,0x00800000],
           [0x60ad78ec,0xe0ad78ec,0x3f800000,0x40000000],
           [0x7f800000,0xff800000,0x7fc12345,0x80000000],
           [0x7f7fffff,0xff7fffff,0x3f800001,0x33800000])
    sets=[]
    for pool in pools:
        sets.append(([pool[(i+i//n)%len(pool)] for i in range(r*n)],
                     [0x3f800000 if i%2 else 0xbf800000 for i in range(n)],
                     [0 if i%2 else 0x80000000 for i in range(n)]))
    for _ in range(3):
        sets.append(tuple([struct.unpack('<I',struct.pack('<f',rng.uniform(-8,8)))[0] for _ in range(size)] for size in (r*n,n,n)))
    return sets


def protocol(record,sets):
    """Canonical hex protocol preserves input bits without decimal conversion."""
    return str(len(sets))+'\n'+'\n'.join(' '.join(f'{v:08x}' for a in case for v in a) for case in sets)+'\n'


def audit_output(record,sets,output):
    """Compare every output with exact-rational binary32 semantics, NaN class only."""
    g=record['guard'];rows=output.splitlines();require(len(rows)==len(sets),'missing host numerical case')
    alias=g['gamma']==g['beta'];checks=0
    for case,line in zip(sets,rows):
        x,gamma,beta=case;actual=[int(v,16) for v in line.split()]
        require(len(actual)==g['rows']*g['width'],'wrong host output extent')
        expected=[]
        for row in range(g['rows']):expected+=reference_row(x[row*g['width']:(row+1)*g['width']],gamma,gamma if alias else beta,g['epsilon_bits'])['y']
        require(all(equivalent(a,b) for a,b in zip(actual,expected)),'WHIRL-derived host numerical mismatch')
        checks+=len(actual)
    return checks
