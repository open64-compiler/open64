#include <workload.h>
#include <cuda_profiler_api.h>
#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstring>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>
#include <utility>

extern "C" size_t baseline_workspace_bytes(const WorkloadArgs*);
extern "C" size_t candidate_workspace_bytes(const WorkloadArgs*);
extern "C" cudaError_t baseline_run(const WorkloadArgs*,void*,cudaStream_t);
extern "C" cudaError_t candidate_run(const WorkloadArgs*,void*,cudaStream_t);

static void check(cudaError_t e) {
    if(e!=cudaSuccess) throw std::runtime_error(cudaGetErrorString(e));
}
static std::string quote(const std::string& s) {
    std::string out="\"";
    for(char c:s) {
        if(c=='"' || c=='\\') { out+='\\'; out+=c; }
        else if(c=='\n') out+="\\n";
        else if(static_cast<unsigned char>(c)<32) out+=' ';
        else out+=c;
    }
    return out+'"';
}
template<class T> static std::vector<T> load(const std::string& path) {
    std::ifstream f(path,std::ios::binary|std::ios::ate);
    if(!f) throw std::runtime_error("cannot read "+path);
    auto n=f.tellg();
    if(n<0 || size_t(n)%sizeof(T)) throw std::runtime_error("invalid size: "+path);
    std::vector<T> v(size_t(n)/sizeof(T));
    f.seekg(0); f.read(reinterpret_cast<char*>(v.data()),n);
    if(!f) throw std::runtime_error("short read: "+path);
    return v;
}
struct Buffer {
    void* ptr=nullptr;
    size_t bytes=0;
    std::vector<unsigned char> original;
    explicit Buffer(size_t n):bytes(n) { if(n) check(cudaMalloc(&ptr,n)); }
    ~Buffer() { if(ptr) cudaFree(ptr); }
    Buffer(const Buffer&)=delete;
    template<class T> void upload(const std::vector<T>& x) {
        if(x.size()*sizeof(T)!=bytes) throw std::runtime_error("buffer size mismatch");
        original.resize(bytes);
        std::memcpy(original.data(),x.data(),bytes);
        check(cudaMemcpy(ptr,x.data(),bytes,cudaMemcpyHostToDevice));
    }
    bool unchanged() const {
        std::vector<unsigned char> current(bytes);
        check(cudaMemcpy(current.data(),ptr,bytes,cudaMemcpyDeviceToHost));
        return current==original;
    }
};
struct Stream {
    cudaStream_t s;
    Stream() { check(cudaStreamCreateWithFlags(&s,cudaStreamNonBlocking)); }
    ~Stream() { cudaStreamDestroy(s); }
};
struct Event {
    cudaEvent_t e;
    Event() { check(cudaEventCreate(&e)); }
    ~Event() { cudaEventDestroy(e); }
};
struct Case {
    WorkloadArgs args{};
    std::vector<std::unique_ptr<Buffer>> inputs;
    std::unique_ptr<Buffer> output;
    std::vector<double> golden;
    template<class T> T* input(const std::string& path) {
        auto values=load<T>(path);
        auto buf=std::make_unique<Buffer>(values.size()*sizeof(T));
        buf->upload(values);
        T* ptr=static_cast<T*>(buf->ptr);
        inputs.push_back(std::move(buf));
        return ptr;
    }
    explicit Case(const std::string& dir) {
        std::ifstream p(dir+"/params.txt");
        auto& a=args;
#ifdef GEMM
        p>>a.m>>a.n>>a.k>>a.lda>>a.ldb>>a.ldc>>a.ldd
         >>a.trans_a>>a.trans_b>>a.alpha>>a.beta;
        a.a=input<__nv_bfloat16>(dir+"/a.bin");
        a.b=input<__nv_bfloat16>(dir+"/b.bin");
        a.c=input<__nv_bfloat16>(dir+"/c.bin");
        output=std::make_unique<Buffer>(size_t(a.m)*a.ldd*sizeof(__nv_bfloat16));
        a.d=static_cast<__nv_bfloat16*>(output->ptr);
#else
        p>>a.batch>>a.heads>>a.latent_dim>>a.rope_dim>>a.page_size>>a.max_pages>>a.scale;
        a.q=input<__nv_bfloat16>(dir+"/q.bin");
        a.kv=input<__nv_bfloat16>(dir+"/kv.bin");
        a.page_table=input<int32_t>(dir+"/page_table.bin");
        a.context_lengths=input<int32_t>(dir+"/lengths.bin");
        output=std::make_unique<Buffer>(size_t(a.batch)*a.heads*a.latent_dim*sizeof(__nv_bfloat16));
        a.out=static_cast<__nv_bfloat16*>(output->ptr);
#endif
        if(!p) throw std::runtime_error("invalid params.txt");
        golden=load<double>(dir+"/golden.bin");
    }
    void reset(cudaStream_t s) {
        // BF16 0xffff is NaN: detects unwritten outputs and changed padding.
        check(cudaMemsetAsync(output->ptr,255,output->bytes,s));
    }
    struct Validation { bool pass=true; double max_abs=0; };
    Validation validate(double atol,double rtol) {
        std::vector<uint16_t> bits(output->bytes/2);
        check(cudaMemcpy(bits.data(),output->ptr,output->bytes,cudaMemcpyDeviceToHost));
        Validation v;
        for(size_t i=0;i<golden.size();++i) {
#ifdef GEMM
            size_t pos=(i/args.n)*args.ldd+i%args.n;
#else
            size_t pos=i;
#endif
            uint32_t raw=uint32_t(bits.at(pos))<<16;
            float value; std::memcpy(&value,&raw,sizeof(value));
            if(!std::isfinite(value)) { v.pass=false; continue; }
            double err=std::abs(double(value)-golden[i]);
            v.max_abs=std::max(v.max_abs,err);
            if(err>atol+rtol*std::abs(golden[i])) v.pass=false;
        }
#ifdef GEMM
        for(int64_t i=0;i<args.m;++i)
            for(int64_t j=args.n;j<args.ldd;++j)
                if(bits[i*args.ldd+j]!=0xffff) v.pass=false;
#endif
        for(auto& x:inputs) if(!x->unchanged()) v.pass=false;
        return v;
    }
};
using Run=cudaError_t(*)(const WorkloadArgs*,void*,cudaStream_t);
static double median(std::vector<double> x) {
    std::sort(x.begin(),x.end()); size_t n=x.size();
    return n%2 ? x[n/2] : (x[n/2-1]+x[n/2])/2;
}
struct Stats {
    std::vector<double> wall,device;
    std::string json(size_t workspace) const {
        double m=median(wall); std::vector<double> deviations;
        for(double x:wall) deviations.push_back(std::abs(x-m));
        std::ostringstream o; o<<std::setprecision(17)
            <<"{\"wall_us\":"<<m<<",\"wall_mad_us\":"<<median(deviations)
            <<",\"device_us\":"<<median(device)<<",\"samples\":"<<wall.size()
            <<",\"workspace_bytes\":"<<workspace;
        // Preserve both independent batches so the shared offline validator can
        // recompute summaries and ratios without trusting aggregate numbers.
        for(const auto& entry : {std::make_pair("wall_samples_us",&wall),
                                  std::make_pair("device_samples_us",&device)}) {
            o<<",\""<<entry.first<<"\":[";
            for(size_t i=0;i<entry.second->size();++i) {
                if(i) o<<',';
                o<<(*entry.second)[i];
            }
            o<<']';
        }
        o<<'}';
        return o.str();
    }
};
static void evaluate(const std::string& dir,int samples,int warmup,double atol,double rtol) {
    Case c(dir); Stream stream;
    Run runs[]={baseline_run,candidate_run};
    size_t sizes[]={baseline_workspace_bytes(&c.args),candidate_workspace_bytes(&c.args)};
    Buffer ws0(sizes[0]),ws1(sizes[1]);
    void* work[]={ws0.ptr,ws1.ptr};
    Case::Validation valid[2];
    for(int j=0;j<2;++j) {
        c.reset(stream.s);
        if(sizes[j]) check(cudaMemsetAsync(work[j],0xA5,sizes[j],stream.s));
        check(runs[j](&c.args,work[j],stream.s)); check(cudaStreamSynchronize(stream.s));
        valid[j]=c.validate(atol,rtol);
        // Stop before a mutated input can contaminate the other implementation.
        if(!valid[j].pass) {
            std::cout<<"{\"status\":\"correctness_failed\",\"reason\":\""
                     <<(j==0?"baseline":"candidate")<<" validation failed\"}"<<std::endl;
            return;
        }
    }
    for(int n=0;n<warmup;++n) for(int j=0;j<2;++j) {
        c.reset(stream.s); check(runs[j](&c.args,work[j],stream.s));
        check(cudaStreamSynchronize(stream.s));
    }
    Stats stats[2];
    for(int n=0;n<samples;++n) for(int order=0;order<2;++order) {
        int j=(n+order)%2; // Alternate baseline/candidate ordering.
        c.reset(stream.s); check(cudaStreamSynchronize(stream.s));
        auto begin=std::chrono::steady_clock::now();
        cudaError_t launch=runs[j](&c.args,work[j],stream.s);
        cudaError_t completed=cudaStreamSynchronize(stream.s);
        auto end=std::chrono::steady_clock::now();
        check(launch); check(completed);
        stats[j].wall.push_back(std::chrono::duration<double,std::micro>(end-begin).count());
        if(n==samples-1 && !c.validate(atol,rtol).pass)
            throw std::runtime_error("validation failed after wall-clock execution");
    }
    // Separate device-event samples: event recording does not perturb wall samples.
    Event start,stop;
    for(int n=0;n<samples;++n) for(int order=0;order<2;++order) {
        int j=(n+order)%2;
        c.reset(stream.s); check(cudaStreamSynchronize(stream.s));
        check(cudaEventRecord(start.e,stream.s));
        check(runs[j](&c.args,work[j],stream.s));
        check(cudaEventRecord(stop.e,stream.s)); check(cudaEventSynchronize(stop.e));
        float ms; check(cudaEventElapsedTime(&ms,start.e,stop.e));
        stats[j].device.push_back(double(ms)*1000);
        // Check the final measured output of each implementation outside timing.
        if(n==samples-1 && !c.validate(atol,rtol).pass)
            throw std::runtime_error("validation failed after measured execution");
    }
    std::cout<<"{\"status\":\"passed\",\"correctness\":{\"baseline_pass\":true,"
             <<"\"candidate_pass\":true,\"baseline_max_abs_error\":"<<valid[0].max_abs
             <<",\"candidate_max_abs_error\":"<<valid[1].max_abs<<"},\"baseline\":"
             <<stats[0].json(sizes[0])<<",\"candidate\":"<<stats[1].json(sizes[1])<<"}"<<std::endl;
}
// Invoked only by the optional profiler subprocess. No benchmark timings emitted.
static int profile_main(int argc,char** argv) {
    if(argc!=8) throw std::runtime_error(
        "usage: runner --profile baseline|candidate warmup atol rtol case_dir expected_gpu_uuid");
    std::string role=argv[2];
    if(role!="baseline" && role!="candidate") throw std::runtime_error("invalid profile role");
    int device; check(cudaGetDevice(&device)); cudaDeviceProp prop;
    check(cudaGetDeviceProperties(&prop,device));
    std::ostringstream uuid;
    for(unsigned char c:prop.uuid.bytes) uuid<<std::hex<<std::setw(2)<<std::setfill('0')<<unsigned(c);
    if(uuid.str()!=argv[7]) throw std::runtime_error("profile GPU differs from benchmark GPU");
    Case c(argv[6]); Stream stream;
    Run fn=role=="baseline" ? baseline_run : candidate_run;
    size_t bytes=role=="baseline" ? baseline_workspace_bytes(&c.args) : candidate_workspace_bytes(&c.args);
    Buffer workspace(bytes);
    int warmup=std::stoi(argv[3]);
    if(warmup<0) throw std::runtime_error("negative profile warmup");
    double atol=std::stod(argv[4]),rtol=std::stod(argv[5]);
    // Validate and warm up before enabling collection.
    for(int n=0;n<=warmup;++n) {
        c.reset(stream.s);
        check(fn(&c.args,workspace.ptr,stream.s));
        check(cudaStreamSynchronize(stream.s));
    }
    if(!c.validate(atol,rtol).pass) throw std::runtime_error("profile pre-validation failed");
    c.reset(stream.s); check(cudaStreamSynchronize(stream.s));
    check(cudaProfilerStart());
    check(fn(&c.args,workspace.ptr,stream.s));
    check(cudaStreamSynchronize(stream.s));
    check(cudaProfilerStop());
    if(!c.validate(atol,rtol).pass) throw std::runtime_error("profile output validation failed");
    std::cout<<"{\"profile_validation\":\"passed\"}"<<std::endl;
    return 0;
}
int main(int argc,char** argv) {
    if(argc>1 && std::string(argv[1])=="--profile") {
        try { return profile_main(argc,argv); }
        catch(const std::exception& e) { std::cerr<<e.what()<<'\n'; return 2; }
    }
    if(argc<6) { std::cerr<<"usage: runner samples warmup atol rtol case_dir...\n"; return 2; }
    std::cout<<std::setprecision(17);
    try {
        int device; check(cudaGetDevice(&device)); cudaDeviceProp p;
        check(cudaGetDeviceProperties(&p,device));
        int driver,runtime; check(cudaDriverGetVersion(&driver)); check(cudaRuntimeGetVersion(&runtime));
        std::ostringstream uuid;
        for(unsigned char c:p.uuid.bytes) uuid<<std::hex<<std::setw(2)<<std::setfill('0')<<unsigned(c);
        std::cout<<"{\"environment\":{\"gpu\":"<<quote(p.name)
                 <<",\"session_id\":"<<quote(uuid.str()+"-"+std::to_string(std::chrono::steady_clock::now().time_since_epoch().count()))
                 <<",\"gpu_uuid\":"<<quote(uuid.str())<<",\"driver\":"<<driver
                 <<",\"cuda_runtime\":"<<runtime<<",\"sm\":"<<p.major*10+p.minor<<"}}"<<std::endl;
        int samples=std::stoi(argv[1]),warmup=std::stoi(argv[2]);
        if(samples<1 || warmup<0) throw std::runtime_error("invalid sampling policy");
        for(int i=5;i<argc;++i) {
            try { evaluate(argv[i],samples,warmup,std::stod(argv[3]),std::stod(argv[4])); }
            catch(const std::exception& e) {
                std::cout<<"{\"status\":\"runtime_failed\",\"reason\":"<<quote(e.what())<<"}"<<std::endl;
                return 2; // Do not reuse a possibly unhealthy CUDA context.
            }
        }
    } catch(const std::exception& e) { std::cerr<<e.what()<<'\n'; return 2; }
}
