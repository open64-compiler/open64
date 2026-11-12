"""Verify native MLA .B -> whirl2c reference .cu without lowering.

Contract: doc/AI-COMPILER-TENSOR-SOURCE-v0.1.md. Host execution checks
the emitted computation. NVCC compilation is recorded separately from GPU
execution, which this CPU-only harness never claims.
"""
import argparse
import hashlib
import json
from pathlib import Path
import re
import shutil
import struct
import subprocess


def digest(path):
    """Bind checks to actual artifact bytes."""
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def review(args):
    """Replay retained inputs through WHIRL-derived source and the FP64 oracle."""
    import torch
    from models.absorbed_mla import INPUT_NAMES, make_fixture
    from models.mla_reference import expanded_reference

    root = args.artifact_dir.resolve()
    root.mkdir(parents=True, exist_ok=False)
    (root / "logs").mkdir()
    shutil.copyfile(args.header, root / "open64_tensor_reference.cuh")
    commands = []

    def run(label, argv, success=True):
        result = subprocess.run(list(map(str, argv)), capture_output=True, text=True)
        (root / "logs" / (label + ".stdout")).write_text(result.stdout)
        (root / "logs" / (label + ".stderr")).write_text(result.stderr)
        commands.append(dict(label=label, argv=list(map(str, argv)), returncode=result.returncode))
        (root / "commands.json").write_text(json.dumps(commands, indent=2) + "\n")
        if (result.returncode == 0) != success:
            raise RuntimeError(label + ": " + result.stderr[-5000:])
        return result

    cases = {}
    for context in ("short", "long"):
        dest = root / context
        dest.mkdir()
        source_root = args.native_review / context
        for name in ("mla.B", "inputs.pt", "reference.pt"):
            shutil.copyfile(source_root / name, dest / name)
        image = dest / "mla.B"
        before = digest(image)
        run(context + "-before", [args.ir_b2a, "-st", "-src", image, dest / "before.T"])
        command = [args.whirl2c, "-cuda", "-tensor-source",
                   "-CLIST:dotc_file=" + str(dest / "mla.cu"), image]
        run(context + "-emit", command)
        run(context + "-after", [args.ir_b2a, "-st", "-src", image, dest / "after.T"])
        assert digest(image) == before
        assert (dest / "before.T").read_bytes() == (dest / "after.T").read_bytes()
        text = (dest / "mla.cu").read_text()
        assert text.count("// native node ") == 46
        assert "__global__" not in text and "OPR_DSL" not in text
        function = re.search(r'extern "C" O64_HD void (\w+)\(', text).group(1)
        formals = re.findall(r"^  (const )?(TENSOR<[^>]+>) &(v\d+)[,)]", text, re.M)
        assert len(formals) == 9 and all(item[0] for item in formals[:7])
        assert not any(item[0] for item in formals[7:])
        runner = ['#include "mla.cu"', '#include <cstdio>', '#include <memory>',
                  'int main(int argc, char **argv) {',
                  '  if(argc!=3) return 2;',
                  '  FILE *in=fopen(argv[1],"rb"); if(!in) return 3;']
        for ordinal, (_, ty, _) in enumerate(formals):
            runner.append('  auto p%d=std::make_unique<%s>();' % (ordinal, ty))
            if ordinal < 7:
                runner.append('  if(fread(p%d->data,sizeof(p%d->data),1,in)!=1) return 4;'
                              % (ordinal, ordinal))
        runner.extend(['  fclose(in);', '  %s(%s);' % (
            function, ','.join('*p%d' % i for i in range(9))),
            '  FILE *out=fopen(argv[2],"wb"); if(!out) return 5;',
            '  if(fwrite(p7->data,sizeof(p7->data),1,out)!=1) return 6;',
            '  if(fwrite(p8->data,sizeof(p8->data),1,out)!=1) return 6;',
            '  return fclose(out)!=0;', '}'])
        (dest / "runner.cxx").write_text("\n".join(runner) + "\n")
        run(context + "-host-compile", [args.cxx, "-std=c++17", "-O1", "-fno-fast-math",
            "-ffp-contract=off", "-I" + str(root), dest / "runner.cxx", "-o", dest / "runner"])
        model, _ = make_fixture(context)
        saved = torch.load(dest / "inputs.pt", map_location="cpu", weights_only=False)
        original = tuple(saved[name] for name in INPUT_NAMES)
        checks = {}
        for variant in ("original", "one-masked-head", "all-masked"):
            inputs = tuple(value.clone() for value in original)
            if variant == "one-masked-head":
                inputs[-1][:, 0, :] = False
            elif variant == "all-masked":
                inputs[-1].fill_(False)
            input_file = dest / (variant + ".inputs.bin")
            input_file.write_bytes(b"".join(x.numpy().tobytes() for x in inputs))
            output_file = dest / (variant + ".outputs.bin")
            run(context + "-host-" + variant, [dest / "runner", input_file, output_file])
            expected = expanded_reference(*(x.tolist() for x in inputs), model.alpha)
            flat = struct.unpack("<21f", output_file.read_bytes())
            actual = (torch.tensor(flat[:18], dtype=torch.float64).reshape(1, 3, 6),
                      torch.tensor(flat[18:], dtype=torch.float64).reshape(1, 3))
            errors = []
            for got, ref in zip(actual, expected):
                ref = torch.tensor(ref, dtype=torch.float64)
                torch.testing.assert_close(got, ref, rtol=2e-5, atol=2e-6)
                finite = torch.isfinite(ref)
                errors.append(float((got[finite] - ref[finite]).abs().max()) if finite.any() else 0.)
            if variant == "original":
                retained = torch.load(dest / "reference.pt", map_location="cpu", weights_only=False)
                for ref, key in zip(expected, ("output", "lse")):
                    torch.testing.assert_close(torch.tensor(ref, dtype=torch.float64),
                        torch.as_tensor(retained[key], dtype=torch.float64), rtol=0, atol=0)
            checks[variant] = dict(status="PASS", max_abs_output=errors[0], max_abs_lse=errors[1])
        cuda = "NOT_RUN"
        if args.nvcc:
            run(context + "-cuda-compile", [args.nvcc, "-std=c++17", "-arch=sm_90", "-dc",
                "--fmad=false", "--ftz=false", "--prec-div=true", "--prec-sqrt=true",
                "-I" + str(root), dest / "mla.cu", "-o", dest / "mla.cuda.o"])
            cuda = "PASS"
        original_source = digest(dest / "mla.cu")
        run(context + "-no-overwrite", command, success=False)
        assert digest(dest / "mla.cu") == original_source
        run(context + "-no-input-alias", [args.whirl2c, "-cuda", "-tensor-source",
            "-CLIST:dotc_file=" + str(image), image], success=False)
        assert digest(image) == before
        rejected = dest / "rejected.cu"
        run(context + "-missing-cuda", [args.whirl2c, "-tensor-source",
            "-CLIST:dotc_file=" + str(rejected), image], success=False)
        assert not rejected.exists()
        run(context + "-unknown-option", [args.whirl2c, "-cuda", "-tensor-source",
            "-CLIST:unimplemented=ON", "-CLIST:dotc_file=" + str(rejected), image], success=False)
        assert not rejected.exists()
        cases[context] = dict(native_image_sha256=before, input_image_unchanged="PASS",
            trace_unchanged="PASS", host_reference=checks, cuda_device_compile=cuda,
            gpu_execution="NOT_RUN", optimized_lowering="NOT_RUN")
    report = dict(status="PASS", cases=cases, tool_sha256={
        "whirl2c": digest(args.whirl2c), "companion": digest(args.whirl2c.parent / "whirl2c_tensor_source"),
        "ir_b2a": digest(args.ir_b2a)}, header_sha256=digest(args.header))
    (root / "report.json").write_text(json.dumps(report, indent=2) + "\n")
    (root / "SHA256SUMS").write_text("".join(
        digest(p) + "  " + str(p.relative_to(root)) + "\n"
        for p in sorted(root.rglob("*")) if p.is_file()))
    print(json.dumps(report, indent=2))


def main():
    """Require explicit fresh output and tool paths for reproducible review."""
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ("artifact-dir", "native-review", "whirl2c", "ir-b2a", "header"):
        parser.add_argument("--" + name, required=True, type=Path)
    parser.add_argument("--cxx", default="g++")
    parser.add_argument("--nvcc", type=Path)
    review(parser.parse_args())


if __name__ == "__main__":
    main()
