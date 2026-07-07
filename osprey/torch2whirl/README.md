# torch2whirl

`torch2whirl` is the standalone Open64 Python/PyTorch frontend driver.
Organizationally, it sits beside `clang2whirl`: a source frontend that emits
WHIRL for the rest of the Open64 pipeline.

This directory owns the current PyTorch ingestion program name and CLI.  The
native DSL builder remains the stable compiler boundary underneath it, so a
different Python ingestion implementation can be adopted later without forcing
changes into the common WHIRL/DSL infrastructure.

Keep this bridge isolated from backend code generation.  `torch2whirl` should
depend on common WHIRL/DSL construction interfaces and emit an artifact for
`opencc -x whirl`; it should not link against `osprey/be/cg` internals.

The staged boundary is:

1. capture a Python/PyTorch program,
2. construct DSL tensor WHIRL through the native builder,
3. write a binary WHIRL artifact,
4. hand the artifact to `opencc -x whirl`.

Combined driver mode is intentionally deferred:

```text
opencc -frontend=torch2whirl model.py ...
```

That mode should become only a convenience wrapper around the same binary WHIRL
artifact.  It must not bypass mapped-image finalization, `ir_b2a -st`
visibility, TensorDescriptorIR persistence, or gatekeeper validation.

This directory currently contains the buildable executable scaffold.  The next
implementation slice should replace the placeholder conversion path with calls
into the native DSL builder and mapped image finalization API.

The Python package skeleton lives under `python/open64_dsc`.  It provides the
initial public API and a mock `_whirl`-shaped backend so graph capture and CLI
integration can grow without requiring the native extension to be built first.

## Standalone build

From an out-of-tree build directory on macOS or Linux:

```sh
/path/to/open64/configure --enable-torch2whirl-only
make -C osprey/targdir/torch2whirl
./osprey/targdir/torch2whirl/torch2whirl --help
```

For quick local iteration from this source directory:

```sh
make -f Makefile.gbase
make -f Makefile.gbase python_test
```

The generated build remains under `osprey/targdir/torch2whirl`, matching the
Open64 build tree layout, but does not require the full Open64 compiler or
backend build to be configured on the host.
