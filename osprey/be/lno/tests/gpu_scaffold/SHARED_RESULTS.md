# Shared results integration

This source snapshot uses the canonical shared result modules in its parent
directory. The controlling contract is
[GPU-TEST-SHARED-RESULTS-v1.0.md](../../../../../doc/GPU-TEST-SHARED-RESULTS-v1.0.md).
Distribution preparation places the full specification and byte-identical
shared modules in the packet root. Run `validate_gpu_results.py` for either a
scaffold result or an MLA Q7 return. Original numerical contracts stay separate.
