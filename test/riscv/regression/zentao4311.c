typedef struct {
  void (*bzfree)()
} bz_stream;
bz_stream BZ2_bzDecompressEnd_strm;
BZ2_bzDecompressEnd() { BZ2_bzDecompressEnd_strm.bzfree(); }
