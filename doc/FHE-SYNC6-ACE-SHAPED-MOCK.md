# SYNC-6 ACE-Shaped Development Mock

Status: focused arithmetic-adapter slice implemented; no ACE dependency in
the test build. This is not the final provider or client/server certification.

## Purpose And Placement

The certified SYNC-5 mock directly implements the stable public
`open64_fhe_*_v1` ABI. Keep it as the generated-C and 147-call reference.
The second mock exercises a different boundary:

```text
Open64-owned CKKS evaluator adapter (Open64 status/ownership rules)
  -> ACE result-first call shape: Add_ciph, Sub_ciph, Mul_ciph,
     Rescale_ciph, Rotate_ciph, Bootstrap
  -> local ACE-shaped mock implementation
```

`osprey/libopen64fhe/open64_fhe_ace_api.h` declares the pinned ACE arithmetic
call shapes in development mode and includes the actual ACE aggregate header
only for a separately configured real-provider build. The adapter's source
uses the same calls in either mode. The mock objects are test tokens with
level, scale-degree, slots, and a diagnostic arithmetic tag. They are not
ciphertexts, do not encrypt or decrypt, and cannot replace actual ACE
capability admission. The core unit build needs neither ACE headers nor
`FHErt_ant`.

The adapter preflights borrowed inputs, a null result slot, operation shape,
scale compatibility for add/subtract, and nonzero signed rotation/target
level. It allocates a distinct result, calls the ACE-shaped function, checks
that input state was preserved and the bootstrap target was reached, and
publishes the result only on success. The mock can inject one failed call to
test rollback and result cleanup. An ACE assertion or process termination
still requires the later supervised worker; the in-process mock does not
prove failure isolation.

## Repeatable Test

```sh
bash osprey/libopen64fhe/tests/fhe_ace_shaped_mock_test.sh \
  /private/tmp/open64-fhe-ace-shaped-mock
bash osprey/libopen64fhe/tests/fhe_mock_profile_test.sh
bash osprey/libopen64fhe/tests/fhe_mock_lifecycle_test.sh
```

The focused test checks source-level call signatures, operation dispatch,
borrowed input preservation, fresh result ownership, negative rotation,
target-level bootstrap, injected failure without a leaked or published
result, cleanup, and absence of key-generation/decryption/dataset-helper
symbols in the linked test. It retains `build.log`, `run.log`, `symbols.txt`,
and the executable in the chosen host directory. The existing public-ABI
mock tests independently retain their 147-call and lifecycle coverage.

## Remaining Integration

This first slice is a private evaluator adapter, not yet a complete
`libopen64_fhe_ace_ant` implementation of the public ABI. It deliberately
does not claim model-package admission, public-context/key/ciphertext
transport, plain-weight convolution, full ReLU stage expansion, client/server
isolation, or a 147-event generated-C run through this second mock. Those
features must be added against the CKKS-conforming IR and frozen descriptors,
then tested at the Open64 ABI boundary. ACE's missing evaluation-only
import/export remains recorded in
`doc/FHE-SYNC6-ACE-ANT-ADMISSION-AUDIT.md`; development with this mock does not
depend on its resolution.

The next adapter slice should add ACE-shaped plaintext add/multiply,
relinearization, modulus switch, and exact context/key capability hooks as
the reviewed CKKS IR operations become available. Avoid implementing
provider-specific scheduling inside the mock or quietly substituting mock
arithmetic for the required CKKS semantic IR.
