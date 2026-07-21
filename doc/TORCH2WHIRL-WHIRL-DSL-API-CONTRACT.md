# torch2whirl And WHIRL DSL API Contract

This document records the current API boundary between the standalone
`torch2whirl` Python/PyTorch frontend and the Open64 WHIRL DSL IR construction
subsystem.  It is the reference checklist to inspect before adding new bridge
APIs or broadening frontend ingestion behavior.

The goal is consistency: Python discovers source-language semantics and passes
opaque handles; common/com owns WHIRL table construction, DSL image records,
verification, mapped-image finalization, and durable IR representation.

## Layering

```text
Python model / DSL
  -> torch2whirl C++ driver
  -> open64_dsc Python package
  -> open64_dsc.WhirlBuilder
  -> native extension module open64_dsc._whirl
  -> Open64_DSC_* bridge functions
  -> DSL_Builder_* common/com C++ APIs
  -> WHIRL WN/ST/TY tables, DSL image tables, ELF mapped image
```

Python must not inspect or construct:

```text
WN*
ST_IDX
TY_IDX
RID*
SRCPOS bit patterns
DSL image row offsets
physical OPR_DSL records
mapped-image sections
backend/CG objects
```

The frontend-visible contract is opaque handles, logical operator names,
semantic versions, tensor descriptors, source/provenance metadata, and
diagnostics.

## Primary Source Files

Python facade and protocol:

```text
osprey/torch2whirl/python/open64_dsc/builder.py
osprey/torch2whirl/python/open64_dsc/backend.py
osprey/torch2whirl/python/open64_dsc/interpreter.py
```

Native Python extension and bridge:

```text
osprey/torch2whirl/python/native/_whirl_module.cxx
osprey/torch2whirl/python/native/open64_dsc_native_bridge.h
osprey/torch2whirl/python/native/open64_dsc_native_bridge.cxx
osprey/torch2whirl/python/native/open64_dsc_native_support.cxx
```

Common/com DSL construction:

```text
osprey/common/com/dsl_builder.h
osprey/common/com/dsl_builder.cxx
```

Reference inspection and validation:

```text
osprey/common/com/ir_reader.cxx
osprey/common/com/dsl_gatekeeper.cxx
osprey/common/com/dsl_ir_print.cxx
```

## API Inventory

The names below are the current categories of interaction.  The
`Open64_DSC_*` functions are the native extension bridge.  The
`DSL_Builder_*` functions are the common/com C++ implementation surface.

### Program Lifecycle

Python-facing:

```text
WhirlBuilder.begin_program()
WhirlBuilder.abort_program()
WhirlBuilder.verify_program()
save_as_whirl()
```

Native/common:

```text
Open64_DSC_Begin_Program
Open64_DSC_Abort_Program
Open64_DSC_Verify_Program
Open64_DSC_Finalize_Mapped_Image

DSL_Builder_Begin_Program
DSL_Builder_Abort_Program
DSL_Builder_Verify_Program
DSL_Builder_Finalize_Mapped_Image
```

Purpose:

Start a builder session, abort safely on failure, verify the program, and
finalize the binary WHIRL artifact through the existing mapped-image and ELF
framework.

### Tensor Types And Descriptors

Python-facing:

```text
WhirlBuilder.tensor_type(...)
WhirlBuilder.attach_tensor_descriptor(...)
```

Native/common:

```text
Open64_DSC_Create_Tensor_Type
Open64_DSC_Intern_Tensor_Type
Open64_DSC_Attach_Tensor_Descriptor

DSL_Builder_Create_Tensor_Type_Core
DSL_Builder_Intern_Tensor_Type
DSL_Builder_Attach_Tensor_Descriptor
DSL_Builder_Get_Tensor_Descriptor
DSL_Builder_Tensor_Type_Is_Canonical
```

Purpose:

Create canonical tensor `TY_IDX` records and attach semantic tensor facts such
as element type, rank, logical shape, layout, traits, placement, memory, and
sharding.  Runtime state and compiler provenance must not affect tensor type
equivalence.

### Symbols And Tensor Values

Python-facing:

```text
WhirlBuilder.symbol(...)
WhirlBuilder.tensor_constant(...)
WhirlBuilder.model_input(...)
WhirlBuilder.external_tensor_constant(...)
WhirlBuilder.value_type(...)
WhirlBuilder.value_result_symbol(...)
```

Native/common:

```text
Open64_DSC_Create_Symbol
Open64_DSC_Create_Tensor_Constant
Open64_DSC_Create_Model_Input
Open64_DSC_Create_External_Tensor_Constant
Open64_DSC_Get_Value_Type
Open64_DSC_Get_Value_Result_Symbol

DSL_Builder_Create_Symbol
DSL_Builder_Create_Tensor_Result_Symbol
DSL_Builder_Create_Tensor_Constant
DSL_Builder_Create_Model_Input
DSL_Builder_Create_External_Tensor_Constant
DSL_Builder_Get_Value_Type
DSL_Builder_Get_Value_Result_Symbol
DSL_Builder_Set_Tensor_Unique_Ownership
DSL_Builder_Tensor_Has_Unique_Ownership
```

Purpose:

Represent model inputs, constants, side-file tensor payloads, tensor result
symbols, and value-to-type/value-to-symbol relationships.

### Metadata And Source Position

Python-facing:

```text
WhirlBuilder.attach_symbol_metadata(...)
WhirlBuilder.attach_value_metadata(...)
WhirlBuilder.attach_value_lineage(...)
WhirlBuilder.register_source_file(...)
WhirlBuilder.set_value_source_position(...)
```

Native/common:

```text
Open64_DSC_Attach_Symbol_Metadata
Open64_DSC_Attach_Value_Metadata
Open64_DSC_Attach_Value_Lineage
Open64_DSC_Register_Source_File
Open64_DSC_Set_Value_Source_Position

DSL_Builder_Attach_Metadata
DSL_Builder_Attach_Value_Metadata
DSL_Builder_Attach_Value_Lineage
DSL_Builder_Register_Source_File
DSL_Builder_Set_Value_Source_Position
```

Purpose:

Attach compiler metadata, source/provenance facts, lineage, source files, and
line/column positions.  This metadata is not part of tensor type equality.

### Logical DSL Operators

Python-facing:

```text
WhirlBuilder.operator(...)
common/cnn/transformer convenience wrappers in WhirlBuilder
```

Native/common:

```text
Open64_DSC_Create_Operator
Open64_DSC_Create_Operator_With_Result

DSL_Builder_Create_Operator
DSL_Builder_Create_Operator_With_Result
DSL_Builder_Attach_Contract
```

Purpose:

Create logical DSL operators with logical name, version, direct operands,
attributes, result type, result symbol, and gatekeeper-visible semantic
contracts.  User-facing diagnostics and dumps must use logical names such as
`common.linear` or `OPR_DSLLINEAR`, not the private physical `OPR_DSL` escape.

### Program Units And Inter-PU Calls

Python-facing:

```text
WhirlBuilder.minimal_program_unit(...)
WhirlBuilder.select_program_unit(...)
WhirlBuilder.set_pu_source_identity(...)
WhirlBuilder.declare_pu_formal(...)
WhirlBuilder.declare_pu_result(...)
WhirlBuilder.return_pu_values(...)
WhirlBuilder.create_pu_call(...)
WhirlBuilder.get_pu_call_result(...)
```

Native/common:

```text
Open64_DSC_Create_Minimal_Program_Unit
Open64_DSC_Select_Program_Unit
Open64_DSC_Set_PU_Source_Identity
Open64_DSC_Declare_PU_Formal
Open64_DSC_Declare_PU_Result
Open64_DSC_Return_PU_Values
Open64_DSC_Create_PU_Call
Open64_DSC_Get_PU_Call_Result

DSL_Builder_Create_Minimal_PU
DSL_Builder_Select_PU
DSL_Builder_Set_PU_Source_Identity
DSL_Builder_Get_PU_Source_Identity
DSL_Builder_Declare_PU_Formal
DSL_Builder_Declare_PU_Result
DSL_Builder_Return_PU_Values
DSL_Builder_Create_PU_Call
DSL_Builder_Get_PU_Call_Result
DSL_Builder_Get_PU_Callsite_Info
```

Purpose:

Create class-centric WHIRL `FUNC_ENTRY` PUs, select a PU before populating its
local symbol/map tables, declare ordered tensor formals and results, return
values, and create standard WHIRL `CALL` edges with callsite metadata.

### Regions

Python-facing:

```text
WhirlBuilder.region(...)
WhirlBuilder.append_region_value(...)
WhirlBuilder.append_program_unit_region(...)
WhirlBuilder.append_child_region(...)
WhirlBuilder.declare_region_value(...)
WhirlBuilder.set_region_source_position(...)
WhirlBuilder.set_region_metadata(...)
```

Native/common:

```text
Open64_DSC_Create_Region
Open64_DSC_Append_Region_Value
Open64_DSC_Append_Program_Unit_Region
Open64_DSC_Append_Child_Region
Open64_DSC_Declare_Region_Value
Open64_DSC_Set_Region_Source_Position
Open64_DSC_Set_Region_Metadata

DSL_Builder_Create_Region
DSL_Builder_Append_Region_Value
DSL_Builder_Append_PU_Region
DSL_Builder_Append_Child_Region
DSL_Builder_Declare_Region_Value
DSL_Builder_Set_Region_Source_Position
DSL_Builder_Set_Region_Metadata
```

Purpose:

Represent structured DSL regions such as transformer prefill, decode, and
decoder-layer contracts.  The frontend passes opaque region handles and
declares region interfaces; common/com owns the WHIRL `OPR_REGION` structure
and persistent region records.

### State And Effects

Python-facing:

```text
WhirlBuilder.state_object(...)
WhirlBuilder.add_state_effect(...)
WhirlBuilder.declare_region_state(...)
```

Native/common:

```text
Open64_DSC_Declare_State_Object
Open64_DSC_Add_State_Effect
Open64_DSC_Declare_Region_State

DSL_Builder_Declare_State_Object
DSL_Builder_Declare_State_Object_With_Flags
DSL_Builder_Add_State_Effect
DSL_Builder_Declare_Region_State
DSL_Builder_Get_State_Symbol
```

Purpose:

Represent stateful model artifacts such as decode K/V cache state, ownership,
ordered region-state interfaces, and READ/MODIFY effects.

### PU Body Attachment And Test Inspection

Python-facing:

```text
WhirlBuilder.append_program_unit_value(...)
WhirlBuilder.append_program_unit_marker(...)
WhirlBuilder.inspect_program_unit_values(...)
WhirlBuilder.inspect_program_unit_markers(...)
```

Native/common:

```text
Open64_DSC_Append_Program_Unit_Value
Open64_DSC_Count_Program_Unit_Values
Open64_DSC_Get_Program_Unit_Value
Open64_DSC_Append_Program_Unit_Marker
Open64_DSC_Count_Program_Unit_Markers
Open64_DSC_Get_Program_Unit_Marker

DSL_Builder_Append_PU_Value
DSL_Builder_Count_PU_Values
DSL_Builder_Get_PU_Value
DSL_Builder_Count_Value_Operands
DSL_Builder_Get_Value_Info
DSL_Builder_Get_Value_Operand
DSL_Builder_Append_PU_Marker
DSL_Builder_Count_PU_Markers
DSL_Builder_Get_PU_Marker
```

Purpose:

Attach logical DSL values/operators to a PU body and inspect high-level value
records in tests.  These inspection calls are not permission for Python to
decode WHIRL node layout.

## Current Import-Support Gap

The current IR-facing API already supports:

```text
PU source identity
callsite identity
source file and line
value metadata
symbol metadata
```

Import support can initially be represented in the Python manifest.  If import
identity must survive `.B` write/read and appear in `ir_b2a -st -src`, the
main/common side needs a durable IR representation or an extension to existing
PU/callsite metadata.  Candidate fields include:

```text
imported spelling
alias set
defining module
importing module
re-export source
import declaration source position
declaration kind
```

Do not encode these facts into tensor descriptors or operator attributes unless
the common/com contract explicitly promotes them there.

## Protocol For Creating New APIs

The torch2whirl subagent and the main/native agent should use the following
protocol whenever a new API is needed.

### 1. Frontend Discovery

The torch2whirl side owns:

```text
source model fixture
graph capture
operator/import/class census
mock or manifest-level tests
diagnostic examples
exact missing capability report
```

The frontend should prove that the source-language requirement is real before
asking common/com for a durable IR addition.

### 2. Contract Request To Main Agent

The request must include:

```text
source model pattern
required semantic fields
whether fields are normative IR or compiler metadata
expected lifetime in .B artifacts
ir_b2a -st -src evidence requirement
gatekeeper validation requirement
lowering ownership, if any
failure diagnostics
minimal API signatures desired by Python
```

The request should avoid implementation details such as WN layout, `ST_IDX`
indexing, physical `OPR_DSL` records, or mapped-image offsets unless the main
agent asks for them.

### 3. Main/Common Implementation

The main/native agent owns:

```text
DSL_Builder_* API declaration and implementation
fixed-row or existing mapped-image table updates
binary reader/writer compatibility
gatekeeper checks
logical ir_b2a printing
VHO/lowering behavior when required
native producer/consumer tests
```

The main agent should publish a clean commit/branch before the frontend
consumes the API.  The torch2whirl side must not copy dirty-tree snapshots.

### 4. Frontend Binding

After native publication, the torch2whirl side owns:

```text
Open64_DSC_* bridge binding
_whirl Python extension method
WhirlBackend protocol update
WhirlBuilder facade update
mock backend behavior
frontend emission logic
Python unit tests
native artifact tests
Docker validation lane
```

The binding must preserve opaque handles.  Python must not inspect C++ WHIRL
objects to compensate for a missing native API.

### 5. Certification

A new API is accepted by the frontend only when the relevant lane proves:

```text
standalone torch2whirl command succeeds
.B artifact is retained
side payloads are retained when applicable
ir_b2a -st -src reopens the .B in a separate process
logical evidence appears in the trace
opencc/openpy path succeeds when in scope
negative cases fail before a usable .B is accepted
git diff --check passes
no-tab scan passes
backend-isolation scan passes
```

### 6. PR Dependency Discipline

Use stacked PRs when native and frontend work both change:

```text
native/common PR
  -> torch2whirl frontend PR
```

The frontend PR should explicitly state:

```text
Depends on <native PR>
Reviewed while stacked on <native branch>
Merge only after native infrastructure PR
```

If the API is frontend-only metadata with no durable IR consequence, the
frontend PR may proceed independently.

## Handoff Template For New API Requests

Use this template when asking the main agent for new WHIRL DSL API support:

```text
API request title:

Frontend requirement:

Source fixture and capture evidence:

Current API limitation:

Required native representation:

Proposed opaque builder API:

Required ir_b2a evidence:

Required gatekeeper checks:

Expected lowering ownership:

Negative cases:

Frontend binding plan after native publication:

PR dependency:
```

## Proactive Maintenance Checklist

Review this document when:

```text
adding a new model family
adding a new DSL operator
adding a new tensor descriptor field
adding import/declaration metadata
adding region/state/effect support
changing multiple-PU callable boundaries
changing binary artifact certification
changing ir_b2a or review trace evidence
```

When an API is added, update:

```text
this document
doc/WHIRL-DSL-INFRASTRUCTURE.md when native representation changes
the relevant model-family ingestion plan
torch2whirl tests and retained artifact expectations
```
