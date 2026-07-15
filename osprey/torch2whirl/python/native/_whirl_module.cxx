/*
 * Copyright (C) 2026 Open64 Project
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include <vector>

#include "open64_dsc_native_bridge.h"

static PyObject *
Open64_DSC_Handle_Result(Open64_DSC_Handle handle, const char *action)
{
    if (handle != 0)
        return PyLong_FromUnsignedLongLong(handle);

    PyErr_Format(PyExc_RuntimeError, "failed to %s", action);
    return NULL;
}

static PyObject *
Open64_DSC_Bool_Result(int ok, const char *action)
{
    if (ok)
        Py_RETURN_TRUE;

    PyErr_Format(PyExc_RuntimeError, "failed to %s", action);
    return NULL;
}

static const char *
Open64_DSC_Dict_String(PyObject *dict, const char *key,
                       const char *fallback)
{
    PyObject *item = PyDict_GetItemString(dict, key);

    if (item == NULL)
        return fallback;
    if (!PyUnicode_Check(item)) {
        PyErr_Format(PyExc_TypeError, "descriptor field %s must be str", key);
        return NULL;
    }
    return PyUnicode_AsUTF8(item);
}

static int
Open64_DSC_Read_Tensor_Descriptor
        (PyObject *dict, Open64_DSC_Tensor_Descriptor *descriptor)
{
    PyObject *rank_obj;
    long rank;

    if (descriptor == NULL)
        return 0;
    if (!PyDict_Check(dict)) {
        PyErr_SetString(PyExc_TypeError, "descriptor must be a dict");
        return 0;
    }

    rank_obj = PyDict_GetItemString(dict, "rank");
    if (rank_obj == NULL) {
        PyErr_SetString(PyExc_KeyError, "descriptor rank is required");
        return 0;
    }
    rank = PyLong_AsLong(rank_obj);
    if (PyErr_Occurred())
        return 0;

    descriptor->kind = Open64_DSC_Dict_String(dict, "kind", "tensor");
    descriptor->dtype = Open64_DSC_Dict_String(dict, "dtype", NULL);
    descriptor->logical_shape =
        Open64_DSC_Dict_String(dict, "logical_shape", "");
    descriptor->traits = Open64_DSC_Dict_String(dict, "traits", NULL);
    descriptor->layout = Open64_DSC_Dict_String(dict, "layout", NULL);
    descriptor->sharding = Open64_DSC_Dict_String(dict, "sharding", NULL);
    descriptor->placement = Open64_DSC_Dict_String(dict, "placement", NULL);
    descriptor->memory = Open64_DSC_Dict_String(dict, "memory", NULL);
    descriptor->quantization =
        Open64_DSC_Dict_String(dict, "quantization", NULL);
    descriptor->runtime_state =
        Open64_DSC_Dict_String(dict, "runtime_state", NULL);
    descriptor->lineage = Open64_DSC_Dict_String(dict, "lineage", NULL);
    descriptor->rank = (int) rank;

    return !PyErr_Occurred();
}

static int
Open64_DSC_Read_External_Tensor_Reference
        (PyObject *dict, Open64_DSC_External_Tensor_Reference *reference)
{
    PyObject *byte_offset_obj;
    PyObject *byte_length_obj;

    if (reference == NULL)
        return 0;
    if (!PyDict_Check(dict)) {
        PyErr_SetString(PyExc_TypeError, "reference must be a dict");
        return 0;
    }

    reference->storage_format =
        Open64_DSC_Dict_String(dict, "storage_format", NULL);
    reference->side_file = Open64_DSC_Dict_String(dict, "side_file", NULL);
    reference->tensor_key = Open64_DSC_Dict_String(dict, "tensor_key", NULL);
    reference->checksum = Open64_DSC_Dict_String(dict, "checksum", "");
    if (PyErr_Occurred())
        return 0;

    byte_offset_obj = PyDict_GetItemString(dict, "byte_offset");
    byte_length_obj = PyDict_GetItemString(dict, "byte_length");
    if (byte_offset_obj == NULL || byte_length_obj == NULL) {
        PyErr_SetString(PyExc_KeyError,
                        "reference byte_offset and byte_length are required");
        return 0;
    }

    reference->byte_offset = PyLong_AsUnsignedLongLong(byte_offset_obj);
    if (PyErr_Occurred())
        return 0;
    reference->byte_length = PyLong_AsUnsignedLongLong(byte_length_obj);
    if (PyErr_Occurred())
        return 0;

    return 1;
}

static PyObject *
Open64_DSC_Backend_Name(PyObject *self, PyObject *args)
{
    (void) self;
    (void) args;

    return PyUnicode_FromString("native");
}

static PyObject *
Open64_DSC_Create_Tensor_Type(PyObject *self, PyObject *args)
{
    const char *name;
    const char *dtype;
    const char *logical_shape;
    int rank;
    Open64_DSC_Handle handle;

    (void) self;

    if (!PyArg_ParseTuple(args, "ssis:create_tensor_type",
                          &name, &dtype, &rank, &logical_shape))
        return NULL;

    handle = Open64_DSC_Create_Tensor_Type(name, dtype, rank, logical_shape);
    return Open64_DSC_Handle_Result(handle, "create tensor type");
}

static PyObject *
Open64_DSC_Intern_Tensor_Type(PyObject *self, PyObject *args)
{
    const char *name;
    PyObject *descriptor_obj;
    Open64_DSC_Tensor_Descriptor descriptor;

    (void) self;
    if (!PyArg_ParseTuple(args, "sO!:intern_tensor_type", &name,
                          &PyDict_Type, &descriptor_obj))
        return NULL;
    if (!Open64_DSC_Read_Tensor_Descriptor(descriptor_obj, &descriptor))
        return NULL;
    return Open64_DSC_Handle_Result
               (Open64_DSC_Intern_Tensor_Type(name, &descriptor),
                "intern tensor type");
}

static PyObject *
Open64_DSC_Attach_Tensor_Descriptor(PyObject *self, PyObject *args)
{
    Open64_DSC_Handle tensor_type;
    PyObject *descriptor_obj;
    Open64_DSC_Tensor_Descriptor descriptor;

    (void) self;

    if (!PyArg_ParseTuple(args, "KO!:attach_tensor_descriptor",
                          &tensor_type, &PyDict_Type, &descriptor_obj))
        return NULL;

    if (!Open64_DSC_Read_Tensor_Descriptor(descriptor_obj, &descriptor))
        return NULL;

    return Open64_DSC_Bool_Result
               (Open64_DSC_Attach_Tensor_Descriptor(tensor_type,
                                                     &descriptor),
                "attach tensor descriptor");
}

static PyObject *
Open64_DSC_Create_Tensor_Constant(PyObject *self, PyObject *args)
{
    const char *name;
    const char *dtype;
    const char *logical_shape;
    const char *value_kind;
    const char *value;
    unsigned int rank;
    Open64_DSC_Handle handle;

    (void) self;

    if (!PyArg_ParseTuple(args, "ssIsss:create_tensor_constant",
                          &name, &dtype, &rank, &logical_shape,
                          &value_kind, &value))
        return NULL;

    handle = Open64_DSC_Create_Tensor_Constant(name, dtype, rank,
                                               logical_shape, value_kind,
                                               value);
    return Open64_DSC_Handle_Result(handle, "create tensor constant");
}

static PyObject *
Open64_DSC_Create_Model_Input(PyObject *self, PyObject *args)
{
    const char *name;
    Open64_DSC_Handle tensor_type;
    unsigned int input_ordinal;
    Open64_DSC_Handle handle;

    (void) self;

    if (!PyArg_ParseTuple(args, "sKI:create_model_input",
                          &name, &tensor_type, &input_ordinal))
        return NULL;

    handle = Open64_DSC_Create_Model_Input(name, tensor_type, input_ordinal);
    return Open64_DSC_Handle_Result(handle, "create model input");
}

static PyObject *
Open64_DSC_Create_External_Tensor_Constant(PyObject *self, PyObject *args)
{
    const char *name;
    Open64_DSC_Handle tensor_type;
    PyObject *reference_obj;
    Open64_DSC_External_Tensor_Reference reference;
    Open64_DSC_Handle handle;

    (void) self;

    if (!PyArg_ParseTuple(args, "sKO!:create_external_tensor_constant",
                          &name, &tensor_type, &PyDict_Type,
                          &reference_obj))
        return NULL;

    if (!Open64_DSC_Read_External_Tensor_Reference(reference_obj, &reference))
        return NULL;

    handle = Open64_DSC_Create_External_Tensor_Constant(name, tensor_type,
                                                       &reference);
    return Open64_DSC_Handle_Result(handle,
                                    "create external tensor constant");
}

static int
Open64_DSC_Read_Handle_Sequence(PyObject *kids_obj,
                                std::vector<Open64_DSC_Handle> *kids)
{
    PyObject *fast;
    Py_ssize_t count;

    if (kids == NULL)
        return 0;

    fast = PySequence_Fast(kids_obj, "kids must be a sequence");
    if (fast == NULL)
        return 0;

    count = PySequence_Fast_GET_SIZE(fast);
    kids->reserve((size_t) count);

    for (Py_ssize_t i = 0; i < count; ++i) {
        PyObject *item = PySequence_Fast_GET_ITEM(fast, i);
        Open64_DSC_Handle handle = PyLong_AsUnsignedLongLong(item);
        if (PyErr_Occurred()) {
            Py_DECREF(fast);
            return 0;
        }
        kids->push_back(handle);
    }

    Py_DECREF(fast);
    return 1;
}

static int
Open64_DSC_Read_Attributes(PyObject *attrs_obj,
                           std::vector<Open64_DSC_Attribute> *attrs)
{
    PyObject *key;
    PyObject *value;
    Py_ssize_t pos = 0;

    if (attrs == NULL)
        return 0;

    if (!PyDict_Check(attrs_obj)) {
        PyErr_SetString(PyExc_TypeError, "attrs must be a dict");
        return 0;
    }

    attrs->reserve((size_t) PyDict_Size(attrs_obj));
    while (PyDict_Next(attrs_obj, &pos, &key, &value)) {
        Open64_DSC_Attribute attr;

        if (!PyUnicode_Check(key) || !PyUnicode_Check(value)) {
            PyErr_SetString(PyExc_TypeError,
                            "attribute names and values must be str");
            return 0;
        }

        attr.name = PyUnicode_AsUTF8(key);
        attr.value = PyUnicode_AsUTF8(value);
        if (attr.name == NULL || attr.value == NULL)
            return 0;

        attrs->push_back(attr);
    }

    return 1;
}

static PyObject *
Open64_DSC_Create_Operator(PyObject *self, PyObject *args)
{
    const char *opcode_name;
    unsigned int version;
    PyObject *kids_obj;
    PyObject *attrs_obj;
    std::vector<Open64_DSC_Handle> kids;
    std::vector<Open64_DSC_Attribute> attrs;
    Open64_DSC_Handle handle;

    (void) self;

    if (!PyArg_ParseTuple(args, "sIOO:create_operator",
                          &opcode_name, &version, &kids_obj, &attrs_obj))
        return NULL;

    if (!Open64_DSC_Read_Handle_Sequence(kids_obj, &kids))
        return NULL;
    if (!Open64_DSC_Read_Attributes(attrs_obj, &attrs))
        return NULL;

    handle = Open64_DSC_Create_Operator
                 (opcode_name, version,
                  kids.empty() ? NULL : &kids[0], (unsigned int) kids.size(),
                  attrs.empty() ? NULL : &attrs[0],
                  (unsigned int) attrs.size());
    return Open64_DSC_Handle_Result(handle, "create operator");
}

static PyObject *
Open64_DSC_Create_Operator_With_Result(PyObject *self, PyObject *args)
{
    const char *opcode_name;
    unsigned int version;
    PyObject *kids_obj;
    PyObject *attrs_obj;
    const char *result_name;
    Open64_DSC_Handle result_type;
    std::vector<Open64_DSC_Handle> kids;
    std::vector<Open64_DSC_Attribute> attrs;

    (void) self;
    if (!PyArg_ParseTuple(args, "sIOOsK:create_operator_with_result",
                          &opcode_name, &version, &kids_obj, &attrs_obj,
                          &result_name, &result_type))
        return NULL;
    if (!Open64_DSC_Read_Handle_Sequence(kids_obj, &kids) ||
        !Open64_DSC_Read_Attributes(attrs_obj, &attrs))
        return NULL;
    return Open64_DSC_Handle_Result
               (Open64_DSC_Create_Operator_With_Result
                    (opcode_name, version,
                     kids.empty() ? NULL : &kids[0],
                     (unsigned int) kids.size(),
                     attrs.empty() ? NULL : &attrs[0],
                     (unsigned int) attrs.size(), result_name, result_type),
                "create operator with result");
}

static PyObject *
Open64_DSC_Create_Symbol(PyObject *self, PyObject *args)
{
    const char *name;
    Open64_DSC_Handle tensor_type;
    Open64_DSC_Handle handle;

    (void) self;

    if (!PyArg_ParseTuple(args, "sK:create_symbol", &name, &tensor_type))
        return NULL;

    handle = Open64_DSC_Create_Symbol(name, tensor_type);
    return Open64_DSC_Handle_Result(handle, "create symbol");
}

static PyObject *
Open64_DSC_Attach_Symbol_Metadata(PyObject *self, PyObject *args)
{
    Open64_DSC_Handle symbol;
    PyObject *metadata_obj;
    std::vector<Open64_DSC_Attribute> metadata;

    (void) self;

    if (!PyArg_ParseTuple(args, "KO!:attach_symbol_metadata",
                          &symbol, &PyDict_Type, &metadata_obj))
        return NULL;

    if (!Open64_DSC_Read_Attributes(metadata_obj, &metadata))
        return NULL;

    return Open64_DSC_Bool_Result
               (Open64_DSC_Attach_Symbol_Metadata
                    (symbol,
                     metadata.empty() ? NULL : &metadata[0],
                     (unsigned int) metadata.size()),
                "attach symbol metadata");
}

static PyObject *
Open64_DSC_Attach_Value_Metadata(PyObject *self, PyObject *args)
{
    Open64_DSC_Handle value;
    PyObject *metadata_obj;
    std::vector<Open64_DSC_Attribute> metadata;

    (void) self;
    if (!PyArg_ParseTuple(args, "KO!:attach_value_metadata", &value,
                          &PyDict_Type, &metadata_obj))
        return NULL;
    if (!Open64_DSC_Read_Attributes(metadata_obj, &metadata))
        return NULL;
    return Open64_DSC_Bool_Result
               (Open64_DSC_Attach_Value_Metadata
                    (value, metadata.empty() ? NULL : &metadata[0],
                     (unsigned int) metadata.size()),
                "attach value metadata");
}

static PyObject *
Open64_DSC_Attach_Value_Lineage(PyObject *self, PyObject *args)
{
    Open64_DSC_Handle value;
    const char *lineage;

    (void) self;
    if (!PyArg_ParseTuple(args, "Ks:attach_value_lineage", &value, &lineage))
        return NULL;
    return Open64_DSC_Bool_Result
               (Open64_DSC_Attach_Value_Lineage(value, lineage),
                "attach value lineage");
}

static PyObject *
Open64_DSC_Get_Value_Type(PyObject *self, PyObject *args)
{
    Open64_DSC_Handle value;

    (void) self;
    if (!PyArg_ParseTuple(args, "K:get_value_type", &value))
        return NULL;
    return Open64_DSC_Handle_Result(Open64_DSC_Get_Value_Type(value),
                                    "get value type");
}

static PyObject *
Open64_DSC_Get_Value_Result_Symbol(PyObject *self, PyObject *args)
{
    Open64_DSC_Handle value;

    (void) self;
    if (!PyArg_ParseTuple(args, "K:get_value_result_symbol", &value))
        return NULL;
    return Open64_DSC_Handle_Result
               (Open64_DSC_Get_Value_Result_Symbol(value),
                "get value result symbol");
}

static PyObject *
Open64_DSC_Begin(PyObject *self, PyObject *args)
{
    (void) self;
    (void) args;
    return Open64_DSC_Bool_Result(Open64_DSC_Begin_Program(),
                                  "begin program");
}

static PyObject *
Open64_DSC_Abort(PyObject *self, PyObject *args)
{
    (void) self;
    (void) args;
    Open64_DSC_Abort_Program();
    Py_RETURN_NONE;
}

static PyObject *
Open64_DSC_Create_Minimal_Program_Unit(PyObject *self, PyObject *args)
{
    const char *name;
    Open64_DSC_Handle handle;

    (void) self;

    if (!PyArg_ParseTuple(args, "s:create_minimal_program_unit", &name))
        return NULL;

    handle = Open64_DSC_Create_Minimal_Program_Unit(name);
    return Open64_DSC_Handle_Result(handle, "create minimal program unit");
}

static PyObject *
Open64_DSC_Register_Source_File(PyObject *self, PyObject *args)
{
    Open64_DSC_Handle program_unit;
    const char *path;
    unsigned int file_id;

    (void) self;
    if (!PyArg_ParseTuple(args, "Ks:register_source_file", &program_unit,
                          &path))
        return NULL;
    file_id = Open64_DSC_Register_Source_File(program_unit, path);
    if (file_id == 0) {
        PyErr_SetString(PyExc_RuntimeError, "failed to register source file");
        return NULL;
    }
    return PyLong_FromUnsignedLong(file_id);
}

static PyObject *
Open64_DSC_Set_Value_Source_Position(PyObject *self, PyObject *args)
{
    Open64_DSC_Handle value;
    unsigned int file_id;
    int line;
    unsigned int column;
    int statement_begin;
    int basic_block_begin;
    Open64_DSC_Source_Position position;

    (void) self;
    if (!PyArg_ParseTuple(args, "KIiIpp:set_value_source_position", &value,
                          &file_id, &line, &column, &statement_begin,
                          &basic_block_begin))
        return NULL;
    if (column > 65535) {
        PyErr_SetString(PyExc_ValueError, "source column is out of range");
        return NULL;
    }
    position.file_id = file_id;
    position.line = line;
    position.column = (unsigned short) column;
    position.statement_begin = statement_begin != 0;
    position.basic_block_begin = basic_block_begin != 0;
    return Open64_DSC_Bool_Result
               (Open64_DSC_Set_Value_Source_Position(value, &position),
                "set value source position");
}

static PyObject *
Open64_DSC_Create_Region(PyObject *self, PyObject *args)
{
    Open64_DSC_Handle program_unit;
    Open64_DSC_Handle parent_region;
    const char *contract_name;
    unsigned int contract_version;
    (void) self;
    if (!PyArg_ParseTuple(args, "KKsI:create_region", &program_unit,
                          &parent_region, &contract_name, &contract_version))
        return NULL;
    return Open64_DSC_Handle_Result
               (Open64_DSC_Create_Region(program_unit, parent_region,
                                         contract_name, contract_version),
                "create region");
}

static PyObject *
Open64_DSC_Append_Region_Value(PyObject *self, PyObject *args)
{
    Open64_DSC_Handle region;
    Open64_DSC_Handle value;
    (void) self;
    if (!PyArg_ParseTuple(args, "KK:append_region_value", &region, &value))
        return NULL;
    return Open64_DSC_Bool_Result
               (Open64_DSC_Append_Region_Value(region, value),
                "append region value");
}

static PyObject *
Open64_DSC_Append_Program_Unit_Region(PyObject *self, PyObject *args)
{
    Open64_DSC_Handle program_unit;
    Open64_DSC_Handle region;
    (void) self;
    if (!PyArg_ParseTuple(args, "KK:append_program_unit_region",
                          &program_unit, &region))
        return NULL;
    return Open64_DSC_Bool_Result
               (Open64_DSC_Append_Program_Unit_Region(program_unit, region),
                "append program unit region");
}

static PyObject *
Open64_DSC_Append_Child_Region(PyObject *self, PyObject *args)
{
    Open64_DSC_Handle parent_region;
    Open64_DSC_Handle child_region;
    (void) self;
    if (!PyArg_ParseTuple(args, "KK:append_child_region", &parent_region,
                          &child_region))
        return NULL;
    return Open64_DSC_Bool_Result
               (Open64_DSC_Append_Child_Region(parent_region, child_region),
                "append child region");
}

static PyObject *
Open64_DSC_Declare_Region_Value(PyObject *self, PyObject *args)
{
    Open64_DSC_Handle region;
    Open64_DSC_Handle value;
    unsigned int roles;
    unsigned int ordinal;
    unsigned int flags;
    (void) self;
    if (!PyArg_ParseTuple(args, "KKIII:declare_region_value", &region,
                          &value, &roles, &ordinal, &flags))
        return NULL;
    return Open64_DSC_Bool_Result
               (Open64_DSC_Declare_Region_Value
                    (region, value, roles, ordinal, flags),
                "declare region value");
}

static PyObject *
Open64_DSC_Set_Region_Source_Position(PyObject *self, PyObject *args)
{
    Open64_DSC_Handle region;
    unsigned int file_id;
    int line;
    unsigned int column;
    int statement_begin;
    int basic_block_begin;
    Open64_DSC_Source_Position position;
    (void) self;
    if (!PyArg_ParseTuple(args, "KIiIpp:set_region_source_position", &region,
                          &file_id, &line, &column, &statement_begin,
                          &basic_block_begin))
        return NULL;
    if (column > 65535) {
        PyErr_SetString(PyExc_ValueError, "source column is out of range");
        return NULL;
    }
    position.file_id = file_id;
    position.line = line;
    position.column = (unsigned short) column;
    position.statement_begin = statement_begin != 0;
    position.basic_block_begin = basic_block_begin != 0;
    return Open64_DSC_Bool_Result
               (Open64_DSC_Set_Region_Source_Position(region, &position),
                "set region source position");
}

static PyObject *
Open64_DSC_Set_Region_Metadata(PyObject *self, PyObject *args)
{
    Open64_DSC_Handle region;
    const char *key;
    const char *value;
    (void) self;
    if (!PyArg_ParseTuple(args, "Kss:set_region_metadata", &region,
                          &key, &value))
        return NULL;
    return Open64_DSC_Bool_Result
               (Open64_DSC_Set_Region_Metadata(region, key, value),
                "set region metadata");
}

static PyObject *
Open64_DSC_Verify(PyObject *self, PyObject *args)
{
    char diagnostic[4096];
    Open64_DSC_Verify_Result result;
    int valid;
    PyObject *record;

    (void) self;
    (void) args;
    result.native_node_count = 0;
    result.result_symbol_count = 0;
    result.error_count = 0;
    result.diagnostic = diagnostic;
    result.diagnostic_capacity = sizeof(diagnostic);
    valid = Open64_DSC_Verify_Program(&result);
    record = Py_BuildValue
                 ("{s:O,s:I,s:I,s:I,s:s}",
                  "valid", valid ? Py_True : Py_False,
                  "native_node_count", result.native_node_count,
                  "result_symbol_count", result.result_symbol_count,
                  "error_count", result.error_count,
                  "diagnostic", diagnostic);
    return record;
}

static PyObject *
Open64_DSC_Append_Program_Unit_Value(PyObject *self, PyObject *args)
{
    Open64_DSC_Handle program_unit;
    Open64_DSC_Handle value;

    (void) self;

    if (!PyArg_ParseTuple(args, "KK:append_program_unit_value",
                          &program_unit, &value))
        return NULL;

    return Open64_DSC_Bool_Result
               (Open64_DSC_Append_Program_Unit_Value(program_unit, value),
                "append program unit value");
}

static PyObject *
Open64_DSC_Append_Program_Unit_Marker(PyObject *self, PyObject *args)
{
    Open64_DSC_Handle program_unit;
    Open64_DSC_Handle marker;

    (void) self;

    if (!PyArg_ParseTuple(args, "KK:append_program_unit_marker",
                          &program_unit, &marker))
        return NULL;

    return Open64_DSC_Bool_Result
               (Open64_DSC_Append_Program_Unit_Marker(program_unit, marker),
                "append program unit marker");
}

static PyObject *
Open64_DSC_Inspect_Program_Unit_Values(PyObject *self, PyObject *args)
{
    Open64_DSC_Handle program_unit;
    unsigned int value_count;
    PyObject *values;

    (void) self;

    if (!PyArg_ParseTuple(args, "K:inspect_program_unit_values",
                          &program_unit))
        return NULL;

    value_count = Open64_DSC_Count_Program_Unit_Values(program_unit);
    values = PyList_New((Py_ssize_t) value_count);
    if (values == NULL)
        return NULL;

    for (unsigned int i = 0; i < value_count; ++i) {
        Open64_DSC_Value_Info info;
        PyObject *record;
        PyObject *opcode;
        PyObject *version;
        PyObject *payload;

        if (!Open64_DSC_Get_Program_Unit_Value(program_unit, i, &info)) {
            Py_DECREF(values);
            PyErr_SetString(PyExc_RuntimeError,
                            "failed to inspect program unit value");
            return NULL;
        }

        record = PyDict_New();
        opcode = PyUnicode_FromStringAndSize(info.opcode_name,
                                             info.opcode_name_len);
        version = PyLong_FromUnsignedLong(info.version);
        payload = PyUnicode_FromString(info.payload == NULL ?
                                       "" : info.payload);
        if (record == NULL || opcode == NULL ||
            version == NULL || payload == NULL) {
            Py_XDECREF(record);
            Py_XDECREF(opcode);
            Py_XDECREF(version);
            Py_XDECREF(payload);
            Py_DECREF(values);
            return NULL;
        }

        if (PyDict_SetItemString(record, "opcode", opcode) != 0 ||
            PyDict_SetItemString(record, "version", version) != 0 ||
            PyDict_SetItemString(record, "payload", payload) != 0) {
            Py_DECREF(record);
            Py_DECREF(opcode);
            Py_DECREF(version);
            Py_DECREF(payload);
            Py_DECREF(values);
            return NULL;
        }

        Py_DECREF(opcode);
        Py_DECREF(version);
        Py_DECREF(payload);
        PyList_SET_ITEM(values, (Py_ssize_t) i, record);
    }

    return values;
}

static PyObject *
Open64_DSC_Inspect_Program_Unit_Markers(PyObject *self, PyObject *args)
{
    Open64_DSC_Handle program_unit;
    unsigned int marker_count;
    PyObject *markers;

    (void) self;

    if (!PyArg_ParseTuple(args, "K:inspect_program_unit_markers",
                          &program_unit))
        return NULL;

    marker_count = Open64_DSC_Count_Program_Unit_Markers(program_unit);
    markers = PyList_New((Py_ssize_t) marker_count);
    if (markers == NULL)
        return NULL;

    for (unsigned int i = 0; i < marker_count; ++i) {
        Open64_DSC_Marker_Info info;
        PyObject *record;
        PyObject *opcode;
        PyObject *version;
        PyObject *payload;

        if (!Open64_DSC_Get_Program_Unit_Marker(program_unit, i, &info)) {
            Py_DECREF(markers);
            PyErr_SetString(PyExc_RuntimeError,
                            "failed to inspect program unit marker");
            return NULL;
        }

        record = PyDict_New();
        opcode = PyUnicode_FromStringAndSize(info.opcode_name,
                                             info.opcode_name_len);
        version = PyLong_FromUnsignedLong(info.version);
        payload = PyUnicode_FromString(info.payload == NULL ?
                                       "" : info.payload);
        if (record == NULL || opcode == NULL ||
            version == NULL || payload == NULL) {
            Py_XDECREF(record);
            Py_XDECREF(opcode);
            Py_XDECREF(version);
            Py_XDECREF(payload);
            Py_DECREF(markers);
            return NULL;
        }

        if (PyDict_SetItemString(record, "opcode", opcode) != 0 ||
            PyDict_SetItemString(record, "version", version) != 0 ||
            PyDict_SetItemString(record, "payload", payload) != 0) {
            Py_DECREF(record);
            Py_DECREF(opcode);
            Py_DECREF(version);
            Py_DECREF(payload);
            Py_DECREF(markers);
            return NULL;
        }

        Py_DECREF(opcode);
        Py_DECREF(version);
        Py_DECREF(payload);
        PyList_SET_ITEM(markers, (Py_ssize_t) i, record);
    }

    return markers;
}

static PyObject *
Open64_DSC_Finalize(PyObject *self, PyObject *args)
{
    const char *path;
    PyObject *manifest;

    (void) self;

    if (!PyArg_ParseTuple(args, "sO!:finalize_mapped_image",
                          &path, &PyDict_Type, &manifest))
        return NULL;

    /*
     * The manifest argument intentionally matches the mock backend shape.
     * Native finalization currently consumes staged builder state rather than
     * reconstructing WHIRL objects from the Python manifest.
     */
    (void) manifest;

    if (Open64_DSC_Finalize_Mapped_Image(path))
        Py_RETURN_TRUE;

    Py_RETURN_FALSE;
}

static PyMethodDef Open64_DSC_Methods[] = {
    {
        "backend_name",
        Open64_DSC_Backend_Name,
        METH_NOARGS,
        "Return the backend name."
    },
    {
        "create_tensor_type",
        Open64_DSC_Create_Tensor_Type,
        METH_VARARGS,
        "Create a native tensor type and return an opaque handle."
    },
    {
        "intern_tensor_type",
        Open64_DSC_Intern_Tensor_Type,
        METH_VARARGS,
        "Intern a canonical native tensor descriptor and return its handle."
    },
    {
        "attach_tensor_descriptor",
        Open64_DSC_Attach_Tensor_Descriptor,
        METH_VARARGS,
        "Attach a tensor descriptor to a native tensor type."
    },
    {
        "create_tensor_constant",
        Open64_DSC_Create_Tensor_Constant,
        METH_VARARGS,
        "Create a native tensor constant and return an opaque handle."
    },
    {
        "create_model_input",
        Open64_DSC_Create_Model_Input,
        METH_VARARGS,
        "Create a native model input and return an opaque handle."
    },
    {
        "create_external_tensor_constant",
        Open64_DSC_Create_External_Tensor_Constant,
        METH_VARARGS,
        "Create a native external tensor constant and return an opaque handle."
    },
    {
        "create_operator",
        Open64_DSC_Create_Operator,
        METH_VARARGS,
        "Create a native DSL operator and return an opaque handle."
    },
    {
        "create_operator_with_result",
        Open64_DSC_Create_Operator_With_Result,
        METH_VARARGS,
        "Create a native DSL operator with an explicit result type."
    },
    {
        "create_symbol",
        Open64_DSC_Create_Symbol,
        METH_VARARGS,
        "Create a native symbol for a tensor type."
    },
    {
        "attach_symbol_metadata",
        Open64_DSC_Attach_Symbol_Metadata,
        METH_VARARGS,
        "Attach compiler metadata to a native symbol."
    },
    {
        "attach_value_metadata",
        Open64_DSC_Attach_Value_Metadata,
        METH_VARARGS,
        "Attach compiler metadata to a native value result."
    },
    {
        "attach_value_lineage",
        Open64_DSC_Attach_Value_Lineage,
        METH_VARARGS,
        "Attach semantic lineage to a native value result."
    },
    {
        "get_value_type",
        Open64_DSC_Get_Value_Type,
        METH_VARARGS,
        "Get the opaque canonical tensor type for a value."
    },
    {
        "get_value_result_symbol",
        Open64_DSC_Get_Value_Result_Symbol,
        METH_VARARGS,
        "Get the opaque result symbol for a value."
    },
    {
        "begin_program",
        Open64_DSC_Begin,
        METH_NOARGS,
        "Begin an isolated native builder program."
    },
    {
        "abort_program",
        Open64_DSC_Abort,
        METH_NOARGS,
        "Discard the current native builder program."
    },
    {
        "create_minimal_program_unit",
        Open64_DSC_Create_Minimal_Program_Unit,
        METH_VARARGS,
        "Create a minimal native PU tree entry and return an opaque handle."
    },
    {
        "register_source_file",
        Open64_DSC_Register_Source_File,
        METH_VARARGS,
        "Register a source file for a native program unit."
    },
    {
        "set_value_source_position",
        Open64_DSC_Set_Value_Source_Position,
        METH_VARARGS,
        "Attach a source position to a native value definition."
    },
    {
        "create_region",
        Open64_DSC_Create_Region,
        METH_VARARGS,
        "Create an opaque structured WHIRL region."
    },
    {
        "append_region_value",
        Open64_DSC_Append_Region_Value,
        METH_VARARGS,
        "Append a DSL value definition to a structured region."
    },
    {
        "append_program_unit_region",
        Open64_DSC_Append_Program_Unit_Region,
        METH_VARARGS,
        "Append a structured region to a native PU body."
    },
    {
        "append_child_region",
        Open64_DSC_Append_Child_Region,
        METH_VARARGS,
        "Append a structured child region to a parent region."
    },
    {
        "declare_region_value",
        Open64_DSC_Declare_Region_Value,
        METH_VARARGS,
        "Declare a structured region value interface."
    },
    {
        "set_region_source_position",
        Open64_DSC_Set_Region_Source_Position,
        METH_VARARGS,
        "Attach a source position to a structured region."
    },
    {
        "set_region_metadata",
        Open64_DSC_Set_Region_Metadata,
        METH_VARARGS,
        "Attach metadata to a structured region."
    },
    {
        "verify_program",
        Open64_DSC_Verify,
        METH_NOARGS,
        "Run structured native DSL gatekeeper verification."
    },
    {
        "append_program_unit_value",
        Open64_DSC_Append_Program_Unit_Value,
        METH_VARARGS,
        "Append a DSL value/operator node to a native PU body."
    },
    {
        "append_program_unit_marker",
        Open64_DSC_Append_Program_Unit_Marker,
        METH_VARARGS,
        "Append a staged DSL marker to a native PU body."
    },
    {
        "inspect_program_unit_values",
        Open64_DSC_Inspect_Program_Unit_Values,
        METH_VARARGS,
        "Inspect DSL values/operators attached to a native PU body."
    },
    {
        "inspect_program_unit_markers",
        Open64_DSC_Inspect_Program_Unit_Markers,
        METH_VARARGS,
        "Inspect staged DSL markers attached to a native PU body."
    },
    {
        "finalize_mapped_image",
        Open64_DSC_Finalize,
        METH_VARARGS,
        "Finalize a mapped WHIRL image through the native builder."
    },
    {NULL, NULL, 0, NULL}
};

static struct PyModuleDef Open64_DSC_Module = {
    PyModuleDef_HEAD_INIT,
    "_whirl",
    "Native Open64 WHIRL builder bridge.",
    -1,
    Open64_DSC_Methods,
    NULL,
    NULL,
    NULL,
    NULL
};

PyMODINIT_FUNC
PyInit__whirl(void)
{
    return PyModule_Create(&Open64_DSC_Module);
}
