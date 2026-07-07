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
     * Native finalization only needs the output path for the current minimal
     * artifact slice; later entry points will consume structured graph data.
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
        "create_tensor_constant",
        Open64_DSC_Create_Tensor_Constant,
        METH_VARARGS,
        "Create a native tensor constant and return an opaque handle."
    },
    {
        "create_operator",
        Open64_DSC_Create_Operator,
        METH_VARARGS,
        "Create a native DSL operator and return an opaque handle."
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
