/*
 * Copyright (C) 2026 Open64 Project
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "open64_dsc_native_bridge.h"

static PyObject *
Open64_DSC_Backend_Name(PyObject *self, PyObject *args)
{
    (void) self;
    (void) args;

    return PyUnicode_FromString("native");
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
    Open64_DSC_Methods
};

PyMODINIT_FUNC
PyInit__whirl(void)
{
    return PyModule_Create(&Open64_DSC_Module);
}
