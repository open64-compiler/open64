/* Copyright (C) 2026 Open64 Project */
/* Pure source-graph query. It never enters the common builder or touches
 * native compiler tables. Whole-graph frontend preflight calls it before PU
 * construction; the artifact reviewer repeats it on independently mapped IR.
 * Design: doc/AI-COMPILER-MLA-Q1-NATIVE-INGESTION-v1.0.md. */
#include "quantized_cache_inference.h"

/* Bound source text and reject embedded NUL; exceptions publish no partial query. */
static std::string Open64_Q1_String(PyObject *value)
{
    if(!value || !PyUnicode_Check(value)) throw std::runtime_error("cache inference string required");
    Py_ssize_t size=0;const char *text=PyUnicode_AsUTF8AndSize(value,&size);
    if(!text || size>4096 || memchr(text,0,size)) throw std::runtime_error("cache inference string bounds");
    return std::string(text,size);
}
/* Decode copied logical records under the GIL; return detached facts or a
 * ValueError without entering builder tables, PU traversal or optimization. */
static PyObject *Open64_Q1_Infer(PyObject *,PyObject *args)
{
    PyObject *records;
    if(!PyArg_ParseTuple(args,"O!:infer_quantized_cache_traits",&PyList_Type,&records)) return NULL;
    try {
        if(PyList_Size(records)>32768) throw std::runtime_error("cache inference graph bound");
        std::vector<open64_q1::DSC_QUANTIZED_SOURCE_VALUE> graph;
        std::map<std::string,size_t> names;
        for(Py_ssize_t i=0;i<PyList_Size(records);++i) {
            PyObject *record=PyList_GetItem(records,i);
            if(!PyDict_Check(record) || PyDict_Size(record)!=7) throw std::runtime_error("cache inference record schema");
            open64_q1::DSC_QUANTIZED_SOURCE_VALUE v;
            v.name=Open64_Q1_String(PyDict_GetItemString(record,"name"));
            v.op=Open64_Q1_String(PyDict_GetItemString(record,"opcode"));
            v.dtype=Open64_Q1_String(PyDict_GetItemString(record,"dtype"));
            PyObject *version=PyDict_GetItemString(record,"version");
            if(!version || !PyLong_CheckExact(version)) throw std::runtime_error("cache inference version");
            unsigned long ver=PyLong_AsUnsignedLong(version);
            if(PyErr_Occurred() || ver>65535) throw std::runtime_error("cache inference version range");
            v.version=(unsigned)ver;
            PyObject *kids=PyDict_GetItemString(record,"operands"),*shape=PyDict_GetItemString(record,"shape"),
                     *attrs=PyDict_GetItemString(record,"attrs");
            if(!kids || !PyList_Check(kids) || PyList_Size(kids)>3 || !shape || !PyList_Check(shape) ||
               PyList_Size(shape)<1 || PyList_Size(shape)>16 || !attrs || !PyDict_Check(attrs) || PyDict_Size(attrs)>16)
                throw std::runtime_error("cache inference operands/shape/attributes");
            for(Py_ssize_t k=0;k<PyList_Size(kids);++k) {
                std::string name=Open64_Q1_String(PyList_GetItem(kids,k));
                if(!names.count(name)) throw std::runtime_error("cache inference undefined operand");
                v.kids.push_back(names[name]);
            }
            for(Py_ssize_t d=0;d<PyList_Size(shape);++d) {
                PyObject *dim=PyList_GetItem(shape,d);
                if(!PyLong_CheckExact(dim)) throw std::runtime_error("cache inference static dimension");
                uint64_t n=PyLong_AsUnsignedLongLong(dim);
                if(PyErr_Occurred() || !n) throw std::runtime_error("cache inference positive dimension");
                v.shape.push_back(n);
            }
            Py_ssize_t pos=0;PyObject *key,*value;
            while(PyDict_Next(attrs,&pos,&key,&value)) v.attrs[Open64_Q1_String(key)]=Open64_Q1_String(value);
            if(names.count(v.name)) throw std::runtime_error("cache inference duplicate value");
            names[v.name]=graph.size();graph.push_back(v);
        }
        open64_q1::DSC_QUANTIZED_SOURCE_FACTS facts=open64_q1::DSC_QUANTIZED_SOURCE_INFERENCE(graph).Infer();
        PyObject *result=PyDict_New();if(!result) return NULL;
        for(open64_q1::DSC_QUANTIZED_SOURCE_FACTS::const_iterator f=facts.begin();f!=facts.end();++f) {
            PyObject *row=Py_BuildValue("{s:s,s:s,s:s,s:s}","role",f->second.role.c_str(),
                "encoding",f->second.encoding.c_str(),"scale",f->second.scale.c_str(),
                "evidence",f->second.evidence.c_str());
            if(!row || PyDict_SetItemString(result,graph[f->first].name.c_str(),row)) {
                Py_XDECREF(row);Py_DECREF(result);return NULL;
            }
            Py_DECREF(row);
        }
        return result;
    } catch(const std::exception &error) {
        PyErr_Clear();PyErr_SetString(PyExc_ValueError,error.what());return NULL;
    }
}
