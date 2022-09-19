/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

// ==================================================================
// JavaScriptCodeGen
// JavaScript CodeGen to generate JavaScript ffi wrapper
// ==================================================================

// builtin types available in node.js ref/ffi module
#define DeclAllTypes() \
  DeclType(unknown),   \
  DeclType(void),      \
  DeclType(int8),      \
  DeclType(uint8),     \
  DeclType(int16),     \
  DeclType(uint16),    \
  DeclType(int32),     \
  DeclType(uint32),    \
  DeclType(int64),     \
  DeclType(uint64),    \
  DeclType(float),     \
  DeclType(double),    \
  DeclType(Object),    \
  DeclType(CString)

// JavaScriptCodeGen
// code generator for JavaScript
class JavaScriptCodeGen {
protected:

  // JavaScript builtin types
#define DeclType(x) Builtin_##x
  enum BuiltinTypes {
    DeclAllTypes(),
    Builtin_last
  };
#undef DeclType

  // JavaScript builtin types description
  struct BuiltinTypeDesc {
    enum BuiltinTypes _type;
    const char*       _name;
  };

  // JavaScript builtin types description
  static const BuiltinTypeDesc _type_desc[Builtin_last];

  // module export table
  mutable std::vector<const char*> _exports;

public:
  // line comment in JavaScript
  const char *Line_comment() const {
    return "//";
  }

  // block comment start in JavaScript
  const char *Block_comment_begin() const {
    return "/*";
  }

  // block comment end in JavaScript
  const char *Block_comment_end() const {
    return "*/";
  }

  // language name
  const char *Lang_name() const {
    return "JavaScript";
  }

  // convert WHIRL MTYPE to JavaScript builtin types
  BuiltinTypes Mtype_to_builtin(TYPE_ID mtype) const {
    switch (mtype) {
    case MTYPE_I1:  return Builtin_int8;
    case MTYPE_I2:  return Builtin_int16;
    case MTYPE_I4:  return Builtin_int32;
    case MTYPE_I8:  return Builtin_int64;
    case MTYPE_U1:  return Builtin_uint8;
    case MTYPE_U2:  return Builtin_uint16;
    case MTYPE_U4:  return Builtin_uint32;
    case MTYPE_U8:  return Builtin_uint64;
    case MTYPE_F4:  return Builtin_float;
    case MTYPE_F8:  return Builtin_double;
    case MTYPE_V:   return Builtin_void;
    default:        return Builtin_unknown;
    }
  }

private:
  // write export table for module
  void Write_export_table(FILE *fp) const {
    fprintf(fp, "\n");
    fprintf(fp, "// export table for this module\n");
    std::vector<const char*>::const_iterator it;
    for (it = _exports.begin(); it != _exports.end(); ++it) {
      fprintf(fp, "module.exports.%s = whirl.%s;\n", *it, *it);
    }
  }

public:
  // write header, to import ref and ffi module
  void Write_header(FILE *fp) const {
    Is_True(fp != NULL, ("invalid output file"));
    fprintf(fp, "// add required packages\n");
    fprintf(fp, "var ref = require(\'ref-napi\');\n");
    fprintf(fp, "var ffi = require(\'ffi-napi\');\n");
    fprintf(fp, "var ArrayType = require(\'ref-array-napi\');\n\n");
  }

  // write footer, do nothing
  void Write_footer(FILE *fp) const { }

  // start type section, almost do nothing
  void Write_type_begin(FILE *fp) const {
    fprintf(fp, "// type declaration\n");
  }

  // write a type, decl indicate it's for type declaration or reference
  void Write_type(FILE *fp, UINT32 ty, BOOL decl) const;

  // get a type, decl indicate, which is called by Write_type
  const char* Get_type(FILE *fp, UINT32 ty, BOOL decl) const;

  // end type section, almost do nothing
  void Write_type_end(FILE *fp) const {
    fprintf(fp, "\n");
  };

  // start symbol section, load wffi library
  void Write_symbol_begin(FILE *fp) const {
    fprintf(fp, "// declare variable whirl used in JavaScript\n");
    fprintf(fp, "var whirl = ffi.Library(\'%s\', {\n", libwffi_name);
  }

  // write a symbol, lastone indicate if this is the last symbol to be written
  void Write_symbol(FILE *fp, const ApiInfo &api, BOOL lastone) const;

  // end symbol section
  void Write_symbol_end(FILE *fp) const {
    fprintf(fp, "});\n");
    if (_exports.size() > 0)
      Write_export_table(fp);
  }
};  // JavaScriptCodeGen

// builtin type description
#define DeclType(x) { Builtin_##x, #x }
const JavaScriptCodeGen::BuiltinTypeDesc JavaScriptCodeGen::_type_desc[] = {
  DeclAllTypes()
};
#undef DeclType
#undef DeclAllTypes

// JavaScriptCodeGen::Write_type
// write type
void
JavaScriptCodeGen::Write_type(FILE *fp, UINT32 ty, BOOL decl) const
{
  const char *type = Get_type(fp, ty, decl);
  if (type != NULL)
    if (GetTypeHint(ty) == ARRAY)
      fprintf(fp, "%s", type);
    else
      fprintf(fp, "\'%s\'", type);
  return;
}

const char*
JavaScriptCodeGen::Get_type(FILE *fp, UINT32 ty, BOOL decl) const {
  TYPE_HINT hint = GetTypeHint(ty);
  // handle c string
  if (hint == CSTRING) {
    // only write 'string' for ref
    if (!decl)
      return "string";
    return NULL;
  }
  // handle array
  if (hint == ARRAY) {
    const char *elem_ty = Get_type(fp, GetTypeWithHint(TY_pointed(ty), NORMAL), FALSE);
    char *array_type = (char *)malloc(sizeof(char *) * 20);
    strcpy(array_type, elem_ty);
    strcat(array_type, "_array");
    if (!decl)
      return array_type;
    else
      fprintf(fp, "var %s = ArrayType(\'%s\');\n", array_type, elem_ty);
    return NULL;
  }
  if (hint != NORMAL) {
    Is_True(FALSE, ("TODO: handle %d %s", ty, TY_name(ty)));
  }
  BuiltinTypes bty = Mtype_to_builtin(TY_mtype(ty));
  if (bty == Builtin_unknown)
    Is_True(FALSE, ("TODO: handle %d %s", ty, TY_name(ty)));

  if (!decl)
    return _type_desc[bty]._name;
  return NULL;
}

// JavaScriptCodeGen::Write_symbol
// write symbol
void
JavaScriptCodeGen::Write_symbol(FILE *fp, const ApiInfo& api, BOOL lastone) const
{
  // function name
  _exports.push_back(api._name);
  fprintf(fp, "  \'%s\': [ ", api._name);
  // return type
  Write_type(fp, api._ret_ty, FALSE);
  // param type
  fprintf(fp, ", [ ");
  for (INT i = 0; i < api._parm_ty.size(); ++i) {
    if (i > 0)
      fprintf(fp, ", ");
    Write_type(fp, api._parm_ty[i], FALSE);
  }
  fprintf(fp, " ] ]%s\n", lastone ? "" : ",");
}

