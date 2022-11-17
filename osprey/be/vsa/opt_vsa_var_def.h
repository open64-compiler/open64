//-*-c++-*-

/*
   Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_vsa_var_def.h
//
// ====================================================================
//

#ifndef opt_vsa_var_def_INCLUDED
#define opt_vsa_var_def_INCLUDED      "opt_vsa_var_def.h"

#include <vector>
#include "mempool_allocator.h"

class CODEREP;
class STMTREP;
class COMP_UNIT;
class TRAV_CONTEXT;
class SRCPOS_HANDLE;
class DNA_NODE;
class CHECK_OBJ;
class VSYM_FLD_REP;

// =============================================================================
//
// DEF_INFO is the CODEREP that is located in a different procedure
//
// =============================================================================
class DEF_INFO {
private:
  DNA_NODE           *_dna;                // define function
  CODEREP            *_cr;                 // define coderep
  STMTREP            *_sr;                 // define stmtrep
  SRCPOS_TREENODE    *_spos_node;          // points to the current srcpos tree
  IDTYPE              _spos_id;            // the index of _spos_node locates in SRCPOS_HANDLE

  DEF_INFO(void);                          // REQUIRED UNDEFINED UNWANTED methods
  DEF_INFO(const DEF_INFO&);               // REQUIRED UNDEFINED UNWANTED methods
  DEF_INFO& operator = (const DEF_INFO&);  // REQUIRED UNDEFINED UNWANTED methods
public:
  DEF_INFO(DNA_NODE *dna, CODEREP *cr, STMTREP *sr):_dna(dna), _cr(cr), _sr(sr),
                                       _spos_node(NULL), _spos_id(0){}
  ~DEF_INFO(void) {}
  DNA_NODE*         Dna()                                    { return _dna; }
  CODEREP*          Coderep()                                { return _cr; }
  STMTREP*          Stmtrep()                                { return _sr; }
  //void              Set_def_cr(DNA_NODE *dna, CODEREP *cr) { _dna = dna; _cr = cr; }
  SRCPOS_TREENODE*  Spos_node()                              { return _spos_node; }
  IDTYPE            Spos_id()                                { return _spos_id; }
  void              Set_spos(SRCPOS_TREENODE *spos_node, IDTYPE spos_id) { _spos_node = spos_node; _spos_id = spos_id; }
};

typedef std::vector<DEF_INFO *, mempool_allocator<DEF_INFO *> > DEF_INFO_VEC;

// ====================================================================
//
// VALUE_TMPL models the value of an aggregate (eg. list) member. The
//            key is overloaded member, represent index/key/fld_info.
//            The defining statement might be an assignment statement,
//            append(), push_back(), or other methods.  Context_cu
//            is needed since all VSA annotation are associated with
//            a Comp_Unit.
//
// ====================================================================
template <typename K>
class VALUE_TMPL {
private:
  K          _key;        // list&vector: index, map: the CR key, class: fld info
  CODEREP   *_val;        // value of an element of an aggregate
  STMTREP   *_def_stmt;   // one aggregate will have multiple def loc
  COMP_UNIT *_context_cu; // where is this value bind to the aggregate

  VALUE_TMPL(void);                            // REQUIRED UNDEFINED UNWANTED methods
  VALUE_TMPL(const VALUE_TMPL&);               // REQUIRED UNDEFINED UNWANTED methods
  VALUE_TMPL& operator = (const VALUE_TMPL&);  // REQUIRED UNDEFINED UNWANTED methods
public:
  VALUE_TMPL(K key, CODEREP *val, STMTREP *stmt, COMP_UNIT *context_cu) :
    _key(key), _val(val), _def_stmt(stmt), _context_cu(context_cu) {}
  ~VALUE_TMPL(void) {}

  K          Key()   const { return _key;        }
  CODEREP   *Val()   const { return _val;        }
  CODEREP   *Value() const { return _val;        }
  STMTREP   *Sr()    const { return _def_stmt;   }
  COMP_UNIT *Cu()    const { return _context_cu; }

  VALUE_TMPL *Clone(MEM_POOL *pool) const {
    return CXX_NEW(VALUE_TMPL(Key(), Val(), Sr(), Cu()), pool);
  }
  BOOL operator== (VALUE_TMPL &obj) const 
  { 
    return (obj.Key() == Key() && obj.Val() == obj.Val() && obj.Cu() == obj.Cu()) ? TRUE : FALSE;
  }

  // TODO: key compare consider different key cr with same def
  //       need U-D traverse to check
  BOOL Same_key(VALUE_TMPL &obj) const
  {
    if(Key() == obj.Key() && Cu() == obj.Cu()) {
      return TRUE;
    }
    return FALSE;
  }

  BOOL Same_key(K key, COMP_UNIT *cu) const
  {
    if(Key() == key && Cu() == cu) {
      return TRUE;
    }
    return FALSE;
  }

  void Print(FILE* fp) const
  {
    fprintf(fp, "cr%d|sr%d", Val()->Coderep_id(), Sr()->Stmtrep_id());
  }
};

typedef VALUE_TMPL<INT> LIST_ENTRY;
typedef VALUE_TMPL<VSYM_FLD_REP> OBJ_ENTRY;  // for class/struct

// ====================================================================
//
// MAP_ENTRY models the value of an aggregate with {key,value} pair.
//           The defining statement might be an assignment statement,
//           append(), push_back(), or other methods.  Context_cu
//           is needed since all VSA annotation are associated with
//           a Comp_Unit.
//           We use it to model MAP or Array/Vector.  When we model
//           vector, the key contains the index in CR form.  If the
//           CR is of Kind CK_VAR, it represents multiple entries in
//           that get referenced as variable index.
//
// ====================================================================
class MAP_ENTRY
{
private:
  typedef VALUE_TMPL<CODEREP*> MAP_BASE;
  char    *_key_str;                      // entry key string for const string
  MAP_BASE _value_entry;                  // entry value

  MAP_ENTRY(void);                           // REQUIRED UNDEFINED UNWANTED methods
  MAP_ENTRY(const MAP_ENTRY&);               // REQUIRED UNDEFINED UNWANTED methods
  MAP_ENTRY& operator = (const MAP_ENTRY&);  // REQUIRED UNDEFINED UNWANTED methods
public:
  MAP_ENTRY(CODEREP *key, CODEREP *value, STMTREP *stmt,
            COMP_UNIT *comp_unit, char *str = NULL) : _key_str(str),
    _value_entry(key, value, stmt, comp_unit) {}

  ~MAP_ENTRY(void) {}

  CODEREP *Cr()      const { return _value_entry.Val();}
  CODEREP *Val()     const { return _value_entry.Val();}
  CODEREP *Value()   const { return _value_entry.Val();}
  CODEREP *Key()     const { return _value_entry.Key();}
  char    *Key_str() const { return _key_str;          }
  STMTREP *Sr()      const { return _value_entry.Sr(); }
  COMP_UNIT *Cu()    const { return _value_entry.Cu(); }

  MAP_ENTRY *Clone(MEM_POOL *pool) const {
    return CXX_NEW(MAP_ENTRY(Key(), Value(), Sr(), Cu(), Key_str()), pool);
  }
  // value equal
  BOOL operator== (MAP_ENTRY &obj) const
  {
    return obj.Value() == Value() && obj.Cu() == Cu() ? TRUE : FALSE;
  }
  // TODO: key compare consider different key cr with same def
  //       need U-D traverse to check
  BOOL Same_key(MAP_ENTRY &obj) const
  {
    if(Key() == obj.Key() && Cu() == obj.Cu()) {
      return TRUE;
    }
    if(Key_str() != NULL && obj.Key_str() != NULL &&
       !strcmp(Key_str(), obj.Key_str())) {
      return TRUE;
    }
    if (Key()->Kind() == CK_CONST && obj.Key()->Kind() == CK_CONST &&
        Key()->Const_val() == obj.Key()->Const_val()) {
      return TRUE;
    }
    return FALSE;
  }
  BOOL Same_key(CODEREP *key, char *key_str, COMP_UNIT *cu) const
  {
    if(Key() == key && Cu() == cu) {
      return TRUE;
    }
    if(_key_str != NULL && key_str != NULL &&
       !strcmp(_key_str, key_str)) {
      return TRUE;
    }
    if (Key()->Kind() == CK_CONST && key->Kind() == CK_CONST &&
        Key()->Const_val() == key->Const_val()) {
      return TRUE;
    }
    return FALSE;
  }

  void Print(FILE* fp) const;
};

class LIST_ITER
{
  private:
    UINT32 _ofst;
  public:
    LIST_ITER(UINT32 ofst) : _ofst(ofst)  {};
    UINT32 Iter()              { return _ofst; }
    void Print(FILE *fp) const { fprintf(fp, "%d", _ofst); }
};

enum VALUE_TYPE {
  UNK_TYPE,
  LIST_TYPE,
  MAP_TYPE,
  FLD_TYPE,
  LIST_ITER_TYPE,
};
template <typename T> class VALUE_OBJS;
typedef VALUE_OBJS<LIST_ENTRY*> LIST_OBJS;
typedef VALUE_OBJS<MAP_ENTRY*>  MAP_OBJS;
typedef VALUE_OBJS<OBJ_ENTRY*>  AGG_ENTRY;
typedef VALUE_OBJS<AGG_ENTRY*>  AGG_OBJS;
typedef VALUE_OBJS<INTPTR>      DEF_OBJS;   // default value object, for abstraction for all types
typedef VALUE_OBJS<LIST_ITER*>  LIST_ITERS;


#define DEF_VALUE_OBJ_SIZE      6
// =============================================================================
//
// VALUE_OBJS: templatized container class to store/operate values
//             It is a vector of LIST_ENTRY* or MAP_ENTRY*
//
// =============================================================================
template <typename T>
class VALUE_OBJS {
  typedef std::vector<T, mempool_allocator<T> > T_OBJS;
  typedef typename T_OBJS::iterator T_ITER;
private:
  VALUE_TYPE   _type;                          // LIST_TYPE or MAP_TYPE
  T_OBJS       _objs;                          // vector of LIST_ENTRY or MAP_ENTRY
  MEM_POOL    *_pool;                          // pool hoding the value object

  VALUE_OBJS(void);                            // REQUIRED UNDEFINED UNWANTED methods
  VALUE_OBJS(const VALUE_OBJS&);               // REQUIRED UNDEFINED UNWANTED methods
  VALUE_OBJS& operator = (const VALUE_OBJS&);  // REQUIRED UNDEFINED UNWANTED methods
public:
  VALUE_OBJS(VALUE_TYPE type, MEM_POOL *pool, int def_size = DEF_VALUE_OBJ_SIZE) :
    _type(type), _pool(pool),
    _objs(typename T_OBJS::allocator_type(pool)) { _objs.reserve(def_size); }
  ~VALUE_OBJS() {}

  VALUE_OBJS<T> *Clone(MEM_POOL *pool) {
    VALUE_OBJS<T> *new_value = CXX_NEW(VALUE_OBJS<T>(_type, pool), pool);
    new_value->Value_objs()->assign(Value_objs()->begin(), Value_objs()->end());
    return new_value;
  }

  void Copy(VALUE_OBJS<T> *objs) {
    Is_True(_type == objs->Type(), ("mismatched value object type"));
    Is_True(objs, ("null objs"));
    Value_objs()->assign(objs->Value_objs()->begin(), objs->Value_objs()->end());
  }

  MEM_POOL  *Mem_pool()             { return _pool;                                  }
  VALUE_TYPE Type()          const  { return _type;                                  }
  void       Set_type(VALUE_TYPE t) { Is_True(_type == t || _type == UNK_TYPE,
                                              ("Set_type from %d to %d", _type, t));
                                      _type = t;                                     }
  UINT32     Size()          const  { return _objs.size();                           }
  void       Clear()                { _objs.clear();                                 }
  void       Push_back(const T &obj){ _objs.push_back(obj);                          }
  T_ITER     End()                  { return _objs.end();                            }
  void       Erase(int i)           { if(Chk_idx(i)) _objs.erase(_objs.begin() + i); }
  BOOL       Empty()         const  { return _objs.empty();                          }
  T          Back()          const  { return _objs.back();                           }
  T          Front()         const  { return _objs.front();                          }
  T_OBJS    *Value_objs()           { return &_objs;                             }
  CODEREP   *Value_cr(int i)  const { return Chk_idx(i) ? _objs[i]->Val() : NULL;}
  STMTREP   *Value_sr(int i)  const { return Chk_idx(i) ? _objs[i]->Sr() : NULL; }
  COMP_UNIT *Value_cu(int i)  const { return Chk_idx(i) ? _objs[i]->Cu() : NULL; }
  T          Value_obj(int i)       { return Chk_idx(i) ? _objs[i] : (T) NULL;   }
  BOOL       Chk_idx(int i)   const {
    Is_True_Ret(i >= 0 && i < Size(), 
                ("VALUE_OJBS idx outof bound"), FALSE);
    return TRUE;
  }

  void       Insert(UINT32 ofst, INT64 cnt, const T &obj)
  {
    T_ITER iter = _objs.begin();
    iter += ofst;
    _objs.insert(iter, cnt, obj);
  }

  void       Push_uniq(const T &obj)
  {
    for (int i = 0; i < Size(); i++) {
      if (*obj == *(_objs[i])) {
        return;
      }
    }
    Push_back(obj);
  }

  void      Print(FILE *fp) const
  {
    switch(_type) {
      case UNK_TYPE:
        Is_True(Size() == 0, ("invalid UNK_TYPE"));
        break;
      case LIST_TYPE:
        ((LIST_OBJS *)this)->Print_objs(fp);
        break;
      case MAP_TYPE:
        ((MAP_OBJS *)this)->Print_objs(fp);
        break;
      case FLD_TYPE:
        ((AGG_OBJS *)this)->Print_objs(fp);
        break;
      case LIST_ITER_TYPE:
        ((LIST_ITERS *)this)->Print_objs(fp);
        break;
      default:
        Is_True(FALSE, ("unknown VALUE_TYPE"));
        break;
    }
  }

  void      Print_objs(FILE *fp) const
  {
    if (!Empty()) {
      fprintf(fp, "val:{");
    }
    for (int i = 0; i < Size(); i++) {
      T obj = _objs[i];
      obj->Print(fp);
      if (i < Size() - 1) {
        fprintf(fp, ",");
      }
    }
    if (!Empty()) {
      fprintf(fp, "}");
    }
  }
};

enum VAR_DEF_KIND
{
  FOR_GENERAL,             // for general var def
  FOR_FLD_NAME,            // for find fld name
  FOR_FLD_CLASS,           // for find fld class
  FOR_CONTAINER_EVAL,      // for collection evaluation
};
// =============================================================================
//
// VAR_DEF_HELPER: helper class for VAR_DEF_TRAV
//                 manage the input/output for VAR_DEF_TRAV
//
// =============================================================================
class VAR_DEF_HELPER
{
private:
  CXX_MEM_POOL   _pool;
  UINT32         _kind;
  BOOL           _srcpos_on;
  BOOL           _def_srcpos_cand_on;
  BOOL           _follow_ctor_ud;   // some implicit assign bind to constructor, follow the rule or not
  TRAV_CONTEXT  *_ctx;
  SRCPOS_HANDLE *_srcpos_h;
  CODEREP       *_local_def;   // stores cr's local definition
  DEF_INFO_VEC  *_def_info_vec;

public:
  VAR_DEF_HELPER(CODEREP *cr, STMTREP *sr, COMP_UNIT *cu,
                 UINT32 kind = FOR_GENERAL, BOOL tracing = TRUE, SRCPOS_HANDLE *sp_h = NULL);
  ~VAR_DEF_HELPER();

  UINT32         Kind()                      { return _kind;        }
  BOOL           Srcpos_on()                 { return _srcpos_on;   }
  void           Set_srcpos_on(BOOL on)      { _srcpos_on = on;     }
  BOOL           Def_srcpos_cand_on()        { return _def_srcpos_cand_on; }
  void           Set_def_srcpos_cand_on(BOOL on) { _def_srcpos_cand_on = on;
                                                   if (on) Set_srcpos_on(on); }
  TRAV_CONTEXT  &Ctx()                       { return *_ctx;        }
  MEM_POOL      *Mem_pool()                  { return _pool();      }
  CODEREP       *Local_def()                 { return _local_def;   }
  SRCPOS_HANDLE *Srcpos()                    { return _srcpos_h;    }
  BOOL           Follow_ctr_ud()             { return _follow_ctor_ud; }
  void           Set_follow_ctr_ud(BOOL on)  { _follow_ctor_ud = on; }
  DEF_INFO_VEC  &Def_info_vec()              { return *_def_info_vec; }

  void           Set_local_def(CODEREP *def)
  {
    _local_def  = (_local_def ? _local_def : def);
  }

  void           Add_def_info(DNA_NODE *dna, CODEREP *cr, STMTREP *sr)
  {
    DEF_INFO *info = CXX_NEW(DEF_INFO(dna, cr, sr), Mem_pool());
    _def_info_vec->push_back(info);

    if (Def_srcpos_cand_on()) {
      Is_True_Ret(Srcpos(), ("null srcpos"));
      info->Set_spos(Srcpos()->Cur_node(), Srcpos()->Cur_idx());
    }
  }
};  // end of class VAR_DEF_HELPER

#endif
