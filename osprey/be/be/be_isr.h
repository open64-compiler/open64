#ifndef ISR_INCLUDED
#define ISR_INCLUDED

#include <vector>
#include "cxx_memory.h"
#include "targ_isa_registers.h"
#include "register.h"
typedef UINT32 REGISTER_SET; 
typedef vector<mINT32> NODE_VECT;

#define INVALID_INDEX -1

class ISR_NODE
{
  friend class ISR_PARENT_ITER;
    mINT32 _id;
    NODE_VECT _parents; // Caller list of this PU 
    BOOL _processed; //  Has backend processed this PU 

    // _regset contains caller-saved registers saved by this PU and its 
    // predecents. It is initialized to empty. Before backend processing
    // a node, it is set to the intersection of its parents' regset. 
    // If one parent is not processed(it means there's a recursive call),
    // we just skip this parent(?)
    REGISTER_SET _regset[ISA_REGISTER_CLASS_MAX+1];

  public:
    ISR_NODE() 
        :_id(INVALID_INDEX), _processed(FALSE) {} 

    BOOL Processed(void) { return _processed; }
    void Set_Processed(void) { _processed = TRUE; }

    mINT32 Idx(void) { return _id; }
    void Set_Idx(mINT32 id) { _id = id; }

    void Add_Parent(mINT32 par) {
        Is_True(par != INVALID_INDEX, ("Invalid ISR call graph parent id: %d", par));
        _parents.push_back(par);
    }

    REGISTER_SET Regset(ISA_REGISTER_CLASS rc) 
        {return _regset[rc]; }

    void Set_Regset(ISA_REGISTER_CLASS rc, REGISTER_SET set)
        { _regset[rc] = set; }
    
    void Add_Reg(ISA_REGISTER_CLASS rc, mREGISTER reg) 
        { _regset[rc] = REGISTER_SET_Union1(_regset[rc], reg); }

    void Regset_Intersect(ISA_REGISTER_CLASS rc, REGISTER_SET set)
        { _regset[rc] = REGISTER_SET_Intersection(_regset[rc], set); }

    void Regset_Union(ISA_REGISTER_CLASS rc, REGISTER_SET set)
        { _regset[rc] = REGISTER_SET_Union(_regset[rc], set); }

    BOOL Regset_Member(ISA_REGISTER_CLASS rc, mREGISTER reg)
        { return REGISTER_SET_MemberP(_regset[rc], reg); }
    
};

class ISR_PARENT_ITER
{
    NODE_VECT::iterator _iter, _end; 

  public:
    ISR_PARENT_ITER(ISR_NODE& v) 
        : _iter(v._parents.begin()), _end(v._parents.end()) {}

    BOOL End() { return _iter == _end; }
    void Next() { _iter++; }
    mINT32 Idx() { return *_iter; }
};



extern class ISR_NODE* isr_cg;

extern INT Read_isr_cg(ISR_NODE* cg, INT pu_num);

extern void Merge_Parents_Regset(ISR_NODE& node);

#endif // ISR_INCLUDED
