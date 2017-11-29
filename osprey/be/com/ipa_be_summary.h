/*
 Copyright (C) 2010, Hewlett-Packard Development Company, L.P.
 All Rights Reserved.

 Open64 is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 2
 of the License, or (at your option) any later version.

 Open64 is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 MA  02110-1301, USA.
*/
#ifndef ipa_be_summary_INCLUDED
#define ipa_be_summary_INCLUDED

class SUMMARY_CONSTRAINT_GRAPH_PU_HEADER
{
public:
  SUMMARY_CONSTRAINT_GRAPH_PU_HEADER() {}

  void Init()
  {
    BZERO(this, sizeof(SUMMARY_CONSTRAINT_GRAPH_PU_HEADER));
  }
  
  ST_IDX stIdx() { return _st_idx; }
  mUINT32 cgNodesIdx() { return _constraint_graph_nodes_idx; }
  mUINT32 cgNodesCount() { return _constraint_graph_nodes_count; }
  mUINT32 cgStInfosIdx() { return _constraint_graph_stinfos_idx; }
  mUINT32 cgStInfosCount() { return _constraint_graph_stinfos_count; }
  mUINT32 cgCallsitesIdx() { return _constraint_graph_callsites_idx; }
  mUINT32 cgCallsitesCount() { return _constraint_graph_callsites_count; }
  mUINT32 cgNodeIdsIdx() { return _constraint_graph_node_ids_idx; }
  mUINT32 cgNodeIdsCount() { return _constraint_graph_node_ids_count; }
  mUINT32 cgFormalsIdx() { return _constraint_graph_formal_parm_idx; }
  mUINT32 cgFormalsCount() { return _constraint_graph_formal_parm_count; }
  mUINT32 cgReturnsIdx() { return _constraint_graph_formal_ret_idx; }
  mUINT32 cgReturnsCount() { return _constraint_graph_formal_ret_count; }
  
  mUINT32 siloedReferenceIdx() { return _siloed_reference_idx; }
  mUINT32 siloedReferenceCount() { return _siloed_reference_count; }

  void stIdx(ST_IDX s) { _st_idx = s; }
  void cgNodesIdx(UINT32 s) {  _constraint_graph_nodes_idx = s; }
  void cgNodesCount(UINT32 s) {  _constraint_graph_nodes_count = s; }
  void cgStInfosIdx(UINT32 s) {  _constraint_graph_stinfos_idx = s; }
  void cgStInfosCount(UINT32 s) {  _constraint_graph_stinfos_count = s; }
  void cgCallsitesIdx(UINT32 s) {  _constraint_graph_callsites_idx = s; }
  void cgCallsitesCount(UINT32 s) {  _constraint_graph_callsites_count = s; }
  void cgNodeIdsIdx(UINT32 s) {  _constraint_graph_node_ids_idx = s; }
  void cgNodeIdsCount(UINT32 s) {  _constraint_graph_node_ids_count = s; }
  void cgFormalsIdx(UINT32 s) {  _constraint_graph_formal_parm_idx = s; }
  void cgFormalsCount(UINT32 s) {  _constraint_graph_formal_parm_count = s; }
  void cgReturnsIdx(UINT32 s) {  _constraint_graph_formal_ret_idx = s; }
  void cgReturnsCount(UINT32 s) {  _constraint_graph_formal_ret_count = s; }
  
  void siloedReferenceIdx(UINT32 s) {  _siloed_reference_idx = s; }
  void siloedReferenceCount(UINT32 s) {  _siloed_reference_count = s; }

private:
  ST_IDX _st_idx;
  mUINT32 _constraint_graph_nodes_idx;
  mUINT32 _constraint_graph_nodes_count;
  mUINT32 _constraint_graph_edges_idx;
  mUINT32 _constraint_graph_edges_count;
  mUINT32 _constraint_graph_stinfos_idx;
  mUINT32 _constraint_graph_stinfos_count;
  mUINT32 _constraint_graph_callsites_idx;
  mUINT32 _constraint_graph_callsites_count;
  mUINT32 _constraint_graph_node_ids_idx;
  mUINT32 _constraint_graph_node_ids_count;
  mUINT32 _constraint_graph_formal_parm_idx;
  mUINT32 _constraint_graph_formal_parm_count;
  mUINT32 _constraint_graph_formal_ret_idx;
  mUINT32 _constraint_graph_formal_ret_count;
  
  mUINT32 _siloed_reference_idx;
  mUINT32 _siloed_reference_count;
};

// Constraint graph node summary for Nystrom Alias Analyzer
class SUMMARY_CONSTRAINT_GRAPH_NODE
{
public:
  SUMMARY_CONSTRAINT_GRAPH_NODE() {}

  void Init()
  {
    BZERO(this, sizeof(SUMMARY_CONSTRAINT_GRAPH_NODE));
  }

  void cgNodeId(UINT32 cgNodeId)  { _cgNodeId = cgNodeId; }
  void cg_st_idx(UINT64 cg_st_idx){ _cg_st_idx = cg_st_idx; }
  void offset(INT32 offset)       { _offset = offset; }
  void flags(UINT32 flags)        { _flags = flags; }
  void inKCycle(UINT32 kcycle)    { _inKCycle = kcycle; }
  void nextOffset(UINT32 noffset) { _nextOffset = noffset; }
  void repParent(UINT32 rparent)  { _repParent = rparent; }
  void numBitsPtsGBL(UINT32 n)    { _numBitsPtsGBL = n; }
  void numBitsPtsHZ(UINT32 n)     { _numBitsPtsHZ = n; }
  void numBitsPtsDN(UINT32 n)     { _numBitsPtsDN = n; }
  void ptsGBLidx(UINT32 idx)      { _pointsToGBLIdx = idx; }
  void ptsHZidx(UINT32 idx)       { _pointsToHZIdx = idx; }
  void ptsDNidx(UINT32 idx)       { _pointsToDNIdx = idx; }
  void ty_idx(UINT32 idx)         { _ty_idx = idx; }
  void collapsedParent(UINT32 p)  { _collapsedParent = p; }

  UINT32 cgNodeId()      const { return _cgNodeId; }
  UINT64 cg_st_idx()     const { return _cg_st_idx; }
  INT32 offset()         const { return _offset; }
  UINT32 nextOffset()    const { return _nextOffset; }
  UINT32 flags()         const { return _flags; }
  UINT32 inKCycle()      const { return _inKCycle; }
  UINT32 repParent()     const { return _repParent; }
  UINT32 numBitsPtsGBL() const { return _numBitsPtsGBL; }
  UINT32 numBitsPtsHZ()  const { return _numBitsPtsHZ; }
  UINT32 numBitsPtsDN()  const { return _numBitsPtsDN; }
  UINT32 ptsGBLidx()     const { return _pointsToGBLIdx; } 
  UINT32 ptsHZidx()      const { return _pointsToHZIdx; } 
  UINT32 ptsDNidx()      const { return _pointsToDNIdx; } 
  UINT32 ty_idx()        const { return _ty_idx; }
  UINT32 collapsedParent() const { return _collapsedParent; }

  void Print_array(FILE *fp, INT32 size) const;
  void Trace_array(INT32 size) const ;
  void Print(FILE *f) const;
  void Trace(void) const;
  
private:
  UINT32 _cgNodeId;
  UINT64 _cg_st_idx;
  INT32  _offset;  
  UINT32 _flags;
  UINT32 _numBitsPtsGBL;   // number of bits in GBL pts-to-set
  UINT32 _numBitsPtsHZ;    // number of bits in HZ pts-to-set
  UINT32 _numBitsPtsDN;    // number of bits in in DN pts-to-set
  UINT32 _pointsToGBLIdx;  // index of the first GBL CGNodeId in ids array
  UINT32 _pointsToHZIdx;   // index of the first HZ CGNodeId in ids array
  UINT32 _pointsToDNIdx;   // index of the first DN CGNodeId in ids array
  UINT32 _inKCycle;
  UINT32 _nextOffset;
  UINT32 _repParent;
  UINT32 _collapsedParent;
  TY_IDX _ty_idx;
};

// Constraint graph edge summary for Nystrom Alias Analyzer
class SUMMARY_CONSTRAINT_GRAPH_EDGE
{
public:
  SUMMARY_CONSTRAINT_GRAPH_EDGE() {}

  void etype(UINT16 et)    { _etype = et; }
  void qual(UINT16 q)      { _qual = q; }
  void flags(UINT16 f)     { _flags = f; }
  void sizeOrSkew(INT32 s) { _sizeOrSkew = s; }
  void src(UINT32 id)      { _srcId = id; }
  void dest(UINT32 id)     { _destId = id; }

  UINT16 etype()      const { return _etype; }
  UINT16 qual()       const { return _qual; }
  UINT16 flags()      const { return _flags; }
  INT32 sizeOrSkew()  const { return _sizeOrSkew; }
  UINT32 src()        const { return _srcId; }
  UINT32 dest()       const { return _destId; }

  void Init()
  {
    BZERO(this, sizeof(SUMMARY_CONSTRAINT_GRAPH_EDGE));
  }

  void Print_array(FILE *fp, INT32 size) const;
  void Trace_array(INT32 size) const ;
  void Print(FILE *f) const;
  void Trace(void) const;

private:
  UINT16 _etype;
  UINT16 _qual;
  UINT16 _flags;
  INT32  _sizeOrSkew;
  UINT32 _srcId;
  UINT32 _destId;
};

// Constraint graph StInfo summary for Nystrom Alias Analyzer
class SUMMARY_CONSTRAINT_GRAPH_STINFO
{
public:
  SUMMARY_CONSTRAINT_GRAPH_STINFO() {}

  UINT64 cg_st_idx()   const { return _cg_st_idx; }
  UINT32 flags()       const { return _flags; }
  INT64 varSize()      const { return _varSize; }
  UINT32 modulus()     const { return _modulus; }
  UINT32 firstOffset() const { return _firstOffset; }
  TY_IDX ty_idx()      const { return _ty_idx; }

  void cg_st_idx(UINT64 s)   { _cg_st_idx = s; }
  void flags(UINT32 f)       { _flags = f; }
  void varSize(INT64 s)      { _varSize = s; }
  void modulus(UINT32 m)     { _modulus = m; }
  void firstOffset(UINT32 o) { _firstOffset = o; }
  void ty_idx(TY_IDX t)      { _ty_idx = t; }

  void Init()
  {
    BZERO(this, sizeof(SUMMARY_CONSTRAINT_GRAPH_STINFO));
  }

  void Print_array(FILE *fp, INT32 size) const;
  void Trace_array(INT32 size) const ;
  void Print(FILE *f) const;
  void Trace(void) const;

private:
  UINT64 _cg_st_idx;
  UINT32 _flags;
  INT64  _varSize;
  UINT32 _modulus;
  UINT32 _firstOffset; 
  TY_IDX _ty_idx;
};

// Constraint graph CallSite summary for Nystrom Alias Analyzer
class SUMMARY_CONSTRAINT_GRAPH_CALLSITE
{
public:
  SUMMARY_CONSTRAINT_GRAPH_CALLSITE() {}

  UINT32 id()           const { return _id; }
  UINT8 flags()         const { return _flags; }
  UINT32 actualModeled()const { return _actualModeled; }
  TY_IDX virtualClass() const { return _virtualClass; }
  ST_IDX st_idx()       const { return _callInfo._st_idx; }
  UINT32 cgNodeId()     const { return _callInfo._cgNodeId; }
  INTRINSIC intrinsic() const { return _callInfo._intrinsic; }
  UINT32 parmNodeIdx()  const { return _paramNodesIdx; }
  UINT32 numParms()     const { return _numParms; }
  UINT32 returnId()     const { return _return; }

  void id(UINT32 i)           { _id = i; }
  void flags(UINT8 f)         { _flags = f; }
  void actualModeled(UINT32 m){ _actualModeled = m; }
  void virtualClass(TY_IDX i) { _virtualClass = i; }
  void st_idx(ST_IDX s)       { _callInfo._st_idx = s; }
  void cgNodeId(UINT32 c)     { _callInfo._cgNodeId = c; }
  void intrinsic(INTRINSIC i) { _callInfo._intrinsic = i; }
  void numParms(UINT32 n)     { _numParms = n; }
  void parmNodeIdx(UINT32 i)  { _paramNodesIdx = i; }
  void returnId(UINT32 i)     { _return  = i; }

  void Init()
  {
    BZERO(this, sizeof(SUMMARY_CONSTRAINT_GRAPH_CALLSITE));
  }

  void Print_array(FILE *fp, INT32 size) const;
  void Trace_array(INT32 size) const ;
  void Print(FILE *f) const;
  void Trace(void) const;

private:
  UINT32 _id;
  UINT8  _flags;
  UINT32 _actualModeled;
  TY_IDX _virtualClass;
  union {
    ST_IDX _st_idx;
    UINT32 _cgNodeId;
    INTRINSIC _intrinsic;
  } _callInfo;
  UINT32 _numParms;        // Number of param nodes: We use the ids array
                           // to store the param nodes
  UINT32 _paramNodesIdx;   // index of the first param CGNodeId in ids array
  UINT32 _return;
  UINT32 _numBitsPtsMod;   // Number of bits in points-to-set of mod set
  UINT32 _numBitsPtsRef;   // Number of bits in points-to-set of ref set
  UINT32 _pointsToModIdx;  // index of the first mod CGNodeId in ids array
  UINT32 _pointsToRefIdx;  // index of the first ref CGNodeId in ids array
};

// Constraint graph CallSite summary for Nystrom Alias Analyzer
class SUMMARY_CONSTRAINT_GRAPH_MODRANGE
{
public:
  SUMMARY_CONSTRAINT_GRAPH_MODRANGE() {}

  void startOffset(UINT32 o) { _startOffset = o; }
  void endOffset(UINT32 o)   { _endOffset = o; }
  void modulus(UINT32 m)     { _modulus = m; }
  void childIdx(UINT32 i)    { _childIdx = i; }
  void nextIdx(UINT32 i)     { _nextIdx = i; }
  void ty_idx(UINT32 i)      { _ty_idx = i; }

  UINT32 startOffset() const { return _startOffset; }
  UINT32 endOffset()   const { return _endOffset; }
  UINT32 modulus()     const { return _modulus; }
  UINT32 childIdx()    const { return _childIdx; }
  UINT32 nextIdx()     const { return _nextIdx; }
  UINT32 ty_idx()      const { return _ty_idx; }

  void Init()
  {
    BZERO(this, sizeof(SUMMARY_CONSTRAINT_GRAPH_MODRANGE));
  }

  void Print_array(FILE *fp, INT32 size) const;
  void Trace_array(INT32 size) const ;
  void Print(FILE *f) const;
  void Trace(void) const;

private:
  UINT32 _startOffset;
  UINT32 _endOffset;
  UINT32 _modulus;
  UINT32 _childIdx;
  UINT32 _nextIdx;
  TY_IDX _ty_idx;
};

class SUMMARY_SILOED_REFERENCE
{
public:
	SUMMARY_SILOED_REFERENCE() {}

	void aliasTag(UINT32 tag)	{ _aliasTag = tag; }
	UINT32 aliasTag() const		{ return _aliasTag; }

	void Init()
	{
		BZERO(this, sizeof(SUMMARY_SILOED_REFERENCE));
	}

	// TODO unimplmented yet
	void Print_array(FILE *fp, INT32 size) const;
	void Trace_array(INT32 size) const ;
	void Print(FILE *f) const;
	void Trace(void) const;

private:
	UINT32 _aliasTag;
};

#endif // ipa_be_summary_INCLUDED
