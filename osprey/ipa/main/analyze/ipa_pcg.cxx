// ====================================================================
//
// Copyright (C) 2011, Hewlett-Packard Development Company, L.P.
// All Rights Reserved.
//
// Open64 is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// Open64 is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
// MA  02110-1301, USA.
//
// ====================================================================

#include "ipo_defs.h"                   // IPO_NODE_CONTEXT
#include "wssa_utils.h"
#include "wssa_mgr.h"
#include "ir_bread.h"					// Read_Local_Info
#include "wn_tree_util.h"				// WN_TREE_ITER
#include "wssa_update.h"
#include "cfg_util.h"                	// DOM_BUILDER
#include "region_main.h"                // REGION_Initialize
#include "ipa_nystrom_alias_analyzer.h"
#include "nystrom_alias_analyzer.h"		// NystromAliasAnalyzer
#include "lwn_util.h"					// LWN_Get_Parent
#include "ipa_pcg.h"

using namespace CFG_UTIL;

// the base class of PCG_NODE, containing a bit-vector flag of
// PCG_NODE kind and a visit flag
template<typename FLAG_TYPE>
class PCG_NODE_BASE {
protected:
	unsigned _flags, _visit_flags;
	typedef std::set<PCG_NODE_BASE<FLAG_TYPE> *> PCG_NODE_SET;
	PCG_NODE_SET _prev_list, _succ_list;

	PCG_NODE_BASE(): _flags(0), _visit_flags(0) {}

	void Propagate(FLAG_TYPE mask, bool up, bool updated);

	void Set_flag(FLAG_TYPE mask)				{ _flags |= mask; }
	void Clear_flag(FLAG_TYPE mask)				{ _flags &= ~(unsigned)mask; }
	bool Is_flag_set(FLAG_TYPE mask) const		{ return (_flags & mask) != 0; }
	bool Is_flag_all_set(FLAG_TYPE mask) const	{ return (_flags & mask) == mask; }
	void Set_visited(FLAG_TYPE mask)			{ _visit_flags |= mask; }
	void Clear_visited(FLAG_TYPE mask)			{ _visit_flags &= ~(unsigned)mask; }
	bool Is_all_visited(FLAG_TYPE mask) const	{ return (_visit_flags & mask) == mask; }
public:
	void Propagate_up(FLAG_TYPE mask)			{ Propagate(mask, true, false); }
	void Propagate_down(FLAG_TYPE mask)			{ Propagate(mask, false, false); }

	void Add_prev(PCG_NODE_BASE<FLAG_TYPE> * prev)	{ _prev_list.insert(prev); }
	void Add_succ(PCG_NODE_BASE<FLAG_TYPE> * succ)	{ _succ_list.insert(succ); }
};

// propagate the flags with mask of current node up or down
template<typename FLAG_TYPE>
void
PCG_NODE_BASE<FLAG_TYPE>::Propagate(FLAG_TYPE mask, bool up, bool updated) {
	// A naive propagate algorithm at present.
	// A better approach should be some SCC-based algorithm
	if(!Is_all_visited(mask) || updated) {
		Set_visited(mask);
		if(Is_flag_set(mask)) {
			std::set<PCG_NODE_BASE<FLAG_TYPE> *> &edge_list =
					up ? _prev_list : _succ_list;
			for(typename PCG_NODE_SET::iterator it = edge_list.begin();
					it != edge_list.end(); it++) {
				PCG_NODE_BASE<FLAG_TYPE> *end_point = *it;
//				if(!end_point->Is_flag_all_set(mask))
//					end_point->Clear_visited(mask);
				bool update = !end_point->Is_flag_all_set(mask);
				end_point->Set_flag((FLAG_TYPE)(_flags & mask));
				end_point->Propagate(mask, up, update);
			}
		}
	}
}

typedef std::vector<WN *> WN_VEC;
typedef std::set<WN *> WN_SET;
typedef std::pair<WN *, AliasTag> WN_TAG_PAIR;
typedef std::set<WN_TAG_PAIR> WN_TAG_PAIR_SET;
typedef std::map<WN *, AliasTag> WN_TAG_MAP;
typedef std::map<AliasTag, WN *> TAG_WN_MAP;

static void Print_aliasTag_set(const ALIAS_TAG_SET &tag_set, FILE *fout = stderr)
{
	fprintf(fout, "\nCount: %d\n", tag_set.size());
	for(ALIAS_TAG_SET::const_iterator cit = tag_set.begin();
			cit != tag_set.end(); cit++) {
		AliasTag tag = *cit;
		fprintf(fout, "%d\n", tag);
	}
}

// PU representation for in PCG
class PCG_NODE : public PCG_NODE_BASE<PCG_FLAG> {
private:
	MEM_POOL *_pool;
	IPA_NODE *_node;
	std::set<ST *> _icall_list;

	// indirect memory accesses: iload and istore
	// prefix of "_follow_" means the WN nodes may be executed after
	// a thread-spawn site or an invocation to a SPAWNER PU
	WN_TAG_MAP *_follow_load_set, *_follow_store_set, *_load_set, *_store_set;

	// call sites
	WN_SET *_call_set, *_follow_call_set;

	// ldids: map from alias tag to WN nodes
	TAG_WN_MAP _ldid_set, _follow_ldid_set;

	// alias tags of loads or stores that may happen concurrently
	ALIAS_TAG_SET _concurrent_load, _concurrent_store;

	// alias tags of the siloed reference in this PU
	ALIAS_TAG_SET _siloed_ref_set;

	friend class IPA_PCG;
public:
	PCG_NODE(IPA_NODE *ipa_node, MEM_POOL *mem_pool)
		: _node(ipa_node), _pool(mem_pool) {
		_load_set = CXX_NEW(WN_TAG_MAP(), _pool);
		_store_set = CXX_NEW(WN_TAG_MAP(), _pool);
		_follow_load_set = CXX_NEW(WN_TAG_MAP(), _pool);
		_follow_store_set = CXX_NEW(WN_TAG_MAP(), _pool);
		_call_set = CXX_NEW(WN_SET(), _pool);
		_follow_call_set = CXX_NEW(WN_SET(), _pool);
	}

	~PCG_NODE() {
		if(_load_set != NULL)
			CXX_DELETE(_load_set, _pool);
		if(_store_set != NULL)
			CXX_DELETE(_store_set, _pool);
		if(_follow_load_set != NULL)
			CXX_DELETE(_follow_load_set, _pool);
		if(_follow_store_set != NULL)
			CXX_DELETE(_follow_store_set, _pool);
		if(_call_set != NULL)
			CXX_DELETE(_call_set, _pool);
		if(_follow_call_set != NULL)
			CXX_DELETE(_follow_call_set, _pool);
	}

	IPA_NODE *Get_ipa_node() const	{ return _node; }
	const char* Get_name() const	{ return IPA_Node_Name(_node); }

	void Add_caller(PCG_NODE *caller) {
		_prev_list.insert(caller);
	}

	void Add_callee(PCG_NODE *callee) {
		_succ_list.insert(callee);
	}

	std::set<ST *> 			&Get_icall_list()		{ return _icall_list; }
	const std::set<ST *>	&Get_icall_list() const	{ return _icall_list; }

	void Set_entry() 		{ Set_flag(PCG_ENTRY); }
	bool Is_entry() const 	{ return Is_flag_set(PCG_ENTRY); }
	void Set_spawner() 		{ Set_flag(PCG_SPAWNER); }
	bool Is_spawner() const { return Is_flag_set(PCG_SPAWNER); }
	void Set_start() 		{ Set_flag(PCG_START); }
	bool Is_start() const 	{ return Is_flag_set(PCG_START); }
	void Set_spawnee() 		{ Set_flag(PCG_SPAWNEE); }
	bool Is_spawnee() const	{ return Is_flag_set(PCG_SPAWNEE); }
	void Set_follow() 		{ Set_flag(PCG_FOLLOW); }
	bool Is_follow() const	{ return Is_flag_set(PCG_FOLLOW); }

	// only PCG_NODE of SPAWNEE FOLLOW can happen concurrently
	bool Is_concurrent() const	{ return Is_spawnee() || Is_follow(); }

	void add_call(WN *wn, bool follow) {
		(follow?_follow_call_set:_call_set)->insert(wn);
	}
	void add_load(WN *stmt, AliasTag tag, bool follow) {
		(*(follow?_follow_load_set:_load_set))[stmt] = tag;
	}
	void add_store(WN *stmt, AliasTag tag, bool follow) {
		if(tag == InvalidAliasTag) {
            // TODO Store with InvalidAliasTag?
			return;
		}
		(*(follow?_follow_store_set : _store_set))[stmt] = tag;
	}

	void add_ldid(WN *wn, AliasTag tag, bool follow) {
		if(follow)
			_follow_ldid_set[tag] = wn;
		else
			_ldid_set[tag] = wn;
	}

	// To save memory space, after PCG is built
	// release the memory space of _follow_xxx_set for concurrent PCG_NODE
	// release the memory space of _xxx_set for none-concurrent PCG_NODE
	void Tidy_load_store_set() {
		if(Is_concurrent()) {
			CXX_DELETE(_follow_load_set, _pool);
			_follow_load_set = NULL;
			CXX_DELETE(_follow_store_set, _pool);
			_follow_store_set = NULL;
		} else {
			CXX_DELETE(_load_set, _pool);
			_load_set = NULL;
			CXX_DELETE(_store_set, _pool);
			_store_set = NULL;
		}
	}

	const WN_SET *Get_concurrent_call_set() const {
		return Is_concurrent() ? _call_set : _follow_call_set;
	}

	const WN_TAG_MAP *Get_concurrent_load_set() const {
		return Is_concurrent() ? _load_set : _follow_load_set;
	}

	const WN_TAG_MAP *Get_concurrent_store_set() const {
		return Is_concurrent() ? _store_set : _follow_store_set;
	}

	void Print(FILE *fout = stderr);
};

void
PCG_NODE::Print(FILE *fout)
{
	fprintf(fout, "[%s]: ", Get_name());
	if(Is_entry())
		fprintf(fout, "ENTRY ");
	if(Is_spawner())
		fprintf(fout, "SPAWNER ");
	if(Is_start())
		fprintf(fout, "START ");
	if(Is_spawnee())
		fprintf(fout, "SPAWNEE ");
	if(Is_follow())
		fprintf(fout, "FOLLOW ");
	fprintf(fout, "\n");

	fprintf(fout, "\tCallers: ");
	for(PCG_NODE_SET::iterator it = _prev_list.begin();
			it != _prev_list.end(); it++) {
		PCG_NODE *end_point = (PCG_NODE *)*it;
		fprintf(fout, "%s ", end_point->Get_name());
	}
	fprintf(fout, "\n");

	fprintf(fout, "\tCallees: ");
	for(PCG_NODE_SET::iterator it = _succ_list.begin();
			it != _succ_list.end(); it++) {
		PCG_NODE *end_point = (PCG_NODE *)*it;
		fprintf(fout, "%s ", end_point->Get_name());
	}
	fprintf(fout, "\n");
}

// BB representation in PCG_NODE
// used to compute "follow" set of statements in a PU
class PCG_BB_NODE : public PCG_NODE_BASE<char> {
private:
	const WN_CFG::BB_NODE *bb_node;
public:
	PCG_BB_NODE(const WN_CFG::BB_NODE *bb_node): bb_node(bb_node) {}
	const WN_CFG::BB_NODE *get_bb_node() const	{ return bb_node; }

	void Set_flag()				{ PCG_NODE_BASE<char>::Set_flag(1); }
	void Clear_flag()			{ PCG_NODE_BASE<char>::Clear_flag(1); }
	bool Is_flag_set() const	{ return PCG_NODE_BASE<char>::Is_flag_set(1); }

	void Propagate_down()		{ PCG_NODE_BASE<char>::Propagate(1, false, false); }
};

typedef std::map<const WN_CFG::BB_NODE *, PCG_BB_NODE *> BB_TO_PCG_BB_MAP;

// CFG representation in PCG
// used to compute "follow" set of statements in a PU
class PCG_CFG {
public:
	class ITERATOR {
	private:
		BB_TO_PCG_BB_MAP::iterator it;
		friend class PCG_CFG;

		ITERATOR(const BB_TO_PCG_BB_MAP::iterator &it): it(it) {}
	public:
		ITERATOR(const ITERATOR &pcg_cfg_it): it(pcg_cfg_it.it) {}

		PCG_BB_NODE *operator *() {
			return it->second;
		}

		ITERATOR &operator++() {
			it++;
			return *this;
		}

		bool operator!=(const ITERATOR &pcg_cfg_it) {
			return it != pcg_cfg_it.it;
		}
	};

private:
	MEM_POOL *_pool;
	BB_TO_PCG_BB_MAP _bb_to_pcg_bb_map;
	friend class ITERATOR;

public:
	PCG_CFG(WN_CFG &wn_cfg, MEM_POOL *mem_pool);
	PCG_BB_NODE *get_pcg_bb_node(const WN_CFG::BB_NODE *bb_node)	{
		BB_TO_PCG_BB_MAP::iterator it = _bb_to_pcg_bb_map.find(bb_node);
		if(it != _bb_to_pcg_bb_map.end())
			return it->second;
		return NULL;
	}

	ITERATOR begin() {
		BB_TO_PCG_BB_MAP::iterator it = _bb_to_pcg_bb_map.begin();
		return ITERATOR(it);
	}
	ITERATOR end() {
		BB_TO_PCG_BB_MAP::iterator it = _bb_to_pcg_bb_map.end();
		return ITERATOR(it);
	}
};

PCG_CFG::PCG_CFG(WN_CFG &wn_cfg, MEM_POOL *mem_pool) : _pool(mem_pool)
{
	for (WN_CFG::bb_iterator bb_it = wn_cfg.BB_begin();
			bb_it != wn_cfg.BB_end(); ++bb_it) {
		const WN_CFG::BB_NODE *bb = *bb_it;
		PCG_BB_NODE *pcg_bb_node = CXX_NEW(PCG_BB_NODE(bb), _pool);
		_bb_to_pcg_bb_map[bb] = pcg_bb_node;
	}

	for(BB_TO_PCG_BB_MAP::iterator it = _bb_to_pcg_bb_map.begin();
			it != _bb_to_pcg_bb_map.end(); it++) {
		PCG_BB_NODE *pcg_bb_node = it->second;
		const WN_CFG::BB_NODE *bb = pcg_bb_node->get_bb_node();

		for(int i = 0; i < bb->Get_preds_count(); i++) {
			const WN_CFG::BB_NODE *pred = bb->Get_pred(i);
			Is_True(_bb_to_pcg_bb_map.find(pred) != _bb_to_pcg_bb_map.end(),
					("WN_CFG::BB_NODE has no corresponding PCG_BB_NODE!"));
			PCG_BB_NODE *pcg_pred = _bb_to_pcg_bb_map[pred];
			pcg_bb_node->Add_prev(pcg_pred);
		}

		for(int i = 0; i < bb->Get_succs_count(); i++) {
			const WN_CFG::BB_NODE *succ = bb->Get_succ(i);
			Is_True(_bb_to_pcg_bb_map.find(succ) != _bb_to_pcg_bb_map.end(),
					("WN_CFG::BB_NODE has no corresponding PCG_BB_NODE!"));
			PCG_BB_NODE *pcg_succ = _bb_to_pcg_bb_map[succ];
			pcg_bb_node->Add_succ(pcg_succ);
		}
	}
}

// currently hard-coded to search for function ST with the name "main"
static bool is_main(ST *func_st)
{
	const char *func_name = ST_name(func_st);
	if(func_name != NULL)
		return strcmp(func_name, "main") == 0;
	return false;
}

// currently hard-coded to search for function ST with the name "pthread_create"
static bool is_spawn_api(ST *func_st)
{
	const char *func_name = ST_name(func_st);
	if(func_name != NULL)
		return strcmp(func_name, "pthread_create") == 0;
	return false;
}

// currently hard-coded to search for function ST with the name "pthread_mutex_lock"
static bool is_lock_api(ST *func_st)
{
	const char *func_name = ST_name(func_st);
	if(func_name != NULL)
		return strcmp(func_name, "pthread_mutex_lock") == 0;
	return false;
}

// currently hard-coded to search for function ST with the name "pthread_mutex_unlock"
static bool is_unlock_api(ST *func_st)
{
	const char *func_name = ST_name(func_st);
	if(func_name != NULL)
		return strcmp(func_name, "pthread_mutex_unlock") == 0;
	return false;
}

void
IPA_PCG::Find_use_in_tree(PCG_NODE *pcg_node, WN *stmt, WN *tree, bool follow)
{
	AliasAnalyzer *aa = AliasAnalyzer::aliasAnalyzer();
	if(WN_operator(tree) == OPR_LDID) {
		ST *st = WN_st(tree);
		WN_OFFSET offset = WN_load_offset(tree);
		INT32 size = WN_object_size(tree);
		AliasTag tag = aa->genAliasTag(st, offset, size, false);
		pcg_node->add_ldid(tree, tag, follow);
	}

	for(int i = 0; i < WN_kid_count(tree); i++) {
		Find_use_in_tree(pcg_node, stmt, WN_kid(tree, i), follow);
	}
}

void
IPA_PCG::Proccess_stmt(PCG_NODE *pcg_node, WN *stmt, bool follow)
{
	AliasAnalyzer *aa = AliasAnalyzer::aliasAnalyzer();

	Find_use_in_tree(pcg_node, stmt, stmt, follow);

	OPERATOR op = WN_operator(stmt);
	if(OPERATOR_is_call(op)) {
		pcg_node->add_call(stmt, follow);
		switch(op) {
		case OPR_CALL: {
			ST *callee_st = WN_st(stmt);
			if(is_spawn_api(callee_st)) {
				// empty
			} else if(is_lock_api(callee_st) || is_unlock_api(callee_st)) {
				// empty
			} else {
				ST_TO_PCG_NODE_VEC_MAP::iterator it =
						st_to_node_vec_map.find(callee_st);
				if(it == st_to_node_vec_map.end()) {

					const char *func_name = ST_name(callee_st);
					// TODO we find an opaque function:
					// A rounded solution should be a table of known external
					// functions with known sides effects; otherwise we should
					// assume that an opaque function of every exposed memory
					// locations, such as non-static global variables, escaping
					// local variables and the possible destination of any pointer-
					// arguments.
					// I did not add handling for opaque function here for now,
					// because I don't have such external functions table, and
					// if I just treat every opaque function in a general approach,
					// I wouldn't get any opportunity for siloed reference analysis.

					// fprintf(stderr, "opaque function %s\n", func_name);
				} else {
					if(follow) {
						PCG_NODE_VEC &node_vec = it->second;
						for(int i = 0; i < node_vec.size(); i++) {
							node_vec[i]->Set_follow();
						}
					}
				}
			}
		}
		break;
		case OPR_ICALL:
		case OPR_INTRINSIC_CALL:
			// TODO should be handled similarly with opaque functions
		break;
		default:
			Is_True(FALSE, ("Unhandled call OPERATOR"));
		break;
		}
	}
	else if(OPERATOR_is_load(op)) {
		switch(op) {
		case OPR_PARM:
		case OPR_MLOAD:
		case OPR_LDBITS:
		case OPR_LDID: {
			ST *st = WN_st(stmt);
			if(st->export_class != EXPORT_LOCAL) {
				WN_OFFSET offset = WN_load_offset(stmt);
				INT32 size = WN_object_size(stmt);
				AliasTag tag = aa->genAliasTag(st, offset, size, false);
				pcg_node->add_load(stmt, tag, follow);
			}
		}
		break;
		case OPR_ILOADX:
		case OPR_ILDBITS:
		case OPR_ILOAD: {
			AliasTag tag = aa->getAliasTag(stmt, Current_IPANode_File_PU_Idx);
			pcg_node->add_load(stmt, tag, follow);
		}
		break;
		default:
			Is_True(FALSE, ("Unhandled load OPERATOR"));
		break;
		}
	} else if(OPERATOR_is_store(op)) {
		switch(op) {
		case OPR_MSTORE:
		case OPR_STBITS:
		case OPR_STID:
		{
			ST *st = WN_st(stmt);
			if(st->export_class != EXPORT_LOCAL) {
				WN_OFFSET offset = WN_store_offset(stmt);
				INT32 size = WN_object_size(stmt);
				AliasTag tag = aa->genAliasTag(st, offset, size, false);
				pcg_node->add_store(stmt, tag, follow);
			}
		}
		break;
		case OPR_ISTOREX:
		case OPR_ISTBITS:
		case OPR_ISTORE:
		{
			AliasTag tag = aa->getAliasTag(stmt, Current_IPANode_File_PU_Idx);
			pcg_node->add_store(stmt, tag, follow);
		}
		break;
		default:
			Is_True(FALSE, ("Unhandled store OPERATOR"));
		break;
		}
	}
}

// Traverse the PU in a flow sensitive way to compute _follow_xxx_set for each PCG_NODE
void
IPA_PCG::Flow_sensitive_traverse(PCG_NODE *pcg_node,
		CFG_UTIL::WN_CFG &wn_cfg, std::vector<WN *> &spawn_site_set)
{
	PCG_CFG pcg_cfg(wn_cfg, _pool);

	// find the BBs which can be reached by spawn_site
	// note that the flag of the BBs containing spawn_site may not be set
	for(int i = 0; i < spawn_site_set.size(); i++) {
		WN_CFG::BB_NODE *spawn_site_bb = wn_cfg.Get_wn_node(spawn_site_set[i]);
		PCG_BB_NODE *pcg_bb_node = pcg_cfg.get_pcg_bb_node(spawn_site_bb);
		if(!pcg_bb_node->Is_flag_set()) {
			pcg_bb_node->Set_flag();
			pcg_bb_node->Propagate_down();
			pcg_bb_node->Clear_flag(); // may not be the first stmt of the BB
		}
	}

	// make up the BBs containing spawn_site but flag is not set
	for(int i = 0; i < spawn_site_set.size(); i++) {
		WN *spawn_site_stmt = spawn_site_set[i];
		WN_CFG::BB_NODE *spawn_site_bb = wn_cfg.Get_wn_node(spawn_site_stmt);
		PCG_BB_NODE *pcg_bb_node = pcg_cfg.get_pcg_bb_node(spawn_site_bb);
		if(!pcg_bb_node->Is_flag_set()) {
			for (WN_CFG::BB_NODE::stmt_iterator stmt_it =
					spawn_site_bb->Stmt_begin(spawn_site_stmt);
					stmt_it != spawn_site_bb->Stmt_end(); ++stmt_it) {
				WN *stmt = &(*stmt_it);
				Proccess_stmt(pcg_node, stmt, true);
			}
		}
	}

	for(PCG_CFG::ITERATOR it = pcg_cfg.begin(); it != pcg_cfg.end(); ++it) {
		PCG_BB_NODE *pcg_bb_node = *it;

		if(pcg_bb_node->Is_flag_set()) {
			// why const_stmt_iterator does not support ++?
			WN_CFG::BB_NODE *bb = (WN_CFG::BB_NODE *)pcg_bb_node->get_bb_node();
			for (WN_CFG::BB_NODE::stmt_iterator stmt_it = bb->Stmt_begin();
					stmt_it != bb->Stmt_end(); ++stmt_it) {
				WN *stmt = &(*stmt_it);
				Proccess_stmt(pcg_node, stmt, true);
			}
		}
	}
}

// Traverse the PU in a flow insensitive way to compute _xxx_set for each PCG_NODE
void
IPA_PCG::Flow_insensitive_traverse(PCG_NODE *pcg_node,
		CFG_UTIL::WN_CFG &wn_cfg, std::vector<WN *> &spawn_site_set)
{
	AliasAnalyzer *aa = AliasAnalyzer::aliasAnalyzer();

	for (WN_CFG::dfs_fwd_iterator bb_it = wn_cfg.Dfs_fwd_begin();
			bb_it != wn_cfg.Dfs_fwd_end(); ++bb_it) {
		WN_CFG::BB_NODE *bb = &(*bb_it);
		for (WN_CFG::BB_NODE::stmt_iterator stmt_it = bb->Stmt_begin();
				stmt_it != bb->Stmt_end(); ++stmt_it) {
			WN *stmt = &(*stmt_it);
			OPERATOR op = WN_operator(stmt);
			if(OPERATOR_is_call(op)) {
				switch(op) {
				case OPR_CALL: {
					ST *callee_st = WN_st(stmt);
					if(is_spawn_api(callee_st)) {
						spawn_site_set.push_back(stmt);
					} else if(is_lock_api(callee_st) || is_unlock_api(callee_st)) {
						// empty
					} else {
						if(st_to_node_vec_map.find(callee_st) ==
								st_to_node_vec_map.end()) {
							// TODO handle opaque function
						} else {
							PCG_NODE_VEC &node_vec = st_to_node_vec_map[callee_st];
							for(int i = 0; i < node_vec.size(); i++)
								if(node_vec[i]->Is_spawner()) {
									spawn_site_set.push_back(stmt);
									break;
								}
						}
					}
				}
				break;
				case OPR_ICALL:
				case OPR_INTRINSIC_CALL:
					// TODO handle indirect calls and intrinsic calls
				break;
				default:
					Is_True(FALSE, ("Unhandled call OPERATOR"));
				break;
				}
			}

			Proccess_stmt(pcg_node, stmt, false);
		}
	}
}
void
IPA_PCG::Traverse_WN_CFG(PCG_NODE *pcg_node)
{
	IPA_NODE *ipa_node = pcg_node->Get_ipa_node();
	IPA_NODE_CONTEXT context(ipa_node);
	WN *wn = ipa_node->Whirl_Tree(TRUE);
	REGION_Initialize (wn, PU_has_region(ipa_node->Get_PU()));

	WN_CFG wn_cfg(_pool);
	wn_cfg.Set_wn_root(wn);
	wn_cfg.Build();

	if (Alias_Nystrom_Analyzer) {
	    Alias_Analyzer_in_IPA = TRUE;
	    UINT16 fileIdx = (UINT16)(ipa_node->File_Index());
	    UINT16 puIdx   = (UINT16)(ipa_node->Proc_Info_Index());
	    Current_IPANode_File_PU_Idx = (fileIdx << 16) | puIdx;

	    IPA_NystromAliasAnalyzer *ipan = IPA_NystromAliasAnalyzer::aliasAnalyzer();
	    ConstraintGraph::IPANodeCG(ipan->cg(ipa_node->Node_Index()));

	    IPA_NystromAliasAnalyzer::aliasAnalyzer()->mapWNToUniqCallSiteCGNodeId(ipa_node);
	}

	std::vector<WN *> spawn_site_set;
	Flow_insensitive_traverse(pcg_node, wn_cfg, spawn_site_set);
	Flow_sensitive_traverse(pcg_node, wn_cfg, spawn_site_set);

	REGION_Finalize();
}

// A simple algorithm to find the set of STs that a pointer MAY point to
static ST_SET &
Find_pt_st(IPA_CALL_GRAPH *graph, const SUMMARY_VALUE &val, IPA_NODE *ipa_node,
		SUMMARY_FORMAL *formal_array, SUMMARY_SYMBOL *symbol_array, ST_SET &res)
{
	IPA_CONST_TYPE const_type = val.Get_const_type();
	switch(const_type) {
	case VALUE_FORMAL: {
		INT32 formal_idx = val.Get_formal_index();
		SUMMARY_FORMAL &formal = formal_array[formal_idx];
		INT32 position = formal.Get_position();

		IPA_PRED_ITER pred_iter (graph, ipa_node);
		for (pred_iter.First(); !pred_iter.Is_Empty(); pred_iter.Next()) {
			if (IPA_EDGE* edge = pred_iter.Current_Edge()) {
				IPA_NODE* caller = graph->Caller(edge);
				SUMMARY_CALLSITE *callsite = edge->Summary_Callsite();
				INT32 actual_idx = callsite->Get_actual_index() + position;
				SUMMARY_ACTUAL *actual_array = IPA_get_actual_array(caller);
				const SUMMARY_ACTUAL &actual = actual_array[actual_idx];
				if(actual.Get_pass_type() == PASS_LDA) {
					if(actual.Is_value_parm()) {
						INT32 value_idx = actual.Get_value_index();
						SUMMARY_VALUE *value_array = IPA_get_value_array(caller);
						SUMMARY_VALUE &value = value_array[value_idx];
						Find_pt_st(graph, value, caller, IPA_get_formal_array(caller),
								IPA_get_symbol_array(caller), res);
					} else {
						INT32 symbol_idx = actual.Get_symbol_index();
						SUMMARY_SYMBOL &symbol = symbol_array[symbol_idx];
						ST *symbol_st = &St_Table[symbol.St_idx()];
						res.insert(symbol_st);
					}
				} else {
					Is_True(FALSE,("Cannot determine ST for func pointer!"));
				}
			}
		}
	}
	break;
	case VALUE_GLOBAL: {
		ST_IDX st_idx;
		if(val.Is_global_st_idx()) {
			st_idx = val.Get_global_st_idx();
		} else {
			INT32 global_idx = val.Get_global_index();
			SUMMARY_SYMBOL &symbol = symbol_array[global_idx];
			st_idx = symbol.St_idx();
		}
		ST *symbol_st = &St_Table[st_idx];
		res.insert(symbol_st);
	}
	break;
	case VALUE_UNKNOWN:
	default:
		break;
	}
	return res;
}

// Construct the PCG
IPA_PCG::IPA_PCG(IPA_CALL_GRAPH *call_graph, MEM_POOL *mem_pool)
	: _graph(call_graph), _pool(mem_pool), entry_node(NULL)
{
	std::set<ST *> spawnee_st_set;
	BOOL spawned_func_pointer = FALSE;
	IPA_NODE_ITER cg_iter(_graph, DONTCARE);
	for (cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next()) {
		if (IPA_NODE *ipa_node = cg_iter.Current()) {

			ST *pu_st = ipa_node->Func_ST();
			PCG_NODE *pcg_node = Get_node(ipa_node);

			if(st_to_node_vec_map.find(pu_st) ==
					st_to_node_vec_map.end()) {
				std::vector<PCG_NODE *> tmp;
				tmp.push_back(pcg_node);
				st_to_node_vec_map[pu_st] = tmp;
			} else {
				st_to_node_vec_map[pu_st].push_back(pcg_node);
			}

			if(is_main(pu_st)) {
				pcg_node->Set_entry();
				entry_node = pcg_node;
			}

			SUMMARY_VALUE *value_array = IPA_get_value_array(ipa_node);
			SUMMARY_FORMAL *fomal_array = IPA_get_formal_array(ipa_node);
			SUMMARY_ACTUAL *actual_array = IPA_get_actual_array(ipa_node);
			SUMMARY_SYMBOL *symbol_array = IPA_get_symbol_array(ipa_node);

			// determine whether current node is a spawner
			// by checking if it calls pthread_create
			IPA_ICALL_LIST &ocall_list = ipa_node->Ocall_List();
			for(int i = 0; i < ocall_list.size(); i++) {
				IPA_ICALL_NODE *ocall = ocall_list[i];
				SUMMARY_CALLSITE *site = ocall->Callsite();
				INT32 func_sym_idx = site->Get_symbol_index();
				SUMMARY_SYMBOL &func_sym = symbol_array[func_sym_idx];
				ST_IDX st_idx = func_sym.St_idx();
				ST* callee_st = &St_Table[st_idx];
				if (is_spawn_api(callee_st)) {
					pcg_node->Set_spawner();

					SUMMARY_CALLSITE *callsite = ocall->Callsite();
					int acount = callsite->Get_param_count();
					Is_True(acount >= 3,
							("pthread_create should have no less than 3 arguments"));
					const SUMMARY_ACTUAL &actual =
							actual_array[callsite->Get_actual_index() + 2];
					if (actual.Get_pass_type() == PASS_LDA) {
						INT32 symbol_idx = actual.Get_symbol_index();
						SUMMARY_SYMBOL &symbol = symbol_array[symbol_idx];
						ST *symbol_st = &St_Table[symbol.St_idx()];
						spawnee_st_set.insert(symbol_st);
					} else {
						spawned_func_pointer = TRUE;
					}
				}
			}

			// find callees through ICALL
			IPA_ICALL_LIST &icall_list = ipa_node->Icall_List();
			for(int i = 0; i < icall_list.size(); i++) {
				IPA_ICALL_NODE *icall = icall_list[i];
				INT32 func_pt_val_idx = icall->Callsite()->Get_value_index();
				SUMMARY_VALUE &func_pt_val = value_array[func_pt_val_idx];
				Find_pt_st(_graph, func_pt_val, ipa_node, fomal_array, symbol_array,
						pcg_node->Get_icall_list());
			}

			// find it's callers
			IPA_PRED_ITER pred_iter (_graph, ipa_node);
			for (pred_iter.First(); !pred_iter.Is_Empty();
					pred_iter.Next()) {
				if (IPA_EDGE* edge = pred_iter.Current_Edge()) {
					IPA_NODE* caller = _graph->Caller(edge);
					PCG_NODE *caller_node = Get_node(caller);
					caller_node->Add_callee(pcg_node);
					pcg_node->Add_caller(caller_node);
				}
			}
		}
	}


	// find spawnee
	for(IPA_NODE_TO_PCG_NODE_MAP::iterator it = _ipa_node_to_pcg_node_map.begin();
			it != _ipa_node_to_pcg_node_map.end(); it++) {
		PCG_NODE *pcg_node = it->second;
		IPA_NODE *ipa_node = pcg_node->Get_ipa_node();

		// find spawnees and set flag
		if(spawned_func_pointer ||
				// very conservative now
				spawnee_st_set.find(ipa_node->Func_ST()) != spawnee_st_set.end()) {
			pcg_node->Set_spawnee();
			pcg_node->Set_start();
		}

		// add callees from icall
		for(ST_SET::iterator it = pcg_node->Get_icall_list().begin();
				it != pcg_node->Get_icall_list().end(); it++) {
			ST *callee_st = *it;
			if(st_to_node_vec_map.find(callee_st) != st_to_node_vec_map.end()) {
				std::vector<PCG_NODE *> &callees = st_to_node_vec_map[callee_st];
				for(int i = 0; i < callees.size(); i++) {
					pcg_node->Add_callee(callees[i]);
					callees[i]->Add_caller(pcg_node);
				}
			}
		}
	}

	// propagate PCG_SPAWNER and PCG_SPAWNEE flags
	for(IPA_NODE_TO_PCG_NODE_MAP::iterator it = _ipa_node_to_pcg_node_map.begin();
			it != _ipa_node_to_pcg_node_map.end(); it++) {
		PCG_NODE *pcg_node = it->second;
		pcg_node->Propagate_up(PCG_SPAWNER);
		pcg_node->Propagate_down(PCG_SPAWNEE);
	}

	// traverse WN_CFG for each node
	// find follow
	for(PCG_NODE_VEC::iterator it = _pcg_node_vec.begin();
			it != _pcg_node_vec.end(); it++) {
		PCG_NODE *pcg_node = *it;
		Traverse_WN_CFG(pcg_node);
	}

	// propagate PCG_FOLLOW flags
	for(PCG_NODE_VEC::iterator it = _pcg_node_vec.begin();
			it != _pcg_node_vec.end(); it++) {
		PCG_NODE *pcg_node = *it;
		pcg_node->Propagate_down(PCG_FOLLOW);
	}

	// choose alias tag set
	for(PCG_NODE_VEC::iterator it = _pcg_node_vec.begin();
			it != _pcg_node_vec.end(); it++) {
		PCG_NODE *pcg_node = *it;
		pcg_node->Tidy_load_store_set();
	}


	// comput whole program interference set
	for(PCG_NODE_VEC::iterator it = _pcg_node_vec.begin();
			it != _pcg_node_vec.end(); it++) {
		PCG_NODE *node_i = *it;
		const WN_TAG_MAP *store_set = node_i->Get_concurrent_store_set();
		for(WN_TAG_MAP::const_iterator cit = store_set->begin();
				cit != store_set->end(); cit++) {
			_wp_interference_set.insert(cit->second);
		}
	}

//	Print();
}

PCG_NODE *
IPA_PCG::Get_node(IPA_NODE *ipa_node)
{
	IPA_NODE_TO_PCG_NODE_MAP::iterator it =
			_ipa_node_to_pcg_node_map.find(ipa_node);

	if(it == _ipa_node_to_pcg_node_map.end()) {
		PCG_NODE *pcg_node = CXX_NEW(PCG_NODE(ipa_node, _pool), _pool);
		_pcg_node_vec.push_back(pcg_node);
		_ipa_node_to_pcg_node_map[ipa_node] = pcg_node;
		return pcg_node;
	} else
		return it->second;
}

void
IPA_PCG::Print(FILE *fout)
{
	for(IPA_NODE_TO_PCG_NODE_MAP::iterator it = _ipa_node_to_pcg_node_map.begin();
			it != _ipa_node_to_pcg_node_map.end(); it++) {
		PCG_NODE *pcg_node = it->second;
		pcg_node->Print(fout);
	}
}

bool
IPA_PCG::Is_siloed(AliasTag tag) const
{
	AliasAnalyzer *aa = AliasAnalyzer::aliasAnalyzer();

	for(ALIAS_TAG_SET::const_iterator cit = _wp_interference_set.begin();
			cit != _wp_interference_set.end(); cit++) {
		AliasTag t = *cit;
		if(aa->aliased(t, tag))
			return false;
	}

	return true;
}

void
IPA_PCG::Collect_siloed_references()
{
	for(PCG_NODE_VEC::iterator it = _pcg_node_vec.begin();
			it != _pcg_node_vec.end(); it++) {
		PCG_NODE *pcg_node = *it;

		TAG_WN_MAP &candidates = pcg_node->_ldid_set;

		for(TAG_WN_MAP::iterator it = candidates.begin();
				it != candidates.end(); it++) {
			AliasTag tag = it->first;

			if(tag != EmptyAliasTag && tag != InvalidAliasTag &&
					Is_siloed(tag)) {
				pcg_node->_siloed_ref_set.insert(tag);
			}
		}
	}
}

const ALIAS_TAG_SET &
IPA_PCG::Get_siloed_references(IPA_NODE *ipa_node) const
{
	IPA_NODE_TO_PCG_NODE_MAP::const_iterator cit = _ipa_node_to_pcg_node_map.find(ipa_node);
	Is_True(cit != _ipa_node_to_pcg_node_map.end(), ("Cannot find PCG node from IPA_NODE"));
	return cit->second->_siloed_ref_set;
}

IPA_PCG *IPA_Concurrency_Graph = NULL;
