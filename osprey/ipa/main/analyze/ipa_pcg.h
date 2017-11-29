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

#ifndef ipa_pcg_INCLUDED
#define ipa_pcg_INCLUDED	"ipa_pcg.h"

#include "ipa_cg.h"
#include "wn_cfg.h"
#include "wssa_mgr.h"
#include <map>
#include <set>

typedef enum {
	PCG_ENTRY = 0x1,	// The entry of this program, 'main'
	PCG_SPAWNER = 0x2,	// A PU which may spawn new threads
	PCG_START = 0x4,	// The entry PU of theads
	PCG_SPAWNEE = 0x8,	// A PU which may be executed in spawned threads
	PCG_FOLLOW = 0x10,	// A PU which can be executed after a thread-spawn site
						// or an invocation to a SPAWNER PU
} PCG_FLAG;

// each IPA_NODE is mapped to a PCG_NODE in siloed reference analysis
class PCG_NODE;

typedef std::map<IPA_NODE *, PCG_NODE *> IPA_NODE_TO_PCG_NODE_MAP;
typedef std::set<ST *> ST_SET;
typedef std::vector<PCG_NODE *> PCG_NODE_VEC;
typedef std::map<ST *, PCG_NODE_VEC> ST_TO_PCG_NODE_VEC_MAP;
typedef std::set<AliasTag> ALIAS_TAG_SET;

// Class for Procedural Concurrency Graph
class IPA_PCG {
private:
	// memory pool
	MEM_POOL *_pool;

	// point to IPA call graph
	IPA_CALL_GRAPH *_graph;

	// all pcg nodes
	PCG_NODE_VEC _pcg_node_vec;

	// map from an ipa node to a pcg node
	IPA_NODE_TO_PCG_NODE_MAP _ipa_node_to_pcg_node_map;

	// map from as ST to pcg nodes with this ST
	ST_TO_PCG_NODE_VEC_MAP st_to_node_vec_map;

	// the entry PU of the program
	PCG_NODE *entry_node;

	// whole program interference set
	ALIAS_TAG_SET _wp_interference_set;

	PCG_NODE *Get_node(IPA_NODE *ipa_node);
	void Traverse_WN_CFG(PCG_NODE *pcg_node);
	void Proccess_stmt(PCG_NODE *pcg_node, WN *stmt, bool follow);
	void Flow_insensitive_traverse(PCG_NODE *pcg_node,
			CFG_UTIL::WN_CFG &wn_cfg, std::vector<WN *> &spawn_site_set);
	void Flow_sensitive_traverse(PCG_NODE *pcg_node,
			CFG_UTIL::WN_CFG &wn_cfg, std::vector<WN *> &spawn_site_set);
	void Find_use_in_tree(PCG_NODE *pcg_node, WN *stmt, WN *tree, bool follow);
	bool Is_siloed(AliasTag tag) const;
public:
	// PCG is built during constructor
	IPA_PCG(IPA_CALL_GRAPH *call_graph, MEM_POOL *mem_pool);

	// do siloed reference and collect the alias tags of
	// siloed_references for each PU
	void Collect_siloed_references();

	// get the alias tags of siloed_references for each PU
	const ALIAS_TAG_SET &Get_siloed_references(IPA_NODE *ipa_node) const;

	void Print(FILE *fout = stderr);
};

extern IPA_PCG *IPA_Concurrency_Graph;

#endif  // ipa_pcg_INCLUDED
