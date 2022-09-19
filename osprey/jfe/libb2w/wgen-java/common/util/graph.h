/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


//  Generic graph library
//
//   It consists three components:
//     -- a graph container,
//     -- a definition of graph iterator,
//     -- and algorithms accessing using only the graph iterator.
//
//
//  Graph Container:
//
//   digraph<NODE, EDGE> --  a directed graph of NODE and EDGE.
//     node/edge are shorthand for digraph<NODE,EDGE>::node,
//     and digraph<NODE,EDGE>::edge respectively.
//     
//     node *add_node() -- add a uninitialized node to the directed graph.
//     node *add_node(NODE v) -- add a node to the directed graph.
//     edge *add_edge(node *tail, node *head) -- add an edge (tail, head) to the graph.
//     void delete_edge(edge *e) -- delete an edge.
//     void delete_node(node *n) -- delete a node.
//     void delete_node_and_edge(node *n) -- delete a node and its succ/pred edges.
//
//     node_set_iter  -- iterates all nodes in the graph
//     succ_node_iter -- successor node forward iterator 
//     succ_edge_iter -- successor edge forward iterator 
//     pred_node_iter -- predecessor node forward iterator
//     pred_edge_iter -- predecessor edge forward iterator
//
//     node_set_iter node_set_begin() -- returns an iterator pointing at the
//          beginning of the node set sequence.
//     node_set_iter node_set_end() -- returns an iterator pointing at the
//          end of the node set sequence.
//
//     succ_node_iter succ_node_begin(node *v) 
//         -- returns the beginning of the successor node* sequence.
//     succ_node_iter succ_node_end(node *v)
//         -- returns the end of the successor node* sequence.
//
//      similiar definitions for {succ,pred}_{node,edge}_{begin,end}().
//
//   digraph<NODE, EDGE>::edge
//      node *head();  // the head node of the edge
//      node *tail();  // the tail node of the edge
//
//
//  Graph Iterator :
//
//    The graph iterator is the interface between the graph container and
//    the graph algorithms.   The iterator is indeed a graph node.
// 
//    It could be divided into several classes: 
//      node_graph_iterator, node_edge_graph_iterator
//
//    Operations defined on the node_graph iterator:
//      succ_node_begin(), succ_node_end(),
//      pred_node_begin(), pred_node_end().
//
//    Operations defined on the node_edge_graph iterator,
//    in addition to the on supported by node_graph iterator:
//      succ_edge_begin(), succ_edge_end(),
//      pred_edge_begin(), pred_edge_end().
//
//
//  Graph Algorithms:
//
//   directed graph Traversal:
//     preorder_iter<GRAPH, ITERATOR, VISITED>
//     postorder_iter<GRAPH, ITERATOR, VISITED>
//     depth_first_iter<GRAPH, ITERATOR, VISITED>
//     breath_first_iter<GRAPH, ITERATOR, VISITED>
//    
//       where ITERATOR contains the direction of graph traversal.
//       If ITERATOR is pred_node_iter, it performs the traversal on G as if
//       it is traversaling a graph G' where edges in G' are inverted edges from G.
//
//       where VISITED is a set keeping track of the state of the traversal.
//       (the set of nodes have been visited).  The default is set<GRAPH::node*>.
//       It could be replaced using a bitset of bool vector if there is a unique-id
//       associated with each node*.
//
//     dep_order_iter<GRAPH, SUCC_ITERATOR, PRED_ITERATOR, VISITED>
//       1. a node is made ready after all of its successor are visited or
//          it has no succ.
//       2. ready nodes are processed in FIFO order.
//       Note that the order is defined for a DAG, but not defined for cyclic graph.
//
//   tree Traversal:
//     preorder_tree_iter<TREE, ITERATOR>
//     postorder_tree_iter<TREE, ITERATOR>
//     depth_first_tree_iter<TREE, ITERATOR>
//     breath_first_tree_iter<TREE, ITERATOR>
//
//  TO DO:
//   * add allocator
//   * decide if succ_node_begin() and succ_node_end() should be
//     a member function of digraph or node.
//   * complete the iterators: traits, ...
//   * implement dominator tree algorithm.
//   
//   

#ifndef __GRAPH_H
#define __GRAPH_H

#include <hash_set.h>
#include <vector>
#include <stack>
#include <bvector.h>
#include <set>

template <class NODE, class EDGE> struct digraph_node;
template <class NODE, class EDGE> struct digraph_edge;
template <class NODE, class EDGE> struct digraph;
template <class DIGRAPH> struct succ_node_iter;
template <class DIGRAPH> struct succ_edge_iter;
template <class DIGRAPH> struct pred_node_iter;
template <class DIGRAPH> struct pred_edge_iter;


template <class NODE, class EDGE>
struct digraph_edge {
  typedef digraph_node<NODE, EDGE> digraph_node;

  EDGE edge;
  digraph_node *head;
  digraph_node *tail;
  digraph_edge *next_succ;
  digraph_edge *next_pred;

  digraph_edge<NODE, EDGE>(digraph_node *v, digraph_node *w) : 
    tail(v),head(w),next_succ(0),next_pred(0),edge() {}
};


template <class NODE, class EDGE>
struct digraph_node {
  typedef digraph_edge<NODE, EDGE> digraph_edge;
  typedef digraph<NODE, EDGE>      digraph;
  typedef succ_node_iter<digraph>  succ_node_iter;
  typedef pred_node_iter<digraph>  pred_node_iter;

  NODE node;
  int n_succ;
  int n_pred;
  digraph_edge *first_succ;
  digraph_edge *first_pred;

  void add_succ(digraph_edge *e) { ++n_succ; e->next_succ = first_succ; first_succ = e; }
  void add_pred(digraph_edge *e) { ++n_pred; e->next_pred = first_pred; first_pred = e; }

  void delete_succ_edge(digraph_edge *e) {
    if (first_succ == e) {
      e = e->next_succ;
      return;
    }
    for (digraph_edge *t = e; t->next_succ != e; t = t->next_succ);
    if (t->next_succ == e)
      t->next_succ = e->next_succ;
  }

  void delete_pred_edge(digraph_edge *e) {
    if (first_pred == e) {
      e = e->next_pred;
      return;
    }
    for (digraph_edge *t = e; t->next_pred != e; t = t->next_pred);
    if (t->next_pred == e)
      t->next_pred = e->next_pred;
  }

  succ_node_iter succ_node_begin() const
    { succ_node_iter s; s.cur = first_succ; return s; }
  
  succ_node_iter succ_node_end()   const
    { succ_node_iter s; s.cur = 0; return s; }

  pred_node_iter pred_node_begin() const
    { pred_node_iter s; s.cur = first_pred; return s; }

  pred_node_iter pred_node_end()   const 
    { pred_node_iter s; s.cur = 0; return s; }

  digraph_node<NODE, EDGE>(NODE v) { 
    node = v; n_succ = n_pred = 0;  first_succ = first_pred = 0; }

  digraph_node<NODE, EDGE>() { 
    n_succ = n_pred = 0;  first_succ = first_pred = 0; }
};


template <class X>
struct ptr_hash
{
 size_t operator()(const X* s) const { return reinterpret_cast<size_t>(s); }
};

template <class NODE, class EDGE>
struct digraph {
  typedef digraph<NODE, EDGE>          self;
  typedef digraph_node<NODE, EDGE>     node;
  typedef digraph_edge<NODE, EDGE>     edge;
  typedef succ_edge_iter<self>         succ_edge_iter;
  typedef succ_node_iter<self>         succ_node_iter;
  typedef pred_edge_iter<self>         pred_edge_iter;
  typedef pred_node_iter<self>         pred_node_iter;
  typedef hash_set<node*, ptr_hash<node> >  node_set_type;
  typedef node_set_type::iterator    node_set_iter;

  node_set_type  node_set;
  
  node_set_iter node_set_begin() { return node_set.begin(); }
  node_set_iter node_set_end()   { return node_set.end(); }

  node *add_node(NODE v) {
    node *p = new node(v);
    node_set.insert(p);
    return p;
  }

  node *add_node() {
    node *p = new node();
    node_set.insert(p);
    return p;
  }

  edge *add_edge(node *v, node *w) {
    edge *e = new edge(v, w);
    v->add_succ(e);
    w->add_pred(e);
    return e;
  }

  void delete_edge(edge *e) {
    e->tail->delete_succ_edge(e);
    e->head->delete_pred_edge(e);
    delete e;
  }

  void delete_node(node *v) {
    node_set.erase(v);
    delete v;
  }

  void delete_node_and_edge(node *v) {
    while (v->first_succ) 
      v->delete_succ_edge(v->first_succ);
    while (v->first_pred) 
      v->delete_succ_edge(v->first_pred);
    delete_node(v);
  }

  edge *find_edge(node *v, node *w);
  node *head(edge *e);
  node *tail(edge *e);

  succ_node_iter succ_node_begin(node *v) const
    { succ_node_iter s; s.cur = v->first_succ; return s; }

  succ_node_iter succ_node_end(node *v)   const
    { succ_node_iter s; s.cur = 0; return s; }

  pred_node_iter pred_node_begin(node *v) const
    { pred_node_iter s; s.cur = v->first_succ; return s; }

  pred_node_iter pred_node_end(node *v)   const 
    { pred_node_iter s; s.cur = 0; return s; }

  succ_edge_iter succ_edge_begin(node *v) const
    { succ_edge_iter s; s.cur = v->first_succ; return s; }

  succ_edge_iter succ_edge_end(node *v)   const
    { succ_edge_iter s; s.cur = 0; return s; }

  pred_edge_iter pred_edge_begin(node *v) const
    { pred_edge_iter s; s.cur = v->first_succ; return s; }

  pred_edge_iter pred_edge_end(node *v)   const 
    { pred_edge_iter s; s.cur = 0; return s; }
};



template <class DIGRAPH>
struct succ_node_iter {
  typedef DIGRAPH::node node;
  typedef DIGRAPH::edge edge;
  typedef succ_node_iter<DIGRAPH> self;

  edge *cur;

  self operator ++(int) { self tmp = *this; cur = cur->next_succ; return tmp; }
  self &operator ++() { cur = cur->next_succ; return *this; }
  node *operator *() { return cur->head; }
  friend bool operator ==(const self& x, const self& y) { return x.cur == y.cur; }

  self begin(node *v) const { self s; s.cur = v->first_succ; return s; }
  self end(node *v)   const { self s; s.cur = 0; return s; }
};


template <class DIGRAPH>
struct succ_edge_iter {
  typedef DIGRAPH::node node;
  typedef DIGRAPH::edge edge;
  typedef succ_edge_iter<DIGRAPH> self;

  edge *cur;

  self operator ++(int) { self tmp = *this; cur = cur->next_succ; return tmp; }
  self& operator ++() { cur = cur->next_succ; return *this; }
  edge *operator *() { return cur; }
  friend bool operator ==(const self& x, const self& y) { return x.cur == y.cur; }

  self begin(node *v) const { self s; s.cur = v->first_succ; return s; }
  self end(node *v)   const { self s; s.cur = 0; return s; }
};


template <class DIGRAPH>
struct pred_node_iter {
  typedef DIGRAPH::node node;
  typedef DIGRAPH::edge edge;
  typedef pred_node_iter<DIGRAPH> self;

  edge *cur;

  self operator ++(int) { self tmp = *this; cur = cur->next_pred; return tmp; }
  self& operator ++() { cur = cur->next_pred; return *this; }
  node *operator *() { return cur->tail; }
  friend bool operator ==(const self& x, const self& y) { return x.cur == y.cur; }

  self begin(node *v) const { self s; s.cur = v->first_pred; return s; }
  self end(node *v)   const { self s; s.cur = 0; return s; }
};


template <class DIGRAPH>
struct pred_edge_iter {
  typedef DIGRAPH::node node;
  typedef DIGRAPH::edge edge;
  typedef pred_edge_iter<DIGRAPH> self;

  edge *cur;

  self operator ++(int) { self tmp = *this; cur = cur->next_pred; return tmp; }
  self& operator ++() { cur = cur->next_pred; return *this; }
  edge *operator *() { return cur; }
  friend bool operator ==(const self& x, const self& y) { return x.cur == y.cur; }

  self begin(node *v) const { self s; s.cur = v->first_pred; return s; }
  self end(node *v)   const { self s; s.cur = 0; return s; }
};


//  preorder graph iterator
template <class GRAPH, class ITERATOR = GRAPH::succ_node_iter, class VISITED = set<GRAPH::node*> >
struct preorder_iter {
  typedef preorder_iter<GRAPH, ITERATOR, VISITED> self;
  stack<ITERATOR>  state;
  VISITED visited_set;

  ITERATOR cur;

  bool visited(GRAPH::node *v) { return visited_set.find(v) != visited_set.end(); }
  void set_visited(GRAPH::node *v) { visited_set.insert(v); }

  self &operator ++() { 
    GRAPH::node *n = *cur;
    if (cur.begin(n) != cur.end(n)) {
      ITERATOR t = cur.begin(n);
      while (t != t.end(0) && visited(*t))
	++t;
      if (t != t.end(0)) {
	set_visited(*t);
	++cur;
	state.push(cur);
	cur = t;
	return *this;
      }
    }
    ++cur;
    while (true) {
      while (cur != cur.end(0) && visited(*cur)) 
	++cur;
      if (cur != cur.end(0)) break;
      while (cur == cur.end(0) && !state.empty()) {
	cur = state.top();
	state.pop();
      }
      if (cur == cur.end(0)) break;
    }
    if (cur != cur.end(0))
      set_visited(*cur);
    return *this; 
  }
  self operator ++(int) { self tmp = *this; ++cur; return tmp; }
  void set_cur(GRAPH::node *v) { 
    cur.cur = new GRAPH::edge(v,v); 
    set_visited(*cur);
  }
  bool empty() { return cur == cur.end(0); }
  GRAPH::node *operator *() { return *cur; }
  
  preorder_iter<GRAPH, ITERATOR, VISITED>() {}; 
  preorder_iter<GRAPH, ITERATOR, VISITED>(GRAPH::node *v) { set_cur(v); };
};


template <class GRAPH, class ITERATOR = GRAPH::succ_node_iter, class VISITED = set<GRAPH::node*> >
struct postorder_iter {
  typedef postorder_iter<GRAPH, ITERATOR, VISITED> self;
  stack<ITERATOR>  state;
  VISITED visited_set;

  ITERATOR cur;
  
  bool visited(GRAPH::node *v) { return visited_set.find(v) != visited_set.end(); }
  void set_visited(GRAPH::node *v) { visited_set.insert(v); }

  self &operator ++() { 
    ++cur;
    while (cur != cur.end(0) && visited(*cur)) 
      ++cur;
    if (cur == cur.end(0)) {
      if (!state.empty()) {
	cur = state.top();
	state.pop();
	return *this;
      }
    } else {
      GRAPH::node *n = *cur;
      while (cur.begin(n) != cur.end(n)) {
	ITERATOR t = cur.begin(n);
	while (t != t.end(0) && visited(*t))
	  ++t;
	if (t != t.end(0)) {
	  set_visited(*t);
	  set_visited(*cur);
	  state.push(cur);
	  cur = t;
	} else
	  break;
	n = *cur;
      }
    }
    if (cur != cur.end(0))
      set_visited(*cur);
    return *this; 
  }
  self operator ++(int) { self tmp = *this; ++cur; return tmp; }
  void set_cur(GRAPH::node *v) { 
    cur.cur = new GRAPH::edge(v,v); 
    GRAPH::node *n = *cur;
    set_visited(v);
    while (cur.begin(n) != cur.end(n)) {
      ITERATOR t = cur.begin(n);
      while (t != t.end(0) && visited(*t))
	++t;
      if (t != t.end(0)) {
	set_visited(*t);
	set_visited(*cur);
	state.push(cur);
	cur = t;
      } else
	break;
      n = *cur;
    }
    if (cur != cur.end(0))
      set_visited(*cur);
  }
  bool empty() { return cur == cur.end(0); }
  GRAPH::node *operator *() { return *cur; }

  postorder_iter<GRAPH, ITERATOR, VISITED>() {}; 
  postorder_iter<GRAPH, ITERATOR, VISITED>(GRAPH::node *v) { set_cur(v); };
};


template <class GRAPH, class ITERATOR = GRAPH::succ_node_iter, class VISITED = set<GRAPH::node*> >
struct depth_first_iter : preorder_iter<GRAPH, ITERATOR, VISITED> {
  depth_first_iter<GRAPH, ITERATOR, VISITED>(GRAPH::node *v) { set_cur(v); };
};


template <class GRAPH, class ITERATOR = GRAPH::succ_node_iter, class VISITED = set<GRAPH::node*> >
struct breath_first_iter {
  typedef breath_first_iter<GRAPH, ITERATOR, VISITED> self;
  deque<ITERATOR>  state;
  VISITED visited_set;

  bool visited(GRAPH::node *v) { return visited_set.find(v) != visited_set.end(); }
  void set_visited(GRAPH::node *v) { visited_set.insert(v); }

  self &operator ++() { 
    if (state.empty()) return *this;
    ITERATOR cur = state.front();
    state.pop_front();
    ITERATOR t;
    GRAPH::node *n = *cur;
    for (t = cur.begin(n); t != cur.end(n); ++t) {
      if (!visited(*t)) {
	state.push_back(t);
	set_visited(*t);
      }
    }
    return *this; 
  }
  self operator ++(int) { self tmp = *this; ++cur; return tmp; }
  void set_cur(GRAPH::node *v) { 
    ITERATOR t;
    t.cur = new GRAPH::edge(v,v); 
    set_visited(*t);
    state.push_back(t);
  }
  bool empty() { return state.empty(); }
  GRAPH::node *operator *() { return *(state.front()); }

  breath_first_iter<GRAPH, ITERATOR, VISITED>() {}; 
  breath_first_iter<GRAPH, ITERATOR, VISITED>(GRAPH::node *v) { set_cur(v); };
};


template <class GRAPH,
   class SUCC_ITERATOR = GRAPH::succ_node_iter, 
   class PRED_ITERATOR = GRAPH::pred_node_iter, 
   class VISITED = set<GRAPH::node*> >
struct dep_order_iter {
  typedef dep_order_iter<GRAPH, SUCC_ITERATOR, PRED_ITERATOR, VISITED> self;
  deque<GRAPH::node *> state;
  VISITED visited_set;

  bool visited(GRAPH::node *v) { return visited_set.find(v) != visited_set.end(); }
  void set_visited(GRAPH::node *v) { visited_set.insert(v); }

private:
  bool all_succ_visited(GRAPH::node *n) {
    SUCC_ITERATOR t;
    for (t = n->succ_node_begin(); t != n->succ_node_end(); ++t) {
      if (!visited(*t))
	return false;
    }
    return true;
  }

public:

  self &operator ++() { 
    if (state.empty()) return *this;
    GRAPH::node *n = state.front();
    state.pop_front();
    set_visited(n);

    PRED_ITERATOR t;
    for (t = n->pred_node_begin(); t != n->pred_node_end(); ++t) {
      if (!visited(*t) && all_succ_visited(*t)) {
	state.push_back(*t);
	set_visited(*t);
      }
    }
    return *this; 
  }
  self operator ++(int) { self tmp = *this; ++cur; return tmp; }
  bool empty() { return state.empty(); }
  GRAPH::node *operator *() { return state.front(); }

  void make_ready(GRAPH::node *n) {
    state.push_back(n);
    set_visited(n);
  }

  dep_order_iter<GRAPH, SUCC_ITERATOR, PRED_ITERATOR, VISITED>(GRAPH *g) { 
    GRAPH::node_set_iter iter;
    for (iter = g->node_set_begin(); iter != g->node_set_end(); ++iter) {
      if (all_succ_visited(*iter)) {
	state.push_back(*iter);
	set_visited(*iter);
      }
    }
  }
};


template <class TREE>
struct never_visited {
  TREE::node *find(TREE::node *n) { return 0; }
  TREE::node *end() { return 0; }
  void insert(TREE::node *n) { }
};


template <class TREE, class ITERATOR>
struct preorder_tree_iter :
preorder_iter<TREE, ITERATOR, never_visited<TREE> > {};

template <class TREE, class ITERATOR>
struct postorder_tree_iter :
postorder_iter<TREE, ITERATOR, never_visited<TREE> > {};

template <class TREE, class ITERATOR>
struct depth_first_tree_iter :
depth_first_iter<TREE, ITERATOR, never_visited<TREE> > {};

template <class TREE, class ITERATOR>
struct breath_first_tree_iter :
breath_first_iter<TREE, ITERATOR, never_visited<TREE> > {};


#endif  /* __GRAPH_H */

// Local Variables:
// mode:C++
// End:
