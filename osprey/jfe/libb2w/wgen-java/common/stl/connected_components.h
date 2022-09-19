/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
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



#ifndef SGI_connected_components_h_INCLUDED
#define SGI_connected_components_h_INCLUDED

#include "misc_extension.h"
#include <algorithm>

namespace SGI {

template <class NodeIterator, class NodeIndex>
NodeIndex find_representative_and_compress_path(NodeIterator c, NodeIndex current)
{
  NodeIndex old = current;
  NodeIndex ancestor = c[current];
  while (ancestor != current) {
    current = ancestor;
    ancestor = c[current];
  }
  current = c[old];
  while (ancestor != current) {
    c[old] = ancestor;
    old = current;
    current = c[old];
  }
  return ancestor;
}

template <class NodeContainer, class RankContainer, class NodeIndex>
inline void extend_components_and_ranks(NodeContainer& c, 
					  RankContainer& rank,
					  NodeIndex n)
{
  typename NodeContainer::size_type needed = n + 1;
  if (needed > c.size()) {
    typename NodeContainer::size_type old_size = c.size();
    rank.insert(rank.end(), (typename RankContainer::size_type)(needed - old_size), (typename RankContainer::value_type)0);
    c.insert(c.end(), int_iterator<NodeIndex>(old_size), int_iterator<NodeIndex>(needed));
  }
}


// RankContainer/RankIterator is a random accessable container/iterator
// (operator[] is defined) 
// with a value type that can represent an integer part of a binary log of
// the value type of the corresponding NodeContainer (char is always enough)
// its size_type is no less than the size_type of the corresponding
// NodeContainer/NodeIterator.
//
template <class NodeIterator, class RankIterator, class NodeIndex>
inline void connect_components(NodeIterator component, 
			       RankIterator rank,
			       NodeIndex i,
			       NodeIndex j)
{
  i = find_representative_and_compress_path(component, i);
  j = find_representative_and_compress_path(component, j);
  if (i == j) return;
  if (rank[i] > rank[j]) 
    component[j] = i;
  else {
    if (rank[i] == rank[j]) 
      ++rank[j];
    component[i] = j;
  }
}  


template <class Iterator, class NodeContainer>
void construct_connected_components(Iterator f, 
				    Iterator l,
				    NodeContainer& component)
{
  vector<unsigned char> rank;
  for (Iterator e = f; e != l; ++e) {
    extend_components_and_ranks(component, rank, max(first(*e), second(*e)));
    connect_components(component.begin(), rank, first(*e), second(*e));
  }
  for (typename Iterator::value_type::first_type i = 0; i < component.size(); ++i)
    find_representative_and_compress_path(component.begin(), i);
  // if halving is used, ...
  // component[i] = find_representative_and_compress_path(component.begin(), i);
}


template <class Iterator, class NodeContainer, class NodeIndex>
void construct_connected_components(Iterator f, 
				    Iterator l,
				    NodeContainer& component,
				    NodeIndex max_node_index)
{
  vector<unsigned char> rank;
  extend_components_and_ranks(component, rank, max_node_index+1);
  for (Iterator e = f; e != l; ++e) 
    connect_components(component.begin(), rank, first(*e), second(*e));
  for (typename Iterator::value_type::first_type i = 0; i < component.size(); ++i)
    find_representative_and_compress_path(component.begin(), i);
  // if halving is used, ...
  // component[i] = find_representative_and_compress_path(component.begin(), i);
}

template <class NodeIterator, class NodeIndex>
bool is_connected(NodeIterator component, NodeIndex i, NodeIndex j)
{
  return component[i] == component[j];
}

template <class NodeIterator, class NodeIndex>
bool is_connected_and_compress_path(NodeIterator component, NodeIndex i, NodeIndex j)
{
  return find_representative_and_compress_path(component, i) ==
    find_representative_and_compress_path(component, j);
}


} // Close namespace SGI

#endif /* SGI_connected_components_h_INCLUDE */

// Local Variables:
// mode:C++
// End:

