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

// namespace SGI {

template <class Parent, class Edge>
void erase_from_parent(Parent& p, const Edge& e) 
{
  p[second(e)] = second(e);
}

template <class Container, class Edge>
void erase_from_child(Container& heir, 
		      Container& next_sibling,  
		      const Edge& e)
{
  typedef typename Container::value_type index;
  index parent = first(e);
  index child = second(e); 
  index oldest_child =  heir[parent];
  if (child == oldest_child) {
    index next_child = next_sibling[oldest_child];
    heir[parent] = oldest_child != next_child ? next_child : parent;
    next_sibling[oldest_child] = oldest_child;
  } else {
    index next_child = next_sibling[oldest_child];
    while (next_child != child) {
      oldest_child = next_child;
      next_child = next_sibling[next_child];
    }
    index next_next_child = next_sibling[next_child];
    next_sibling[oldest_child] = 
      next_child == next_next_child ?  oldest_child : next_next_child;
    next_sibling[next_child] = next_child;
  }
}


template <class Container, class Edge>
void erase_from_child(Container& heir, 
		      Container& next_sibling,
		      Container& previous_sibling,
		      const Edge& e) 
{
  typedef typename Container::value_type index;
  index parent = first(e);
  index child = second(e); 
  index oldest_child =  heir[parent];
  index next_child = next_sibling[oldest_child];
  index previous_child = previous_sibling[oldest_child];
  if (child == oldest_child) 
    heir[parent] = oldest_child != next_child ? next_child : parent;
  next_sibling[child] = child;
  previous_sibling[child] = child;
  next_sibling[oldest_child] = oldest_child;
  previous_sibling[next_child] = next_child;
}

template <class Parent, class Edge>
void insert(Parent& parent, const Edge& edge) 
{
  parent[second(edge)] = first(edge);
}

template <class Container, class Edge>
void insert(Container& parents,
	    Container& heir,
	    Container& next_sibling,
	    const Edge& edge) {
	      typedef typename Parent::index Index;
	      Index child = second(edge);
	      Index parent = first(edge);
	      Index oldest_child = heir[parent];
	      next_sibling[child] = (oldest_child == parent)
		? child : oldest_child;
	      heir[parent] = child;
	      parents[child] = parent;
}

template <class Container, class Edge>
void insert(Container& parents,
	    Container& heir,
	    Container& next_sibling,
	    Container& prev_sibling,
	    const Edge& edge)
{
  typedef typename Parent::index Index;
  Index child = second(edge);
  Index parent = first(edge);
  Index oldest_child = heir[parent];
  prev_sibling[child] = child;
  if (oldest_child == parent) {
    next_sibling[child] = child;
  } else {
    next_silbing[child] = oldest_child;
    prev_silbing[oldest_child] = child;
  }
  heir[parent] = child;
  parents[child] = parent;
}


} // Close namespace SGI

#endif /* SGI_connected_components_h_INCLUDE */

// Local Variables:
// mode:C++
// End:
