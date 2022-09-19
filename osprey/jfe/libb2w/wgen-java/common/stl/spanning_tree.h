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



#ifndef SGI_spanning_tree_h_INCLUDED
#define SGI_spanning_tree_h_INCLUDED

#include "bucket_container.h"
#include "container_with_membership.h"

#include <stdio.h>

namespace SGI {

template <class ClusterIterator,
          class OutputIterator,
          class Compare,
          class DestinationFunction,
          class Sizetype>
OutputIterator minimum_spanning_tree(// const
                                     ClusterIterator initial,
                                     OutputIterator result,
                                     Compare comp,
                                     DestinationFunction destination,
				     Sizetype size)
{
  typedef iterator_traits<ClusterIterator>::value_type::iterator edge_iterator;
  typedef iterator_traits<edge_iterator>::value_type heap_element;
  typedef heap_with_membership<heap_element,
                               DestinationFunction,
                               Compare> heap_type;
  heap_type heap(size, destination, comp);  
  typename heap_type::insert_iterator inserter(heap); 
  heap.remove(initial);
  copy(initial->begin(), initial->end(), inserter);
  while (!heap.empty()) {
    initial = destination(heap.top());
    *result++ = heap.top();
    heap.pop();
    copy(initial->begin(), initial->end(), inserter);
  }
  return result;
}

} // Close namespace SGI

#endif /* SGI_spanning_tree_h_INCLUDE */

// Local Variables:
// mode:C++
// End:
