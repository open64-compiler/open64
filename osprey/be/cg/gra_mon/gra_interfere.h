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

// Interference represenation for GRA
/////////////////////////////////////
//
//  Description:
//
//      The inteference graph neighbors of a LRANGE <lr> are the set of
//      LRANGEs that may not have the same register as <lr>.  For our
//      purposes, we'll need these sets to support the following operations:
//
//          1. Creation of the sets by presenting each member in turn
//          2. Cardinality, or Count of members (for simplification.)
//          3. Deletion of a member (during LRANGE splitting)
//          4. Iteration over members (both during simplification and
//             splittting).
//
//      It is especially important that the representation be compact and that
//      the operations be effecient because register allocation via coloring
//      is N**2 in the complexity of the interference graph, and these are the
//      arcs of that graph.
//
//      We opt for a dual representation of interference.  It may be
//      represented by either a bitset or by a vector, whichever is smaller in
//      each case.  We also provide two creation methods, one more appropriate
//      for LRANGEs in complement regions where it is natural to present all
//      the neighbors of a given LRANGE at once.  And one more appropriate for
//      the previously allocated regions where we are forced to present the
//      inteferences in random order, but already know that we will represent
//      the result as a bitset instead of a vector.)

//  $Revision: 1.2 $
//  $Date: 02/11/07 23:41:29-00:00 $
//  $Author: fchow@keyresearch.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_interfere.h $


#ifndef GRA_INTERFERE_INCLUDED
#define GRA_INTERFERE_INCLUDED

#ifndef GRA_INTERFERE_RCS_ID
#define GRA_INTERFERE_RCS_ID
#ifdef _KEEP_RCS_ID
static char *gra_interfere_rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_interfere.h $ $Revision: 1.2 $";
#endif
#endif

// forward declarations
class LRANGE;
class LRANGE_VSET;
class INTERFERE_DEREF;

typedef INTERFERE_DEREF* INTERFERE;

// A set of LRANGEs all in a particular LRANGE_SUBUNIVERSE.  This class
// has no field.  If "this"'s least significant bit is 1, it is in vector
// representation; otherwise, it is in bitset representation
class INTERFERE_DEREF {
public:
  INTERFERE_DEREF(void) {}
  ~INTERFERE_DEREF(void) {}
 
  // inlined member functions
  BOOL Is_Vec(void) 		{ return ((INTPTR) this) & ((INTPTR) 1); }
  LRANGE_VSET *Vec(void)	{ 
		Is_True(Is_Vec(), ("Interfere not a vector"));
		return (LRANGE_VSET *) (((INTPTR) this) & ~ ((INTPTR) 1)); }
  BOOL Is_Bitset(void)		{ return (((INTPTR) this) & ((INTPTR) 1)) == 0;}
  LRANGE_SET *Bitset(void)	{
		Is_True(Is_Bitset(), ("Interfere not a bitset"));
		return (LRANGE_SET *) this; }
  INT32 Count(void)		{ if (Is_Vec()) 
				    return Vec()->Count();
				  else return LRANGE_SET_Size(Bitset()); }

  // non-inlined member functions
  INTERFERE Add_Neighbor(LRANGE *lrange, LRANGE_SUBUNIVERSE *subuniverse);
  INTERFERE Remove_Neighbor(LRANGE *lrange, LRANGE_SUBUNIVERSE *subuniverse);
  INTERFERE Replace_Neighbor(LRANGE* old_lr, LRANGE* new_lr,
  			     LRANGE_SUBUNIVERSE* subuniverse);
};

class INTERFERE_MGR {
  MEM_POOL             neighbor_vec_pool;      // Tempory store for above

  // fields to represent the state for interference creation, initialized
  // in Create_Begin
  LRANGE**             neighbor_vec;           // Scratch vector
  size_t               neighbor_vec_size;      // Its allocated size
  LRANGE_SUBUNIVERSE*  neighbor_subuniverse;   // All neighbors here
  size_t               neighbor_count;         // Current size
  INT                  neighbor_id_max;        // Minimal bitset size

public:
  INTERFERE_MGR(void) {
    MEM_POOL_Initialize(&neighbor_vec_pool,"GRA interference creation pool",
			FALSE);
    }
  ~INTERFERE_MGR(void) {}

  // inlined member functions
  INTERFERE Make_Vec(LRANGE_VSET *vec) {
    Is_True((((INTPTR) vec) & ((INTPTR) 1)) == 0,("Low bit unexpectedly set"));
    return (INTERFERE) (((INTPTR) vec) | ((INTPTR) 1));
  }
  INTERFERE Make_Bitset(LRANGE_SET *set) {
    Is_True((((INTPTR) set) & ((INTPTR) 1)) == 0,("Low bit unexpectedly set"));
    return (INTERFERE) set;
  }

  // non-inlined member functions
  INTERFERE Create_Empty( LRANGE_SUBUNIVERSE* subuniverse );
  void Create_Begin( LRANGE_SUBUNIVERSE* subuniverse );
  void Create_Add_Neighbor( LRANGE* neighbor );
  INTERFERE Create_End( void );
};

extern INTERFERE_MGR intf_mgr;

// Used to iterate over the interference graph neighbors of an INTERFERE
class INTERFERE_ITER {
  struct {
    LRANGE_SET*         set;
    LRANGE*             current;
    LRANGE_SUBUNIVERSE* subuniverse;
  }                set_iter;       // Homebrew bitset iterator
  LRANGE_VSET_ITER vec_iter;
  BOOL is_set;
public:
  INTERFERE_ITER(void) {}
  ~INTERFERE_ITER(void) {}

  void Init(INTERFERE intf, LRANGE_SUBUNIVERSE *subuniv) {
	  // Prepare <iter> to iterate over the neighbors of <interfere>
	  // which must have all its neighbors in the given <subuniverse>.
	  if (intf->Is_Vec()) {
	    is_set = FALSE;
	    vec_iter.Init(intf->Vec());
	  }
	  else {
	    is_set = TRUE;
	    set_iter.set = intf->Bitset();
	    set_iter.subuniverse = subuniv;
	    set_iter.current = LRANGE_SET_ChooseS(set_iter.set, subuniv);
	  }
	}
  BOOL Done(void)	{ if (is_set)
			    return set_iter.current== LRANGE_SET_CHOOSE_FAILURE;
			  else return vec_iter.Done(); }
  LRANGE *Current(void)	{ return is_set ? set_iter.current: vec_iter.Current();}
  void Step(void) {
	  if (is_set)
	    set_iter.current = LRANGE_SET_Choose_NextS(set_iter.set,
				set_iter.current, set_iter.subuniverse);
	  else vec_iter.Step(); }
};

//      Iterator type LRANGE_NEIGHBOR_ITER -- Looping over interference
//      graph neighbors:
//
//  The representation of the LRANGE interference graph is
//  non-trivial.  This iterator type is provided to support looping
//  over the interference graph neighbors of a LRANGE.
//  Depending on what type we are iterating over, we'll need very different
//  information and have to perform very different actions.  We'll compute the
//  functions to use for each type at _Init time and then we'll just
//  dereference the pointers.
class LRANGE_NEIGHBOR_ITER {
public:
  void (*step)(LRANGE_NEIGHBOR_ITER *);
    //  _Step method, a pointer to the function that knows how to step from
    //  here.  Notice this can be replaced during the iteration when, e.g, we
    //  from stepping the global neighbors to the local neighbors.
  BOOL      done;
  LRANGE*   current;
    //  Computed in the _Init and _Step functions so the _Done/_Current
    //  functions can be trivial inlines.  This saves 2/3s of the function
    //  calls in the loops.
  ISA_REGISTER_CLASS rc;
    //  All the neighbors are in this.
  union {
    // Each different type of LRANGE needs different stuff to step its neighbors
    struct ll {
      LRANGE* lrange; // So we can avoid it when stepping the locals for the BB
    } l;
    struct cc {
      GRA_BB* current_bb;
    } c;
  } u;
  INTERFERE_ITER neighbor_iter; // neighboring complement LRANGEs, used by c/r
  LRANGE_LIVE_GBB_ITER live_gbb_iter; // The BBs in its live range, used by c
  GRA_BB_LOCAL_LRANGE_ITER bb_local_iter; // the locals in each BB, used by c/l
  INTERFERE_ITER bb_live_global_iter;//interfering globals for l

  LRANGE_NEIGHBOR_ITER(void) {}  
  ~LRANGE_NEIGHBOR_ITER(void) {}  

  BOOL Done(void)		{ return done; }
  LRANGE *Current(void)		{ return current; }
  void Step(void)		{ (*step)(this); }

  void Init(LRANGE *lrange, GRA_REGION *region);
};

#endif
