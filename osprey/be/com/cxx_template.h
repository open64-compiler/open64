/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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


//-*-c++-*-
/** ====================================================================
*** ====================================================================
***
*** Module: cxx_template.h
*** $Revision: 1.2 $
*** $Date: 02/11/07 23:41:35-00:00 $
*** $Author: fchow@keyresearch.com $
*** $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.cxx_template.h $
***
*** Revision history:
***  8-SEP-94 shin - Original Version
*** 
*** Description:
*** 
*** This interface describes the DYN_ARRAY template for dynamic array and
*** the STACK template which uses an dynamic array to implement the stack.
*** 
*** The size of the array can grow dynamically.  The subscript operator is
*** overloaded such that it appear to the user is the same as any ordinary
*** array.  Internally, the subscript index must be less than the array
*** size and less equal to the allocated index.  As the result, user must
*** call Newidx to allocate an array element.  Once an index is allocated,
*** it can be used with subscript operator.
*** 
*** Reserver Prefixes:
*** 
***     None.
*** 
*** Exported Types:
*** 
***     None.
*** 
*** Exported Functions:
*** 
***     DYN_ARRAY(MEM_POOL *pool)
*** 
***         Construct a dynamic array using the MEM_POOL *pool. This
***         memory pool will be used in future allocation, reallocation
***         and free of this dynamic array.
*** 
***     ~DYN_ARRAY(void)
*** 
***         Destruct this dynamic array and free the space if necessary.
*** 
***     void    Set_Mem_Pool(MEM_POOL* mpool)
***
***         Set the memory pool for future memory allocation to be mpool.
***         This should be used only in places where DYN_ARRAY(MEM_POOL *pool)
***         cannot be used, e.g., array of dynamic arrays.
***
***     void    Alloc_array(mUINT32 arr_size)
*** 
***         Allocate an array of size max(arr_size, MIN_ARRAY_SIZE)
***
***	void	Force_Alloc_array(mUINT32 arr_size)
*** 
***         Allocate an array of size max(arr_size, 1)
*** 
***     void    Realloc_array(mUINT32 new_size)
*** 
***         Re-allocate array for size 'new_size'.
*** 
***     void    Free_array(void)
*** 
***         Free the space to the memory pool and re-set the index.
*** 
***     void    Bzero_array(void)
*** 
***         Clear (fill with 0) the array at its current size
*** 
***     DYN_ARRAY& operator = (const DYN_ARRAY& a)
*** 
***         The size and the index of the input dynamic array 'a' is
***         copied to the current array.  Using own memory pool, the
***         array re-allocate space in order to change to have the same
***         size as 'a'.
*** 
***     T&      operator[] (mUINT32 idx) const
***     T&      operator[] (mUINT32 idx)
***     T&      Get(mUINT32 idx) const
***     void    Set(mUINT32 idx, const T& val)
***     
***     void AddElement (const T& val)
***         Grow the array by 1 element and add val at the end of the array.
***
***     mUINT32 Elements () const
***         Return the number of elements in the array, useful for iterating 
***         over all the elements.
*** 
***         Access the element 'idx'.
*** 
***     mUINT32 Newidx(void)
*** 
***         Allocate a new index. It controls whether re-allocation is needed.
***         If an index is returned from Newidx(), it is sure to be valid.
***
***         Note: although the space is allocated for the returned
***         index, the data is NOT initialized.
*** 
***     void    Decidx(void)
*** 
***         Decrement the last index pointer by one which essentially makes the
***         last index available for Newidx() again.
*** 
***     void    Initidx(UINT32 idx)
*** 
***         Set the last index pointer to 'idx'. Allocate more space if
***         necessary but no copying is done.
*** 
***     void    Setidx(UINT32 idx)
*** 
***         Set the last index pointer to 'idx'. Allocate more space if
***         necessary and old contents are copied.
*** 
***     void    Resetidx(void)
*** 
***         Set the last index pointer to -1 and the whole array are available
***         starting from the first ('0'-th) element.
*** 
***     mUINT32 Sizeof(void)  const
*** 
***         Return the current size of the array.
*** 
***     mINT32  Lastidx(void) const
*** 
***         Return the current value of the last index pointer.
*** 
*** ====================================================================
*** ====================================================================
**/


#ifndef cxx_template_INCLUDED
#define cxx_template_INCLUDED	  "cxx_template.h"
#ifdef _KEEP_RCS_ID
static char *cxx_templatercs_id = cxx_template_INCLUDED"$Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */

#include "mempool.h"
#include "erglob.h"
#include "errors.h"

template <class T>
class DYN_ARRAY {
private:
  MEM_POOL *_mpool;
  mUINT32  _size;
  mINT32   _lastidx;
  T       *_array;

          DYN_ARRAY(const DYN_ARRAY&);
public:

          DYN_ARRAY(void);
          DYN_ARRAY(MEM_POOL *pool);
         ~DYN_ARRAY(void);

  MEM_POOL* Get_Mem_Pool() { return _mpool; }
  void    Set_Mem_Pool(MEM_POOL* mpool) { _mpool = mpool; }

  void    Alloc_array(mUINT32 arr_size);
  void    Force_Alloc_array(mUINT32 arr_size);
  void    Realloc_array(mUINT32 new_size);
  void    Free_array(void);
  void    Bzero_array(void);

  DYN_ARRAY<T>& operator = (const DYN_ARRAY<T>& a);
  T&      Get(mUINT32 idx) const;        // to access the array by idx
  void    Set(mUINT32 idx, const T& val);
  T&      operator[] (mUINT32 idx) const // to access the array by idx
            { Is_True(idx <= _lastidx, ("DYN_ARRAY::[]:Subscript out of range"));
              return (_array[idx]); }
  T&      operator[] (mUINT32 idx)      // to access the array by idx
            { Is_True(idx <= _lastidx, ("DYN_ARRAY::[]:Subscript out of range"));
              return (_array[idx]); }

  void    AddElement (const T& val)
  {
#ifdef KEY /* bug 11670: Newidx() may need to allocate _array. */
    mUINT32 idx = Newidx();
    _array[idx] = val;
#else
    _array[Newidx()] = val;
#endif
  }
  void DeleteElement(mUINT32 idx)
  {
    if (idx < _lastidx) {
       for (int iter = idx; iter < _lastidx; iter++) {
          _array[iter] = _array[iter+1];
       }
       Decidx();
    }
    else if (idx == _lastidx) {
        Decidx();
    }
  }

  void ReplaceElement(mUINT32 idx, const T& val) 
  {
      if (idx <= _lastidx) {
          _array[idx] = val;
      }
  }

  mUINT32 Elements () const  { return (_lastidx+1); }

  mUINT32 Newidx(void);                  // allocate a valid index
  void    Decidx(void)                   { _lastidx--; }
  void    Initidx(UINT32 idx);           // initialize idx without copying
  void    Setidx(UINT32 idx);            // initialize idx and copy old stuff
  void    Resetidx(void)                 { _lastidx = -1; }
  mUINT32 Sizeof(void)  const            { return _size; }
  mINT32  Lastidx(void) const            { return _lastidx; }
  mUINT32 Idx(T *t) const                { return t - _array; }
};

/**
*** 
*** Description:
*** 
***     class STACK is a simple extension of DYN_ARRAY with stack specific methods.
*** 
*** Reserved Prefix:
*** 
***     None.
*** 
*** Exported Types:
*** 
***     None.
*** 
*** Exported Funcitons:
*** 
***     STACK(MEM_POOL *pool)
*** 
***         Construct a stack using space from *pool.
*** 
***     ~STACK(void)
*** 
***         Destruct the stack.
*** 
***     void    Push(const T& val)
*** 
***     	Push a 'val' to the top of the stack.
*** 
***     void    Settop(const T& val)
*** 
***     	Set 'val' as the top of the stack (no push and pop involved)
***     	if the stack has allocated space (size >= 1).
***
***     void    Settop_nth(const T& val, INT32 n)
***
***             Set 'val' as the top nth of ths stack (no push and pop involved)
***             if the stack has allocated space (size >= 1).
*** 
***     T       Pop(void)
*** 
***     	Pop the stack.
*** 
***     T&      Top_nth(const INT32 n) const
*** 
***     	Get the top 'n'-th element of the stack. Stack top is Nth(0).
*** 
***     T&      Bottom_nth(const INT32 n) const
*** 
***     	Get the bottom 'n'-th element of the stack. Stack bottom is Nth(0).
*** 
***     T&      Top(void) const
*** 
***     	Same as Nth(0).
*** 
***     void    Clear(void)
*** 
***     	Wipe out the entire stack.
*** 
***     void    Alloc(const INT32 n)
*** 
***     	Allocate space for 'n' element in the stack.
***
***     mINT32   Elements() const
***
***		How many elements are on the stack.
***
***     BOOL    Contains(const T& val) const
***             Whether the stack contains 'val'.
***
***     INT32   Get_index(const T& val) const
***             If the stack contains 'va', get top nth index.  Otherwise return -1.
*** 
**/

template <class T>
class STACK {
private:
  DYN_ARRAY<T> _stack;

          STACK(const STACK&);
          STACK& operator = (const STACK&);

public:
          STACK(MEM_POOL *pool):_stack(pool)	{}
	  ~STACK(void)		                {}
  void    Push(const T& val)                    { _stack[_stack.Newidx()]=val;}
  void    Settop(const T& val);
  void    Settop_nth(const T& val, INT32 n);
  INT32   Topidx(void)                          { return _stack.Lastidx(); }
  T       Pop(void) {
    T t;
    INT32 idx = _stack.Lastidx();
    FmtAssert(idx >= 0, ("STACK::pop(): Stack Empty"));
    t = _stack[idx];
    _stack.Decidx();
    return t;
  }

  T&      Top_nth(const INT32 n) const;
  T&      Bottom_nth(const INT32 n) const;
  T&      Top(void) const;
  BOOL    Is_Empty(void) const;
  BOOL    Contains(const T&) const;
  INT32   Get_index(const T& val) const;
  void    Clear(void)                           { _stack.Resetidx(); }
  void    Free()                                { _stack.Free_array(); }
  void    Alloc(const INT32 n)                  { _stack.Alloc_array(n); }
  mINT32  Elements() const			{ return(_stack.Lastidx() +1);}
  // Delete Top_nth(n) element.
  void    DeleteTop(mUINT32 n)         { _stack.DeleteElement(_stack.Lastidx() - n); }
};


/**
***
*** Description:
***
***   Remove_if takes a predicate struct and removes an item from the container
***   if pred(item) returns TRUE.
***
***   An example predicate:
***
***   struct Predicate {
***       BOOL operator()(EXP_WORKLST *wk) { return wk->Real_occurs().Head() == NULL; };
***   }
***
**/
template <class CONTAINER, class PREDICATE>
void Remove_if(CONTAINER& container, PREDICATE pred);


// Implementation stuff follows. This was taken from cxx_template.cxx,
// since g++ (rightly) doesn't do the "implicit .cxx file inclusion"
// thing.

#define MIN_ARRAY_SIZE 16

template <class T >
DYN_ARRAY<T>::DYN_ARRAY(void)
{
  _lastidx = -1;
  _size = 0;
  _array = NULL;
  _mpool = NULL;
}

template <class T >
DYN_ARRAY<T>::DYN_ARRAY(MEM_POOL *pool)
{
  _lastidx = -1;
  _size = 0;
  _array = NULL;
  _mpool = pool;
}

template <class T >
DYN_ARRAY<T>::~DYN_ARRAY()
{
  Free_array();
}

/* must guarantee a min. non-zero size */
template <class T>
void
DYN_ARRAY<T>::Alloc_array(mUINT32 arr_size)
{
   _size = arr_size > MIN_ARRAY_SIZE ? arr_size : MIN_ARRAY_SIZE;
  _array = (T*)MEM_POOL_Alloc(_mpool, _size * sizeof(T));
  if ( _array == NULL ) ErrMsg ( EC_No_Mem, "DYN_ARRAY::Alloc_array" );
}

/* min. size is 1, instead of MIN_ARRAY_SIZE */
template <class T>
void
DYN_ARRAY<T>::Force_Alloc_array (mUINT32 arr_size)
{
    _size = arr_size > 1 ? arr_size : 1;
    _array = (T*)MEM_POOL_Alloc(_mpool, _size * sizeof(T));
    if ( _array == NULL ) ErrMsg ( EC_No_Mem, "DYN_ARRAY::Alloc_array" );
} 

template <class T>
void
DYN_ARRAY<T>::Realloc_array(mUINT32 new_size)
{
  _array = (T*)MEM_POOL_Realloc(_mpool,
			       _array,
			       sizeof(T) * _size,
			       sizeof(T) * new_size);
  if ( _array == NULL ) ErrMsg ( EC_No_Mem, "DYN_ARRAY::Realloc_array" );
  _size = new_size;
}

template <class T>
void
DYN_ARRAY<T>::Free_array()
{
  if (_array != NULL) {
      MEM_POOL_FREE(_mpool,_array);
      _array = NULL;
      _size = 0;
  }
}

template <class T>
void
DYN_ARRAY<T>::Bzero_array()
{
  if (_array != NULL) BZERO(_array,sizeof(T) * _size);
}

template <class T>
DYN_ARRAY<T>&
DYN_ARRAY<T>::operator = (const DYN_ARRAY<T>& a)
{
  if (_size != a._size) Realloc_array(a._size);
  _lastidx = a._lastidx;
  memcpy (_array, a._array, a._size * sizeof(T));
  return *this;
}

template <class T >
mUINT32
DYN_ARRAY<T>::Newidx()
{
  _lastidx++;
  if (_lastidx >= _size) {
    // overflow the allocated array, resize the array
    if (_array == NULL) {
	Alloc_array (MIN_ARRAY_SIZE); // Alloc_array guarantees non-zero size
    } else {
	Realloc_array (_size * 2);
    }
  }
  return _lastidx;
}

template <class T >
void
DYN_ARRAY<T>::Initidx(UINT32 idx)
{
  _lastidx=idx;
  if (_lastidx >= _size) {
    // overflow the allocated array, resize the array
    if (_array != NULL) {
      Free_array();
    }
    Alloc_array(_lastidx + 1);
  }
}

template <class T >
void
DYN_ARRAY<T>::Setidx(UINT32 idx)
{
  _lastidx=idx;
  if (_lastidx >= _size) {
    // overflow the allocated array, resize the array
    if (_array == 0)
      Alloc_array(_lastidx + 1);
    else {
      INT32 new_size = _size * 2;
      while (new_size < _lastidx + 1) new_size *= 2;
      Realloc_array(new_size);
    }
  }
}

template <class T>
void STACK<T>::Settop(const T& val)
{
  INT32 idx = _stack.Lastidx();
  
  Is_True(idx >= 0, ("STACK::Settop(): Stack Empty"));
  _stack[idx] = val;
}

template <class T>
void STACK<T>::Settop_nth(const T& val, INT32 n)
{
  INT32 idx = _stack.Lastidx();
  Is_True(idx >= n, ("STACK::Top_nth(): Access beyond stack bottom"));
  _stack[idx-n] = val;
}

template <class T>
T& STACK<T>::Top_nth(const INT32 n) const
{
  INT32 idx = _stack.Lastidx();
  
  Is_True(idx >= n, ("STACK::Top_nth(): Access beyond stack bottom"));
  return _stack[idx - n];
}


template <class T>
T& STACK<T>::Bottom_nth(const INT32 n) const
{
  INT32 idx = _stack.Lastidx();
  
  Is_True(n <= idx, ("STACK::Bottom_nth(): Access beyond stack top"));
  return _stack[n];
}

    
template <class T>
T& STACK<T>::Top(void) const
{
  INT32 idx = _stack.Lastidx();
  
  Is_True(idx >= 0, ("STACK::Top(): Stack Empty"));
  return _stack[idx];
}

template <class T>
BOOL STACK<T>::Is_Empty(void) const
{
  return _stack.Lastidx() < 0;
}

template <class T>
BOOL STACK<T>::Contains(const T& val) const
{
 INT32 last_idx = _stack.Lastidx();
 for (INT32 idx = 0; idx <= last_idx; idx++) {
     if (_stack[idx] == val) {
         return TRUE;
     }
 }
 return FALSE;
}

template <class T>
INT32 STACK<T>::Get_index(const T& val) const
{
 INT32 last_idx = _stack.Lastidx();
 for (INT32 idx = 0; idx <= last_idx; idx++) {
     if (_stack[idx] == val) 
         return (last_idx - idx);
 }
 return -1;
}

template <class CONTAINER, class PREDICATE>
void Remove_if(CONTAINER& container, PREDICATE predicate)
{
  typename CONTAINER::CONTAINER_NODE *prev = NULL, *curr, *next;
  for (curr = container.Head();  curr != NULL;  curr = next) {
    next = curr->Next();
    if (predicate(curr)) {
      if (prev == NULL)
	container.Set_Head(next);
      else
	prev->Set_Next(next);
    } else {
      prev = curr;
    }
  }
  if (prev == NULL)
    container.Set_Tail(container.Head());
  else
    container.Set_Tail(prev);
}

#endif  // cxx_template_INCLUDED
