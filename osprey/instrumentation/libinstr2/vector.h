/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * Further, this software is distributed without any warranty that it is
 * free of the rightful claim of any third person regarding infringement
 * or the like.  Any license provided herein, whether implied or
 * otherwise, applies only to this software file.  Patent licenses, if
 * any, provided herein do not apply to combinations of this program with
 * other software, or any other product whatsoever.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write the Free Software Foundation, Inc., 59
 * Temple Place - Suite 330, Boston MA 02111-1307, USA.
 *
 */

//
// file: vector.h
// author: Gautam Chakrabarti <gautam@pathscale.com>
//

#ifndef _vector_h_included
#define _vector_h_included

namespace Instr {

template <class T> class vector {
public:
    typedef T value_type;
    typedef unsigned int size_type;
    typedef const T& const_reference;
    typedef T * iterator;
    typedef const T * const_iterator;
    vector() : start(NULL), finish(NULL), stfinish(NULL) {}
    vector(size_type n, T t) : start(NULL), finish(NULL), stfinish(NULL) {
        for (int i=0; i<n; i++)
	  push_back (t);
    }

    ~vector() {
        if (start != NULL) {
            free (start);
        }
    }

    void reserve (size_type n) {
      // TODO: fill in code here.
    }

    void push_back (T t) {
        if (finish == stfinish) {
            allocate ();
        }
        *finish++ = t;
    }

    void pop_back () {
	// memory erased in destructor
        finish--;
    }

    int size() const {
        return finish - start;
    }

    bool empty() const {
        return size() == 0;
    }

    T &operator[] (int n) const {
        return start[n];
    }

    const_reference front () const {
        return *start;
    }

    const_reference back () const {
        return *finish;
    }

    iterator begin () {
        return start;
    }

    iterator end () {
        return finish;
    }

    const_iterator begin () const {
        return start;
    }
    
    const_iterator end () const {
        return finish;
    }

    void resize (int n, T t = T()) {
      int current_size = finish - start;

      if (n == current_size) return;
      if (n < current_size) {// erase elements at the end
        finish -= current_size - n;
      } else { // insert elements at the end
        for (int i = current_size; i < n; i++)
	  push_back (t);
      }
    }

    template <class InputIterator>
    void insert (iterator pos, InputIterator f, InputIterator l)
    {
      if (pos == finish)
      { // special case, inserting at end
        for (InputIterator iter = f; iter < l; iter++)
	  push_back (*iter);
      }
      else
      {
        int ofst = pos - start;
	int n = l - f;
        int newsize = stfinish - start + n;
        T * oldstart = start;
        start = (T *) realloc (start, newsize * sizeof (T));
        finish = start + (finish - oldstart);
        stfinish = start + newsize;
        // new value of pos
        pos = start + ofst;
	{
	  // make space for n elements
	  iterator iter = pos;
	  int i = 0;
	  InputIterator ii = f;
	  for (; i < n; iter++, i++, ii++)
	  {
	    *(iter + n) = *iter;
	    *iter = *ii;
	  }
	}
      }
    }
private:
    void allocate () {
        if (start == NULL) {
            start = finish = (T*)malloc (10 * sizeof(T));
            stfinish = start + 10;
        } else {
            T *oldstart = start;
            int newsize = (finish - start) * 2;
            start = (T*)realloc (start, newsize * sizeof(T));
            finish = start + (finish - oldstart);
            stfinish = start + newsize;
        }
    }

    // start of memory
    T *start;
    // end of used memory
    T *finish;
    // end of memory
    T *stfinish;
};

}

#endif
