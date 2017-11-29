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
// file: hash_map.h
// author: Gautam Chakrabarti <gautam@pathscale.com>
//

#ifndef _hash_map_h_included
#define _hash_map_h_included

namespace Instr {

template <class T1, class T2> class pair
{
  public:
  typedef T1 first_type;
  typedef T2 second_type;
  T1 first;
  T2 second;

  pair () : first(), second() {}
  pair (const T1& f, const T2& s) : first (f), second (s) {}
};

#define mapsize 1009

template <class T1, class T2> struct _Hash_node;
template <class T1, class T2> class hash_map;

template <class _Key, class _Data>
struct _Hash_map_iterator
{
  typedef _Hash_map_iterator<_Key, _Data> iterator;
  typedef _Hash_node<_Key, _Data> _Bucket_type;
  typedef hash_map<_Key, _Data> hash_map_type;

  _Bucket_type * curr;
  hash_map_type * h;

  _Hash_map_iterator () : curr (NULL), h(NULL) {}
  _Hash_map_iterator (_Bucket_type * b, hash_map_type * h)
      : curr (b), h (h) {}

  bool operator== (const iterator& it) const
  {
    return curr == it.curr;
  }
  bool operator!= (const iterator& it) const
  {
    return curr != it.curr;
  }
  iterator& operator++ ()
  {
    _Bucket_type * old = curr;
    curr = curr->next;
    if (!curr)
    {
      int bucket_num = ((long)old->val.first) % mapsize;
      while (!curr && ++bucket_num < mapsize)
        curr = h->start[bucket_num];
    }
    return *this;
  }
  pair<_Key, _Data>& operator* ()
  {
    return curr->val;
  }
};

template <class T1, class T2> struct _Hash_node
{
  typedef pair<T1, T2> val_type;
  _Hash_node* next;
  val_type val;
  typename val_type::second_type second; // this is a hack
  void Set_val (val_type v)
  {
    val = v;
    second = v.second;
  }
};

template <class _Key, class _Data> class hash_map
{
  typedef pair<_Key, _Data> _Node;
  typedef _Hash_node<_Key, _Data> _Bucket_type;
public:
  friend struct _Hash_map_iterator<_Key, _Data>;
  typedef _Hash_map_iterator<_Key, _Data> iterator;
  hash_map () { for (int i=0; i<mapsize; i++) start[i] = NULL; }

  _Data& operator[] (const _Key& k)
  {
    long key = (long) k;
    int bucket_num = key % mapsize;

    _Bucket_type * f = start[bucket_num];
    for (; f; f = f->next)
      if (f->val.first == k)
        return f->val.second;

    _Bucket_type * newobj = (_Bucket_type *) malloc (sizeof (_Bucket_type));
    newobj->next = start[bucket_num];
    _Node tmp (k, _Data());
    newobj->Set_val (tmp);
    start[bucket_num] = newobj;

    return newobj->val.second;
  }

  iterator begin ()
  {
    for (int i=0; i<mapsize; ++i)
      if (start[i])
        return iterator (start[i], this);
    return end();
  }

  iterator end ()
  {
    return iterator (NULL, this);
  }

private:
    _Bucket_type * start[mapsize];
};

}

#endif
