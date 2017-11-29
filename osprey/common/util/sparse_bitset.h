/*
 Copyright (C) 2010, Hewlett-Packard Development Company, L.P.
 All Rights Reserved.

 Open64 is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 2
 of the License, or (at your option) any later version.

 Open64 is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 MA  02110-1301, USA.
*/
#ifndef sparse_bitset__INCLUDED
#define sparse_bitset__INCLUDED

#include <iostream>
#include <limits.h>
#include <assert.h>
#include "cxx_memory.h"

/* Fundamental storage type for bitmap.  */
typedef unsigned long BITMAP_WORD;

#define BITMAP_WORD_BITS (sizeof(BITMAP_WORD) * CHAR_BIT)

/* Number of words to use for each element in the linked list.  */

#define BITMAP_ELEMENT_WORDS ((128 + BITMAP_WORD_BITS - 1) / BITMAP_WORD_BITS)

/* Number of bits in each actual element of a bitmap.  */

#define BITMAP_ELEMENT_ALL_BITS (BITMAP_ELEMENT_WORDS * BITMAP_WORD_BITS)

typedef struct SparseBitSetElementDef
{
  SparseBitSetElementDef *_next;           // Next element
  SparseBitSetElementDef *_prev;           // Prev element
  UINT32 _idx;                             // index of this element
  BITMAP_WORD _bits[BITMAP_ELEMENT_WORDS]; // Bits that are set
} SparseBitSetElement;

static SparseBitSetElement zeroBitElem;

template <class T>
class SparseBitSet
{
public:
  class SparseBitSetIterator 
  {
  public:
    SparseBitSetIterator(const SparseBitSet<T> *bitSet, T minIdx) 
    {
      UINT32 startBit = (UINT32)minIdx;
      _currElem = bitSet->_firstElem;
      // Advance _currElem until it is not before the block containing 
      // startBit.
      while (1) {
        if (!_currElem) {
          _currElem = &zeroBitElem;
          break;
        }
        if (_currElem->_idx >= startBit / BITMAP_ELEMENT_ALL_BITS)
          break;
        _currElem = _currElem->_next;
      }

      // We might have gone past the start bit, so reinitialize it.
      if (_currElem->_idx != startBit / BITMAP_ELEMENT_ALL_BITS)
        startBit = _currElem->_idx * BITMAP_ELEMENT_ALL_BITS;

      // Initialize for what is now startBit.
      _wordNum = startBit / BITMAP_WORD_BITS % BITMAP_ELEMENT_WORDS;
      _bits = _currElem->_bits[_wordNum];
      _bits >>= startBit % BITMAP_WORD_BITS;

      // If this word is zero, we must make sure we're not pointing at the
      // first bit, otherwise our incrementing to the next word boundary
      // will fail.  It won't matter if this increment moves us into the
      // next word. 
      startBit += !_bits;
      _bitNum = startBit;
    }

    bool operator!=(const UINT32 i) 
    {
      assert(i == 0);
      // If our current word is nonzero, it contains the bit we want. 
      if (_bits) {
      next_bit:
        while (!(_bits & 1)) {
          _bits >>= 1;
          _bitNum += 1;
        }
        return true;
      }

      // Round up to the word boundary.  We might have just iterated past
      // the end of the last word, hence the -1.  It is not possible for
      // bit_no to point at the beginning of the now last word.  */
      _bitNum = ((_bitNum + BITMAP_WORD_BITS - 1)
                / BITMAP_WORD_BITS * BITMAP_WORD_BITS);
      _wordNum++;

      while (1) {
        // Find the next nonzero word in this elt.
        while (_wordNum != BITMAP_ELEMENT_WORDS) {
          _bits = _currElem->_bits[_wordNum];
          if (_bits)
            goto next_bit;
          _bitNum += BITMAP_WORD_BITS;
          _wordNum++;
        }

        // Advance to the next element.  */
        _currElem = _currElem->_next;
        if (!_currElem)
          return false;
        _bitNum = _currElem->_idx * BITMAP_ELEMENT_ALL_BITS;
        _wordNum = 0;
      }
    }

    // Preincrement
    inline SparseBitSetIterator& operator++()
    {
      _bits >>= 1;
      _bitNum += 1;
      return *this;
    }

    // Postincrement.
    inline SparseBitSetIterator operator++(int) 
    {
      SparseBitSetIterator tmp = *this;
      ++*this;
      return tmp;
    }

    T operator*() const 
    {
      return (T)_bitNum;
    }
      
  private:
    SparseBitSetElement *_currElem; // Pointer to the current bitmap element
    UINT32 _wordNum;                // Word within the current element.
    BITMAP_WORD _bits;              // Contents of the actually processed word.
                                    // When finding next bit it is shifted 
                                    // right, so that the actual bit is always 
                                    // the least significant bit of ACTUAL.
    UINT32 _bitNum;                 // Current bit number
  };

  SparseBitSetElement *allocElem() 
  {
    SparseBitSetElement *ptr =
                         TYPE_MEM_POOL_ALLOC(SparseBitSetElement, _memPool);
    ptr->_next = ptr->_prev = NULL;
    memset(ptr->_bits, 0, sizeof(ptr->_bits));
    return ptr;
  }

  void
  freeElem(SparseBitSetElement *elt)
  {
    SparseBitSetElement *next = elt->_next;
    SparseBitSetElement *prev = elt->_prev;

    if (prev)
      prev->_next = next;

    if (next)
      next->_prev = prev;

    if (_firstElem == elt)
      _firstElem = next;

    // Since the first thing we try is to insert before current,
    // make current the next entry in preference to the previous.
    if (_currElem == elt) {
      _currElem = next != 0 ? next : prev;
      if (_currElem)
        _currIdx = _currElem->_idx;
      else
        _currIdx = 0;
    }
    MEM_POOL_FREE(_memPool, elt);
  }

  SparseBitSetElement *findElem(UINT32 bit)
  {
    SparseBitSetElement *element;
    UINT32 elemIdx = bit / BITMAP_ELEMENT_ALL_BITS;

    if (_currElem == 0 || _currIdx == elemIdx)
      return _currElem;

    if (_currIdx < elemIdx)
      // elemIdx is beyond _currIdx. Search from _currElem forward. 
      for (element = _currElem;
           element->_next != 0 && element->_idx < elemIdx;
           element = element->_next)
        ;

    else if (_currIdx / 2 < elemIdx)
      // elemIdx is less than _currIdx and closer to _currIdx than to 0
      // Search from _currElem backward. 
      for (element = _currElem;
           element->_prev != 0 && element->_idx > elemIdx;
           element = element->_prev)
        ;

    else
      // elemIdx is less than _currIdx and closer to 0 than to
      // _currIdx.  Search from _firstElem forward. 
      for (element = _firstElem;
           element->_next != 0 && element->_idx < elemIdx;
           element = element->_next)
        ;

    // `element' is the nearest to the one we want.  If it's not the one we
    //  want, the one we want doesn't exist.
    _currElem = element;
    _currIdx = element->_idx;
    if (element != 0 && element->_idx != elemIdx)
      element = 0;

    return element;
  }

  void elementLink(SparseBitSetElement *element)
  {
    SparseBitSetElement *ptr;

    // If this is the first and only element, set it in.
    if (_firstElem == 0) {
      element->_next = element->_prev = NULL;
      _firstElem = element;
    }

    // If this index is less than that of the current element, it goes someplace
    // before the current element. 
    else if (element->_idx < _currIdx) {
      for (ptr = _currElem; ptr->_prev != 0 && ptr->_prev->_idx > element->_idx;
           ptr = ptr->_prev)
        ;
      if (ptr->_prev)
        ptr->_prev->_next = element;
      else
        _firstElem = element;

      element->_prev = ptr->_prev;
      element->_next = ptr;
      ptr->_prev = element;
    }

    // Otherwise, it must go someplace after the current element.
    else {
      for (ptr = _currElem; ptr->_next != 0 && ptr->_next->_idx < element->_idx;
           ptr = ptr->_next)
        ;
      if (ptr->_next)
        ptr->_next->_prev = element;

      element->_next = ptr->_next;
      element->_prev = ptr;
      ptr->_next = element;
    }

    // Set up so this is the first element searched.
    _currElem = element;
    _currIdx = element->_idx;
  }

  // Insert a new uninitialized element after element elt.  
  // If elt is NULL, insert the element at the start.  Return the
  // new element.
  SparseBitSetElement *
  insertAfter(SparseBitSetElement *elt, UINT32 idx)
  {
    SparseBitSetElement *node = allocElem();
    node->_idx = idx;
    if (!elt) {
      if (!_currElem) {
        _currElem = node;
        _currIdx = idx;
      }
      node->_next = _firstElem;
      if (node->_next)
        node->_next->_prev = node;
      _firstElem = node;
      node->_prev = NULL;
    } else {
      assert(_currElem);
      node->_next = elt->_next;
      if (node->_next)
        node->_next->_prev = node;
      elt->_next = node;
      node->_prev = elt;
    }
    return node;
  }

  bool isElementZero(SparseBitSetElement *element) 
  {
    for (UINT32 i = 0; i < BITMAP_ELEMENT_WORDS; i++)
      if (element->_bits[i] != 0)
        return false;
    return true;
  }

  SparseBitSetElement *_firstElem; // First element in linked list.
  SparseBitSetElement *_currElem;  // Last element looked at
  UINT32 _currIdx;                 // Index of last element looked at.
  MEM_POOL *_memPool;              // Pool to allocate elements from.

public:
  typedef SparseBitSetIterator iterator;

  SparseBitSet(MEM_POOL *memPool = Malloc_Mem_Pool) :
    _firstElem(NULL), 
    _currElem(NULL),
    _currIdx(0),
    _memPool(memPool)
  {
    T obj;
    assert(sizeof((UINT32)obj) <= sizeof(UINT32));
  }

  SparseBitSet(const SparseBitSet &rhs) :
    _firstElem(NULL), 
    _currElem(NULL),
    _currIdx(0),
    _memPool(Malloc_Mem_Pool)
  {
    SparseBitSetElement *fromPtr = NULL;
    SparseBitSetElement *toPtr = NULL;

    // Copy elements in forward direction one at a time.
    for (fromPtr = rhs._firstElem; fromPtr; fromPtr = fromPtr->_next) {
      SparseBitSetElement *toElt = allocElem();
      toElt->_idx = fromPtr->_idx;
      memcpy(toElt->_bits, fromPtr->_bits, sizeof (toElt->_bits));

      if (toPtr == NULL) {
        _firstElem = _currElem = toElt;
        _currIdx = toElt->_idx;
        toElt->_next = toElt->_prev = NULL;
      } else {
        toElt->_prev = toPtr;
        toElt->_next = NULL;
        toPtr->_next = toElt;
      }
      toPtr = toElt;
    }
  }

  ~SparseBitSet() { clear(); }

  bool setBit(T idx)
  {
    UINT32 bit = (UINT32)idx;
    SparseBitSetElement *ptr = findElem(bit);
    UINT32 wordNum = bit / BITMAP_WORD_BITS % BITMAP_ELEMENT_WORDS;
    UINT32 bitNum  = bit % BITMAP_WORD_BITS;
    BITMAP_WORD bitVal = ((BITMAP_WORD) 1) << bitNum;

    bool changed = true;
    if (ptr == NULL) {
      ptr = allocElem();
      ptr->_idx = bit / BITMAP_ELEMENT_ALL_BITS;
      ptr->_bits[wordNum] = bitVal;
      elementLink(ptr);
    } else {
      if (ptr->_bits[wordNum] & bitVal)
        changed = false;
      ptr->_bits[wordNum] |= bitVal;
    }
    return changed;
  }

  bool clearBit(T idx)
  {
    UINT32 bit = (UINT32)idx;
    SparseBitSetElement *ptr = findElem(bit);
    UINT32 wordNum = bit / BITMAP_WORD_BITS % BITMAP_ELEMENT_WORDS;
    UINT32 bitNum  = bit % BITMAP_WORD_BITS;
    BITMAP_WORD bitVal = ((BITMAP_WORD) 1) << bitNum;

    bool changed = false;
    if (ptr != NULL) {
      if (ptr->_bits[wordNum] & bitVal) {
        changed = true;
        ptr->_bits[wordNum] &= (~bitVal);
      }
      if (isElementZero(ptr))
        freeElem(ptr);
    }
    return changed;
  }

  // Union rhs into 'this'. Return true if 'this' changes. 
  bool setUnion(const SparseBitSet &rhs)
  {
    SparseBitSetElement *thisElem = _firstElem;
    SparseBitSetElement *rhsElem = rhs._firstElem;
    SparseBitSetElement *thisPrevElem = NULL;
    bool changed = false;

    while (rhsElem) {
      if (!thisElem || rhsElem->_idx < thisElem->_idx) {
        /* Copy rhsElem.  */
        SparseBitSetElement *dst = insertAfter(thisPrevElem, rhsElem->_idx) ;
        memcpy(dst->_bits, rhsElem->_bits, sizeof (dst->_bits));
        thisPrevElem = dst;
        rhsElem = rhsElem->_next;
        changed = true;
      } else if (thisElem->_idx < rhsElem->_idx) {
        thisPrevElem = thisElem;
        thisElem = thisElem->_next;
      } else {
        // Matching elts, generate this |= rhs.
        UINT32 ix;
        if (changed) {
          for (ix = BITMAP_ELEMENT_WORDS; ix--;) {
            BITMAP_WORD r = thisElem->_bits[ix] | rhsElem->_bits[ix];
            thisElem->_bits[ix] = r;
          }
        } else {
          for (ix = BITMAP_ELEMENT_WORDS; ix--;) {
            BITMAP_WORD r = thisElem->_bits[ix] | rhsElem->_bits[ix];
            if (thisElem->_bits[ix] != r) {
              changed = true;
              thisElem->_bits[ix] = r;
            }
          }
        }
        rhsElem = rhsElem->_next;
        thisPrevElem = thisElem;
        thisElem = thisElem->_next;
      }
    }
    assert (!_currElem == !_firstElem);
    if (_currElem)
      _currIdx = _currElem->_idx;
    return changed;
  }

  // this &= rhs
  void setIntersect(const SparseBitSet &rhs)
  {
    SparseBitSetElement *thisElt = _firstElem;
    SparseBitSetElement *rhsElt = rhs._firstElem;
    SparseBitSetElement *next;

    while (thisElt && rhsElt) {
      if (thisElt->_idx < rhsElt->_idx) {
        next = thisElt->_next;
        freeElem(thisElt);
        thisElt = next;
      } else if (rhsElt->_idx < thisElt->_idx)
        rhsElt = rhsElt->_next;
      else {
        // Matching elts, generate this &= rhs. 
        UINT32 ix;
        BITMAP_WORD ior = 0;

        for (ix = BITMAP_ELEMENT_WORDS; ix--;) {
          BITMAP_WORD r = thisElt->_bits[ix] & rhsElt->_bits[ix];
          thisElt->_bits[ix] = r;
          ior |= r;
        }
        next = thisElt->_next;
        if (!ior)
          freeElem(thisElt);
        thisElt = next;
        rhsElt = rhsElt->_next;
      }
    }
    // Delete till end of list
    while (thisElt) {
      next = thisElt->_next;
      freeElem(thisElt);
      thisElt = next;
    }
    assert (!_currElem == !_firstElem);
    assert (!_currElem || _currIdx == _currElem->_idx);
  }

  void clear()
  {
    SparseBitSetElement *nelem = NULL;
    SparseBitSetElement *elem = _firstElem;
    while (elem) {
      nelem = elem->_next;
      MEM_POOL_FREE(_memPool, elem);
      elem = nelem;
    }
    _firstElem = _currElem = NULL;
    _currIdx = 0;
  }

  SparseBitSet& operator=(const SparseBitSet &rhs)
  {
    SparseBitSetElement *fromPtr = NULL;
    SparseBitSetElement *toPtr = NULL;

    clear();
    // Copy elements in forward direction one at a time.
    for (fromPtr = rhs._firstElem; fromPtr; fromPtr = fromPtr->_next) {
      SparseBitSetElement *toElt = allocElem();
      toElt->_idx = fromPtr->_idx;
      memcpy(toElt->_bits, fromPtr->_bits, sizeof (toElt->_bits));

      if (toPtr == NULL) {
        _firstElem = _currElem = toElt;
        _currIdx = toElt->_idx;
        toElt->_next = toElt->_prev = NULL;
      } else {
        toElt->_prev = toPtr;
        toElt->_next = NULL;
        toPtr->_next = toElt;
      }
      toPtr = toElt;
    }
    return *this;
  }

  bool operator==(const SparseBitSet &rhs)
  {
    SparseBitSetElement *ptr;
    SparseBitSetElement *ptr_rhs;
    for (ptr = _firstElem, ptr_rhs = rhs._firstElem; 
         ptr && ptr_rhs; 
         ptr = ptr->_next, ptr_rhs = ptr_rhs->_next) {
      if (ptr->_idx != ptr_rhs->_idx)
        return false;
      if (memcmp(ptr->_bits, ptr_rhs->_bits, sizeof(ptr->_bits)))
        return false;
    }

    if(ptr == NULL && ptr_rhs == NULL)
      return true;
    return false;
  }

  // Return true if this AND rhs is not empty.
  bool
  intersect(const SparseBitSet& rhs) const
  {
    SparseBitSetElement *thisElt;
    SparseBitSetElement *rhsElt;
    UINT32 ix;

    for (thisElt = _firstElem, rhsElt = rhs._firstElem; thisElt && rhsElt;) {
      if (thisElt->_idx < rhsElt->_idx)
        thisElt = thisElt->_next;
      else if (rhsElt->_idx < thisElt->_idx)
        rhsElt = rhsElt->_next;
      else {
        for (ix = BITMAP_ELEMENT_WORDS; ix--;)
          if (thisElt->_bits[ix] & rhsElt->_bits[ix])
            return true;
        thisElt = thisElt->_next;
        rhsElt = rhsElt->_next;
      }
    }
    return false;
  }

  bool isSet(T idx)
  {
    UINT32 bit = (UINT32)idx;
    SparseBitSetElement *ptr;
    UINT32 bitNum;
    UINT32 wordNum;

    ptr = findElem(bit);
    if (ptr == NULL)
      return false;

    bitNum = bit % BITMAP_WORD_BITS;
    wordNum = bit / BITMAP_WORD_BITS % BITMAP_ELEMENT_WORDS;
    return (ptr->_bits[wordNum] >> bitNum) & 1;
  }

  // 'this' &= ~rhs. Returns true if 'this' changes
  bool
  setDiff(const SparseBitSet& rhs)
  {
    SparseBitSetElement *thisElt = _firstElem;
    SparseBitSetElement *rhsElt = rhs._firstElem;
    SparseBitSetElement *next;
    BITMAP_WORD changed = 0;

    while (thisElt && rhsElt) {
      if (thisElt->_idx < rhsElt->_idx)
        thisElt = thisElt->_next;
      else if (rhsElt->_idx < thisElt->_idx)
        rhsElt = rhsElt->_next;
      else {
        // Matching elts, generate this &= ~rhs. 
        UINT32 ix;
        BITMAP_WORD ior = 0;

        for (ix = BITMAP_ELEMENT_WORDS; ix--;) {
          BITMAP_WORD cleared = thisElt->_bits[ix] & rhsElt->_bits[ix];
          BITMAP_WORD r = thisElt->_bits[ix] ^ cleared;
          thisElt->_bits[ix] = r;
          changed |= cleared;
          ior |= r;
        }
        next = thisElt->_next;
        if (!ior)
          freeElem(thisElt);
        thisElt = next;
        rhsElt = rhsElt->_next;
      }
    }
    assert (!_currElem == !_firstElem);
    assert (!_currElem || _currIdx == _currElem->_idx);
    return changed != 0;
  }

  // return true if rhs is a subset of this.
  bool
  subset(const SparseBitSet& rhs) const
  {
    SparseBitSetElement *thisElt = _firstElem;
    SparseBitSetElement *rhsElt = rhs._firstElem;
    if (rhs.isEmpty())
      return true;
    if (isEmpty())
      return false;
    
    // rhs's element must be found in this
    while (rhsElt && thisElt) {
      if (thisElt->_idx > rhsElt->_idx) {
        return false;
      }
      else if (thisElt->_idx == rhsElt->_idx) {
        // compare each word
        for (UINT32 ix = 0; ix < BITMAP_ELEMENT_WORDS; ix++) {
          if (thisElt->_bits[ix] != (thisElt->_bits[ix] | rhsElt->_bits[ix]))
            return false;
        }
        rhsElt = rhsElt->_next;
        thisElt = thisElt->_next;
      }
      else {
        thisElt = thisElt->_next;
      }
    }
    // all thisElt's idx smaller than rhsElt's idx
    if(rhsElt)
      return false;
    return true;
  }

  bool isEmpty() const { return _firstElem == NULL; }

  UINT32 numElements() const {
    UINT32 count = 0;
    SparseBitSetElement *elt;

    for (elt = _firstElem; elt; elt = elt->_next)
      count += 1;

    return count;
  }

  UINT32 numBits() const
  {
    UINT32 count = 0;
    SparseBitSetElement *elt;
    UINT32 ix;

    for (elt = _firstElem; elt; elt = elt->_next) {
      for (ix = 0; ix != BITMAP_ELEMENT_WORDS; ix++) {
        // Note that popcountl matches BITMAP_WORD in type, so the actual size
        // of BITMAP_WORD is not material. 
        count += __builtin_popcountl(elt->_bits[ix]);
      }
    }
    return count;
  }

  void print(FILE *file) const
  {
    const char *comma = "";
    SparseBitSetIterator si(this, 0);
    while (si != 0) {
      fprintf(file, "%s%d", comma, *si);
      si++;
      comma = ", ";
    }
  }

  void debug(FILE *file)
  {
    SparseBitSetElement *ptr;

    fprintf (file, "\nfirst = %p current = %p _idx = %u\n",
             (void *)_firstElem, (void *)_currElem, _currIdx);

    for (ptr = _firstElem; ptr; ptr = ptr->_next) {
      UINT32 i, j, col = 26;

      fprintf (file, "\t%p next = %p prev = %p _idx = %u\n\t\tbits = {",
               (void *)ptr, (void *)ptr->_next, (void *)ptr->_prev, ptr->_idx);

      for (i = 0; i < BITMAP_ELEMENT_WORDS; i++) 
      {
        for (j = 0; j < BITMAP_WORD_BITS; j++) 
        {
          if ((ptr->_bits[i] >> j) & 1) 
          {
            if (col > 70) 
            {
              fprintf (file, "\n\t\t\t");
              col = 24;
            }
            fprintf (file, " %u", (ptr->_idx * BITMAP_ELEMENT_ALL_BITS
                                     + i * BITMAP_WORD_BITS + j));
            col += 4;
          }
        }
      }
      fprintf (file, " }\n");
    }
  }
};

template<class T>
std::ostream& operator<<(std::ostream& str, const SparseBitSet<T>& bs)
{
  const char *comma = "";
  typename SparseBitSet<T>::iterator si(&bs, 0);
  while (si != 0) {
    str << comma << *si;
    si++;
    comma = ", ";
  }
  return str;
}

#endif // sparse_bitset__INCLUDED
