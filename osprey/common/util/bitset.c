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


/* ====================================================================
 * ====================================================================
 *
 * Module: bitset.c
 *
 * Revision history:
 *  05-03-93 - Original Version
 *
 * Description:
 *
 *      Bitset implementation.
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef _KEEP_RCS_ID
static const char source_file[] = __FILE__;
#endif

#include "defs.h"
#include "mempool.h"
#include "errors.h"
#include "bitset.h"

/* A designated bad memory pool to support assertions that the sets
 * shouldn't have to grow:
 */
static MEM_POOL bad_pool_struct;
static MEM_POOL *bad_pool = &bad_pool_struct;

/* ====================================================================
 *
 *  bs_Malloc
 *
 *  Return a new BS*
 *
 *      length  is the number of words to allocate for members
 *      pool    is a pool in which to allocate it
 *
 *
 * ====================================================================
 */

static BS*
bs_Malloc(
  size_t    length,
  MEM_POOL *pool
)
{
    BS *new_set;

    Is_True(pool != bad_pool,("Shouldn't be allocating."));

    new_set = (BS *) TYPE_MEM_POOL_ALLOC_N(BS_WORD,pool,length + 1);

    BS_word_count(new_set) = length;
    return new_set;
}

/* ====================================================================
 *
 *  bs_Realloc
 *
 *  Grows the allocation of a BS, potentially returning a new pointer
 *  to it.
 *
 *      set     is the set to grow
 *      length  is the new number of words to allocate for members
 *      pool    is a pool to use for this
 *
 * ====================================================================
 */

static BS*
bs_Realloc(
  BS       *set,
  size_t    length,
  MEM_POOL *pool
)
{
  BS    *new_set;
  BS_ELT i;
  BS_ELT old_length = BS_word_count(set);
  size_t new_length;

  Is_True(pool != bad_pool,("Shouldn't be allocating."));

  if (length <= old_length) return set;

  // extend length to be a power of 2
  for (new_length = 2; new_length < length; new_length <<= 1);
  length = new_length;

  new_set = (BS *)TYPE_MEM_POOL_REALLOC_N(BS_WORD, pool, set, old_length+1,
					  length+1);

  if (!MEM_POOL_Zeroed(pool))
    for ( i = old_length; i < length; ++i )
      BS_word(new_set,i) = bs_ZEROS;

  BS_word_count(new_set) = length;

  return new_set;
}

/* ====================================================================
 *
 *  BS_Create
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_Create(
  BS_ELT    size,
  MEM_POOL *pool
)
{
  Is_True(size >= 0,("BS_Create called with negative size."));

  return bs_Malloc(bs_QBPW(size + (BITS_PER_BS_WORD - 1)),pool);
}

/* ====================================================================
 *
 *  BS_Size_Alloc_Size
 *
 *  See interface description
 *
 * ====================================================================
 */

extern size_t
BS_Size_Alloc_Size(
  BS_ELT size
)
{
  return (1 + bs_QBPW(size + (BITS_PER_BS_WORD - 1) )) * sizeof(BS_WORD);
}

/* ====================================================================
 *
 *  BS_Alloc_Size
 *
 *  See interface description
 *
 * ====================================================================
 */

extern size_t
BS_Alloc_Size(
  BS *set
)
{
  return (BS_word_count(set) + 1) * sizeof(BS_WORD);
}

/* ====================================================================
 *
 *  BS_ClearD
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_ClearD(
  BS *set
)
{
  BS_ELT i;

  for ( i = 0; i < BS_word_count(set); ++i )
    BS_word(set,i) = bs_ZEROS;

  return set;
}

/* ====================================================================
 *
 *  BS_Create_Empty
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_Create_Empty(
  BS_ELT    size,
  MEM_POOL *pool
)
{
  BS *set = BS_Create(size, pool);
  if (!MEM_POOL_Zeroed(pool))
    BS_ClearD(set);
  return set;
}

/* ====================================================================
 *
 *  BS_Resize
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_ResizeD(
  BS*	    set,
  BS_ELT    new_size,
  MEM_POOL *pool
)
{
  BS_ELT new_words = bs_QBPW(new_size + (BITS_PER_BS_WORD - 1));

  if ( new_words > BS_word_count(set) ) {
    set = bs_Realloc(set,new_words,pool);
  }
  return set;
}

#ifndef MONGOOSE_BE
/* ====================================================================
 *
 *  BS_Range
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_Range(
  BS_ELT    low,
  BS_ELT    high,
  MEM_POOL *pool
)
{
  return BS_RangeD(BS_Create(high + 1,pool),low,high,bad_pool);
}
#endif /* MONGOOSE_BE */

/* ====================================================================
 *
 *  BS_RangeD
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_RangeD(
  BS*       set,
  BS_ELT    low,
  BS_ELT    high,
  MEM_POOL *pool
)
{
  BS_ELT  first_w, last_w, first_b, last_b;
  BS_ELT  i;

  Is_True(low >= 0,
            ("BS_RangeD called with negative low element."));

  Is_True(high - low + 1 >= 0,
            ("BS_RangeD called with negative range size."));

  if ( low > high )
    return BS_ClearD(set);

  first_w = bs_QBPW(low);     /* index of first non 0 word */
  last_w  = bs_QBPW(high);    /* index of last non 0 word  */
  first_b = bs_QBPB(low);
  last_b  = bs_QBPB(high);

  /* Reallocate if necessary.
   */
  if ( last_w >= BS_word_count(set) )
    set = bs_Realloc(set,last_w + 1,pool);

  set = BS_ClearD(set);

  /* Set every byte above the first up to and including the last to be
   * all ones.  This complexity is required in order to make the
   * representation endian independent.  We win back with a fast
   * endian independent ffo for machines with load byte instructions.
   */

  /* Set every word above the first and below the last to be all ones.
   */
  for ( i = first_w + 1; i < last_w; ++i )
    BS_word(set,i) = bs_ONES;

  /* Set every byte in the first word above the first byte to be all
   * ones.
   */
  /* for ( i = first_b; i < bs_PBPB(first_w + 1); ++i ) shin 03-29-95 */
  for ( i = first_b; i < bs_PBytesPW(first_w + 1) && i <= last_b; ++i )
    BS_byte(set,i) = (BS_BYTE)bs_ONES;

  /* Set every byte in the last word up to and including the last byte
   * to be all ones.
   */
  /* for ( i = bs_PBPB(last_w); i <= last_b; ++i ) fchow 04-11-95 */
  if (first_w != last_w)
    for ( i = bs_PBytesPW(last_w); i <= last_b; ++i )
      BS_byte(set,i) = (BS_BYTE)bs_ONES;

  /* Fix up the first and last bytes.  Order is important!  Remember
   * that we have already set all the bits in the last byte and that
   * the first and last bytes might be the same.
   */
  BS_byte(set,first_b) = bs_ONES << bs_RBPB(low);
  BS_byte(set,last_b) &=
    bs_ONES >> ((BITS_PER_BS_WORD - 1) - bs_RBPB(high));

  return set;
}

/* ====================================================================
 *
 *  BS_Singleton
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_Singleton(
  BS_ELT element,
  MEM_POOL *pool
)
{
  return BS_SingletonD(BS_Create(element + 1,pool),
                       element,
                       bad_pool);
}

/* ====================================================================
 *
 *  BS_SingletonD
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_SingletonD(
  BS       *set,
  BS_ELT    element,
  MEM_POOL *pool
)
{
  BS_ELT word;

  Is_True(element >= 0,
            ("BS_SingletonD called with negative element."));

  word = bs_QBPW(element);

  /* Reallocate if necessary.
   */
  if ( word >= BS_word_count(set) )
    set = bs_Realloc(set,word + 1,pool);

  set = BS_ClearD(set);
  BS_byte(set,bs_QBPB(element)) = bs_ONE << bs_RBPB(element);

  return set;
}

/* ====================================================================
 *
 *  BS_Universe
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_Universe(
  BS_ELT    size,
  MEM_POOL *pool
)
{
  return BS_UniverseD(BS_Create(size,pool),size,bad_pool);
}

/* ====================================================================
 *
 *  BS_UniverseD
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_UniverseD(
  BS       *set,
  BS_ELT    size,
  MEM_POOL *pool
)
{
  return BS_RangeD(set,0,size - 1,bad_pool);
}

/* ====================================================================
 *
 *  BS_Copy
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_Copy(
  BS       *set,
  MEM_POOL *pool
)
{
  size_t size;
  BS*    newset;
  BS_ELT i;

  size = BS_word_count(set);
  newset = bs_Malloc(size,pool);

  for ( i = 0; i < size; ++i )
    BS_word(newset,i) = BS_word(set,i);

  return newset;
}

/* ====================================================================
 *
 *  BS_CopyD
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_CopyD(
  BS       *set1,
  BS       *set2,
  MEM_POOL *pool
)
{
  BS_ELT i;
  BS_ELT size1 = BS_word_count(set1);
  BS_ELT size2 = BS_word_count(set2);

  /* Reallocate if necessary.
   */
  if ( size1 < size2 )
    set1 = bs_Realloc(set1,size2,pool);
  else {
    /* Else zero out excess.
     */
    for ( i = size2; i < size1; ++i )
      BS_word(set1,i) = bs_ZEROS;
  }

  /* Copy in common part.
   */
  for ( i = 0; i < size2; ++i )
    BS_word(set1,i) = BS_word(set2,i);

  return set1;
}

/* Mapping from 8 bit unsigned integers to the index of the first one
 * bit:
 */
static const mUINT8 first_one [256] = {
  0, /*   0 */
  0, /*   1 */
  1, /*   2 */
  0, /*   3 */
  2, /*   4 */
  0, /*   5 */
  1, /*   6 */
  0, /*   7 */
  3, /*   8 */
  0, /*   9 */
  1, /*  10 */
  0, /*  11 */
  2, /*  12 */
  0, /*  13 */
  1, /*  14 */
  0, /*  15 */
  4, /*  16 */
  0, /*  17 */
  1, /*  18 */
  0, /*  19 */
  2, /*  20 */
  0, /*  21 */
  1, /*  22 */
  0, /*  23 */
  3, /*  24 */
  0, /*  25 */
  1, /*  26 */
  0, /*  27 */
  2, /*  28 */
  0, /*  29 */
  1, /*  30 */
  0, /*  31 */
  5, /*  32 */
  0, /*  33 */
  1, /*  34 */
  0, /*  35 */
  2, /*  36 */
  0, /*  37 */
  1, /*  38 */
  0, /*  39 */
  3, /*  40 */
  0, /*  41 */
  1, /*  42 */
  0, /*  43 */
  2, /*  44 */
  0, /*  45 */
  1, /*  46 */
  0, /*  47 */
  4, /*  48 */
  0, /*  49 */
  1, /*  50 */
  0, /*  51 */
  2, /*  52 */
  0, /*  53 */
  1, /*  54 */
  0, /*  55 */
  3, /*  56 */
  0, /*  57 */
  1, /*  58 */
  0, /*  59 */
  2, /*  60 */
  0, /*  61 */
  1, /*  62 */
  0, /*  63 */
  6, /*  64 */
  0, /*  65 */
  1, /*  66 */
  0, /*  67 */
  2, /*  68 */
  0, /*  69 */
  1, /*  70 */
  0, /*  71 */
  3, /*  72 */
  0, /*  73 */
  1, /*  74 */
  0, /*  75 */
  2, /*  76 */
  0, /*  77 */
  1, /*  78 */
  0, /*  79 */
  4, /*  80 */
  0, /*  81 */
  1, /*  82 */
  0, /*  83 */
  2, /*  84 */
  0, /*  85 */
  1, /*  86 */
  0, /*  87 */
  3, /*  88 */
  0, /*  89 */
  1, /*  90 */
  0, /*  91 */
  2, /*  92 */
  0, /*  93 */
  1, /*  94 */
  0, /*  95 */
  5, /*  96 */
  0, /*  97 */
  1, /*  98 */
  0, /*  99 */
  2, /* 100 */
  0, /* 101 */
  1, /* 102 */
  0, /* 103 */
  3, /* 104 */
  0, /* 105 */
  1, /* 106 */
  0, /* 107 */
  2, /* 108 */
  0, /* 109 */
  1, /* 110 */
  0, /* 111 */
  4, /* 112 */
  0, /* 113 */
  1, /* 114 */
  0, /* 115 */
  2, /* 116 */
  0, /* 117 */
  1, /* 118 */
  0, /* 119 */
  3, /* 120 */
  0, /* 121 */
  1, /* 122 */
  0, /* 123 */
  2, /* 124 */
  0, /* 125 */
  1, /* 126 */
  0, /* 127 */
  7, /* 128 */
  0, /* 129 */
  1, /* 130 */
  0, /* 131 */
  2, /* 132 */
  0, /* 133 */
  1, /* 134 */
  0, /* 135 */
  3, /* 136 */
  0, /* 137 */
  1, /* 138 */
  0, /* 139 */
  2, /* 140 */
  0, /* 141 */
  1, /* 142 */
  0, /* 143 */
  4, /* 144 */
  0, /* 145 */
  1, /* 146 */
  0, /* 147 */
  2, /* 148 */
  0, /* 149 */
  1, /* 150 */
  0, /* 151 */
  3, /* 152 */
  0, /* 153 */
  1, /* 154 */
  0, /* 155 */
  2, /* 156 */
  0, /* 157 */
  1, /* 158 */
  0, /* 159 */
  5, /* 160 */
  0, /* 161 */
  1, /* 162 */
  0, /* 163 */
  2, /* 164 */
  0, /* 165 */
  1, /* 166 */
  0, /* 167 */
  3, /* 168 */
  0, /* 169 */
  1, /* 170 */
  0, /* 171 */
  2, /* 172 */
  0, /* 173 */
  1, /* 174 */
  0, /* 175 */
  4, /* 176 */
  0, /* 177 */
  1, /* 178 */
  0, /* 179 */
  2, /* 180 */
  0, /* 181 */
  1, /* 182 */
  0, /* 183 */
  3, /* 184 */
  0, /* 185 */
  1, /* 186 */
  0, /* 187 */
  2, /* 188 */
  0, /* 189 */
  1, /* 190 */
  0, /* 191 */
  6, /* 192 */
  0, /* 193 */
  1, /* 194 */
  0, /* 195 */
  2, /* 196 */
  0, /* 197 */
  1, /* 198 */
  0, /* 199 */
  3, /* 200 */
  0, /* 201 */
  1, /* 202 */
  0, /* 203 */
  2, /* 204 */
  0, /* 205 */
  1, /* 206 */
  0, /* 207 */
  4, /* 208 */
  0, /* 209 */
  1, /* 210 */
  0, /* 211 */
  2, /* 212 */
  0, /* 213 */
  1, /* 214 */
  0, /* 215 */
  3, /* 216 */
  0, /* 217 */
  1, /* 218 */
  0, /* 219 */
  2, /* 220 */
  0, /* 221 */
  1, /* 222 */
  0, /* 223 */
  5, /* 224 */
  0, /* 225 */
  1, /* 226 */
  0, /* 227 */
  2, /* 228 */
  0, /* 229 */
  1, /* 230 */
  0, /* 231 */
  3, /* 232 */
  0, /* 233 */
  1, /* 234 */
  0, /* 235 */
  2, /* 236 */
  0, /* 237 */
  1, /* 238 */
  0, /* 239 */
  4, /* 240 */
  0, /* 241 */
  1, /* 242 */
  0, /* 243 */
  2, /* 244 */
  0, /* 245 */
  1, /* 246 */
  0, /* 247 */
  3, /* 248 */
  0, /* 249 */
  1, /* 250 */
  0, /* 251 */
  2, /* 252 */
  0, /* 253 */
  1, /* 254 */
  0, /* 255 */
};

/* ====================================================================
 *
 *  BS_Choose
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS_ELT
BS_Choose(
  const BS* set
)
{
  BS_ELT i, j;

  for ( i = 0; i < BS_word_count(set); ++i ) {
    if ( BS_word(set,i) != bs_ZEROS ) {
      BS_ELT first_byte = bs_PBytesPW(i);

      for ( j = 0; j < BYTES_PER_BS_WORD; ++j ) {
        BS_BYTE byte = BS_byte(set,j + first_byte);

        if ( byte != bs_ZEROS )
          return first_one[byte] + bs_PBPB(j + first_byte);
      }

      Is_True(FALSE,("Word not zero, but no byte found"));
    }
  }

  return BS_CHOOSE_FAILURE;
}

/* ====================================================================
 *
 *  BS_Choose_Range
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS_ELT
BS_Choose_Range(
  BS    *set,
  BS_ELT low,
  BS_ELT high
)
{
  BS_BYTE byte;
  BS_ELT  last_first_word_full_byte, i, j;
  BS_ELT  first_word, last_word;
  BS_ELT  first_byte, last_byte;
  BS_ELT  last_elt_in_set;

  Is_True(low >= 0,
          ("BS_Choose_Range called with negative low element."));
  Is_True(high - low >= -1,
          ("BS_Choose_Range called with negative range size."));

  /* Adjust high to be within the set:
   */
  last_elt_in_set = bs_PBPW(BS_word_count(set)) - 1;
  if ( high > last_elt_in_set )
    high = last_elt_in_set;

  /* The following check is needed becaause we won't catch it if low is in
   * the next byte from high.
   */
  if ( low > high )
    return BS_CHOOSE_FAILURE;

  first_byte = bs_QBPB(low);
  last_byte  = bs_QBPB(high);

  /* Check first byte.  Since it also might be the last byte, we have
   * to be careful to mask out both ends of the range.  After this we
   * can assume the first byte and last byte are different, which
   * simplifies things quite a bit.
   */
  byte = BS_byte(set,first_byte) & (bs_ONES << bs_RBPB(low));
  if ( first_byte == last_byte )
    byte &= bs_ONES >> ((BITS_PER_BS_WORD - 1) - bs_RBPB(high));

  if ( byte != bs_ZEROS )
    return first_one[byte] + bs_PBPB(first_byte);
  else if (first_byte == last_byte)
    return BS_CHOOSE_FAILURE;

  /* Chack remaining full bytes in the first word.  Since the last
   * byte might be in the first word, we have to avoid checking it as
   * a full byte.
   */
  first_word = bs_QBPW(low);
  last_first_word_full_byte = (first_word + 1) * sizeof(BS_WORD) - 1;

  if ( last_first_word_full_byte >= last_byte )
    last_first_word_full_byte = last_byte - 1;

  for ( i = first_byte + 1; i <= last_first_word_full_byte;  ++i ) {
    byte = BS_byte(set,i);

    if ( byte != bs_ZEROS )
      return first_one[byte] + bs_PBPB(i);
  }

  /* Now we are either full word aligned or the last byte was in the
   * first word.  Check full words above the first word and below the
   * last word.  Notice that this loop won't trip if the last byte was
   * in the first word (or the next one for that matter.)
   */
  last_word = bs_QBPW(high);

  for ( i = first_word + 1; i < last_word; ++i ) {
    if ( BS_word(set,i) != bs_ZEROS ) {
      /* Found the word, now locate the byte in that word.
       */
      for ( j = 0; j < sizeof(BS_WORD); ++j ) {
        byte = BS_byte(set, j + i * BYTES_PER_BS_WORD);

        if ( byte != 0 )
          return first_one[byte] + bs_PBPB(j) + bs_PBPW(i);
      }
    }
  }

  /* Check any unchecked full bytes in the last word.  This loop will
   * only trip if the last word is greater than the first word, since
   * otherwise the loop above leaves i above the first word, no matter
   * what.  In fact, at this point i is the index of the first
   * unchecked word.
   */
  for ( i *= sizeof(BS_WORD); i < last_byte; ++i ) {
    byte = BS_byte(set,i);

    if ( byte != 0 )
      return first_one[byte] + 8 * i;
  }

  /* Now we'll check the final byte, which we have assumed is partial.
   * Even if it is full, the following will work.
   */
  byte =   BS_byte(set,last_byte)
         & (bs_ONES >> ((BITS_PER_BS_WORD - 1) - bs_RBPB(high)));

  if ( byte != 0 )
    return first_one[byte] + bs_PBPB(last_byte);

  return BS_CHOOSE_FAILURE;
}


/* =======================================================================
 *
 *  BS_Choose_Next
 *
 *  See interface description.
 *
 * =======================================================================
 */

extern BS_ELT
BS_Choose_Next(
  const BS*    set,
  BS_ELT bound 
)
{
  BS_ELT  i, j, inx_first_byte, inx_first_byte_second_word, word_count;
  BS_BYTE byte;

  ++bound;  /* Now bound is inclusive. */

  if ( bound >= bs_PBPW(BS_word_count(set)) )
    return BS_CHOOSE_FAILURE;

  /* Search first byte with stuff below bound masked out:
   */
  inx_first_byte = bs_QBPB(bound);
  byte = BS_byte(set,inx_first_byte) & (bs_ONES << bs_RBPB(bound));
  if ( byte != bs_ZEROS )
    return first_one[byte] + bs_PBPB(inx_first_byte);

  /* Search remaining bytes in first word:
   */
  inx_first_byte_second_word = (bs_QBPW(bound) + 1) * sizeof(BS_WORD);

  for ( i = inx_first_byte + 1; i < inx_first_byte_second_word; ++i ) {
    byte = BS_byte(set,i);

    if ( byte != bs_ZEROS )
      return first_one[byte] + bs_PBPB(i);
  }

  /* search remaining words:
   */
  word_count = BS_word_count(set);

  for ( i = bs_QBPW(bound) + 1; i < word_count; ++i ) {
    if ( BS_word(set,i) != bs_ZEROS ) {
      BS_ELT first_byte = bs_PBytesPW(i);

      /* There's something in this word, search each byte for it:
       */
      for ( j = 0; j < BYTES_PER_BS_WORD; ++j ) {
        BS_BYTE byte = BS_byte(set,j + first_byte);

        if ( byte != bs_ZEROS )
          return first_one[byte] + bs_PBPB(j + first_byte);
      }

      Is_True(FALSE,("Word not zero, but no byte found"));
    }
  }
    
  return BS_CHOOSE_FAILURE;
}


/* ====================================================================
 *
 *  BS_Intersection_Choose
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS_ELT
BS_Intersection_Choose(
  BS* set1,
  BS* set2
)
{
  BS_ELT i, j;
  BS_ELT last_word;

  if ( BS_word_count(set1) < BS_word_count(set2) )
    last_word = BS_word_count(set1);
  else
    last_word = BS_word_count(set2);

  for ( i = 0; i < last_word; ++i ) {
    if ( (BS_word(set1,i) & BS_word(set2,i)) != bs_ZEROS ) {
      BS_ELT first_byte = bs_PBytesPW(i);

      for ( j = 0; j < BYTES_PER_BS_WORD; ++j ) {
        BS_BYTE byte =   BS_byte(set1,j + first_byte)
                       & BS_byte(set2,j + first_byte);

        if ( byte != bs_ZEROS )
          return first_one[byte] + bs_PBPB(j + first_byte);
      }

      Is_True(FALSE,("Word not zero, but no byte found"));
    }
  }

  return BS_CHOOSE_FAILURE;
}



/* =======================================================================
 *
 *  BS_Intersection_Choose_Next
 *
 *  See interface description.
 *
 * =======================================================================
 */

extern BS_ELT
BS_Intersection_Choose_Next(
  BS*    set1,
  BS*    set2,
  BS_ELT bound 
)
{
  BS_ELT  i, j, inx_first_byte, inx_first_byte_second_word, word_count;
  BS_BYTE byte;

  if ( BS_word_count(set1) < BS_word_count(set2) )
    word_count = BS_word_count(set1);
  else
    word_count = BS_word_count(set2);

  ++bound;  /* Now bound is inclusive. */

  if ( bound >= bs_PBPW(word_count) )
    return BS_CHOOSE_FAILURE;

  /* Search first byte with stuff below bound masked out:
   */
  inx_first_byte = bs_QBPB(bound);
  byte =   BS_byte(set1,inx_first_byte)
         & BS_byte(set2,inx_first_byte)
         & (bs_ONES << bs_RBPB(bound));
  if ( byte != bs_ZEROS )
    return first_one[byte] + bs_PBPB(inx_first_byte);

  /* Search remaining bytes in first word:
   */
  inx_first_byte_second_word = (bs_QBPW(bound) + 1) * sizeof(BS_WORD);

  for ( i = inx_first_byte + 1; i < inx_first_byte_second_word; ++i ) {
    byte =   BS_byte(set1,i)
           & BS_byte(set2,i);

    if ( byte != bs_ZEROS )
      return first_one[byte] + bs_PBPB(i);
  }

  /* search remaining words:
   */
  for ( i = bs_QBPW(bound) + 1; i < word_count; ++i ) {
    if ( (BS_word(set1,i) & BS_word(set2,i)) != bs_ZEROS ) {
      BS_ELT first_byte = bs_PBytesPW(i);

      /* There's something in this word, search each byte for it:
       */
      for ( j = 0; j < BYTES_PER_BS_WORD; ++j ) {
        BS_BYTE byte =   BS_byte(set1,j + first_byte)
                       & BS_byte(set2,j + first_byte);

        if ( byte != bs_ZEROS )
          return first_one[byte] + bs_PBPB(j + first_byte);
      }

      Is_True(FALSE,("Word not zero, but no byte found"));
    }
  }
    
  return BS_CHOOSE_FAILURE;
}



/* ====================================================================
 *
 *  BS_Difference
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_Difference(
  BS       *set1,
  BS       *set2,
  MEM_POOL *pool
)
{
  BS    *set;
  BS_ELT i;
  BS_ELT size1   = BS_word_count(set1);
  BS_ELT size2   = BS_word_count(set2);
  BS_ELT minsize = Min(size1,size2);

  set = bs_Malloc(size1,pool);

  /* Common part: copy in the difference.
   */
  for ( i = 0; i < minsize; ++i )
    BS_word(set,i) = BS_word(set1,i) & ~BS_word(set2,i);

  /* Excess of set1 over set2: just copy it in.
   */
  for ( i = minsize; i < size1; ++i )
    BS_word(set,i) = BS_word(set1,i);

  return set;
}

/* ====================================================================
 *
 *  BS_DifferenceD
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_DifferenceD(
  BS* set1,
  BS* set2
)
{
  BS_ELT i;
  BS_ELT minsize = Min(BS_word_count(set1),BS_word_count(set2));

  for ( i = 0; i < minsize; ++i )
    BS_word(set1,i) &= ~BS_word(set2,i);

  return set1;
}

/* ====================================================================
 *
 *  BS_Difference1
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_Difference1(
  BS       *set,
  BS_ELT    x,
  MEM_POOL *pool
)
{
  return BS_Difference1D(BS_Copy(set,pool),x);
}

/* ====================================================================
 *
 *  BS_Difference1D
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_Difference1D(
  BS    *set,
  BS_ELT x
)
{
  Is_True(x >= 0,
            ("BS_Difference1D called with negative element."));

  if ( bs_QBPW(x) < BS_word_count(set) )
    BS_byte(set,bs_QBPB(x)) &= ~(bs_ONE << bs_RBPB(x));

  return set;
}

/* ====================================================================
 *
 *  BS_Intersection
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_Intersection(
  BS       *set1,
  BS       *set2,
  MEM_POOL *pool
)
{
  BS_ELT i;
  BS_ELT size;
  BS    *newset;

  if ( BS_word_count(set1) < BS_word_count(set2) )
    size = BS_word_count(set1);
  else
    size = BS_word_count(set2);

  newset = bs_Malloc(size,pool);

  for ( i = 0; i < size; ++i )
    BS_word(newset,i) = BS_word(set1,i) & BS_word(set2,i);

  return newset;
}

/* ====================================================================
 *
 *  BS_IntersectionD
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_IntersectionD(
  BS* set1,
  BS* set2
)
{
  BS_ELT i;
  BS_ELT minsize;

  minsize = MIN( BS_word_count(set1), BS_word_count(set2) );

  /* Form intersection of common part:
   */
  for ( i = 0; i < minsize; ++i )
    BS_word(set1,i) &= BS_word(set2,i);

  /* Zero surplus words in set1:
   */
  for ( i = i; i < BS_word_count(set1); ++i )
    BS_word(set1,i) = bs_ZEROS;

  return set1;
}


/* ====================================================================
 *
 *  BS_IntersectionR
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_IntersectionR(
  BS* result,
  const BS* set1,
  const BS* set2
)
{
  BS_ELT i;
  BS_ELT minsize;

  minsize = MIN( BS_word_count(set1), BS_word_count(set2) );

  /* Form intersection of common part:
   */
  for ( i = 0; i < minsize; ++i )
    BS_word(result,i) = BS_word(set1,i) & BS_word(set2,i);

  /* Zero surplus words in result:
   */
  for ( i = i; i < BS_word_count(result); ++i )
    BS_word(result,i) = bs_ZEROS;

  return result;
}


/* Count of bits in all the one byte numbers.  Used to count members.
 */
static unsigned const char bit_count[256] = {
  0, /*   0 */
  1, /*   1 */
  1, /*   2 */
  2, /*   3 */
  1, /*   4 */
  2, /*   5 */
  2, /*   6 */
  3, /*   7 */
  1, /*   8 */
  2, /*   9 */
  2, /*  10 */
  3, /*  11 */
  2, /*  12 */
  3, /*  13 */
  3, /*  14 */
  4, /*  15 */
  1, /*  16 */
  2, /*  17 */
  2, /*  18 */
  3, /*  19 */
  2, /*  20 */
  3, /*  21 */
  3, /*  22 */
  4, /*  23 */
  2, /*  24 */
  3, /*  25 */
  3, /*  26 */
  4, /*  27 */
  3, /*  28 */
  4, /*  29 */
  4, /*  30 */
  5, /*  31 */
  1, /*  32 */
  2, /*  33 */
  2, /*  34 */
  3, /*  35 */
  2, /*  36 */
  3, /*  37 */
  3, /*  38 */
  4, /*  39 */
  2, /*  40 */
  3, /*  41 */
  3, /*  42 */
  4, /*  43 */
  3, /*  44 */
  4, /*  45 */
  4, /*  46 */
  5, /*  47 */
  2, /*  48 */
  3, /*  49 */
  3, /*  50 */
  4, /*  51 */
  3, /*  52 */
  4, /*  53 */
  4, /*  54 */
  5, /*  55 */
  3, /*  56 */
  4, /*  57 */
  4, /*  58 */
  5, /*  59 */
  4, /*  60 */
  5, /*  61 */
  5, /*  62 */
  6, /*  63 */
  1, /*  64 */
  2, /*  65 */
  2, /*  66 */
  3, /*  67 */
  2, /*  68 */
  3, /*  69 */
  3, /*  70 */
  4, /*  71 */
  2, /*  72 */
  3, /*  73 */
  3, /*  74 */
  4, /*  75 */
  3, /*  76 */
  4, /*  77 */
  4, /*  78 */
  5, /*  79 */
  2, /*  80 */
  3, /*  81 */
  3, /*  82 */
  4, /*  83 */
  3, /*  84 */
  4, /*  85 */
  4, /*  86 */
  5, /*  87 */
  3, /*  88 */
  4, /*  89 */
  4, /*  90 */
  5, /*  91 */
  4, /*  92 */
  5, /*  93 */
  5, /*  94 */
  6, /*  95 */
  2, /*  96 */
  3, /*  97 */
  3, /*  98 */
  4, /*  99 */
  3, /* 100 */
  4, /* 101 */
  4, /* 102 */
  5, /* 103 */
  3, /* 104 */
  4, /* 105 */
  4, /* 106 */
  5, /* 107 */
  4, /* 108 */
  5, /* 109 */
  5, /* 110 */
  6, /* 111 */
  3, /* 112 */
  4, /* 113 */
  4, /* 114 */
  5, /* 115 */
  4, /* 116 */
  5, /* 117 */
  5, /* 118 */
  6, /* 119 */
  4, /* 120 */
  5, /* 121 */
  5, /* 122 */
  6, /* 123 */
  5, /* 124 */
  6, /* 125 */
  6, /* 126 */
  7, /* 127 */
  1, /* 128 */
  2, /* 129 */
  2, /* 130 */
  3, /* 131 */
  2, /* 132 */
  3, /* 133 */
  3, /* 134 */
  4, /* 135 */
  2, /* 136 */
  3, /* 137 */
  3, /* 138 */
  4, /* 139 */
  3, /* 140 */
  4, /* 141 */
  4, /* 142 */
  5, /* 143 */
  2, /* 144 */
  3, /* 145 */
  3, /* 146 */
  4, /* 147 */
  3, /* 148 */
  4, /* 149 */
  4, /* 150 */
  5, /* 151 */
  3, /* 152 */
  4, /* 153 */
  4, /* 154 */
  5, /* 155 */
  4, /* 156 */
  5, /* 157 */
  5, /* 158 */
  6, /* 159 */
  2, /* 160 */
  3, /* 161 */
  3, /* 162 */
  4, /* 163 */
  3, /* 164 */
  4, /* 165 */
  4, /* 166 */
  5, /* 167 */
  3, /* 168 */
  4, /* 169 */
  4, /* 170 */
  5, /* 171 */
  4, /* 172 */
  5, /* 173 */
  5, /* 174 */
  6, /* 175 */
  3, /* 176 */
  4, /* 177 */
  4, /* 178 */
  5, /* 179 */
  4, /* 180 */
  5, /* 181 */
  5, /* 182 */
  6, /* 183 */
  4, /* 184 */
  5, /* 185 */
  5, /* 186 */
  6, /* 187 */
  5, /* 188 */
  6, /* 189 */
  6, /* 190 */
  7, /* 191 */
  2, /* 192 */
  3, /* 193 */
  3, /* 194 */
  4, /* 195 */
  3, /* 196 */
  4, /* 197 */
  4, /* 198 */
  5, /* 199 */
  3, /* 200 */
  4, /* 201 */
  4, /* 202 */
  5, /* 203 */
  4, /* 204 */
  5, /* 205 */
  5, /* 206 */
  6, /* 207 */
  3, /* 208 */
  4, /* 209 */
  4, /* 210 */
  5, /* 211 */
  4, /* 212 */
  5, /* 213 */
  5, /* 214 */
  6, /* 215 */
  4, /* 216 */
  5, /* 217 */
  5, /* 218 */
  6, /* 219 */
  5, /* 220 */
  6, /* 221 */
  6, /* 222 */
  7, /* 223 */
  3, /* 224 */
  4, /* 225 */
  4, /* 226 */
  5, /* 227 */
  4, /* 228 */
  5, /* 229 */
  5, /* 230 */
  6, /* 231 */
  4, /* 232 */
  5, /* 233 */
  5, /* 234 */
  6, /* 235 */
  5, /* 236 */
  6, /* 237 */
  6, /* 238 */
  7, /* 239 */
  4, /* 240 */
  5, /* 241 */
  5, /* 242 */
  6, /* 243 */
  5, /* 244 */
  6, /* 245 */
  6, /* 246 */
  7, /* 247 */
  5, /* 248 */
  6, /* 249 */
  6, /* 250 */
  7, /* 251 */
  6, /* 252 */
  7, /* 253 */
  7, /* 254 */
  8  /* 255 */
};

/* ====================================================================
 *
 *  BS_Size
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS_ELT
BS_Size(
  BS* set
)
{
  /* Add up the population count of each byte in the set.  We get the
   * population counts from the table above.  Great for a machine with
   * effecient loadbyte instructions!
   */
  BS_ELT i;
  BS_ELT byte_count = BS_word_count(set) * sizeof(BS_WORD);
  size_t result = 0;

  for ( i = 0; i < byte_count; ++i )
    result += bit_count[BS_byte(set,i)];

  return result;
}

/* ====================================================================
 *
 *  BS_Union
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_Union(
  BS       *set1,
  BS       *set2,
  MEM_POOL *pool
)
{
  BS    *set;
  BS_ELT i;
  BS_ELT size1 = BS_word_count(set1);
  BS_ELT size2 = BS_word_count(set2);

  if ( size1 < size2 ) {
    /* Normalize so set1 is at least as large:
     */
    BS    *tmps = set1;
    BS_ELT tmpe = size1;

    set1 = set2;
    set2 = tmps;
    size1 = size2;
    size2 = tmpe;
  }

  set = bs_Malloc(size1,pool);
  for ( i = 0; i < size2; ++i )
    BS_word(set,i) = BS_word(set1,i) | BS_word(set2,i);
  for ( i = size2; i < size1; ++i )
    BS_word(set,i) = BS_word(set1,i);

  return set;
}

/* ====================================================================
 *
 *  BS_UnionD
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_UnionD(
  BS       *set1,
  BS       *set2,
  MEM_POOL *pool
)
{
  BS_ELT i;
  BS_ELT size1 = BS_word_count(set1);
  BS_ELT size2 = BS_word_count(set2);

  if ( size1 < size2 )
    set1 = bs_Realloc(set1,size2,pool);

  for ( i = 0; i < size2; ++i )
    BS_word(set1,i) |= BS_word(set2,i);

  return set1;
}

/* ====================================================================
 *
 *  BS_UnionR
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_UnionR(
  BS       *result,
  BS       *set1,
  BS       *set2,
  MEM_POOL *pool
)
{
  BS_ELT i;
  BS_ELT size1 = BS_word_count(set1);
  BS_ELT size2 = BS_word_count(set2);
  BS_ELT maxsize = MAX( size1, size2 );
  BS_ELT rsize = BS_word_count(result);

  if ( rsize < maxsize )
    result = bs_Realloc(result,maxsize,pool);

  for ( i = 0; i < maxsize; ++i )
    BS_word(result,i) = BS_word(set1,i) | BS_word(set2,i);

  return result;
}

/* ====================================================================
 *
 *  BS_UnionD_Intersection
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_UnionD_Intersection(
  BS       *set1,
  BS       *set2,
  BS       *set3,
  MEM_POOL *pool
)
{
  BS_ELT i;
  BS_ELT size1 = BS_word_count(set1);
  BS_ELT minsize = MIN( BS_word_count(set2), BS_word_count(set3) );

  if ( size1 < minsize )
    set1 = bs_Realloc(set1,minsize,pool);

  for ( i = 0; i < minsize; ++i )
    BS_word(set1,i) |= BS_word(set2,i) & BS_word(set3,i);

  return set1;
}

/* ====================================================================
 *
 *  BS_Union1
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_Union1(
  BS       *set,
  BS_ELT    x,
  MEM_POOL *pool
)
{
  BS_ELT newsize;
  BS    *newset;

  if ( BS_word_count(set) > bs_QBPW(x) + 1 )
    newsize = bs_PBPW(BS_word_count(set));
  else
    newsize = x + 1;

  newset = BS_Create_Empty(newsize,pool);
  newset = BS_CopyD(newset,set,bad_pool);
  return BS_Union1D(newset,x,bad_pool);
}

/* ====================================================================
 *
 *  BS_Union1D
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_Union1D(
  BS       *set,
  BS_ELT    x,
  MEM_POOL *pool
)
{
  BS_ELT minsize = bs_QBPW(x) + 1;

  Is_True(x >= 0,("BS_Union1D called with negative element."));

  if ( minsize > BS_word_count(set) )
    set = bs_Realloc(set,minsize,pool);

  BS_byte(set,bs_QBPB(x)) |= bs_ONE << bs_RBPB(x);

  return set;
}

/* ====================================================================
 *
 *  BS_2_1_Minus_3_Or_R
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_2_1_Minus_3_Or_R(
  BS       *result,
  const BS *set1,
  const BS *set2,
  const BS *set3,
  MEM_POOL *pool
)
{
  BS_ELT i;
  BS_ELT rsize = BS_word_count(result);
  BS_ELT size3 = BS_word_count(set3);

  Is_True( BS_word_count(set1) == size3,
    ("BS_2_1_Minus_3_Or_R: set1 size (%d) < set3 size (%d)",
     (BS_ELT)BS_word_count(set1), size3) );
  Is_True( BS_word_count(set2) == size3,
    ("BS_2_1_Minus_3_Or_R: set2 size (%d) < set3 size (%d)",
     (BS_ELT)BS_word_count(set2), size3) );

  if ( rsize < size3 )
    result = bs_Realloc(result,size3,pool);

  for ( i = 0; i < size3; ++i )
    BS_word(result,i) = (~BS_word(set1,i) & BS_word(set2,i)) |
			  BS_word(set3,i);

  return result;
}

extern BS*
BS_3_2_Minus_1_Or_D(
  BS       *set1,
  const BS *set2,
  const BS *set3,
  MEM_POOL *pool
)
{
  BS_ELT i;
  BS_ELT size1 = BS_word_count(set1);
  BS_ELT size2 = BS_word_count(set2);
  BS_ELT size3 = BS_word_count(set3);

  Is_True( size2 == size3,
    ("BS_3_2_Minus_1_Or_D: set2 size (%d) != set3 size (%d)",
     size2, size3) );

  if ( size1 < size3 )
    set1 = bs_Realloc(set1,size3,pool);

  for ( i = 0; i < size3; ++i )
    BS_word(set1,i) |= (~BS_word(set2,i) & BS_word(set3,i));

  return set1;
}

/* ====================================================================
 * 
 * BS_3_2_Minus_4_Or_1_Or_D
 *
 * set1 += ((~set2) AND set3) OR set4
 * set1 += (set3 - set2) + set4
 * 
 * ====================================================================
 */

extern BS*
BS_3_2_Minus_4_Or_1_Or_D(
  BS       *set1,
  const BS *set2,
  const BS *set3,
  const BS *set4,
  MEM_POOL *pool )
{
  BS_ELT i;
  BS_ELT size1 = BS_word_count(set1);
  BS_ELT size2 = BS_word_count(set2);
  BS_ELT size3 = BS_word_count(set3);
  BS_ELT size4 = BS_word_count(set4);

  Is_True( size2 == size3 && size3 == size4,
    ("BS_3_2_Minus_4_Or_1_Or_D: sizes not equal %d, %d, %d",
     size2, size3, size4) );

  if ( size1 < size2 )
    set1 = bs_Realloc(set1,size2,pool);

  for ( i = 0; i < size2; ++i )
    BS_word(set1,i) |= (~BS_word(set2,i) & BS_word(set3,i)) |
		       BS_word(set4,i);

  return set1;
}


/* ====================================================================
 * 
 * BS_3_2_Minus_4_Or_5_Or_1_Or_D
 *
 * set1 += ((~set2) AND set3) OR set4 OR set5
 * set1 += (set3 - set2) + set4 + set5
 * 
 * ====================================================================
 */

extern BS*
BS_3_2_Minus_4_Or_5_Or_1_Or_D(
  BS       *set1,
  const BS *set2,
  const BS *set3,
  const BS *set4,
  const BS *set5,
  MEM_POOL *pool )
{
  BS_ELT i;
  BS_ELT size1 = BS_word_count(set1);
  BS_ELT size5  = BS_word_count(set5);

  Is_True( BS_word_count(set2) == size5,
    ("BS_3_2_Minus_4_Or_5_Or_1_Or_D: set2 size (%d) != set5 size (%d)",
     (BS_ELT)BS_word_count(set2), size5) );
  Is_True( BS_word_count(set3) == size5,
    ("BS_3_2_Minus_4_Or_5_Or_1_Or_D: set3 size (%d) != set5 size (%d)",
     (BS_ELT)BS_word_count(set3), size5) );
  Is_True( BS_word_count(set4) == size5,
    ("BS_3_2_Minus_4_Or_5_Or_1_Or_D: set4 size (%d) != set5 size (%d)",
     (BS_ELT)BS_word_count(set4), size5) );

  if ( size1 < size5 )
    set1 = bs_Realloc(set1,size5,pool);

  for ( i = 0; i < size5; ++i )
    BS_word(set1,i) |= (~BS_word(set2,i) & BS_word(set3,i)) |
			  BS_word(set4,i) | BS_word(set5,i);

  return set1;
}


/* ====================================================================
 *
 *  BS_2_1_Minus_3_Or_4_And_5_And_6_And_R
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_2_1_Minus_3_Or_4_And_5_And_6_And_R(
  BS       *result,
  const BS *set1,
  const BS *set2,
  const BS *set3,
  const BS *set4,
  const BS *set5,
  const BS *set6,
  MEM_POOL *pool
)
{
  BS_ELT i;
  BS_ELT rsize = BS_word_count(result);
  BS_ELT size3 = BS_word_count(set3);

  Is_True( BS_word_count(set1) == size3,
    ("BS_2_1_Minus_3_Or_4_And_5_And_6_And_R: set1 size (%d) < set3 size (%d)",
     (BS_ELT)BS_word_count(set1), size3) );
  Is_True( BS_word_count(set2) == size3,
    ("BS_2_1_Minus_3_Or_4_And_5_And_6_And_R: set2 size (%d) < set3 size (%d)",
     (BS_ELT)BS_word_count(set2), size3) );
  Is_True( BS_word_count(set4) == size3,
    ("BS_2_1_Minus_3_Or_4_And_5_And_6_And_R: set4 size (%d) < set3 size (%d)",
     (BS_ELT)BS_word_count(set4), size3) );
  Is_True( BS_word_count(set5) == size3,
    ("BS_2_1_Minus_3_Or_4_And_5_And_6_And_R: set5 size (%d) < set3 size (%d)",
     (BS_ELT)BS_word_count(set4), size3) );
  Is_True( BS_word_count(set6) == size3,
    ("BS_2_1_Minus_3_Or_4_And_5_And_6_And_R: set6 size (%d) < set3 size (%d)",
     (BS_ELT)BS_word_count(set6), size3) );

  if ( rsize < size3 )
    result = bs_Realloc(result,size3,pool);

  for ( i = 0; i < size3; ++i )
    BS_word(result,i) = ((~BS_word(set1,i) & BS_word(set2,i)) |
			 BS_word(set3,i)) &
			BS_word(set4,i) &
			BS_word(set5,i) &
			BS_word(set6,i);

  return result;
}

/* ====================================================================
 *
 *  BS_2_1_Minus_3_Or_4_And_R
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_2_1_Minus_3_Or_4_And_R(
  BS       *result,
  const BS *set1,
  const BS *set2,
  const BS *set3,
  const BS *set4,
  MEM_POOL *pool
)
{
  BS_ELT i;
  BS_ELT rsize = BS_word_count(result);
  BS_ELT size3 = BS_word_count(set3);

  Is_True( BS_word_count(set1) == size3,
    ("BS_2_1_Minus_3_Or_4_And_R: set1 size (%d) < set3 size (%d)",
     (BS_ELT)BS_word_count(set1), size3) );
  Is_True( BS_word_count(set2) == size3,
    ("BS_2_1_Minus_3_Or_4_And_R: set2 size (%d) < set3 size (%d)",
     (BS_ELT)BS_word_count(set2), size3) );
  Is_True( BS_word_count(set4) == size3,
    ("BS_2_1_Minus_3_Or_4_And_R: set4 size (%d) < set3 size (%d)",
     (BS_ELT)BS_word_count(set4), size3) );

  if ( rsize < size3 )
    result = bs_Realloc(result,size3,pool);

  for ( i = 0; i < size3; ++i )
    BS_word(result,i) = ((~BS_word(set1,i) & BS_word(set2,i)) |
			 BS_word(set3,i)) &
			BS_word(set4,i);

  return result;
}

/* ====================================================================
 *
 *  BS_1_Not_2_Or_3_Minus_4_And_R
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_1_Not_2_Or_3_Minus_4_And_R(
  BS       *result,
  const BS *set1,
  const BS *set2,
  const BS *set3,
  const BS *set4,
  MEM_POOL *pool
)
{
  BS_ELT i;
  BS_ELT rsize = BS_word_count(result);
  BS_ELT size2 = BS_word_count(set2);

  Is_True( BS_word_count(set1) == size2,
    ("BS_1_Not_2_Or_3_Minus_4_And_R: set1 size (%d) < set2 size (%d)",
     (BS_ELT)BS_word_count(set1), size2) );
  Is_True( BS_word_count(set3) == size2,
    ("BS_1_Not_2_Or_3_Minus_4_And_R: set3 size (%d) < set2 size (%d)",
     (BS_ELT)BS_word_count(set3), size2) );
  Is_True( BS_word_count(set4) == size2,
    ("BS_1_Not_2_Or_3_Minus_4_And_R: set4 size (%d) < set2 size (%d)",
     (BS_ELT)BS_word_count(set4), size2) );

  if ( rsize < size2 )
    result = bs_Realloc(result,size2,pool);

  for ( i = 0; i < size2; ++i )
    BS_word(result,i) = (~BS_word(set1,i) | BS_word(set2,i)) &
			(~BS_word(set3,i)) &
			BS_word(set4,i);
  return result;
}

/* ====================================================================
 * 
 * BS_2_3_Or_1_Or_D
 *
 * set1 += (set2 OR set3)
 * set1 += (set2 + set3)
 * 
 * ====================================================================
 */

extern BS*
BS_2_3_Or_1_Or_D(
  BS       *set1,
  const BS *set2,
  const BS *set3,
  MEM_POOL *pool )
{
  BS_ELT i;
  BS_ELT size1 = BS_word_count(set1);
  BS_ELT size2  = BS_word_count(set2);
  BS_ELT size3  = BS_word_count(set3);

  Is_True( size2 == size3,
    ("BS_2_3_Or_1_Or_D: sizes not equal %d, %d",
     size2, size3) );

  if ( size1 < size2 )
    set1 = bs_Realloc(set1,size2,pool);

  for ( i = 0; i < size2; ++i )
    BS_word(set1,i) |= (BS_word(set2,i) | BS_word(set3,i));

  return set1;
}

/* ====================================================================
 *
 *  BS_1_2_Or_3_And_R
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_1_2_Or_3_And_R(
  BS       *result,
  const BS *set1,
  const BS *set2,
  const BS *set3,
  MEM_POOL *pool
)
{
  BS_ELT i;
  BS_ELT rsize = BS_word_count(result);
  BS_ELT size1 = BS_word_count(set1);

  Is_True( BS_word_count(set2) == size1,
    ("BS_1_2_Or_3_And_R: set2 size (%d) < set1 size (%d)",
     (BS_ELT)BS_word_count(set2), size1) );
  Is_True( BS_word_count(set3) == size1,
    ("BS_1_2_Or_3_And_R: set3 size (%d) < set1 size (%d)",
     (BS_ELT)BS_word_count(set3), size1) );

  if ( rsize < size1 )
    result = bs_Realloc(result,size1,pool);

  for ( i = 0; i < size1; ++i )
    BS_word(result,i) = (BS_word(set1,i) | BS_word(set2,i)) &
			BS_word(set3,i);

  return result;
}

/* ====================================================================
 *
 *  BS_2_3_And_1_Or_D
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_2_3_And_1_Or_D(
  BS *set1,
  const BS *set2,
  const BS *set3,
  MEM_POOL *pool
)
{
  BS_ELT i;
  BS_ELT size1 = BS_word_count(set1);
  BS_ELT size = MIN(BS_word_count(set2),BS_word_count(set3));

  if ( size1 < size )
    set1 = bs_Realloc(set1,size,pool);

  for ( i = 0; i < size; ++i )
    BS_word(set1,i) |= BS_word(set2,i) & BS_word(set3,i);

  return set1;
}

/* ====================================================================
 *
 *  BS_3_Not_4_Or_2_And_1_Or_D
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_3_Not_4_Or_2_And_1_Or_D(
  BS       *set1,
  const BS *set2,
  const BS *set3,
  const BS *set4,
  MEM_POOL *pool
)
{
  BS_ELT i;
  BS_ELT size1 = BS_word_count(set1);
  BS_ELT size2 = BS_word_count(set2);

  Is_True( BS_word_count(set3) == size2,
    ("BS_3_Not_4_Or_2_And_1_Or_D: set3 size (%d) < set2 size (%d)",
     (BS_ELT)BS_word_count(set3), size2) );
  Is_True( BS_word_count(set4) == size2,
    ("BS_3_Not_4_Or_2_And_1_Or_D: set4 size (%d) < set2 size (%d)",
     (BS_ELT)BS_word_count(set4), size2) );

  if ( size1 < size2 )
    set1 = bs_Realloc(set1,size2,pool);

  for ( i = 0; i < size2; ++i )
    BS_word(set1,i) |= BS_word(set2,i) &
                       (~BS_word(set3,i) | BS_word(set4,i));

  return set1;
}

/* ====================================================================
 *
 *  BS_4_3_Minus_2_Not_Or_1_And_D
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_4_3_Minus_2_Not_Or_1_And_D(
  BS       *set1,
  const BS *set2,
  const BS *set3,
  const BS *set4,
  MEM_POOL *pool
)
{
  BS_ELT i;
  BS_ELT size1 = BS_word_count(set1);
  BS_ELT size2 = BS_word_count(set2);

  Is_True( BS_word_count(set3) == size2,
    ("BS_4_3_Minus_2_Not_Or_1_And_D: set3 size (%d) < set2 size (%d)",
     (BS_ELT)BS_word_count(set3), size2) );
  Is_True( BS_word_count(set4) == size2,
    ("BS_4_3_Minus_2_Not_Or_1_And_D: set4 size (%d) < set2 size (%d)",
     (BS_ELT)BS_word_count(set4), size2) );

  if ( size1 < size2 )
    set1 = bs_Realloc(set1,size2,pool);

  for ( i = 0; i < size2; ++i )
    BS_word(set1,i) &= ~BS_word(set2,i) |
                       (BS_word(set4,i) & ~BS_word(set3,i));

  return set1;
}

/* ====================================================================
 *
 *  BS_2_3_Minus_1_Or_D
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_2_3_Minus_1_Or_D(
  BS       *set1,
  const BS *set2,
  const BS *set3,
  MEM_POOL *pool
)
{
  BS_ELT i;
  BS_ELT size1 = BS_word_count(set1);
  BS_ELT size2 = BS_word_count(set2);
  BS_ELT size3 = BS_word_count(set3);

  Is_True( size2 == size3,
    ("BS_2_3_Minus_1_Or_D: sizes not equal %d, %d",
     size2, size3) );

  if ( size1 < size2 )
    set1 = bs_Realloc(set1,size2,pool);

  for ( i = 0; i < size2; ++i )
    BS_word(set1,i) |= BS_word(set2,i) & ~BS_word(set3,i);

  return set1;
}

/* ====================================================================
 *
 *  BS_2_3_Minus_4_Minus_1_Or_D
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BS*
BS_2_3_Minus_4_Minus_1_Or_D(
  BS       *set1,
  const BS *set2,
  const BS *set3,
  const BS *set4,
  MEM_POOL *pool
)
{
  BS_ELT i;
  BS_ELT size1 = BS_word_count(set1);
  BS_ELT size2 = BS_word_count(set2);
  BS_ELT size3 = BS_word_count(set3);
  BS_ELT size4 = BS_word_count(set4);

  Is_True( size2 == size3 && size3 == size4,
    ("BS_2_3_Minus_4_Minus_1_Or_D: sizes not equal %d, %d, %d",
     size2, size3, size4) );

  if ( size1 < size2 )
    set1 = bs_Realloc(set1,size2,pool);

  for ( i = 0; i < size2; ++i )
    BS_word(set1,i) |= BS_word(set2,i) & ~BS_word(set3,i)
			 & ~BS_word(set4,i);

  return set1;
}

/* ====================================================================
 *
 *  BS_ContainsP
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BOOL
BS_ContainsP(
  BS *set1,
  BS *set2
)
{
  BS_ELT minsize;
  BS_ELT i;

  if ( BS_word_count(set1) < BS_word_count(set2) )
    minsize = BS_word_count(set1);
  else
    minsize = BS_word_count(set2);

  /* Check common part.
   */
  for ( i = 0; i < minsize; ++i ) {
    if ( BS_word(set1,i) != (BS_word(set1,i) | BS_word(set2,i)) )
      return FALSE;
  }

  /* Check excess in set2.
   */
  for ( ; i < BS_word_count(set2); ++i ) {
    if ( BS_word(set2,i) != bs_ZEROS )
      return FALSE;
  }

  return TRUE;
}

/* ====================================================================
 *
 *  BS_EmptyP
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BOOL
BS_EmptyP(
  BS *set
)
{
  BS_ELT i;

  for ( i = 0; i < BS_word_count(set); ++i ) {
    if ( BS_word(set,i) != bs_ZEROS )
      return FALSE;
  }

  return TRUE;
}

/* ====================================================================
 *
 *  BS_EqualP
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BOOL
BS_EqualP(
  BS *set1,
  BS *set2
)
{
  BS_ELT i;

  /* Normalize so set1 is smaller:
   */
  if ( BS_word_count(set1) > BS_word_count(set2) ) {
    BS *tmp = set1;
    set1 = set2;
    set2 = tmp;
  }

  for ( i = 0; i < BS_word_count(set1); ++i ) {
    if ( BS_word(set1,i) != BS_word(set2,i) )
      return FALSE;
  }

  for ( ; i < BS_word_count(set2); ++i ) {
    if ( BS_word(set2,i) != bs_ZEROS )
      return FALSE;
  }

  return TRUE;
}

/* ====================================================================
 *
 *  BS_IntersectsP
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BOOL
BS_IntersectsP(
  BS *set1,
  BS *set2
)
{
  BS_ELT minsize, i;

  if ( BS_word_count(set1) < BS_word_count(set2) )
    minsize = BS_word_count(set1);
  else
    minsize = BS_word_count(set2);

  for ( i = 0; i < minsize; ++i ) {
    if ( (BS_word(set1,i) & BS_word(set2,i)) != bs_ZEROS )
      return TRUE;
  }

  return FALSE;
}

/* ====================================================================
 *
 *  BS_MemberP
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BOOL
BS_MemberP(
  BS    *set,
  BS_ELT x
)
{
  Is_True(x >= 0,("BS_Member called with negative element."));

  if ( bs_QBPW(x) >= BS_word_count(set) )
    return FALSE;
  else
    return    (BS_byte(set,bs_QBPB(x)) & (bs_ONE << bs_RBPB(x)))
           != bs_ZEROS;
}

/* ====================================================================
 *
 *  BS_Intersection_MemberP
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BOOL
BS_Intersection_MemberP(
  BS    *set1,
  BS    *set2,
  BS_ELT x
)
{
  Is_True(x >= 0,("BS_Member called with negative element."));

  if (    bs_QBPW(x) >= BS_word_count(set1)
       || bs_QBPW(x) >= BS_word_count(set2)
  ) {
    return FALSE;
  }
  else {
    return    (   BS_byte(set1,bs_QBPB(x))
                & BS_byte(set2,bs_QBPB(x))
                & (bs_ONE << bs_RBPB(x)))
           != bs_ZEROS;
  }
}

/* ====================================================================
 *
 *  PrintRange
 *
 *  Subroutine for BS_Print to print a range within the set.
 *
 *  f       - The file to print to
 *  low     - First elemnt of range
 *  high    - Last element of range
 *  first   - First range to be printed.  (Reference argument)
 *
 * ====================================================================
 */

static void
PrintRange(
  FILE  *f,
  BS_ELT low,
  BS_ELT high,
  BOOL  *first
)
{
  if ( *first )
    *first = FALSE;
  else
    fprintf(f,",");

  if ( low == high )
    fprintf(f,"%d",low);
  else
    fprintf(f,"%d-%d",low,high);
}

/* ====================================================================
 *
 *  BS_Print
 *
 *  See interface description
 *
 * ====================================================================
 */

extern void
BS_Print(
  BS   *set,
  FILE *f
)
{
  BS_ELT range_first, last_elt, elt;
  BOOL   first = TRUE;

  if ( set == NULL ) {
    fprintf ( f, "<NULL>" );
    return;
  }

  fprintf(f,"{");

  elt = BS_Choose(set);
  if ( elt == BS_CHOOSE_FAILURE ) {
    fprintf(f,"}");
    return;
  }
  range_first = elt;
  last_elt = range_first;

  while ( (elt = BS_Choose_Next(set,elt)) != BS_CHOOSE_FAILURE ) {
    if ( elt != last_elt + 1 ) {
      /* renge_first .. last_elt was a range
       */

      PrintRange(f,range_first,last_elt,&first);
      range_first = elt;
    }
    last_elt = elt;
  }

  PrintRange(f,range_first,last_elt,&first);
  fprintf(f,"}");
}

/* ====================================================================
 *
 *  BS_Print_dbg - same as BS_Print, but to stderr, to be called from
 *  dbx
 *
 * ====================================================================
 */

extern void
BS_Print_dbg(BS   *set)
{
  BS_Print(set, stderr);
  fprintf(stderr, "\n");
}


/* ====================================================================
 *
 *  FBS_MemberP
 *
 *  See interface description
 *
 * ====================================================================
 */

extern BOOL
FBS_MemberP_Validate(
  BS    *set,
  BS_ELT x
)
{
  Is_True(x >= 0,("FBS_Member called with negative element."));
  Is_True( bs_QBPW(x) < BS_word_count(set), ("FBS_Member called with out of bound element."));
  return  (BS_byte(set,bs_QBPB(x)) & (bs_ONE << bs_RBPB(x))) != bs_ZEROS;
}


/* ====================================================================
 *
 *  FBS_Union1D
 *
 *  See interface description
 *
 * ====================================================================
 */

void
FBS_Union1D_Validate(
  BS       *set,
  BS_ELT    x
)
{
  BS_ELT minsize = bs_QBPW(x) + 1;
  Is_True(x >= 0,("FBS_Union1D called with negative element."));
  Is_True( minsize <= BS_word_count(set), ("FBS_Union1D called with out of bound element."));
  BS_byte(set,bs_QBPB(x)) |= bs_ONE << bs_RBPB(x);
}



