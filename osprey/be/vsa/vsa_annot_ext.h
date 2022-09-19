/*
   Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */

#ifndef vsa_annot_ext_INCLUDED
#define vsa_annot_ext_INCLUDED

// ==================================================================
// extended annotation (EANT)
//
// refer osprey/doc/design_flag_prop.txt for documentation
// ==================================================================

// annotations may contain parameters. in this case, they can't be
// represented by 1 or 2 bits. we designed a variant length data
// structure to track this kind of annotations. multiple annotations
// can be saved in one buffer and attached to coderep, stmtrep or dna.

// some annotations may contain 1 parameter, for example:
// length(n):
//   for parameter, buffer length is the nth param, for example:
//     foo(p, sizeof(p)); // p <- length(2)
//   for buffer in struct, buffer length is the nth field. for example:
//     p->buf_len = ...;             // assume field_id = 1
//     p->buf = malloc(p->buf_len);  // p->buf <- length(1)
// assign(n):
//   return value is always the nth param, for example:
//   strcat();                       // assign(1)
// is_null(n):
//   return value is true/false if the nth param is null, for example:
//   bool f(int *p) { return (!p) ? true : false; }  // is_null(1)
// is_not_null(n)
//   return value is false/true if the nth param is null, for example:
//   bool f(int *p) { return p ? true : false; }     // is_not_null(1)
//
// assume n is less than 32, we can encode them into 1 byte, lower 3
// bit for annotation kind and high 5 bit for value

enum EANT_KIND {
  // 1-byte annotation, with low 3-bit kind and high 5-bit parameter
  ANT1_FIRST    = 0,
  ANT1_LENGTH   = ANT1_FIRST,
  ANT1_ASSIGN   = 1,
  ANT1_IS_NULL  = 2,
  ANT1_NOT_NULL = 3,
  ANT1_LAST     = 4,     // last extended annotation
  ANT1_MAX      = 0x7,   // only support 6 annot for 1 bit
  // TBD: 2-byte annotation, with low 3-bit 0x7 and high 5-bit kind and 1-byte parameter
  ANT2_FIRST    = 0x7,
  // TBD: 3-byte annotation, with low 3-bit 0x7 and high 5-bit kind and 2-byte parameter
  // TBD: multi-byte annotation, with LEB128 encoding, etc
};

// utility class to manipulate extended annotation
class EANT_UTIL {
private:
  UINT8 *_annot;       // annotation buffer
  UINT16 _size;        // current used
  UINT16 _max_size;    // max size

private:
  UINT32 Skip(EANT_KIND annot) const {
    // TODO: adjust skip according to EXT_ANNOT to support multi-bytes
    return 1;
  }

  void Move(UINT32 idx, UINT32 bytes) {
    Is_True(idx + bytes < _size, ("out of bound"));
    while (idx < _size) {
      _annot[idx] = _annot[idx + bytes];
    }
    _size -= bytes;
  }

public:
  // constructor
  EANT_UTIL(UINT8 *buffer, UINT32 size) : _annot(buffer), _size(0), _max_size(size) { }

  // set annotation
  BOOL Set(EANT_KIND annot, UINT32 parm) {
    Is_True(annot < ANT1_MAX && parm <= 31, ("bad param"));
    // TODO: support multi-bytes
    if (_size + 1 == _max_size)
      return FALSE;
    _annot[_size] = (parm << 3) | annot;
    ++ _size;
    return TRUE;
  }

  // get annotation
  UINT32 Get(EANT_KIND annot) const {
    Is_True(annot < ANT1_MAX, ("bad param"));
    // TODO: support multi-bytes
    UINT32 idx = 0;
    while (idx < _size) {
      UINT8 v = _annot[idx];
      EANT_KIND x = (EANT_KIND)(v & 0x7);
      if (x == annot)
        return v >> 3;
      idx += Skip(x);
    }
    return UINT32_MAX;
  }

  // clear annotation
  void Clear(EANT_KIND annot) {
    Is_True(annot < ANT1_MAX, ("bad param"));
    // TODO: support multi-bytes
    UINT32 idx = 0;
    while (idx < _size) {
      UINT8 v = _annot[idx];
      EANT_KIND x = (EANT_KIND)(v & 0x7);
      if (x == annot) {
        Move(idx, 1);
        return;
      }
      idx += Skip(x);
    }
  }

  // dump annotation into file
  void Dump(FILE *fp) const;

};

#endif /* vsa_annot_ext_INCLUDED */
