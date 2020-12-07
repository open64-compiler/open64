/*
  Copyright (C) 2019-2020 Xcalibyte Limited, Inc.  All Rights Reserved.

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

  http://www.xcalibyte.com

  For more information, see:
  http://github.com/open64-compiler/open64
  http://gitee.com/open64-compiler/open64

*/

#ifndef OSPREY_C2W_CONVERTER_H
#define OSPREY_C2W_CONVERTER_H

#include "c2w_map.h"

namespace wgen {

class WhirlBuilder;


template <class K, class V>
using MT = hash_map<K, V, wgen_ptr_hash>;

template <class K, class V>
using MTI = typename hash_map<K, V, wgen_ptr_hash>::iterator;

template <class Derived, class K, class V>
class Converter {
private:
  WhirlBuilder *_builder;
  MT<K, V> _m;

public:
  explicit Converter(WhirlBuilder *builder): _builder(builder)  {
  }

  Derived &D() {
    return *static_cast<Derived *>(this);
  }

  WhirlBuilder * B() {
    return _builder;
  }

  V Get(K k) {
    MTI<K, V> it = _m.find(k);
    if (it != _m.end()) {
      return it->second;
    }
    V v = D().Convert(k);
    _m[k] = v;
    return v;
  }

  bool Is_in(K k) {
    MTI<K, V> it = _m.find(k);
    return it != _m.end();
  }
};

}


#endif //OSPREY_C2W_CONVERTER_H
