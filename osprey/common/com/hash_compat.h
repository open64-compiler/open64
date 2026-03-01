/*
 * hash_compat.h — GCC hash_map compatibility for 64-bit integer types
 *
 * GCC's <ext/hash_map> does not provide __gnu_cxx::hash specializations
 * for 'long long' or 'unsigned long long'. This header adds them.
 *
 * Include this header AFTER <ext/hash_map> and BEFORE any hash_map
 * instantiation with 64-bit integer keys.
 */

#ifndef HASH_COMPAT_H
#define HASH_COMPAT_H

#ifdef __cplusplus

#include <ext/hash_map>

namespace __gnu_cxx {
  template<> struct hash<unsigned long long> {
    size_t operator()(unsigned long long __x) const {
      return static_cast<size_t>(__x);
    }
  };
  template<> struct hash<long long> {
    size_t operator()(long long __x) const {
      return static_cast<size_t>(__x);
    }
  };
}

#endif /* __cplusplus */
#endif /* HASH_COMPAT_H */
