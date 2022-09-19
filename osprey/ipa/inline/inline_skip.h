/*
 * Copyright (C) 2020-2021 Xcalibyte Limited, Inc.  All Rights Reserved.
 */

//-*-c++-*-

#ifndef INLINE_SKIP_H
#define INLINE_SKIP_H 1

#include "defs.h"
#include <ext/hash_set>

class INL_SKIPLST {
  struct streq {
    bool operator()(const char *s1, const char *s2) const {
      return strcmp(s1, s2) == 0;
    }
  };
  typedef __gnu_cxx::hash_set<const char*, __gnu_cxx::hash<const char*>, streq> STR_MAP;

private:
  const char *_skiplst_file;      // File that contains the inline skiplst
  STR_MAP     _skiplst;
  
  INL_SKIPLST(void);                            // REQUIRED UNDEFINED UNWANTED methods
  INL_SKIPLST(const INL_SKIPLST&);              // REQUIRED UNDEFINED UNWANTED methods
  INL_SKIPLST& operator = (const INL_SKIPLST&); // REQUIRED UNDEFINED UNWANTED methods

  const char *Skiplst_file(void)  { return _skiplst_file; }

public:
  INL_SKIPLST(const char *sf) : _skiplst_file(sf) { if (sf) Build_skiplst(sf); }
  ~INL_SKIPLST(void) {}

  void    Build_skiplst(const char *);
  BOOL    Inl_skip(const char *s)   { return _skiplst.find(s) != _skiplst.end(); }

  void    Print(FILE *fp) const;
};

#endif //INLINE_SKIP_H
