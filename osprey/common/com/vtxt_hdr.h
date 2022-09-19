//-*-c++-*-

/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited. 
 */

#ifndef VTXT_HDR_H
#define VTXT_HDR_H

typedef enum {
  VTXT_ATTR_NONE  = 0x0,
  VTXT_ATTR_XFA   = 0x1,   // use Interface: Put_xfa(), Get_xfa()
  VTXT_ATTR_NOXFA = 0x2,   // use Interface: Put_noxfa(), Get_noxfa()
} VTXT_HDR_ATTR;
//
// =============================================================================
//
// VTXT_HDR is the fist line in the .vtxt file, served as file header
//
// =============================================================================
//
class VTXT_HDR {
  enum { RSVD_SZ = 4 };
  union {
    mUINT64  _rsvd[RSVD_SZ];
  };
  struct {
    mUINT8   _major_ver;  // three level versioning
    mUINT8   _minor_ver;  // three level versioning
    mUINT8   _mminor_ver; // three level versioning
  };
  VTXT_HDR(void);                          // REQUIRED UNDEFINED UNWANTED methods
  VTXT_HDR(const VTXT_HDR&);               // REQUIRED UNDEFINED UNWANTED methods
  VTXT_HDR& operator = (const VTXT_HDR&);  // REQUIRED UNDEFINED UNWANTED methods

  void Print(FILE *fp) {
    fprintf(fp, "%.16llx%.16llx%.16llx%.16llx\n", _rsvd[0], _rsvd[1], _rsvd[2], _rsvd[3]);
  }

  // define these two interface to centralize in one place which 8-byte area to use
  void Attr(VTXT_HDR_ATTR a)   { _rsvd[0] |= a; }
  BOOL Is_attr(VTXT_HDR_ATTR a){ return (_rsvd[0] & a) != VTXT_ATTR_NONE; }

public:
  VTXT_HDR(mUINT64 r0,
           mUINT64 r1,
           mUINT64 r2,
           mUINT64 r3): _major_ver(0),
                        _minor_ver(0),
                        _mminor_ver(0) {_rsvd[0] = r0; _rsvd[1] = r1;
                                        _rsvd[2] = r2; _rsvd[3] = r3; }
  VTXT_HDR(mUINT64 r): _major_ver(0),
                       _minor_ver(0),
                       _mminor_ver(0) { for (int i = 0; i < 4; ++i) _rsvd[i] = r; }
  VTXT_HDR(mUINT8 maj,
           mUINT8 min,
           mUINT8 mmin): _major_ver(maj),
                         _minor_ver(min),
                         _mminor_ver(mmin){for (int i = 0; i < 4; ++i) _rsvd[i] = 0; }
  VTXT_HDR(char *buf)          { Read_filehdr(buf); }

  ~VTXT_HDR(void) { }


  mUINT8 Major_ver(void)       { return _major_ver; }
  void   Major_ver(int v)      { _major_ver = v;    }
  mUINT8 Minor_ver(void)       { return _minor_ver; }
  void   Minor_ver(int v)      { _minor_ver = v;    }
  mUINT8 MMinor_ver(void)      { return _mminor_ver; }
  void   MMinor_ver(int v)     { _mminor_ver = v;    }

  char  *Version(char *buffer) {
    sprintf(buffer, "%d.%d.%d", Major_ver(), Minor_ver(), MMinor_ver());
    return buffer;
  }

  BOOL   operator==(VTXT_HDR const& rhs) const {
    return (_rsvd[0] == rhs._rsvd[0] &&
            _rsvd[1] == rhs._rsvd[1] &&
            _rsvd[2] == rhs._rsvd[2] &&
            _rsvd[3] == rhs._rsvd[3]);
  }

  void   Put_xfa(void)         { Attr( VTXT_ATTR_XFA );   }
  BOOL   Get_xfa(void)         { return Is_attr( VTXT_ATTR_XFA );   }
  void   Put_noxfa(void)       { Attr( VTXT_ATTR_NOXFA ); }
  BOOL   Get_noxfa(void)       { return Is_attr( VTXT_ATTR_NOXFA ); }

  char  *Filehdr(char *buffer) {
    sprintf(buffer, "%.16llx%.16llx%.16llx%.16llx", _rsvd[0], _rsvd[1], _rsvd[2], _rsvd[3]);
    return buffer;
  }

  void   Read_filehdr(char *buffer) {
    sscanf(buffer, "%16llx%16llx%16llx%16llx", &_rsvd[0], &_rsvd[1], &_rsvd[2], &_rsvd[3]);
  }
};
#endif // VEXT_HDR_H
