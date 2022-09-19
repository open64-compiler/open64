/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

//-*-c++-*-
// =============================================================================
// =============================================================================
//
// Module: vsa_json_writter.h
//
// =============================================================================
//
// Description:
//
// write vsa report in json plain text or cbor format
//
// =============================================================================
// =============================================================================

#ifndef vsa_json_writter_INCLUDED
#define vsa_json_writter_INCLUDED

#include "defs.h"
#include "opt_vsa_report.h"
#include <stack>

// tag kind, array or map in json
enum TAG_KIND {
  TAG_ARRAY,     // array in json, [ ... ]
  TAG_MAP        // map in json, { ... }
 };

// =============================================================================
//
// VSA_JSON_WRITTER_TEXT
// Write json in plain text
//
// =============================================================================
class VSA_JSON_WRITTER_TEXT {
private:
  FILE  *_fp;       // file to be written
  UINT32 _indent;   // ident for current item
  UINT32 _comma;    // if a comma is needed before current item

public:
  // constructor
  VSA_JSON_WRITTER_TEXT(FILE* fp) : _fp(fp), _indent(0), _comma(FALSE) { }

  // start an array or map
  template<TAG_KIND _tag>
  void Start(const char* key, UINT32 size) {
    const char ch = _tag == TAG_ARRAY ? '[' : '{';
    Is_True(_fp != NULL, ("fp is null"));
    if (_comma)
      fprintf(_fp, ",\n");
    else
      _comma = TRUE;  // need a comma for next item
    if (key != NULL) {
      fprintf(_fp, "%*s\"%s\": %c\n", _indent, " ", key, ch);
    }
    else {
      fprintf(_fp, "%*s%c\n", _indent, " ", ch);
    }
    ++_indent;
    _comma = FALSE;  // no comma needed for next item
  }

  // end an array or map
  template<TAG_KIND _tag>
  void End(const char* key, UINT32 size) {
    const char ch = _tag == TAG_ARRAY ? ']' : '}';
    Is_True(_fp != NULL, ("fp is null"));
    --_indent;
    _comma = TRUE;   // need a comma for next item
    fprintf(_fp, "\n%*s%c", _indent, " ", ch);
  }

  // append <key, value> to current map
  void Append(const char* key, long value) {
    Is_True(_fp != NULL, ("fp is null"));
    Is_True(key != NULL, ("null key for map"));
    if (_comma)
      fprintf(_fp, ",\n");
    else
      _comma = TRUE;  // need a comma for next item
    fprintf(_fp, "%*s\"%s\": %ld", _indent, " ", key, value);
  }

  // append <key, value> to current map
  void Append(const char* key, const char* value) {
    Is_True(_fp != NULL, ("fp is null"));
    Is_True(key != NULL, ("null key for map"));
    if (_comma)
      fprintf(_fp, ",\n");
    else
      _comma = TRUE;  // need a comma for next item
    if (value != NULL) {
      fprintf(_fp, "%*s\"%s\":\"", _indent, " ", key);
      Json_write(value, _fp);
      fprintf(_fp, "\"");
    }
    else {
      fprintf(_fp, "%*s\"%s\":null", _indent, " ", key);
    }
  }

  // flush buffer
  void Flush() {
    Is_True(_fp != NULL, ("fp is null"));
    fflush(_fp);
  }
};

// =============================================================================
//
// VSA_JSON_WRITTER_CBOR
// Write json in cbor format
//
// =============================================================================
class VSA_JSON_WRITTER_CBOR {
private:
  // CBOR major types, refer
  // https://tools.ietf.org/html/rfc7049
  enum MAJOR_TYPE {
    T_UINT      = 0,    // unsigned integer
    T_NINT      = 1,    // negative integer
    T_BSTR      = 2,    // byte string
    T_USTR      = 3,    // utf-8 string
    T_ARRAY     = 4,    // array
    T_MAP       = 5,    // map
    T_TAG       = 6,    // tag
    T_FLOAT     = 7,    // float
    T_ARRAY_INF = 0x9f, // indefinite-length array
    T_MAP_INF   = 0xbf, // indefinite-length array
    T_NULL      = 0xf6, // NULL
    T_BREAK     = 0xff, // break current inf array or map
  };
  FILE *_fp;            // file to be written
  char  _buffer[256];   // buffer
  char *_bufptr;        // current pointer to buffer

  // write buffer to file and reset buffer pointer
  void flush_buffer() {
    Is_True(_fp != NULL, ("fp is null"));
    unsigned long ret = fwrite(_buffer, 1, _bufptr - _buffer, _fp);
    Is_True(ret == _bufptr - _buffer, ("fwrite failed"));
#ifdef Is_True_On
    memset(_buffer, 0, sizeof(_buffer));
#endif
    _bufptr = _buffer;
  }

  // write data to file directly
  void write_direct(const char* bytes, unsigned long len) {
    Is_True(_fp != NULL, ("fp is null"));
    unsigned long ret = fwrite(bytes, 1, len, _fp);
    Is_True(ret == len, ("fwrite failed"));
  }

  // check if unused buffer is large enough for len
  // return NULL if len is larger than buffer
  char* check_buffer(unsigned long len) {
    unsigned long avail = _buffer + sizeof(_buffer) - _bufptr;
    if (len >= avail) {
      // flush current buffer
      flush_buffer();
    }
    if (len >= sizeof(_buffer))
      return NULL;     // data should be written directly
    else
      return _bufptr;  // data can be append to buffer
  }

  // append a byte to buffer
  void append_byte(unsigned int byte) {
    Is_True(_buffer + sizeof(_buffer) - _bufptr > 1,
            ("not enough buffer"));
    *_bufptr++ = byte;
  }

  // append bytes to buffer
  void append_bytes(const char* bytes, unsigned long len) {
    Is_True(_buffer + sizeof(_buffer) > _bufptr + len,
            ("not enough buffer"));
    memcpy(_bufptr, bytes, len);
    _bufptr += len;
  }

  // put a byte
  void put_byte(unsigned int byte) {
    check_buffer(1);   // need 1 byte
    append_byte(byte);
  }

  // put bytes who is len bytes
  void put_bytes(const char* bytes, unsigned long len) {
    if (check_buffer(len) == NULL) {
      // write to file directly
      write_direct(bytes, len);
    }
    else {
      append_bytes(bytes, len);
    }
  }

  // put value with a major type
  void put_type_value(unsigned int type, unsigned long value) {
    type <<= 5;
    if (value < 24ULL) {
      check_buffer(1);  // need 1 byte
      append_byte(type | value);
    }
    else if (value < 256ULL) {
      check_buffer(2);  // need 2 byte
      append_byte(type | 24);
      append_byte(value);
    }
    else if (value < 65536ULL) {
      check_buffer(3);  // need 3 byte
      append_byte(type | 25);
      append_byte(value >> 8);
      append_byte(value);
    }
    else if (value < 4294967296ULL) {
      check_buffer(5);  // need 5 byte
      append_byte(type | 26);
      append_byte(value >> 24);
      append_byte(value >> 16);
      append_byte(value >> 8);
      append_byte(value);
    }
    else {
      check_buffer(9);  // need 9 byte
      append_byte(type | 27);
      append_byte(value >> 56);
      append_byte(value >> 48);
      append_byte(value >> 40);
      append_byte(value >> 32);
      append_byte(value >> 24);
      append_byte(value >> 16);
      append_byte(value >> 8);
      append_byte(value);
    }
  }

  // put an integer value
  void put_int(long value) {
    if (value < 0)
      put_type_value(T_NINT, -(value+1));  // negative integer
    else
      put_type_value(T_UINT, value);       // unsigned integer
  }

  // put a string value
  void put_string(const char* str) {
    if (str != NULL) {
      unsigned long len = strlen(str);
      put_type_value(T_USTR, len);
      put_bytes(str, len);
    }
    else {
      put_byte((unsigned int)T_NULL);
    }
  }

public:
  // constructor
  VSA_JSON_WRITTER_CBOR(FILE* fp) : _fp(fp), _bufptr(_buffer)
  {
#ifdef Is_True_On
    memset(_buffer, 0, sizeof(_buffer));
#endif
  }

  // destructor
  ~VSA_JSON_WRITTER_CBOR()
  {
    Is_True(_bufptr == _buffer, ("buffer not flushed"));
  }

  // start a new array or map
  template<TAG_KIND _tag>
  void Start(const char* key, UINT32 size)
  {
    // put key if it's not NULL
    if (key != NULL)
      put_string(key);

    if (size == 0) {
      unsigned int data = _tag == TAG_ARRAY ? T_ARRAY_INF
                                            : T_MAP_INF;
      put_byte(data);   // indefinite-length array
    }
    else {
      unsigned int data = _tag == TAG_ARRAY ? T_ARRAY
                                            : T_MAP;
      put_type_value(data, size);
    }
  }

  // end current array or map
  template<TAG_KIND _tag>
  void End(const char* key, UINT32 size)
  {
    if (size == 0)
      put_byte((unsigned int)T_BREAK);
  }

  // append <key, value> to current map
  void Append(const char* key, long value)
  {
    put_string(key);
    put_int(value);
  }

  // append <key, value> to current map
  void Append(const char* key, const char* value)
  {
    put_string(key);
    put_string(value);
  }

  // flush buffer
  void Flush() {
    Is_True(_fp != NULL, ("fp is null"));
    if (_bufptr != _buffer)
      flush_buffer();
    fflush(_fp);
  }
};

// VSA_JSON_WRITTER
// wrapper class for json writter. _IMPL can be either
// plain text json writter or cbor writter
template<class _IMPL>
class VSA_JSON_WRITTER : public _IMPL {
private:
#ifdef Is_True_On
  // to verify if json are constructed correctly
  struct TAG {
    const char* _key;       // name of the tag
    UINT32      _size;      // size of the tag, 0 is infinite
    TAG_KIND    _tag;       // kind of the tag
    TAG(const char* key, UINT32 size, TAG_KIND tag)
     : _key(key), _size(size), _tag(tag) { }
  };
  
  // stack to verify if tag are opened/closed and
  //  attrs are appended correctly
  std::stack<TAG> _stack;
#endif

  // unique instance for this writter
  static VSA_JSON_WRITTER<_IMPL> *_instance;

  // constructor
  VSA_JSON_WRITTER(FILE* fp) : _IMPL(fp) { }

public:
  static VSA_JSON_WRITTER<_IMPL>& Get_writter(FILE* fp)
  {
    if (_instance == NULL)
      _instance = new VSA_JSON_WRITTER<_IMPL>(fp);
    return *_instance;
  }

  // destructor, verify all tag are closes
  ~VSA_JSON_WRITTER() { Is_True(_stack.empty(), ("stack not empty")); }

public:
  // start a new tag
  template<TAG_KIND _tag>
  void Start(const char* key, UINT32 size)
  {
#ifdef Is_True_On
    if (key == NULL) {
      Is_True(_stack.empty() ||
              _stack.top()._tag == TAG_ARRAY, ("not top nor array"));
    }
    else {
      Is_True(!_stack.empty() &&
              _stack.top()._tag == TAG_MAP, ("not map"));
    }
    _stack.push(TAG(key, size, _tag));
#endif
    _IMPL::template Start<_tag>(key, size); 
  }

  // end current tag
  template<TAG_KIND _tag>
  void End(const char* key, UINT32 size)
  {
#ifdef Is_True_On
    Is_True(!_stack.empty(), ("stack empty"));
    TAG& top = _stack.top();
    Is_True((top._key == key ||
             strcmp(top._key, key) == 0) &&
            top._size == size &&
            top._tag == _tag, ("stack element mismatch"));
    _stack.pop();
#endif
    _IMPL::template End<_tag>(key, size);
  }

  // append pair of <key, value> to current map
  void Append(const char* key, long value)
  {
#ifdef Is_True_On
    Is_True(key != NULL, ("key is null"));
    Is_True(!_stack.empty(), ("stack empty"));
    TAG& top = _stack.top();
    Is_True(top._tag == TAG_MAP, ("stack top wrong"));
#endif
    _IMPL::Append(key, value);
  }

  // append pair of <key, value> to current map
  void Append(const char* key, const char* value)
  {
#ifdef Is_True_On
    Is_True(key != NULL, ("key is null"));
    Is_True(!_stack.empty(), ("stack empty"));
    TAG& top = _stack.top();
    Is_True(top._tag == TAG_MAP, ("stack top wrong"));
#endif
    _IMPL::Append(key, value);
  }

  // flush buffer
  void Flush()
  {
    _IMPL::Flush();
  }
};


// typedef for plain text writter
typedef VSA_JSON_WRITTER<VSA_JSON_WRITTER_TEXT> PLAIN_TEXT_WRITTER;

// typedef for cbor writter
typedef VSA_JSON_WRITTER<VSA_JSON_WRITTER_CBOR> CBOR_WRITTER;

#endif /* vsa_json_writter_INCLUDED */

