# Dynamic Type Language Runtime for JavaScript

## 1. Object representation

All object with reference semantics are represented by a 64-bit handle.
```
typedef long long jsrt_handle;
```
The representation for object with value semantics are carefully designed so that it won't be messed up with jsrt_handle. The lower 2-bit are reserved for object kind:

|**bits**|**description**         |
|:-------|:-----------------------|
|00      |Integer literal or value|
|01      |Float literal or value  |
|10      |Object with handle      |
|11      |Object with handle      |

### 1.1 Object with value semantics
Object with value semantics are stored and passed by value. No GC needed for this kind of object.

#### 1.1.1 Integer literal:
Integer literal is represented with higher 62b for value and lower 2b hard-coded to '00b' for kind. Value range is from -2^62~2^62.

```
        |63................32|31...............10|
--------+--------------------+-------------------+
Integer |xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx00|
--------+----------------------------------------+
        |            value = rep >> 2            |
        +----------------------------------------+
```
```
bool IsIntegerLiteral(jsrt_handle h) {
  return (h & 0x3) == 0;
}
jsrt_handle IntegerToHandle(int64 v) {
  return v << 2;
}
int64 HandleToInteger(jsrt_handle h) {
  return h >> 2;
}
```

### 1.1.2 Boolean:
Boolean is represented with '000b' for false and '100b' fot true.

#### 1.1.2 Float literal:
Float literal follows IEEE 754 double precision floating point format. The lower 2 bit of mantisa was reset to '01' to indicate this is a float literal.
```
        |63................32|31...............10|
--------+--------------------+-------------------+
Float   |xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx01|
--------+----------------------------------------+
        |      IEEE 754 double precision         |
        +----------------------------------------+
```
```
bool IsFloatLiteral(jsrt_handle h) {
  return (h & 0x3) == 1;
}
jsrt_handle FloatToHandle(double v) {
  return (reinterpret_cast<int64_t>(v) & (~0x3)) | 0x1;
}
double HandleToDouble(jsrt_handle h) {
  return reinterpret_cast<double>(h);
}
```

#### 1.1.3 Number:
Same as Float literal if range and precision is enough. Otherwise a 96b (or 128b) data structure is required.

#### 1.1.4 BigInt:
Same as Integer literal.

#### 1.1.4 Date:
Higher 32b is seconds since the Epoch. Middle 16b is the milliseconds. Middle 8b not used. The lowest 8b is '00000010b'.
```
        |63................32|31..16|15..8|7.....10|
--------+--------------------+------+-----+--------+
Date    |32b seconds         |16b ms|N/A  |00000010|
--------+--------------------+------+-----+--------+
```
```
bool IsDate(jsrt_handle h) {
  return (uint8)(h) == 0b10;
}
```


### 1.2 Object with const reference semantics
Object with const reference semantics are stored in constant area and a handle is used to access the object. No GC required on these objects.

#### 1.2.1 String literal
Contents of the string literal is saved in constant pool. The index (offset) is stored in higher 32b. Middle 24b stores the length of the string lieral. The lowest 8b is '00000110'.

```
        |63................32|32...............10|
--------+--------------------+----------+--------+
CstStr  |constant pool index | 24b-len  |00000110|
--------+--------------------+----------+--------+
```
```
bool IsStringLiteral(jsrt_handle h) {
  return (uint8_t)h == 0b110;
}
```

#### 1.2.2 Function object
Higher 32b stores the index of the function in functioon description table, which describes the prototype include attributes and methods. 8b for parameter count. 8b for function flags. 8b for call template (uwasm call, native call, etc). The lowest 8b is '00001010b'.
```
        |63................32|32...............10|
--------+--------------------+----------+--------+
FuncObj |function desc index |parm count|00001010|
--------+--------------------+----------+--------+
```
```
bool IsFunctionObject(jsrt_handle h) {
  return (uint8_t)h == 0b1010;
}
```

### 1.3 Object with write-once reference semantics

#### 1.3.1 String
```
        |63................32|32...............10|
--------+--------------------+----------+--------+
CstStr  |heap pool index     | 24b-len  |00001110|
--------+--------------------+----------+--------+
```
```
struct DynString {
  uint32_t rc;
  uint32_t capacity;
  uint8_t  buffer[];
};
bool IsString(jsrt_handle h) {
  return (uint8_t)h == 0b1110;
}
```

#### 1.3.2 RegExp
```
        |63................32|32...............10|
--------+--------------------+----------+--------+
RegExp  |heap pool index     | 24b-len  |00010010|
--------+--------------------+----------+--------+
```
```
struct RegExp {
  uint32_t rc;
};
bool IsRegExp(jsrt_handle h) {
  return (uint8_t)h == 0b10010;
}
```

### 1.4 Object with dynamic reference semantics

#### 1.4.1 Array
```
        |63................32|32...............10|
--------+--------------------+----------+--------+
Array   |heap pool index     | 24b-len  |00010110|
--------+--------------------+----------+--------+
```
```
struct Array {
  uint32_t rc;
  uint32_t capacity;
  uint32_t first_elem;
  uint32_t filler;
};
bool IsArray(jsrt_handle h) {
  return (uint8_t)h == 0b10110;
}
```

#### 1.4.2 FixArray
```
        |63................32|32...............10|
--------+--------------------+----------+--------+
FixArray|heap pool index     | 24b-len  |00011010|
--------+--------------------+----------+--------+
```
```
TODO: ArrayBuffer
struct Array {
  uint32_t rc;
  uint32_t capacity : 24;
  uint8_t  elem_type;
  uint32_t first_elem;
  uint32_t filler;
};
bool IsFixArray(jsrt_handle h) {
  return (uint8_t)h == 0x11010;
}
```


#### 1.4.3 Map
```
        |63................32|32...............10|
--------+--------------------+----------+--------+
Map     |heap pool index     | 24b-size |00011110|
--------+--------------------+----------+--------+
```
```
struct Map {
  uint32_t rc;
  uint32_t root;
};
bool IsMap(jsrt_handle h) {
  return (uint8_t)h == 0b11110;
}
```

#### 1.4.4 Set
```
        |63................32|32...............10|
--------+--------------------+----------+--------+
Set     |heap pool index     | 24b-size |00100010|
--------+--------------------+----------+--------+
```
```
struct Set {
  uint32_t rc;
  uint32_t root;
};
bool IsSet(jsrt_handle h) {
  return (uint8_t)h == 0b100010;
}
```

#### 1.4.5 WeakMap
```
        |63................32|32...............10|
--------+--------------------+----------+--------+
WeakMap |heap pool index     | 24b-size |00100110|
--------+--------------------+----------+--------+
```
```
struct WeakMap {
  uint32_t rc;
  uint32_t root;
};
bool IsWeakMap(jsrt_handle h) {
  return (uint8_t)h == 0b100110;
}
```

#### 1.4.6 WeakSet
```
        |63................32|32...............10|
--------+--------------------+----------+--------+
WeakSet |heap pool index     | 24b-size |00101010|
--------+--------------------+----------+--------+
```
```
struct WeakSet {
  uint32_t rc;
  uint32_t root;
};
bool IsWeakSet(jsrt_handle h) {
  return (uint8_t)h == 0b101010;
}
```

#### 1.4.7 ArrayBuffer
```
bool IsArrayBuffer(jsrt_handle h) {
  return (uint8_t)h == 0b101110;
}
```

#### 1.4.8 SharedArrayBuffer
```
bool IsSharedArrayBuffer(jsrt_handle h) {
  return (uint8_t)h == 0b110010;
}
```

#### 1.4.9 DataView
```
bool IsDataView(jsrt_handle h) {
  return (uint8_t)h == 0b110110;
}
```

#### 1.4.10 Promise
```
bool IsPromise(jsrt_handle h) {
  return (uint8_t)h == 0b111010;
}
```

#### 1.4.11 Generator
```
bool IsPromise(jsrt_handle h) {
  return (uint8_t)h == 0b111110;
}
```

#### 1.4.12 GeneratorFunction
```
bool IsGeneratorFunction(jsrt_handle h) {
  return (uint8_t)h == 0b1000010;
}
```

#### 1.4.13 AsyncFunction
```
bool IsAsyncFunction(jsrt_handle h) {
  return (uint8_t)h == 0b1000010;
}
```

#### 1.4.14 Proxy
```
bool IsAsyncFunction(jsrt_handle h) {
  return (uint8_t)h == 0b1000110;
}
```

#### 1.4.15 Intl
|**Name**                |**Id**    |
|:-----------------------|:---------|
|Intl                    |0x01001010|
|Intl.Collator           |0x01001110|
|Intl.DateTimeFormat     |0x01010010|
|Intl.ListFormat         |0x01010110|
|Intl.NumberFormat       |0x01011010|
|Intl.PluralRules        |0x01011110|
|Intl.RelativeTimeFormat |0x01100010|
|Intl.Locale             |0x01100110|

#### 1.4.16 WebAssembly
|**Name**                |**Id**    |
|:-----------------------|:---------|
|WebAssembly             |0x01101010|
|WebAssembly.Module      |0x01101110|
|WebAssembly.Instance    |0x01110010|
|WebAssembly.Memory      |0x01110110|
|WebAssembly.Table       |0x01111110|
|WebAssembly.CompileError|0x10000010|
|WebAssembly.LinkError   |0x10000110|
|WebAssembly.RuntimeError|0x10001010|

#### 1.4.17 User Object
|**Name**                |**Id**    |
|:-----------------------|:---------|
|User object             |0x00000011|

#### 1.4.18 Reserved
|**Name**                |**Id**    |
|:-----------------------|:---------|
|Reserved                |0x11111111|

## Other languages
python
php
lua
ruby
