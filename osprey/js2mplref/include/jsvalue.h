#ifndef JSVALUE_H
#define JSVALUE_H
#define JSFUNCPROP_STRICT ((uint8_t)0x01)
#define JSFUNCPROP_NATIVE ((uint8_t)0x02)
#define JSFUNCPROP_USERFUNC ((uint8_t)0x04)
namespace maple {
enum __jstype {
  JSTYPE_UNDEFINED = 0,
  JSTYPE_NULL,
  JSTYPE_BOOLEAN,
  JSTYPE_STRING,
  JSTYPE_NUMBER,
  JSTYPE_OBJECT,
  JSTYPE_ENV,
  JSTYPE_UNKNOWN,
  JSTYPE_NONE,
};

// Bit is set for 16-bit code units
#define JSSTRING_UNICODE ((uint8_t)0x01)
// Bit is set for non-const code units
#define JSSTRING_GEN ((uint8_t)0x02)
// Bit is set for large strs whose length > 255
#define JSSTRING_LARGE ((uint8_t)0x04)

enum js_builtin_id {  // must in accordance with js_value.h:js_builtin_id in the runtime
  JS_BUILTIN_GLOBAL = 0,
  JS_BUILTIN_OBJECT,
  JS_BUILTIN_OBJECT_PROTOTYPE,
  JS_BUILTIN_FUNCTION,
  JS_BUILTIN_FUNCTION_PROTOTYPE,
  JS_BUILTIN_ARRAY,
  JS_BUILTIN_ARRAY_PROTOTYPE,
  JS_BUILTIN_STRING,
  JS_BUILTIN_STRING_PROTOTYPE,
  JS_BUILTIN_BOOLEAN,
  JS_BUILTIN_BOOLEAN_PROTOTYPE,
  JS_BUILTIN_NUMBER,
  JS_BUILTIN_NUMBER_PROTOTYPE,
  JS_BUILTIN_EXPORTS,
  JS_BUILTIN_MODULE,
  JS_BUILTIN_MATH,
  JS_BUILTIN_JSON,
  JS_BUILTIN_COUNT
};
}  // namespace maple
#endif
