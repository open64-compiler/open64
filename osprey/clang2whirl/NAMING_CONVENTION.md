### naming convention

follow the code located into osprey/be/opt

### rule

#### class

1. class name: all uppercase, with underscores between words
2. member field name: start with underscore, lowercase, with underscores between words
3. brace: open brace is on the same line with class name, close brace is always on the last line by itself
4. getter method: use same name with member field name, start with uppercase
5. setter method: start with Set_, followed with member field name
4. defined print function: void Printf(FILE *fp=stderr)
5. undefined unwanted method: copy constructor, assign operator, or default constructor
6. comment: a simple description of this class

#### function

1. name: first word start with uppercase letter, other letter are lowercase, with underscores between words
2. return: return type is always on the previous line of function name
3. function body brace: 
    a. multi line function: open brace is always on the next line of function name, close brace is always on the last line by itself
    b. single line function: open brace is always on the same line of function name, close brace is always on the same line of open brace
4. code block brace: open brace is always on the same line of key words (if, else, for, while etc), close brace is always on the last line by itself

#### variable

1. name: global variable or local variable should be lowercase, with underscores between words

#### enum

1. enum name: all uppercase, with underscores between words
2. value name: all uppercase, with underscores between words
3. brace: open brace is on the same line with class name, close brace is always on the last line by itself

### example

``` c++

// =============================================================================
// CLASS_NAME : a small description of this class
// =============================================================================
class CLASS_NAME {

private:
  CLASS_NAME(void);
  CLASS_NAME(const CLASS_NAME &);
  CLASSNAME& operator = (const CLASS_NAME &);

private:
  int _member_name;

public:
  int       Member_name(void) const         { return _member_name; }
  void      Set_member_name(int m)      { _member_name = m; }
  void      Print(FILE *fp=stderr);
};

void
CLASS_NAME::Print(FILE *fp)
{
  int local_variable;
  fprintf(fp, "debug info.\n");
  if (_member_name > 0) {
    fprintf(fp, "x\n");
  } else {
    fprintf(fp, "y\n");
  }
}

enum ENUM_NAME {
  ENUM_ONE,
  ENUM_TWO,
};

```
