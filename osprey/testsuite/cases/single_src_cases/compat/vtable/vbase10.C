//OBJ
//CXXFLAGS: -Wabi

struct A { virtual void f(); char c1; };
struct B { B(); char c2; };
struct C : public A, public virtual B {}; // { dg-warning "ABI" }

