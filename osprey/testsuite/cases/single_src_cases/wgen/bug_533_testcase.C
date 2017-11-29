//CXXFLAGS:-O0 -c
////This is the testcase of bug #533

void* foo(void* );
static union {
    int a;
    char *b;
};
int main(){
    foo((void*)(&a));
}
