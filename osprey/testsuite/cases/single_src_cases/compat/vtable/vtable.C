class A{
public:
	virtual void foo1();
	virtual void foo2();
};
void A::foo1(){}
void A::foo2(){}
int main(void){
	A *a = new A();
	delete a;
	return 0;
}

