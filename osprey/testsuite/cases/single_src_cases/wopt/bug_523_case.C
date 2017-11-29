//CFLAGS: -O2 -O3
//openCC -O3 bug_523_case.C
//get wrong result

#include <iostream>
struct already_AddRefed
{
    int a ;
};
static already_AddRefed temp1={19};
class nsINode
{
    virtual void* GetProperty(){};
};
class nsIScriptElement  {
public:
    virtual already_AddRefed GetScriptURI() = 0;
};
class nsHTMLScriptElement : public nsINode,
                            public nsIScriptElement
{
public:
    virtual already_AddRefed GetScriptURI()
    {
        return temp1;
    };

};
int main()
{
    nsIScriptElement *ns;
    already_AddRefed t;

    ns=new nsHTMLScriptElement;
    t = ns->GetScriptURI();
    if ( t.a == temp1.a )
        std::cout << "PASS\n" ;
    else
    {
        std::cout << "FAIL\n" ;
        exit(1);
    }
}
