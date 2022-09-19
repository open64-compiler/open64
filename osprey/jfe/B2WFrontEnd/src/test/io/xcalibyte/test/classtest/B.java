package io.xcalibyte.test.classtest;

public class B extends AA{
    @Override
    public void a_func(){
        System.out.println("I'm B");
    }

    public static void main(String[] args){
        IA new_ia1 = new B();
        new_ia1.ia_func();

        AA p = new B();
        p.aa_func();
        p.ia_func();
        p.a_func();

        IA p1 = p;
        p1.ia_func();
    }

    @Override
    public int ia_func() {
        return 0;
    }

    @Override
    public int ia_func(int a) {
        return 0;
    }

    @Override
    public int fan2() {
        return 0;
    }

    @Override
    public int fan3() {
        return 0;
    }
}
