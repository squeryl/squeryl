package org.squeryl.tests.labo;

public class JavaTests {

    class A {}
    class B {}

    class C<U> {}

    //<T extends A> void m(C<T> t) {}
    <T extends B> void m(C<T> t) {}

    //public static void main(String[] args) {}
}
