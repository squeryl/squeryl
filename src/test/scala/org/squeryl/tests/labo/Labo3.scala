package org.squeryl.tests

object Labo3 {

  class A {
    def ~ = new A
  }
  class B {
    def ~ = new B
  }

  implicit def int2A(i: Int) = new A  
  implicit def int2B(i: Int) = new B

  def takeA(a: A) = {}
  def takeB(a: B) = {}

//  Here's the thing, a call to
//  1~
//  is clearly not valid, since there is totally  ambiguous,
//  now wouldn't it be great if the type checker could infer
//  that in this context :
//  takeA(1~)
//  the result of 1~ has to be of type A ?
}