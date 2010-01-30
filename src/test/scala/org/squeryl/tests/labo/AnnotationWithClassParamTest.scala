package org.squeryl.tests

import labo.AnnotationWithClassParam


object AnnotationWithClassParamTest {


  class C(
    @AnnotationWithClassParam(optionType=classOf[Int])
    var i:Option[Int]
  )


  def go = {

    val m = classOf[C].getMethods
    val f = classOf[C].getDeclaredFields

    val i = m(0)
    val a = i.getAnnotations()
    val a2 = i.getDeclaredAnnotations

    val fa = f(0).getAnnotations
    val fda = f(0).getDeclaredAnnotations

    //println(a(0))
  }
  
}