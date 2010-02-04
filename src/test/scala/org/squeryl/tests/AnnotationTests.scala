package org.squeryl.tests

import org.squeryl.annotations.{Row, Column}
import org.squeryl.Schema

@Row("T_TOASTER")
class Toaster(

  @Column(optionType=classOf[Int])
  var yearOfManufacture: Option[Int],

//  @Column(optionType=classOf[String], length=25)
//  var countryOfOrigin: Option[String],

  @Column(length=25)
  var countryOfOrigin: String,

  @Column(name="BRAND_NAME", length=32)
  var brandName: String) {

  @Column(name="WEIGHT", optionType=classOf[Float])
  var weightInGrams: Option[String] = None
}

class AnnotationTests {


  class C(
    @Column(optionType=classOf[Long]) var j: Option[Long],
    @Column(optionType=classOf[java.lang.String]) var k: Option[String]) (
  
    @Column(optionType=classOf[Int])
    var i:Option[Int]
  )

  def allTests = {
    //rudimentaryTests
  }

  class ToastersInc extends Schema {

    val toasters = table[Toaster]
  }

  def testMetaData = {

    scalaReflectionTests
    
    val ti = new ToastersInc
    import ti._

    
    val brandNameMD = toasters.findFieldMetaDataForProperty("brandName").get
    assert(brandNameMD.name == "BRAND_NAME")
    assert(brandNameMD.length == 32)

  }

  /**
   * There has been a Scala bug with obtaining a Class[_] member in annotations,
   * if this test fails, it means that Scala has regressed TODO: file a bug 
   */
  def scalaReflectionTests = {
    val colAnotations =
      classOf[C].getDeclaredFields.toList.sortBy(f => f.getName).map(f => f.getAnnotations.toList).flatten

    val t1 = colAnotations.apply(0).asInstanceOf[Column].optionType
    val t2 = colAnotations.apply(1).asInstanceOf[Column].optionType
    val t3 = colAnotations.apply(2).asInstanceOf[Column].optionType

    assert(classOf[Int].isAssignableFrom(t1), "expected classOf[Int], got " + t1.getName)
    assert(classOf[Long].isAssignableFrom(t2), "expected classOf[Long], got " + t2.getName)
    assert(classOf[String].isAssignableFrom(t3), "expected classOf[String], got " + t3.getName)
  }

  class ToMapConverter[T,U](i: Iterable[(T,U)]) {

    def toMap2: Map[T,U] = error("implementation is not relevant here...")
  }

  implicit def iterable2ToMapConverter[T,U](i: Iterable[(T,U)]) = new ToMapConverter[T,U](i)
  
  val candidateForConversion:Iterable[(String,Int)] = List(("a",1),("b",2))  
  candidateForConversion.toMap2 

//  class Z[K](k:K)
//
//  class UViewOnlyAvailableToZs[K,A] {
//    def tryMe = {}
//  }
//
//  class U[A](a:A)
//
//  implicit def uOfZ2View[K, A <: Z[K]](u: U[A]) = new UViewOnlyAvailableToZs[K,A]
//
//  val u = new U(new Z(0))
//
//  u : U[Z[Int]] // sanity check
//
//  uOfZ2View[Int,Z[Int]]( u ).tryMe  // this compiles
//
//  uOfZ2View( u ).tryMe // <- this won't compile
//
//  u.tryMe // <- my goal is to have this compile only when u is U[A] and A <: Z[K], but the line above needs to compile first
//  // *and* I need uOfZ2View to have 2 type params, because in my real scenario, the class UViewOnlyAvailableToZs[K,A]
//  // needs 2 type params.

  class Z[K](k:K)

  class UViewOnlyAvailableToZs[K,A] {
    def tryMe(i:Int) = {}
  }

  class U[A](a:A) {
    def tryMe(param2TrickTheTypeSystem: List[String])
  }

  implicit def uOfZ2View[K, A <: Z[K]](u: U[A]) = new UViewOnlyAvailableToZs[K,A]

  val u = new U(new Z(0))

  u : U[Z[Int]] // sanity check

  uOfZ2View[Int,Z[Int]]( u ).tryMe(1)  // this compiles

  //uOfZ2View( u ).tryMe // <- this won't compile

  u.tryMe(1) // <- my goal is to have this compile only when u is U[A] and A <: Z[K], but the line above needs to compile first
  // *and* I need uOfZ2View to have 2 type params, because in my real scenario, the class UViewOnlyAvailableToZs[K,A]
  // needs 2 type params.  
}