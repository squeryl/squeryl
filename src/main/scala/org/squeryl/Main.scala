package org.squeryl

// import scala.quoted.staging._
import org.squeryl.internals.{ TypeInfo, Tag }

class Professor(val lastName: String, val id: Option[Int], val bossId: Option[Long])
// class Professor(val lastName: String, val id: Option[Int])

object BorisMain extends App {

  // println(Tag.tag[Int])
  // println(Tag.tag[List])
  // println(Tag.tag[Option[Int]])
  // println(Tag.tag[Professor])
  println("hello")
  val fields = TypeInfo.fieldsInfo[Professor]
  println(fields)
  println(fields("bossId").getName)
  println(fields("bossId").getPackage)
  // val info = TypeInfo.fieldsInfo[Professor]
  // println(info("bossId").getName)
  // println(info("bossId").getPackage)


}
