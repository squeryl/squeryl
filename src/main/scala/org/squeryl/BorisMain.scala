package org.squeryl

// import scala.quoted.staging._
import org.squeryl.internals.{ TypeInfo, Tag, EraseValue }

class Professor(
  val lastName: String,
  val id: Option[Int],
  val bossId: Option[Long],
  val test: Option[Professor]
)

object BorisMain extends App {

  // println(Tag.tag[Professor])
  println("hello")
  val fields = TypeInfo.fieldsInfo[Professor]
  println(fields)

  println(EraseValue.sizeOf[Int])

}
