package org.squeryl.test.musicdb

object Genre extends Enumeration {
  type Genre = Value
  val Jazz = Value(1, "Jazz")
  val Rock = Value(2, "Rock")
  val Latin = Value(3, "Latin")
  val Bluegrass = Value(4, "Bluegrass")
  val RenaissancePolyphony = Value(5, "RenaissancePolyphony")
}
