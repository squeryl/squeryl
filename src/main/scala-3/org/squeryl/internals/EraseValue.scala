package org.squeryl.internals

import scala.compiletime.erasedValue

object EraseValue {
  
    inline def sizeOf[T]: Int = 
        inline erasedValue[T] match {
            case _: Byte => 1
            case _: Short => 2
            case _: Int => 4
            case _: Long => 8
            case _: Float => 4
            case _: Double => 8
        }

}
