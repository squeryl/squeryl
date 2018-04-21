package org

package object squeryl {

  implicit class IdOps[A](a: A) {
    def unused: Unit = ()
  }

}
