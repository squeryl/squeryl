package org.squeryl.test.schooldb

import org.squeryl.dsl.*
import org.squeryl.*

object AppSpecificTypeMode extends org.squeryl.PrimitiveTypeMode {
  implicit object personKED extends KeyedEntityDef[Student, Int] {
    def getId(a: Student) = a.id
    def isPersisted(a: Student) = a.id > 0
    def idPropertyName = "id"
  }

  implicit object schoolDbObjectKED extends KeyedEntityDef[SchoolDbObject, Int] {
    def getId(a: SchoolDbObject) = a.id
    def isPersisted(a: SchoolDbObject) = a.id > 0
    def idPropertyName = "id"
  }

  implicit object courseKED extends KeyedEntityDef[Course, Int] {
    def getId(a: Course) = a.id
    def isPersisted(a: Course) = a.id > 0
    def idPropertyName = "id"
    override def optimisticCounterPropertyName: Option[String] = Some("occVersionNumber")
  }

  implicit object course2KED extends KeyedEntityDef[Course2, Int] {
    def getId(a: Course2) = a.id
    def isPersisted(a: Course2) = a.id > 0
    def idPropertyName = "id"
    override def optimisticCounterPropertyName: Option[String] = Some("occVersionNumber")
  }

  implicit object courseOfferingKED extends KeyedEntityDef[CourseOffering, CompositeKey3[Int, Long, Int]] {
    def getId(a: CourseOffering) = a.id
    def isPersisted(a: CourseOffering) = a.isPersisted
    def idPropertyName = "id"
  }
}
