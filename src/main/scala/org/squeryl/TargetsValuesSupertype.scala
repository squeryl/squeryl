package org.squeryl

/**
  * This marker interface tells squeryl that it should look up jdbc mappers for the supertype
  * of any values of marked type instead of the type directly.  Useful for mapping jdbc into objects as their sample
  * types are unique and usually the field type is the supertype instead.
  */
trait TargetsValuesSupertype {

}
