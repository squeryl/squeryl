package org.squeryl.annotations;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * Persistent members of type Option[T] must to be annotated with Column
 * and specifying@optionType(classOf[TheOptionnalFieldType]), is mandatory
 * since int Option[T], the type of T is erased (JVM type erasure).
 * When not present, an exception will be thrown at Table instantiation time.
 * For this reason it is a good practice that all Tables be instantiated at
 * bootstrap time, which is the case if all Tables are declared as members of
 * a singleton org.squeryl.Schema
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface Column {

    String name() default "";
    Class<?> optionType();// default Column.class;
}