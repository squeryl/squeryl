package org.squeryl.annotations;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;


/**
 *  in a Table[T] declaration, T is said to be a Row, the @Row annotation
 * is optional, i.e. absence of annotation is equivalent to :
 * @Row(fieldToColumnCorrespondanceMode=FieldToColumnCorrespondanceMode.IMPLICIT) 
 * which means that all fields and properties (pairs of setters and getters) will
 * be mapped to a column of the same name as the field, unless annotated by @Transient.
 * FieldToColumnCorrespondanceMode.EXPLICIT is the inverse policy : fields must be annotated
 * with @Column to be mapped 
 */

@Retention(RetentionPolicy.RUNTIME)
public @interface Row {
  String value() default "";
  FieldToColumnCorrespondanceMode fieldToColumnCorrespondanceMode() default FieldToColumnCorrespondanceMode.IMPLICIT;
}
