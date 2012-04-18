/*******************************************************************************
 * Copyright 2010 Maxime Lévesque
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
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
