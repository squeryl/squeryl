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
 * This annotation should not be used directly, but via it's subtype : 
 * org.squeryl.annotations.Column
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface ColumnBase {

    String value() default "";
    
    String name() default "";

  /**
   * The unit of length is dependent on the field type,
   * for numbers the length is in bytes, for Strings, it is the
   * number of characters, booleans in bits, and for Date : -1.
   */
    int length() default -1;

  /**
   * BigDecimal needs a second parameter in addition to field length.
   */
    int scale() default -1;

    Class<?> optionType() default Object.class;

}
