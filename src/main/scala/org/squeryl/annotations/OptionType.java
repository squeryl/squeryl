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
 * Persistent members of type Option[T] must to be annotated with
 * @OptionType(classOf[TheOptionnalFieldType]), this is mandatory
 * since int Option[T], the type of T is erased (JVM type erasure).
 * When not present, an exception will be thrown at Table instantiation time.
 * For this reason it is a good practice that all Tables be instantiated at
 * bootstrap time, which is the case if all Tables are declared as members of
 * a singleton org.squeryl.Schema   
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface OptionType {

  Class<?> value();    
}
