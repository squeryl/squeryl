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
 ***************************************************************************** */
package org.squeryl.adapters

/**
*   Since MySQL 5.5 InnoDB has replaced MyISAM as the default storage engine.
*   Thus, to take full advantage of the database abilities, new MySQL installs
*   should use this Adapter. 
*   see: http://dev.mysql.com/doc/refman/5.5/en/innodb-default-se.html
*/
class MySQLInnoDBAdapter extends MySQLAdapter {
    
    /**
    *   InnoDB MySQL tables support foreign key constraints,
    *   see http://dev.mysql.com/doc/refman/5.5/en/innodb-foreign-key-constraints.html
    */
    override def supportsForeignKeyConstraints = true
    
}