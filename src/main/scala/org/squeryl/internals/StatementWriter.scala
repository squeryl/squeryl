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
package org.squeryl.internals

import org.squeryl.dsl.ast.{ExpressionNode}
import collection.mutable.{HashSet, HashMap, ArrayBuffer}

/**
 * @arg isForDisplay: when true, users of StatementWriter should write
 *   jdbc params as strings in statement,
 *   otherwise a jdbc param declarations '?' should be written, and
 *   the param values should be accumulated with addParam(s)
 */
class StatementWriter(val isForDisplay: Boolean, val databaseAdapter: DatabaseAdapter) {
  outer =>

  def this(databaseAdapter: DatabaseAdapter) = this(false, databaseAdapter)

  val scope = new HashSet[String]

  protected val _paramList = new ArrayBuffer[AnyRef]

  /**
   * a surrogate writer will accumulate text within itself (not the parent)
   * while param accumulation (addParam) will go to the root writer, this
   * is useful when it is easier to first build a string and to write it
   * afterwards
   */
  def surrogate:StatementWriter = new StatementWriter(isForDisplay, databaseAdapter) {
    
    indentWidth = outer.indentWidth
    
    override def surrogate = outer.surrogate

    override def addParam(p: AnyRef) = outer.addParam(p)
  }

  def params: Iterable[AnyRef] = _paramList

  private val _stringBuilder = new StringBuilder(256)

  def statement = _stringBuilder.toString

  def addParam(p: AnyRef) = _paramList.append(p)

  override def toString =
    if(_paramList.size == 0)
      statement
    else
      _paramList.mkString(statement+"\njdbcParams:[",",","]")
  
  private val INDENT_INCREMENT = 2
  
  private var indentWidth = 0

  def indent(width: Int) = indentWidth += width
  def unindent(width: Int) = indentWidth -= width

  def indent: Unit = indent(INDENT_INCREMENT)
  def unindent: Unit = unindent(INDENT_INCREMENT)

  private def _dumpToConsole(s: String) = print(s)
  
  private def _append(s: String) = {
    //_dumpToConsole(s)
    _flushPendingNextLine
    _stringBuilder.append(s)
  }

  private def _writeIndentSpaces: Unit = 
    _writeIndentSpaces(indentWidth)
  
  private def _writeIndentSpaces(c: Int) =
    for( i <- 1 to c)
      _append(" ")

  def nextLine = {
    _append("\n")
    _writeIndentSpaces
  }

  private var _lazyPendingLine: Option[() => Unit] = None

  def pushPendingNextLine =
   _lazyPendingLine = Some(()=> nextLine)

  private def _flushPendingNextLine =
    if(_lazyPendingLine != None)  {
      val pl = _lazyPendingLine
      _lazyPendingLine = None
      val lpl = pl.get
      lpl()
   }
  
  def writeLines(s: String*) = {
    val size = s.size
    var c = 1

    for(l <- s) {
      _append(l)
      if(c < size)
        nextLine
    }
  }

  def writeLinesWithSeparator(s: Iterable[String], separator: String) = {
    val size = s.size
    var c = 1
    for(l <- s) {
      _append(l)
      if(c < size)
        _append(separator)
      nextLine
      c += 1
    }
  }

  def writeNodesWithSeparator(s: Iterable[ExpressionNode], separator: String, newLineAfterSeparator: Boolean) = {
    val size = s.size
    var c = 1
    for(n <- s) {
      n.write(this)
      if(c < size) {
        _append(separator)
        if(newLineAfterSeparator)
          nextLine
      }
      c += 1
    }
  }

  def write(s: String*) =
    for(s0 <- s)
      _append(s0)

  def writeIndented(u: =>Unit): Unit =
    writeIndented(INDENT_INCREMENT, u)

  def writeIndented(width: Int, u: =>Unit) = {
    indent(width)
    _writeIndentSpaces(width)
    u
    unindent(width)
  }
}
