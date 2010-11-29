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

import java.lang.reflect.Method
import net.sf.cglib.proxy._
import collection.mutable.{HashSet, ArrayBuffer}
import org.squeryl.dsl.ast._
import org.squeryl.dsl.CompositeKey

object FieldReferenceLinker {

  def pushExpressionOrCollectValue[T](e: ()=>TypedExpressionNode[T]): T = {

    val yi = _yieldInspectionTL.get
    if(yi.isOn) {
      val expr = yi.callWithoutReentrance(e)
      yi.addSelectElement(new ValueSelectElement(expr, yi.resultSetMapper, expr.mapper, yi.queryExpressionNode))
      val r = expr.sample
      r
    }
    else {
      val r = _yieldValues.get.remove(0).asInstanceOf[T]
      r
    }
  }

  def pushYieldValue(v:AnyRef) = _yieldValues.get.append(v)

  def isYieldInspectionMode = _yieldInspectionTL.get.isOn

  def inspectedQueryExpressionNode = _yieldInspectionTL.get.queryExpressionNode
  
  private val _yieldValues = new ThreadLocal[ArrayBuffer[AnyRef]] {
    override def initialValue = new ArrayBuffer[AnyRef]
  }
  
  private val __lastAccessedFieldReference = new ThreadLocal[Option[SelectElement]] {
    override def initialValue = None
  }

  private [squeryl] def _lastAccessedFieldReference: Option[SelectElement] =
    __lastAccessedFieldReference.get

  private [squeryl] def _lastAccessedFieldReference_=(se: Option[SelectElement]) =
    __lastAccessedFieldReference.set(se)
  
  private val _compositeKeyMembers = new ThreadLocal[Option[ArrayBuffer[SelectElement]]] {
    override def initialValue = None
  }

  /**
   * _lastAccessedFieldReference is unique per thread, AST construction can be nested and can interfere with
   * one another, this method is used for  preserving the previous _lastAccessedFieldReference when a nested
   * AST construction takes place *and* during the construction of 'sample' POSOs, because they are proxied,
   * and can call their intercepted fields during construction, calling the constructor for 'sample' POSO construction
   * without wrapping with this methor would have the effect of 'polluting' the _lastAccessedFieldReference (issue 68). 
   */
  def executeAndRestoreLastAccessedFieldReference[A](expressionWithSideEffectsASTConstructionThreadLocalState: =>A): A = {
    // if we are currently building an AST, we must save the (last) _lastAccessedFieldReference
    val prev = FieldReferenceLinker._lastAccessedFieldReference
    val a = expressionWithSideEffectsASTConstructionThreadLocalState
    // and restore it to the previous state (issue19)
    FieldReferenceLinker._lastAccessedFieldReference = prev
    a
  }
  
  class YieldInspection {
    
    private val _utilizedFields = new ArrayBuffer[SelectElement]
    var _on = false
    var queryExpressionNode: QueryExpressionNode[_] = null
    var _resultSetMapper: ResultSetMapper = null

    def isOn = _on

    def callWithoutReentrance[U](f: ()=>U) = {
      val prev = _on
      _on = false
      val res = f()
      _on = prev
      res
    }

    def addSelectElement(e: SelectElement) =
      if(!e.inhibited) {
        _utilizedFields.append(e)
        e.prepareColumnMapper(_utilizedFields.size)
      }    
    
    def addOutGroupExpressionNodes(oen: Iterable[SelectElement]): Int = {

//      if(! isOn)
//        error("cannot call while not in yield inspection mode")

//      val startIndex = _utilizedFields.size + 1
//      for(e <- oen)
//        _utilizedFields.append(e)
//      startIndex
      1
    }
    
    def resultSetMapper = _resultSetMapper

    def cleanUp = {
      _resultSetMapper = null
      queryExpressionNode = null
      _on = false
      _utilizedFields.clear
      _lastAccessedFieldReference = None
    }

    def turnOn(q: QueryExpressionNode[_], rsm: ResultSetMapper) = {
      queryExpressionNode = q
      _on = true
      _resultSetMapper = rsm
    }

    def turnOffAndCollectOutExpressions: List[SelectElement] = {
      _resultSetMapper = null
      _on = false
      val res = _utilizedFields.toList
      _utilizedFields.clear
      res
    }
  }
  
  private val _yieldInspectionTL = new ThreadLocal[YieldInspection] {
    override def initialValue = new YieldInspection
  }

  def addOutGroupExpressionNodes(oen: Iterable[SelectElement]) =
    _yieldInspectionTL.get.addOutGroupExpressionNodes(oen)

  def putLastAccessedSelectElement(e: SelectElement) = {
    if(_yieldInspectionTL.get.isOn)
      _yieldInspectionTL.get.addSelectElement(new ExportedSelectElement(e))
    else
    _lastAccessedFieldReference = Some(e)
  }

  def takeLastAccessedFieldReference: Option[SelectElement] = {
    val res = _lastAccessedFieldReference
    _lastAccessedFieldReference = None
    res
  }

  private def _takeLastAccessedUntypedFieldReference: SelectElementReference[_] =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case Some(n:SelectElement) => new SelectElementReference(n)(NoOpOutMapper)
      case None => error("Thread local does not have a last accessed field... this is a severe bug !")
  }

  def createEqualityExpressionWithLastAccessedFieldReferenceAndConstant(e: Any, c: Any): LogicalBoolean = {

    if(e.isInstanceOf[CompositeKey])
      e.asInstanceOf[CompositeKey].buildEquality(c.asInstanceOf[CompositeKey])
    else
      createEqualityExpressionWithLastAccessedFieldReferenceAndConstant(c)    
  }

  def createEqualityExpressionWithLastAccessedFieldReferenceAndConstant(c: Any): LogicalBoolean = {
    val fr = _takeLastAccessedUntypedFieldReference

    new BinaryOperatorNodeLogicalBoolean(
      fr,
      new ConstantExpressionNode[Any](c),
      "=")
  }
  
  /**
   * It is assumed that yield invocation for inspection will never be nested, since
   * a query is completely built (and it's yield inspection is done) before it can
   * be nested, this is unlikely to change, but documenting this assumption was
   * deemed usefull, because this method would stop working (without complaining)
   * if (the assumption) was broken.   
   */

  def determineColumnsUtilizedInYeldInvocation(q: QueryExpressionNode[_], rsm: ResultSetMapper, selectClosure: ()=>AnyRef) = {
    
    val yi = _yieldInspectionTL.get
    var result:(List[SelectElement],AnyRef) = null
    try {
      yi.turnOn(q, rsm)

      val prev = _lastAccessedFieldReference
      val res0 =
        try {
          selectClosure()
        }
        finally {
          _lastAccessedFieldReference = prev
        }

      if(res0 == null)
        error("query " + q + " yielded null")

      val visitedSet = new HashSet[Int]
      //val visitedSet = new HashSet[AnyRef]
      
      _populateSelectColsRecurse(visitedSet, yi, q, res0)

      result = (yi.turnOffAndCollectOutExpressions, res0)
    }
    finally {
      if(result == null)
        yi.cleanUp
    }
    result
  }

  private def _populateSelectColsRecurse(visited: HashSet[Int] , yi: YieldInspection,q: QueryExpressionElements, o: AnyRef):Unit = {

    val idHashCode = System.identityHashCode(o)

    if(o == null || o.getClass.getName.startsWith("java.") || visited.contains(idHashCode))
      return

    //visited.add(o)
    visited.add(idHashCode)
    
    _populateSelectCols(yi, q, o)
    for(f <-o.getClass.getDeclaredFields) {
      f.setAccessible(true);
      val ob = f.get(o)

      // don't follow closures 
      if(! f.getType.getName.startsWith("scala.Function"))
        _populateSelectColsRecurse(visited, yi, q, ob)
    }
  }
  
  private def _populateSelectCols(yi: YieldInspection,q: QueryExpressionElements, sample: AnyRef): Unit = {

    var owner = _findQENThatOwns(sample, q)
    if(owner == None)
      return

    for(e <- owner.get.getOrCreateAllSelectElements(q))
      yi.addSelectElement(e)
  }

  def findOwnerOfSample(s: Any): Option[QueryableExpressionNode] =
// TODO: could we enforce that Query[AnyVal] are not nested in some other way ?   
//    if(s.isInstanceOf[AnyVal])
//      error("A query that returns a AnyVal cannot be nested " + Utils.failSafeString(FieldReferenceLinker.inspectedQueryExpressionNode.toString))
//    else
     _findQENThatOwns(s.asInstanceOf[AnyRef], FieldReferenceLinker.inspectedQueryExpressionNode)

  private def _findQENThatOwns(sample: AnyRef, q: QueryExpressionElements): Option[QueryableExpressionNode] = {

    for(d <- q.filterDescendantsOfType[QueryableExpressionNode])
      if(d.owns(sample))
        return Some(d)

    None
  }

  def createCallBack(v: ViewExpressionNode[_]): Callback =
    new PosoPropertyAccessInterceptor(v)
  
  private class PosoPropertyAccessInterceptor(val viewExpressionNode: ViewExpressionNode[_]) extends MethodInterceptor {

      def fmd4Method(m: Method) =
        viewExpressionNode.view.findFieldMetaDataForProperty(m.getName)

      def intercept(o: Object, m: Method, args: Array[Object], proxy: MethodProxy): Object = {

        lazy val fmd = fmd4Method(m)

        val isComposite =
          classOf[CompositeKey].isAssignableFrom(m.getReturnType)

        if(isComposite)
          _compositeKeyMembers.set(Some(new ArrayBuffer[SelectElement]))

        var res = proxy.invokeSuper(o, args);

        if(isComposite) {
          val ck = res.asInstanceOf[CompositeKey]
          ck._members = Some(_compositeKeyMembers.get.get.map(new SelectElementReference[Any](_)(NoOpOutMapper)))
          ck._propertyName = Some(m.getName)
          //_compositeKey.set(Some(_compositeKeyMembers.get.get.map(new SelectElementReference[Any](_)(NoOpOutMapper))))
          _compositeKeyMembers.set(None)
        }

        if(m.getName.equals("toString") && m.getParameterTypes.length == 0)
          res = "sample:"+viewExpressionNode.view.name+"["+Integer.toHexString(System.identityHashCode(o)) + "]"

        if(fmd != None) {
          val yi = _yieldInspectionTL.get

          if(yi.isOn)
            yi.addSelectElement(viewExpressionNode.getOrCreateSelectElement(fmd.get, yi.queryExpressionNode))

          if(_compositeKeyMembers.get == None)
            _lastAccessedFieldReference = Some(viewExpressionNode.getOrCreateSelectElement(fmd.get));
          else
            _compositeKeyMembers.get.get.append(viewExpressionNode.getOrCreateSelectElement(fmd.get))
        }
        
        res
      }          
  }
}
