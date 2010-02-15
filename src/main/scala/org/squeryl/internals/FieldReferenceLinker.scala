package org.squeryl.internals

import java.lang.reflect.Method
import net.sf.cglib.proxy._
import collection.mutable.{HashSet, ArrayBuffer}
import org.squeryl.dsl.ast._

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
  
  private val _lastAccessedFieldReference = new ThreadLocal[Option[SelectElement]] {
    override def initialValue = None
  }

  class YieldInspection {
    
    val _utilizedFields = new ArrayBuffer[SelectElement]
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

    def addSelectElement(e: SelectElement) = {
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
      _lastAccessedFieldReference.set(None)
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
    _lastAccessedFieldReference.set(Some(e))
  }

  def takeLastAccessedFieldReference: Option[SelectElement] = {
    val res = _lastAccessedFieldReference.get
    _lastAccessedFieldReference.set(None)
    res
  }
    
  /**
   * It is assumed that yield invocation for inspection will never be nested, since
   * a query is completely built (and it's yield inspection is done) before it can
   * be nested, this is unlikely to change, but documenting this assumption was
   * deemed usefull, because this method would stop working (without complaining)
   * if (the assumption) was broken.   
   */

  def determineColumnsUtilizedInYeldInvocation(q: QueryExpressionNode[_], rsm: ResultSetMapper, yieldClosure: ()=>AnyRef) = {
    
    val yi = _yieldInspectionTL.get
    var result:(List[SelectElement],AnyRef) = null
    try {
      yi.turnOn(q, rsm)
      val res0 = yieldClosure()

      if(res0 == null)
        error("query " + q + " yielded null")

      _populateSelectColsRecurse(new HashSet[AnyRef], yi, q, res0)

      result = (yi.turnOffAndCollectOutExpressions, res0)
    }
    finally {
      if(result == null)
        yi.cleanUp
    }
    result
  }

  private def _populateSelectColsRecurse(visited: HashSet[AnyRef] , yi: YieldInspection,q: QueryExpressionElements, o: AnyRef):Unit = {

    if(o == null || o.getClass.getName.startsWith("java.") || visited.contains(o))
      return

    visited.add(o)
    
    _populateSelectCols(yi, q, o)
    for(f <-o.getClass.getDeclaredFields) {
      f.setAccessible(true);
      val ob = f.get(o)
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
  
  class PosoPropertyAccessInterceptor(val viewExpressionNode: ViewExpressionNode[_]) extends MethodInterceptor {

      def fmd4Method(m: Method) =
        viewExpressionNode.view.findFieldMetaDataForProperty(m.getName)

      def intercept(o: Object, m: Method, args: Array[Object], proxy: MethodProxy): Object = {

        lazy val fmd = fmd4Method(m)
        var res = proxy.invokeSuper(o, args);

        if(m.getName.equals("toString") && m.getParameterTypes.length == 0)
          res = "sample:"+viewExpressionNode.view.name+"["+Integer.toHexString(System.identityHashCode(o)) + "]"

        if(fmd != None) {
          val yi = _yieldInspectionTL.get

          if(yi.isOn)
            yi.addSelectElement(viewExpressionNode.getOrCreateSelectElement(fmd.get, yi.queryExpressionNode))

          _lastAccessedFieldReference.set(Some(viewExpressionNode.getOrCreateSelectElement(fmd.get)));
        }
        
        res
      }          
    }
}
