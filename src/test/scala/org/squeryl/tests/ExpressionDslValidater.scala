package org.squeryl.tests

import org.squeryl.dsl.ExpressionDsl
import java.lang.reflect.Method
import org.squeryl.dsl.ast.BinaryOperatorNode

object ExpressionDslValidater extends ExpressionDsl {

  def numericTypesSampes = List[AgregateNumericalOps](
    new BinaryOperatorNodeScalarByte(null, null, ""),
    new BinaryOperatorNodeScalarByteOption(null, null, ""),
    new BinaryOperatorNodeAgregateByteOption(null, null, ""),
    new BinaryOperatorNodeScalarInt(null, null, ""),
    new BinaryOperatorNodeScalarIntOption(null, null, ""),
    new BinaryOperatorNodeAgregateIntOption(null, null, ""),
    new BinaryOperatorNodeScalarLong(null, null, ""),
    new BinaryOperatorNodeScalarLongOption(null, null, ""),
    new BinaryOperatorNodeAgregateLong(null, null, ""),
    new BinaryOperatorNodeAgregateLongOption(null, null, ""),
    new BinaryOperatorNodeScalarFloat(null, null, ""),
    new BinaryOperatorNodeScalarFloatOption(null, null, ""),
    new BinaryOperatorNodeAgregateFloatOption(null, null, ""),
    new BinaryOperatorNodeScalarDouble(null, null, ""),
    new BinaryOperatorNodeScalarDoubleOption(null, null, ""),
    new BinaryOperatorNodeAgregateDouble(null, null, ""),
    new BinaryOperatorNodeAgregateDoubleOption(null, null, "")
  )

  def numericTypes = numericTypesSampes.map(o=>{val i = o.getClass.getInterfaces; i(0)})

  //class NumericTypesProperties(val lengthInBytes: Int, val isOption: Boolean, val isAgregate: Boolean, val isFloatingPoint: Boolean)

  def numericTypesProperties =
    numericTypesSampes.map(o=>{
      val i = o.getClass.getInterfaces;
      (i(0), o)
    })

  def numericTypesPropertiesMap = (numericTypesProperties.toList :List[(Class[_], AgregateNumericalOps)]).toMap

  lazy val asmdOps = List("$minus", "$plus", "$times", "$div")

  //(Numeric, operator, typeOfRightOperand, typeOfResult)
  def ops:Iterable[(Class[_], Method, Class[_], Class[_])] =
    for(c <- numericTypes;
        m <- c.getMethods
      if m.getName.startsWith("$") && asmdOps.contains(m.getName))
      yield (c,m, m.getParameterTypes.headOption.get, m.getReturnType)

  def validateRuledOfResultTyp = {

    for(op <- ops) {

      val argLeft = numericTypesPropertiesMap.get(op._1).get
      val argRight = numericTypesPropertiesMap.get(op._3).get
      val isDiv = op._2.getName == "$div"
      val result = numericTypesPropertiesMap.get(op._4).getOrElse(
        error("no sample found for " + op._4 + " in " + numericTypesPropertiesMap))

      val dump = ()=> "\n "+_short(op._1) + " " + op._2.getName + " " + _short(op._3) + " : " + _short(op._4)

      if(argLeft.isOption || argRight.isOption)
        _assert(result.isOption, "Option does not absorb : " + dump())

      if(argLeft.isFloatingPoint || argRight.isFloatingPoint)
        _assert(result.isFloatingPoint, "isFloatingPoint does not absorb : " + dump())

      if(isDiv)
        _assert(result.isFloatingPoint, "result should be floating point : " + dump())

      if(argLeft.isAgregate || argRight.isAgregate)
        _assert(result.isAgregate, "Agregate does not absorb : " + dump())

      val maxLengthOfArgs = List(argLeft.lengthInBytes, argRight.lengthInBytes).max
      val increaseInByteLength = (result.lengthInBytes - maxLengthOfArgs)

      _assert(maxLengthOfArgs <= result.lengthInBytes && (increaseInByteLength == 0 || increaseInByteLength == 3),
        "wrong length of result " + dump())

      if(increaseInByteLength == 3)
        _assert(isDiv && argLeft.lengthInBytes == 1 && argRight.lengthInBytes == 1, "only byte division yields 4 bytes float result : " + dump())
    }
  }

  private def _validateOps = {

    val emptyA = new Array[Object](1)

    val sampleMap = numericTypesPropertiesMap

    for(op <- ops) {
      val sample = sampleMap.get(op._1).get
      val res = op._2.invoke(sample, null)
      val funcName = op._2.getName
      val bn = res.asInstanceOf[BinaryOperatorNode]
      val msg = "Wrong func name in " + op._1 + " method " + op._2.getName + "(" + op._3 + ")"
      bn.operatorToken match {
        case "+" => _assert(funcName == "$plus", msg)
        case "*" => _assert(funcName == "$times", msg)
        case "-" => _assert(funcName == "$minus", msg)
        case "/" => _assert(funcName == "$div", msg)
      }
    }
  }

  private def _assert(b: Boolean, msg: String) = {

    if(!b)
      println(msg)

    //  assert(b, msg)
  }


  def countOperators = {
    _validateOps
    validateRuledOfResultTyp

//    for(c <- numericTypes) {
//
//      println(c)
//      val i = c.getInterfaces
//
//      println(i(0))
//    }

//    for(rt <- ops.map(t=>t._3))
//      //println("->"+rt.getName)
//      assert(
//       rt.getName.startsWith("org.squeryl.dsl.ExpressionDsl2$Scalar") ||
//       rt.getName.startsWith("org.squeryl.dsl.ExpressionDsl2$Agregate"),
//       rt.getName + " should startsWith org.squeryl.dsl.ExpressionDsl2$(Scalar|Agregate")

      //assert(new Object with ScalarNumerical].isAssignableFrom(rt), rt.getName + " should be a ScalarNumerical")

//    for(types2Ops <- ops.groupBy(t => t._1.getName + "." + t._2.getName).iterator.toList.sort((e1,e2) => e1._1 < e2._1))
//      println(types2Ops._1 + " : " + types2Ops._2.size)
    val ops2TypeTransformTuple:Iterable[(String,Iterable[(Class[_],Class[_])])] =
    for(types2Ops <- ops.groupBy(t => t._1.getName + "." + t._2.getName).iterator.toList.sortWith((e1,e2) => e1._1 < e2._1))
      yield (_short(types2Ops._1),  types2Ops._2.map(ops => (ops._3, ops._4)))

    val numericTypesSet = numericTypes.toSet

    for(op2TypeTransformT <- ops2TypeTransformTuple) {
      val missing = numericTypes.filterNot(c=> op2TypeTransformT._2.exists(p=>p._1.getName == c.getName))
      val missingCount = missing.size
      if(missingCount > 0) {
        println(op2TypeTransformT._1 + " : " + op2TypeTransformT._2.size + " ops, missing " + missingCount)
        println(" missing: " + missing.map(t=>_short(t)))
      }
    }
  }

  private def _short(c: Class[_]): String =
    _short(c.getSimpleName)

  private def _short(className: String): String =
    className.substring(className.indexOf("$") + 1, className.length)

}