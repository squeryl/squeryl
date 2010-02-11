package org.squeryl.dsl

import ast._
import org.squeryl.internals.StatementWriter
import org.squeryl.{Session, Query}

/**
 *  Nemeric types are Byte, Int, Long, Float, Double
 *
 * Rules for Type conversions for +*-/ operations
 *
 * In an expression :
 *
 *   t1:T1 OP t2:T2   : T3
 *
 *  where T1 and T2 are Numeric types, and OP is either +*-/
 *
 *  The resulting type T3 is determined by the following rules :
 *
 * i)   T3 is an Option[] if and only if  either T2 or T2 is an Option[]
 * ii)  T3 is an Agregate if and only if either T2 or T2 is an Agregate
 * iii) T3 is a floating point type (Float or Double) if and only if either T2 or T2 is a floating point type
 *   OR if the OP is a division
 * iv)  T3 has the largest representation (in number of bytes) of T1 and T2
 *      ex.: b1:Byte * l1:Long : Long
 *
 *  (t1:Option[Byte] * t2:Float + l2:Long)  yields a Option[Double]
 *  since
 *   - the largest of the 3 operands (l2:Long) has a representation of 8 bytes,
 *   - there exists a floating point operand (t2:Float)
 *   - there exists an Optio[]
 *
 *  The result must be an Agregate, an Option[] and of 8 bytes,
 *  the type that satisfies all of these constraints is Option[Double]
 *
 *
 *  Scalar                 :  5
 *  ScalarOption           :  5
 *  Agregate               :  2 (AgregateLong and AgregateDouble are the only two non Option agregate)
 *  AgregateOption         :  5
 *
 *  number of numeric types : 17
 *
 *  operators : +/-*        : 4
 *
 *  So each types have 17 signatures for each of the 4 operators, hence 75 arithmetic operators methods each
 *
 */

trait ExpressionDsl extends NumericalTypeArithmetic {



}
