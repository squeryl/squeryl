package org.squeryl.dsl

import ast.{TypedExpressionNode, TokenExpressionNode, FunctionNode}

/*


*/
trait SqlFunctions  {
  self: ExpressionDsl =>

  def Max(e: ScalarInt)      = new FunctionNode("max",e) with AgregateIntOption
  def Max(e: ScalarDouble)   = new FunctionNode("max",e) with AgregateDoubleOption
  def Max(e: ScalarFloat)    = new FunctionNode("max",e) with AgregateFloatOption
  def Max(e: BaseScalarString)   = new FunctionNode("max",e) with AgregateStringOption
  def Max(e: ScalarLong)     = new FunctionNode("max",e) with AgregateLongOption
  def Max(e: ScalarIntOption)      = new FunctionNode("max",e) with AgregateIntOption
  def Max(e: ScalarDoubleOption)   = new FunctionNode("max",e) with AgregateDoubleOption
  def Max(e: ScalarFloatOption)    = new FunctionNode("max",e) with AgregateFloatOption
//  def Max(e: ScalarStringOption)   = new FunctionNode("max",e) with AgregateStringOption
  def Max(e: ScalarLongOption)     = new FunctionNode("max",e) with AgregateLongOption
  def Max(e: ScalarBoolean)     = new FunctionNode("max",e) with AgregateBooleanOption
  def Max(e: ScalarBooleanOption)     = new FunctionNode("max",e) with AgregateBooleanOption
  def Max(e: BaseScalarDate)     = new FunctionNode("max",e) with AgregateDateOption

  def Min(e: ScalarInt)      = new FunctionNode("min",e) with AgregateIntOption
  def Min(e: ScalarDouble)   = new FunctionNode("min",e) with AgregateDoubleOption
  def Min(e: ScalarFloat)    = new FunctionNode("min",e) with AgregateFloatOption
  def Min(e: BaseScalarString)   = new FunctionNode("min",e) with AgregateStringOption
  def Min(e: ScalarLong)     = new FunctionNode("min",e) with AgregateLongOption
  def Min(e: ScalarIntOption)      = new FunctionNode("min",e) with AgregateIntOption
  def Min(e: ScalarDoubleOption)   = new FunctionNode("min",e) with AgregateDoubleOption
  def Min(e: ScalarFloatOption)    = new FunctionNode("min",e) with AgregateFloatOption
//BaseScalarString  def Min(e: ScalarStringOption)   = new FunctionNode("min",e) with AgregateStringOption
  def Min(e: ScalarLongOption)     = new FunctionNode("min",e) with AgregateLongOption
  def Min(e: ScalarBoolean)     = new FunctionNode("min",e) with AgregateBooleanOption
  def Min(e: ScalarBooleanOption)     = new FunctionNode("min",e) with AgregateBooleanOption
  def Min(e: BaseScalarDate)     = new FunctionNode("min",e) with AgregateDateOption

  //TODO: introduce Numeric4 and Numeric8 (byte,int,float) and (long, double) and have only 2 Avg Signatures ?
  def Avg(e: ScalarInt)      = new FunctionNode("avg",e) with AgregateFloatOption
  def Avg(e: ScalarDouble)   = new FunctionNode("avg",e) with AgregateDoubleOption
  def Avg(e: ScalarFloat)    = new FunctionNode("avg",e) with AgregateFloatOption
  def Avg(e: ScalarLong)     = new FunctionNode("avg",e) with AgregateDoubleOption
  def Avg(e: ScalarIntOption)      = new FunctionNode("avg",e) with AgregateFloatOption
  def Avg(e: ScalarDoubleOption)   = new FunctionNode("avg",e) with AgregateDoubleOption
  def Avg(e: ScalarFloatOption)    = new FunctionNode("avg",e) with AgregateFloatOption
  def Avg(e: ScalarLongOption)     = new FunctionNode("avg",e) with AgregateDoubleOption

  def Count                  = new FunctionNode("count", new TokenExpressionNode("*")) with AgregateLong

  //TODO: eliminate, replace with || since this form can't deal with *non null* absorbance,
  // contrary to number operations, string concat has absorbance of non nulls 
  // for convenience, Concat returns an empty instead of null, so the return type is ScalarString, not ScalarStringOption  
  def Concat(e: TypedExpressionNode[Scalar,_]*) = new ConcatFunction(e) with ScalarString
}