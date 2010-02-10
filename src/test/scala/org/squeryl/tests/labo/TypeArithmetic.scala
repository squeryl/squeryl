package org.squeryl.tests.labo


object TypeArithmetic {

  type ByteType = Byte
  type IntType = Int
  type LongType = Long
  type FloatType = Float
  type DoubleType = Double


  class TypedExpression[A] {

    def +[B](b: TypedExpression[B]) = new BinAMSOp[A,B](this, b, "+")
    def *[B](b: TypedExpression[B]) = new BinAMSOp[A,B](this, b, "+")

    def /[B](b: TypedExpression[B]) = new BinDivOp[A,B](this, b, "/")

    def =?(s: TypedExpression[_]) = new LBool

    def ~ = this
  }

  class TypedExpressionWrapper[A](e: ExpressionN) extends TypedExpression[A]

  trait ExpressionN

  class BinAMSOp[A1,A2](a1: TypedExpression[A1], a2: TypedExpression[A2], op: String) extends ExpressionN

  class BinDivOp[A1,A2](a1: TypedExpression[A1], a2: TypedExpression[A2], op: String) extends ExpressionN

  implicit def op2TypedExpression1(op: BinAMSOp[ByteType,ByteType]) = new TypedExpressionWrapper[ByteType](op)
  implicit def op2TypedExpression2(op: BinAMSOp[ByteType,IntType]) = new TypedExpressionWrapper[IntType](op)
  implicit def op2TypedExpression3(op: BinAMSOp[ByteType,LongType]) = new TypedExpressionWrapper[LongType](op)
  implicit def op2TypedExpression4(op: BinAMSOp[ByteType,FloatType]) = new TypedExpressionWrapper[FloatType](op)
  implicit def op2TypedExpression5(op: BinAMSOp[ByteType,DoubleType]) = new TypedExpressionWrapper[DoubleType](op)
  implicit def op2TypedExpression6(op: BinAMSOp[ByteType,Option[ByteType]]) = new TypedExpressionWrapper[Option[ByteType]](op)
  implicit def op2TypedExpression7(op: BinAMSOp[ByteType,Option[IntType]]) = new TypedExpressionWrapper[Option[IntType]](op)
  implicit def op2TypedExpression8(op: BinAMSOp[ByteType,Option[LongType]]) = new TypedExpressionWrapper[Option[LongType]](op)
  implicit def op2TypedExpression9(op: BinAMSOp[ByteType,Option[FloatType]]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression10(op: BinAMSOp[ByteType,Option[DoubleType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression11(op: BinAMSOp[IntType,ByteType]) = new TypedExpressionWrapper[IntType](op)
  implicit def op2TypedExpression12(op: BinAMSOp[IntType,IntType]) = new TypedExpressionWrapper[IntType](op)
  implicit def op2TypedExpression13(op: BinAMSOp[IntType,LongType]) = new TypedExpressionWrapper[LongType](op)
  implicit def op2TypedExpression14(op: BinAMSOp[IntType,FloatType]) = new TypedExpressionWrapper[FloatType](op)
  implicit def op2TypedExpression15(op: BinAMSOp[IntType,DoubleType]) = new TypedExpressionWrapper[DoubleType](op)
  implicit def op2TypedExpression16(op: BinAMSOp[IntType,Option[ByteType]]) = new TypedExpressionWrapper[Option[IntType]](op)
  implicit def op2TypedExpression17(op: BinAMSOp[IntType,Option[IntType]]) = new TypedExpressionWrapper[Option[IntType]](op)
  implicit def op2TypedExpression18(op: BinAMSOp[IntType,Option[LongType]]) = new TypedExpressionWrapper[Option[LongType]](op)
  implicit def op2TypedExpression19(op: BinAMSOp[IntType,Option[FloatType]]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression20(op: BinAMSOp[IntType,Option[DoubleType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression21(op: BinAMSOp[LongType,ByteType]) = new TypedExpressionWrapper[LongType](op)
  implicit def op2TypedExpression22(op: BinAMSOp[LongType,IntType]) = new TypedExpressionWrapper[LongType](op)
  implicit def op2TypedExpression23(op: BinAMSOp[LongType,LongType]) = new TypedExpressionWrapper[LongType](op)
  implicit def op2TypedExpression24(op: BinAMSOp[LongType,FloatType]) = new TypedExpressionWrapper[DoubleType](op)
  implicit def op2TypedExpression25(op: BinAMSOp[LongType,DoubleType]) = new TypedExpressionWrapper[DoubleType](op)
  implicit def op2TypedExpression26(op: BinAMSOp[LongType,Option[ByteType]]) = new TypedExpressionWrapper[Option[LongType]](op)
  implicit def op2TypedExpression27(op: BinAMSOp[LongType,Option[IntType]]) = new TypedExpressionWrapper[Option[LongType]](op)
  implicit def op2TypedExpression28(op: BinAMSOp[LongType,Option[LongType]]) = new TypedExpressionWrapper[Option[LongType]](op)
  implicit def op2TypedExpression29(op: BinAMSOp[LongType,Option[FloatType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression30(op: BinAMSOp[LongType,Option[DoubleType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression31(op: BinAMSOp[FloatType,ByteType]) = new TypedExpressionWrapper[FloatType](op)
  implicit def op2TypedExpression32(op: BinAMSOp[FloatType,IntType]) = new TypedExpressionWrapper[FloatType](op)
  implicit def op2TypedExpression33(op: BinAMSOp[FloatType,LongType]) = new TypedExpressionWrapper[DoubleType](op)
  implicit def op2TypedExpression34(op: BinAMSOp[FloatType,FloatType]) = new TypedExpressionWrapper[FloatType](op)
  implicit def op2TypedExpression35(op: BinAMSOp[FloatType,DoubleType]) = new TypedExpressionWrapper[DoubleType](op)
  implicit def op2TypedExpression36(op: BinAMSOp[FloatType,Option[ByteType]]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression37(op: BinAMSOp[FloatType,Option[IntType]]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression38(op: BinAMSOp[FloatType,Option[LongType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression39(op: BinAMSOp[FloatType,Option[FloatType]]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression40(op: BinAMSOp[FloatType,Option[DoubleType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression41(op: BinAMSOp[DoubleType,ByteType]) = new TypedExpressionWrapper[DoubleType](op)
  implicit def op2TypedExpression42(op: BinAMSOp[DoubleType,IntType]) = new TypedExpressionWrapper[DoubleType](op)
  implicit def op2TypedExpression43(op: BinAMSOp[DoubleType,LongType]) = new TypedExpressionWrapper[DoubleType](op)
  implicit def op2TypedExpression44(op: BinAMSOp[DoubleType,FloatType]) = new TypedExpressionWrapper[DoubleType](op)
  implicit def op2TypedExpression45(op: BinAMSOp[DoubleType,DoubleType]) = new TypedExpressionWrapper[DoubleType](op)
  implicit def op2TypedExpression46(op: BinAMSOp[DoubleType,Option[ByteType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression47(op: BinAMSOp[DoubleType,Option[IntType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression48(op: BinAMSOp[DoubleType,Option[LongType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression49(op: BinAMSOp[DoubleType,Option[FloatType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression50(op: BinAMSOp[DoubleType,Option[DoubleType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression51(op: BinAMSOp[Option[ByteType],ByteType]) = new TypedExpressionWrapper[Option[ByteType]](op)
  implicit def op2TypedExpression52(op: BinAMSOp[Option[ByteType],IntType]) = new TypedExpressionWrapper[Option[IntType]](op)
  implicit def op2TypedExpression53(op: BinAMSOp[Option[ByteType],LongType]) = new TypedExpressionWrapper[Option[LongType]](op)
  implicit def op2TypedExpression54(op: BinAMSOp[Option[ByteType],FloatType]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression55(op: BinAMSOp[Option[ByteType],DoubleType]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression56(op: BinAMSOp[Option[ByteType],Option[ByteType]]) = new TypedExpressionWrapper[Option[ByteType]](op)
  implicit def op2TypedExpression57(op: BinAMSOp[Option[ByteType],Option[IntType]]) = new TypedExpressionWrapper[Option[IntType]](op)
  implicit def op2TypedExpression58(op: BinAMSOp[Option[ByteType],Option[LongType]]) = new TypedExpressionWrapper[Option[LongType]](op)
  implicit def op2TypedExpression59(op: BinAMSOp[Option[ByteType],Option[FloatType]]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression60(op: BinAMSOp[Option[ByteType],Option[DoubleType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression61(op: BinAMSOp[Option[IntType],ByteType]) = new TypedExpressionWrapper[Option[IntType]](op)
  implicit def op2TypedExpression62(op: BinAMSOp[Option[IntType],IntType]) = new TypedExpressionWrapper[Option[IntType]](op)
  implicit def op2TypedExpression63(op: BinAMSOp[Option[IntType],LongType]) = new TypedExpressionWrapper[Option[LongType]](op)
  implicit def op2TypedExpression64(op: BinAMSOp[Option[IntType],FloatType]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression65(op: BinAMSOp[Option[IntType],DoubleType]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression66(op: BinAMSOp[Option[IntType],Option[ByteType]]) = new TypedExpressionWrapper[Option[IntType]](op)
  implicit def op2TypedExpression67(op: BinAMSOp[Option[IntType],Option[IntType]]) = new TypedExpressionWrapper[Option[IntType]](op)
  implicit def op2TypedExpression68(op: BinAMSOp[Option[IntType],Option[LongType]]) = new TypedExpressionWrapper[Option[LongType]](op)
  implicit def op2TypedExpression69(op: BinAMSOp[Option[IntType],Option[FloatType]]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression70(op: BinAMSOp[Option[IntType],Option[DoubleType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression71(op: BinAMSOp[Option[LongType],ByteType]) = new TypedExpressionWrapper[Option[LongType]](op)
  implicit def op2TypedExpression72(op: BinAMSOp[Option[LongType],IntType]) = new TypedExpressionWrapper[Option[LongType]](op)
  implicit def op2TypedExpression73(op: BinAMSOp[Option[LongType],LongType]) = new TypedExpressionWrapper[Option[LongType]](op)
  implicit def op2TypedExpression74(op: BinAMSOp[Option[LongType],FloatType]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression75(op: BinAMSOp[Option[LongType],DoubleType]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression76(op: BinAMSOp[Option[LongType],Option[ByteType]]) = new TypedExpressionWrapper[Option[LongType]](op)
  implicit def op2TypedExpression77(op: BinAMSOp[Option[LongType],Option[IntType]]) = new TypedExpressionWrapper[Option[LongType]](op)
  implicit def op2TypedExpression78(op: BinAMSOp[Option[LongType],Option[LongType]]) = new TypedExpressionWrapper[Option[LongType]](op)
  implicit def op2TypedExpression79(op: BinAMSOp[Option[LongType],Option[FloatType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression80(op: BinAMSOp[Option[LongType],Option[DoubleType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression81(op: BinAMSOp[Option[FloatType],ByteType]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression82(op: BinAMSOp[Option[FloatType],IntType]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression83(op: BinAMSOp[Option[FloatType],LongType]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression84(op: BinAMSOp[Option[FloatType],FloatType]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression85(op: BinAMSOp[Option[FloatType],DoubleType]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression86(op: BinAMSOp[Option[FloatType],Option[ByteType]]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression87(op: BinAMSOp[Option[FloatType],Option[IntType]]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression88(op: BinAMSOp[Option[FloatType],Option[LongType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression89(op: BinAMSOp[Option[FloatType],Option[FloatType]]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression90(op: BinAMSOp[Option[FloatType],Option[DoubleType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression91(op: BinAMSOp[Option[DoubleType],ByteType]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression92(op: BinAMSOp[Option[DoubleType],IntType]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression93(op: BinAMSOp[Option[DoubleType],LongType]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression94(op: BinAMSOp[Option[DoubleType],FloatType]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression95(op: BinAMSOp[Option[DoubleType],DoubleType]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression96(op: BinAMSOp[Option[DoubleType],Option[ByteType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression97(op: BinAMSOp[Option[DoubleType],Option[IntType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression98(op: BinAMSOp[Option[DoubleType],Option[LongType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression99(op: BinAMSOp[Option[DoubleType],Option[FloatType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression100(op: BinAMSOp[Option[DoubleType],Option[DoubleType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)

  implicit def op2TypedExpression1(op: BinDivOp[ByteType,ByteType]) = new TypedExpressionWrapper[FloatType](op)
  implicit def op2TypedExpression2(op: BinDivOp[ByteType,IntType]) = new TypedExpressionWrapper[FloatType](op)
  implicit def op2TypedExpression3(op: BinDivOp[ByteType,LongType]) = new TypedExpressionWrapper[DoubleType](op)
  implicit def op2TypedExpression4(op: BinDivOp[ByteType,FloatType]) = new TypedExpressionWrapper[FloatType](op)
  implicit def op2TypedExpression5(op: BinDivOp[ByteType,DoubleType]) = new TypedExpressionWrapper[DoubleType](op)
  implicit def op2TypedExpression6(op: BinDivOp[ByteType,Option[ByteType]]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression7(op: BinDivOp[ByteType,Option[IntType]]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression8(op: BinDivOp[ByteType,Option[LongType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression9(op: BinDivOp[ByteType,Option[FloatType]]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression10(op: BinDivOp[ByteType,Option[DoubleType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression11(op: BinDivOp[IntType,ByteType]) = new TypedExpressionWrapper[FloatType](op)
  implicit def op2TypedExpression12(op: BinDivOp[IntType,IntType]) = new TypedExpressionWrapper[FloatType](op)
  implicit def op2TypedExpression13(op: BinDivOp[IntType,LongType]) = new TypedExpressionWrapper[DoubleType](op)
  implicit def op2TypedExpression14(op: BinDivOp[IntType,FloatType]) = new TypedExpressionWrapper[FloatType](op)
  implicit def op2TypedExpression15(op: BinDivOp[IntType,DoubleType]) = new TypedExpressionWrapper[DoubleType](op)
  implicit def op2TypedExpression16(op: BinDivOp[IntType,Option[ByteType]]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression17(op: BinDivOp[IntType,Option[IntType]]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression18(op: BinDivOp[IntType,Option[LongType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression19(op: BinDivOp[IntType,Option[FloatType]]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression20(op: BinDivOp[IntType,Option[DoubleType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression21(op: BinDivOp[LongType,ByteType]) = new TypedExpressionWrapper[DoubleType](op)
  implicit def op2TypedExpression22(op: BinDivOp[LongType,IntType]) = new TypedExpressionWrapper[DoubleType](op)
  implicit def op2TypedExpression23(op: BinDivOp[LongType,LongType]) = new TypedExpressionWrapper[DoubleType](op)
  implicit def op2TypedExpression24(op: BinDivOp[LongType,FloatType]) = new TypedExpressionWrapper[DoubleType](op)
  implicit def op2TypedExpression25(op: BinDivOp[LongType,DoubleType]) = new TypedExpressionWrapper[DoubleType](op)
  implicit def op2TypedExpression26(op: BinDivOp[LongType,Option[ByteType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression27(op: BinDivOp[LongType,Option[IntType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression28(op: BinDivOp[LongType,Option[LongType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression29(op: BinDivOp[LongType,Option[FloatType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression30(op: BinDivOp[LongType,Option[DoubleType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression31(op: BinDivOp[FloatType,ByteType]) = new TypedExpressionWrapper[FloatType](op)
  implicit def op2TypedExpression32(op: BinDivOp[FloatType,IntType]) = new TypedExpressionWrapper[FloatType](op)
  implicit def op2TypedExpression33(op: BinDivOp[FloatType,LongType]) = new TypedExpressionWrapper[DoubleType](op)
  implicit def op2TypedExpression34(op: BinDivOp[FloatType,FloatType]) = new TypedExpressionWrapper[FloatType](op)
  implicit def op2TypedExpression35(op: BinDivOp[FloatType,DoubleType]) = new TypedExpressionWrapper[DoubleType](op)
  implicit def op2TypedExpression36(op: BinDivOp[FloatType,Option[ByteType]]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression37(op: BinDivOp[FloatType,Option[IntType]]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression38(op: BinDivOp[FloatType,Option[LongType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression39(op: BinDivOp[FloatType,Option[FloatType]]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression40(op: BinDivOp[FloatType,Option[DoubleType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression41(op: BinDivOp[DoubleType,ByteType]) = new TypedExpressionWrapper[DoubleType](op)
  implicit def op2TypedExpression42(op: BinDivOp[DoubleType,IntType]) = new TypedExpressionWrapper[DoubleType](op)
  implicit def op2TypedExpression43(op: BinDivOp[DoubleType,LongType]) = new TypedExpressionWrapper[DoubleType](op)
  implicit def op2TypedExpression44(op: BinDivOp[DoubleType,FloatType]) = new TypedExpressionWrapper[DoubleType](op)
  implicit def op2TypedExpression45(op: BinDivOp[DoubleType,DoubleType]) = new TypedExpressionWrapper[DoubleType](op)
  implicit def op2TypedExpression46(op: BinDivOp[DoubleType,Option[ByteType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression47(op: BinDivOp[DoubleType,Option[IntType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression48(op: BinDivOp[DoubleType,Option[LongType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression49(op: BinDivOp[DoubleType,Option[FloatType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression50(op: BinDivOp[DoubleType,Option[DoubleType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression51(op: BinDivOp[Option[ByteType],ByteType]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression52(op: BinDivOp[Option[ByteType],IntType]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression53(op: BinDivOp[Option[ByteType],LongType]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression54(op: BinDivOp[Option[ByteType],FloatType]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression55(op: BinDivOp[Option[ByteType],DoubleType]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression56(op: BinDivOp[Option[ByteType],Option[ByteType]]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression57(op: BinDivOp[Option[ByteType],Option[IntType]]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression58(op: BinDivOp[Option[ByteType],Option[LongType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression59(op: BinDivOp[Option[ByteType],Option[FloatType]]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression60(op: BinDivOp[Option[ByteType],Option[DoubleType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression61(op: BinDivOp[Option[IntType],ByteType]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression62(op: BinDivOp[Option[IntType],IntType]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression63(op: BinDivOp[Option[IntType],LongType]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression64(op: BinDivOp[Option[IntType],FloatType]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression65(op: BinDivOp[Option[IntType],DoubleType]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression66(op: BinDivOp[Option[IntType],Option[ByteType]]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression67(op: BinDivOp[Option[IntType],Option[IntType]]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression68(op: BinDivOp[Option[IntType],Option[LongType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression69(op: BinDivOp[Option[IntType],Option[FloatType]]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression70(op: BinDivOp[Option[IntType],Option[DoubleType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression71(op: BinDivOp[Option[LongType],ByteType]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression72(op: BinDivOp[Option[LongType],IntType]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression73(op: BinDivOp[Option[LongType],LongType]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression74(op: BinDivOp[Option[LongType],FloatType]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression75(op: BinDivOp[Option[LongType],DoubleType]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression76(op: BinDivOp[Option[LongType],Option[ByteType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression77(op: BinDivOp[Option[LongType],Option[IntType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression78(op: BinDivOp[Option[LongType],Option[LongType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression79(op: BinDivOp[Option[LongType],Option[FloatType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression80(op: BinDivOp[Option[LongType],Option[DoubleType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression81(op: BinDivOp[Option[FloatType],ByteType]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression82(op: BinDivOp[Option[FloatType],IntType]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression83(op: BinDivOp[Option[FloatType],LongType]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression84(op: BinDivOp[Option[FloatType],FloatType]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression85(op: BinDivOp[Option[FloatType],DoubleType]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression86(op: BinDivOp[Option[FloatType],Option[ByteType]]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression87(op: BinDivOp[Option[FloatType],Option[IntType]]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression88(op: BinDivOp[Option[FloatType],Option[LongType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression89(op: BinDivOp[Option[FloatType],Option[FloatType]]) = new TypedExpressionWrapper[Option[FloatType]](op)
  implicit def op2TypedExpression90(op: BinDivOp[Option[FloatType],Option[DoubleType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression91(op: BinDivOp[Option[DoubleType],ByteType]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression92(op: BinDivOp[Option[DoubleType],IntType]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression93(op: BinDivOp[Option[DoubleType],LongType]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression94(op: BinDivOp[Option[DoubleType],FloatType]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression95(op: BinDivOp[Option[DoubleType],DoubleType]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression96(op: BinDivOp[Option[DoubleType],Option[ByteType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression97(op: BinDivOp[Option[DoubleType],Option[IntType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression98(op: BinDivOp[Option[DoubleType],Option[LongType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression99(op: BinDivOp[Option[DoubleType],Option[FloatType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  implicit def op2TypedExpression100(op: BinDivOp[Option[DoubleType],Option[DoubleType]]) = new TypedExpressionWrapper[Option[DoubleType]](op)
  
//  implicit def op2TypedExpression1(e: BinDivOp[IntType,LongType]) = new TypedExpressionWrapper[Double](e)
//  implicit def op2TypedExpression2(e: BinDivOp[LongType,IntType]) = new TypedExpressionWrapper[Double](e)
//  implicit def op2TypedExpression3(e: BinDivOp[IntType,FloatType]) = new TypedExpressionWrapper[FloatType](e)
//  implicit def op2TypedExpression4(e: BinDivOp[FloatType,IntType]) = new TypedExpressionWrapper[FloatType](e)
//  implicit def op2TypedExpression5(e: BinDivOp[IntType,IntType]) = new TypedExpressionWrapper[FloatType](e)
//  implicit def op2TypedExpression6(e: BinDivOp[LongType,LongType]) = new TypedExpressionWrapper[Double](e)
  
  implicit def i2s(i:Int) = new TypedExpression[IntType]
  implicit def f2s(i:Long) = new TypedExpression[LongType]
  implicit def l2s(i:Float) = new TypedExpression[FloatType]

  class LBool {
    def and(b: LBool) = new LBool
  }


  class TypeDescription(val token: String, val length:Int, val isFloat: Boolean, val isOption: Boolean) {
    override def toString =
      if(isOption)
        "Option[" + token + "]"
      else
        token
  }

  val tByte    = new TypeDescription("ByteType", 1, false, false)
  val tInt     = new TypeDescription("IntType", 4, false, false)
  val tLong    = new TypeDescription("LongType", 8, false, false)
  val tFloat   = new TypeDescription("FloatType", 4, true, false)
  val tDouble  = new TypeDescription("DoubleType", 8, true, false)
  
  val tByteO   = new TypeDescription("ByteType", 1, false, true)
  val tIntO    = new TypeDescription("IntType", 4, false, true)
  val tLongO   = new TypeDescription("LongType", 8, false, true)
  val tFloatO  = new TypeDescription("FloatType", 4, true, true)
  val tDoubleO = new TypeDescription("DoubleType", 8, true, true)


  def allNumericTypes =
    List(tByte,tInt,tLong,tFloat,tDouble,
         tByteO,tIntO,tLongO,tFloatO,tDoubleO) 

  def printBinaryTypeConversions(inputParamTypeName : String, outTypeFunc: (TypeDescription,TypeDescription) => TypeDescription) = {
    var i = 1
    for(t1 <- allNumericTypes;
        t2 <- allNumericTypes) {
      val outputType = computeTypeASM(t1,t2)
      println("  implicit def op2TypedExpression"+i+
              "(op: "+inputParamTypeName+"[" + t1 + "," + t2 +
              "]) = new TypedExpressionWrapper["+outTypeFunc(t1,t2)+"](op)")
      i += 1
    }
  }

  val computeTypeASM = (t1: TypeDescription, t2: TypeDescription) => {

    val maxLength = if(t1.length > t2.length) t1.length else t2.length
    val isOption = t1.isOption || t2.isOption
    val isFloat = t1.isFloat || t2.isFloat

    (maxLength, isOption, isFloat) match {
      case (1, false, false) => tByte
      case (1, true,  false) => tByteO
      case (1, a:Any,   b:Any)   => invalidTypeCombination(t1,t2)
      case (4, false, false) => tInt
      case (4, true,  false) => tIntO
      case (4, false, true)  => tFloat
      case (4, true,  true)  => tFloatO
      case (8, false, false) => tLong
      case (8, true,  false) => tLongO
      case (8, false, true)  => tDouble
      case (8, true,  true)  => tDoubleO
      case a:Any => invalidTypeCombination(t1,t2)
    }
  }

  val computeTypeDIV = (t1: TypeDescription, t2: TypeDescription) => {  

    val maxLength = if(t1.length > t2.length) t1.length else t2.length
    val isOption = t1.isOption || t2.isOption

    (maxLength, isOption) match {
      case (1, false) => tFloat
      case (1, true) => tFloatO
      case (4, false)  => tFloat
      case (4, true)  => tFloatO
      case (8, false)  => tDouble
      case (8, true)  => tDoubleO
      case a:Any => invalidTypeCombination(t1,t2)
    }
  }

  def invalidTypeCombination(t1: TypeDescription,t2: TypeDescription) =
    error("invalidTypeCombination(" + t1 + "," + t2)
  
  def main(args : Array[String]) : Unit = {

    printBinaryTypeConversions("BinAMSOp", computeTypeASM)
    printBinaryTypeConversions("BinDivOp", computeTypeDIV)

    val v1 = (1 ~) + 1.5F : TypedExpression[FloatType]
    val v2 = (1 ~) + 10L;
    v2 : TypedExpression[Long]

    (1 ~) / 4L : TypedExpression[Double]

    (1 ~) / 4 : TypedExpression[Float]

    34 * (1 ~) / 4L : TypedExpression[Double]

    4 =? (1 ~) + 10L and (1 ~) / 4 =? (1 ~) + 10L :LBool
    println(v1)
  }  
}