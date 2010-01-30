package org.squeryl.dsl.boilerplate

import org.squeryl.dsl.QueryYield
import org.squeryl.{Queryable, Query}


trait FromSignatures {

  def From[T1,R](t1:Queryable[T1])(f: Function1[T1,QueryYield[R]]): Query[R] =
    new Query1[T1,R](t1,f,true)

  def From[T1,T2,R](t1:Queryable[T1], t2:Queryable[T2])(f: Function2[T1,T2,QueryYield[R]]): Query[R] =
    new Query2[T1,T2,R](t1,t2,f,true)

  def From[T1,T2,T3,R](t1:Queryable[T1],t2:Queryable[T2],t3:Queryable[T3])
                      (f: Function3[T1,T2,T3,QueryYield[R]]): Query[R] =
    new Query3[T1,T2,T3,R](t1,t2,t3,f,true)

  def From[T1,T2,T3,T4,R](t1:Queryable[T1],t2:Queryable[T2],t3:Queryable[T3],t4:Queryable[T4])
                      (f: Function4[T1,T2,T3,T4,QueryYield[R]]): Query[R] =
    new Query4[T1,T2,T3,T4,R](t1,t2,t3,t4,f,true)

  def From[T1,T2,T3,T4,T5,R](
          t1:Queryable[T1],
          t2:Queryable[T2],
          t3:Queryable[T3],
          t4:Queryable[T4],
          t5:Queryable[T5]
          )(f: Function5[T1,T2,T3,T4,T5,QueryYield[R]]): Query[R] =
    new Query5[T1,T2,T3,T4,T5,R](t1,t2,t3,t4,t5,f,true)

  def From[T1,T2,T3,T4,T5,T6,R](
          t1:Queryable[T1],
          t2:Queryable[T2],
          t3:Queryable[T3],
          t4:Queryable[T4],
          t5:Queryable[T5],
          t6:Queryable[T6]
          )(f:Function6[T1,T2,T3,T4,T5,T6,QueryYield[R]]): Query[R] =
    new Query6[T1,T2,T3,T4,T5,T6,R](t1,t2,t3,t4,t5,t6,f,true)

  def From[T1,T2,T3,T4,T5,T6,T7,R](
          t1:Queryable[T1],
          t2:Queryable[T2],
          t3:Queryable[T3],
          t4:Queryable[T4],
          t5:Queryable[T5],
          t6:Queryable[T6],
          t7:Queryable[T7]
          )(f:Function7[T1,T2,T3,T4,T5,T6,T7,QueryYield[R]]): Query[R] =
    new Query7[T1,T2,T3,T4,T5,T6,T7,R](t1,t2,t3,t4,t5,t6,t7,f,true)  
}