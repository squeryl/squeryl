package org.squeryl
import org.squeryl.dsl._
import org.squeryl.dsl.ast.TypedExpressionNode
import org.squeryl.internals.OutMapper
import org.squeryl.dsl.ast.ConstantExpressionNode
import org.squeryl.internals.StatementWriter
import java.io.Closeable
import org.squeryl.logging.StatementInvocationEvent
import org.squeryl.internals.Utils
import org.squeryl.internals.ResultSetUtils
import java.sql.Connection
import java.sql.PreparedStatement

trait ReParametrizableStatement {
  
  
  protected def varArgN[A,B <% NumericalExpression[A]](b: B) = {
    
    val c = b : NumericalExpression[A]
    val r = c.asInstanceOf[ConstantExpressionNode[A] with NumericalExpression[A]]
    r.isVarArg = true
    r
  }
  
  protected def varArgNN[A,B <% NonNumericalExpression[A]](b: B) = {
    
    val c = b : NonNumericalExpression[A]
    
    val r = c.asInstanceOf[ConstantExpressionNode[A] with NonNumericalExpression[A]]
    r.isVarArg = true
    r
  }      
  
  def define[R](q: Query[R]): Iterable[R] = new Iterable[R] {
    
    val aq = q.asInstanceOf[AbstractQuery[R]]

    val _dbAdapter = Session.currentSession.databaseAdapter
	val sw = new StatementWriter(false, _dbAdapter)
	aq.ast.write(sw)
    	
    private def parametrize(sw: StatementWriter, s: PreparedStatement): PreparedStatement = {    

      var i = 1;
      for(p0 <- sw.params) {

        var p = 
          if(p0.isInstanceOf[ConstantExpressionNode[_]]) {
            val cen = p0.asInstanceOf[ConstantExpressionNode[_]]
            cen.value.asInstanceOf[AnyRef]
          }
          else {
            p0
          }
        
        if(p.isInstanceOf[Option[_]]) {
          val o = p.asInstanceOf[Option[_]]
          if(o == None)
            s.setObject(i, null)
          else
            s.setObject(i, _dbAdapter.convertToJdbcValue(o.get.asInstanceOf[AnyRef]))
        }
        else
          s.setObject(i, _dbAdapter.convertToJdbcValue(p))
        i += 1
      }
      s
    }	
	    
    val s = Session.currentSession	    			
	val stmt = s.connection.prepareStatement(sw.statement)
	    
    override def iterator = new Iterator[R] with Closeable {
	
	    parametrize(sw, stmt)
	    
	    val rs = stmt.executeQuery
	    
	    s._addStatement(stmt) // if the iteration doesn't get completed, we must hang on to the statement to clean it up at session end.
	    s._addResultSet(rs) // same for the result set
	    
	    var _nextCalled = false;
	    var _hasNext = false;
	
	    var rowCount = 0
	    
	    def close {
	      stmt.close
	      rs.close
	    }
	
	    def _next = {
	      _hasNext = rs.next
	
	      if(!_hasNext) {// close it since we've completed the iteration
	        Utils.close(rs)
	        stmt.close
	      }
	      
	      rowCount = rowCount + 1
	      _nextCalled = true
	    }
	
	    def hasNext = {
	      if(!_nextCalled)
	        _next
	      _hasNext
	    }
	
	    def next: R = {
	      if(!_nextCalled)
	        _next
	      if(!_hasNext)
	        org.squeryl.internals.Utils.throwError("next called with no rows available")
	      _nextCalled = false
	
	      if(s.isLoggingEnabled)
	        s.log(ResultSetUtils.dumpRow(rs))
	
	      aq.give(aq.resultSetMapper, rs)
	    }
	  }    
  }
  
}