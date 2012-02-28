package org.squeryl.sharding.builder

import org.scalatest.FlatSpec
import org.squeryl.adapters._
import org.squeryl.SquerylException
import org.scalatest.matchers.{MustMatchers, MatchResult, Matcher, ShouldMatchers}
import org.squeryl.internals.DatabaseAdapter
import java.sql.SQLException

/**
 *
 * User: takeshita
 * Create: 12/01/21 19:40
 */

class AdaptorSelectorTest extends FlatSpec with MustMatchers {

  class AdapterMatcher(clazz : Class[_]) extends Matcher[Option[AnyRef]]{
    def apply(left: Option[AnyRef]): MatchResult = {
      MatchResult(
        left.isDefined && left.get.getClass.equals(clazz),
        "Adapter must be %s but was %s".format(clazz.getName,left.map(_.getClass.getName).getOrElse("None")),
        "negated message",
        "adapter matcher",
        "adapter matcher"
      )
    }
  }
  def beAdapterOf[T]( implicit m : Manifest[T]) = {
    new AdapterMatcher(m.erasure)
  }
  

  "correct url" should "be detected" in{
    AdapterSelector("jdbc:mysql://hoge") must beAdapterOf[MySQLInnoDBAdapter]
    AdapterSelector("jdbc:postgresql://localhost/test") must beAdapterOf[PostgreSqlAdapter]
    AdapterSelector("jdbc:microsoft:sqlserver://localhost:1433") must beAdapterOf[MSSQLServer]
    AdapterSelector("jdbc:h2:/test") must beAdapterOf[H2Adapter]
    AdapterSelector("jdbc:db2://hoge") must beAdapterOf[DB2Adapter]
    AdapterSelector("jdbc:derby://hoge") must beAdapterOf[DerbyAdapter]
    AdapterSelector("jdbc:oracle://hoge") must beAdapterOf[OracleAdapter]


  }

  "wrong url" should "return None" in{
    AdapterSelector("jdbc:") must be(None)
    AdapterSelector("jdbc:non_exist_database name") must be(None)
    AdapterSelector("http:wrong.uri/") must be(None)
    AdapterSelector("") must be(None)
    AdapterSelector(null) must be(None)
    AdapterSelector("not uri") must be(None)
  }

  "Registered adapters" should  "return class name" in{
    AdapterSelector.getDriverClassName(new MySQLInnoDBAdapter) must include("mysql")
    AdapterSelector.getDriverClassName(new MySQLAdapter) must include("mysql")
    AdapterSelector.getDriverClassName(new H2Adapter) must include("h2")
    AdapterSelector.getDriverClassName(new PostgreSqlAdapter) must include("postgre")
    AdapterSelector.getDriverClassName(new OracleAdapter) must include("oracle")
    AdapterSelector.getDriverClassName(new DB2Adapter) must include("db2")
    AdapterSelector.getDriverClassName(new DerbyAdapter) must include("derby")
    AdapterSelector.getDriverClassName(new MSSQLServer) must include("microsoft")
  }

  "Not registered adapters" should "return null" in{
    AdapterSelector.getDriverClassName(new DatabaseAdapter(){
      def isTableDoesNotExistException(e: SQLException): Boolean = false
    }) must be(null)
    AdapterSelector.getDriverClassName(null) must be(null)

  }



}