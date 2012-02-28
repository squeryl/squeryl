package org.squeryl.sharding

import org.scalatest.matchers.MustMatchers
import org.scalatest.{FlatSpec, FunSuite}

/**
 *
 * User: takeshita
 * Create: 12/02/15 1:19
 */

class ShardModeTest extends FlatSpec with MustMatchers {


  "ShardModeTest#hasSameFunction" should "judge" in{
    ShardMode.hasSameFunction(ShardMode.Read,ShardMode.Read) must be(true)
    ShardMode.hasSameFunction(ShardMode.Write,ShardMode.Write) must be(true)
    ShardMode.hasSameFunction(ShardMode.Write,ShardMode.Read) must be(true)
    ShardMode.hasSameFunction(ShardMode.Read,ShardMode.Write) must be(false)

  }



}