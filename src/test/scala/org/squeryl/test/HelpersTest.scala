package org.squeryl.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.squeryl.Schema
import org.squeryl.test.PrimitiveTypeModeForTests._

class NamingConventionTransformsTest extends FunSuite with ShouldMatchers {

  object FooSchema extends Schema
  
  def snakify(name: String) = FooSchema.NamingConventionTransforms.snakify(name)
  
  test("replace CamelCase with underscore") {
    assert(snakify("MyTableName") == "my_table_name")
    assert(snakify("TableName") == "table_name")
    assert(snakify("Table") == "table")
    assert(snakify("MyTable12Name") == "my_table12_name")
    assert(snakify("TableName12") == "table_name12")
    assert(snakify("Table12") == "table12")
  }

  test("don't modify existing snake case strings") {
    assert(snakify("my_snake_case") == "my_snake_case")
    assert(snakify("snake") == "snake")
  }

  test("handle abbeviations") {
    assert(snakify("HTML") == "html")
    assert(snakify("HTMLEditor") == "html_editor")
    assert(snakify("EditorTOC") == "editor_toc")
    assert(snakify("HTMLEditorTOC") == "html_editor_toc")

    assert(snakify("HTML5") == "html5")
    assert(snakify("HTML5Editor") == "html5_editor")
    assert(snakify("Editor2TOC") == "editor2_toc")
    assert(snakify("HTML5Editor2TOC") == "html5_editor2_toc")
  }
  
  test("prepend underscore if it starts with a number") {
    assert(snakify("12MyTableName") == "_12_my_table_name")
    assert(snakify("12TableName") == "_12_table_name")
    assert(snakify("12Table") == "_12_table")
    assert(snakify("12MyTable12Name") == "_12_my_table12_name")
    assert(snakify("12TableName12") == "_12_table_name12")
    assert(snakify("12Table12") == "_12_table12")

    assert(snakify("12_my_snake_case") == "_12_my_snake_case")
    assert(snakify("12snake") == "_12snake")

    assert(snakify("12HTML") == "_12_html")
    assert(snakify("12HTMLEditor") == "_12_html_editor")
    assert(snakify("12EditorTOC") == "_12_editor_toc")
    assert(snakify("12HTML5") == "_12_html5")
    assert(snakify("12HTML5Editor") == "_12_html5_editor")
    assert(snakify("12Editor2TOC") == "_12_editor2_toc")
  }
}