package org.squeryl.test

import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.squeryl.Schema
import org.squeryl.test.PrimitiveTypeModeForTests._

class NamingConventionTransformsTest extends FunSuite with Matchers {

  object FooSchema extends Schema
  
  private def snakify(name: String) = FooSchema.NamingConventionTransforms.snakify(name)
  
  test("replace CamelCase with underscore") {
    snakify("MyTableName") shouldBe "my_table_name"
    snakify("TableName") shouldBe "table_name"
    snakify("Table") shouldBe "table"
    snakify("MyTable12Name") shouldBe "my_table12_name"
    snakify("TableName12") shouldBe "table_name12"
    snakify("Table12") shouldBe "table12"
  }

  test("don't modify existing snake case strings") {
    snakify("my_snake_case") shouldBe "my_snake_case"
    snakify("snake") shouldBe "snake"
  }

  test("handle abbeviations") {
    snakify("HTML") shouldBe "html"
    snakify("HTMLEditor") shouldBe "html_editor"
    snakify("EditorTOC") shouldBe "editor_toc"
    snakify("HTMLEditorTOC") shouldBe "html_editor_toc"

    snakify("HTML5") shouldBe "html5"
    snakify("HTML5Editor") shouldBe "html5_editor"
    snakify("Editor2TOC") shouldBe "editor2_toc"
    snakify("HTML5Editor2TOC") shouldBe "html5_editor2_toc"
  }
  
  test("prepend underscore if it starts with a number") {
    snakify("12MyTableName") shouldBe "_12_my_table_name"
    snakify("12TableName") shouldBe "_12_table_name"
    snakify("12Table") shouldBe "_12_table"
    snakify("12MyTable12Name") shouldBe "_12_my_table12_name"
    snakify("12TableName12") shouldBe "_12_table_name12"
    snakify("12Table12") shouldBe "_12_table12"

    snakify("12_my_snake_case") shouldBe "_12_my_snake_case"
    snakify("12snake") shouldBe "_12snake"

    snakify("12HTML") shouldBe "_12_html"
    snakify("12HTMLEditor") shouldBe "_12_html_editor"
    snakify("12EditorTOC") shouldBe "_12_editor_toc"
    snakify("12HTML5") shouldBe "_12_html5"
    snakify("12HTML5Editor") shouldBe "_12_html5_editor"
    snakify("12Editor2TOC") shouldBe "_12_editor2_toc"
  }
}
