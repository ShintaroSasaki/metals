package tests

import munit.Location
import munit.TestOptions

/**
 * Test for request "textDocument/semanticTokens/full"
 * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_semanticTokens
 */
class SemanticHighlightLspSuite extends BaseLspSuite("SemanticHighlight") {

  check(
    "Basic",
    s"""|
        |class  Test{
        | var wkStr = "Dog-"
        | val nameStr = "Jack"
        | def Main={
        |  val titleStr= "I am "
        |  wkStr="Cat-"
        |  testC.bc(titleStr + wkStr)
        | }
        |}
        |
        |object  testC{
        | // Single Line Comment
        | def bc(msg:String)={
        |   println(msg)
        | }
        |}
        |""".stripMargin
  )

  // check(
  //   "Object,Method",
  //   // note(@tgodzik) Looking at the token types it seems we don't need to include braces etc.
  //   s"""|<<object>>/*keyword*/ <<Main>>/*class*/{
  //       |  <<def>>/*keyword*/ <<add>>/*method*/(<<a>>/*parameter*/ : <<Int>>/*type*/) = {
  //       |    <<a>>/*parameter*/ + 1
  //       |   }
  //       |}""".stripMargin
  // )

  // check(
  //   "Modifiers",
  //   s"""|<<package>>/*keyword*/ scala.meta.internal.pc
  //       |
  //       |<<abstract>>/*keyword*/ <<class>>/*keyword*/ <<Pet>>/*class,abstract*/ (<<name>>/*variable,readonly*/: String) {
  //       |    <<def>>/*keyword*/ <<speak>>/*method*/: <<Unit>>/*abstract*/ = println(s"My name is $$name")
  //       |}
  //       |
  //       |<<final>>/*keyword*/ <<class>>/*keyword*/ <<Dog>>/*class*/(<<name>>/*variable,readonly*/: <<String>>>>/*class*/) <<extends>>/*keyword*/ <<Pet>>/*abstract*/(<<name>>/*parameter*/)
  //       |<<final>>/*keyword*/ <<abstract>>/*keyword*/ <<class>>/*keyword*/ <<Cat>>/*class,abstract*/(<<name>>/*variable,readonly*/: <<String>>>>/*class*/) 
  //       | <<extends>>/*keyword*/ <<Pet>>/*abstract*/(<<name>>/*parameter*/)
  //       |
  //       |<<object>>/*keyword*/ <<Main>>/*class*/{
  //       |  <<val>>/*keyword*/ <<d>>/*variable,readonly*/ = <<new>>/*keyword*/ Dog("Fido") // Declaration
  //       |  <<d>>/*variable,readonly*/.<<speak>>/*method*/
  //       |  <<val>>/*keyword*/ <<c>>/*variable,readonly*/ = <<new>>/*keyword*/ Dog("Mike") // Declaration
  //       |}
  //       |
  //       |""".stripMargin
  // )

  def check(
      name: TestOptions,
      expected: String
  ) = {
    val fileContent =
      expected.replaceAll(raw"/\*[\w,]+\*/", "").replaceAll(raw"\<\<|\>\>", "")
    test(name) {
      for {
        // potentially we could derive input from
        _ <- initialize(
          s"""/metals.json
             |{"a":{}}
             |/a/src/main/scala/a/Main.scala
             |${fileContent}
             |""".stripMargin,
          expectError = true
        )
        _ <- server.didOpen("a/src/main/scala/a/Main.scala")
        _ <- server.assertSemanticHighlight(
          "a/src/main/scala/a/Main.scala",
          expected,
          fileContent
        )
      } yield ()
    }
  }

}
