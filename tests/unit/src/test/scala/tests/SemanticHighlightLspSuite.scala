package tests

import munit.Location
import munit.TestOptions

/**
 * Test for request "textDocument/semanticTokens/full"
 * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_semanticTokens
 */
class SemanticHighlightLspSuite extends BaseLspSuite("SemanticHighlight") {

  check(
    "sample",
    // note(@tgodzik) Looking at the token types it seems we don't need to include braces etc.
    s"""|<<object>>/*keyword*/ <<Main>>/*class*/{
        |  <<def>>/*keyword*/ <<add>>/*method*/(<<a>>/*parameter*/ : <<Int>>/*type*/) = {
        |    <<a>>/*parameter*/ + 1
        |   }
        |}""".stripMargin
  )

  // check(
  //   ".sbt file",
  //   // note(@tgodzik) Looking at the token types it seems we don't need to include braces etc.
  //   s"""|<<object>>/*keyword*/ <<Main>>/*class*/{
  //       |  <<def>>/*keyword*/ <<add>>/*method*/
  //       |
  //       |    (<<a>>/*parameter*/ : <<Int>>/*type*/) = <<i>>/*variable*/
  //       |}""".stripMargin
  // )


  def check(
      name: TestOptions,
      expected: String
  ) = {
    val fileContent = expected.replaceAll(raw"/\*\w+\*/", "").replaceAll(raw"\<\<|\>\>", "")
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
