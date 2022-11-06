package tests

import munit.TestOptions

/**
 * Test for request "textDocument/semanticTokens/full"
 * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_semanticTokens
 */
class SemanticHighlightLspSuite extends BaseLspSuite("SemanticHighlight") {

    check(
    "Enum",
    s"""|
        |<<package>>/*keyword*/ <<example>>/*namespace*/
        |<<import>>/*keyword*/ java.nio.file.<<AccessMode>>/*enum*/
        |<<import>>/*keyword*/ java.nio.file.AccessMode.<<READ>>/*enumMember*/
        |<<import>>/*keyword*/ java.nio.file.AccessMode.<<WRITE>>/*enumMember*/
        |<<import>>/*keyword*/ java.nio.file.AccessMode.<<EXECUTE>>/*enumMember*/
        |<<object>>/*keyword*/ <<Main>>/*class*/ {
        |  (<<null>>/*keyword*/: <<AccessMode>>/*enumMember,abstract*/) <<match>>/*keyword*/ {
        |    <<case>>/*keyword*/ <<READ>>/*enumMember*/ <<=>>>/*operator*/ <<0>>/*number*/
        |    <<case>>/*keyword*/ <<WRITE>>/*enumMember*/ <<=>>>/*operator*/
        |    <<case>>/*keyword*/ <<EXECUTE>>/*enumMember*/ <<=>>>/*operator*/
        |
        |""".stripMargin,
  )


  // check(
  //   "StructuralTypes",
  //   s"""|
  //       |<<package>>/*keyword*/ <<example>>/*namespace*/
  //       |
  //       |<<object>>/*keyword*/ <<StructuralTypes>>/*class*/ {
  //       |  <<type>>/*keyword*/ <<User>>/*type*/ = {
  //       |    <<def>>/*keyword*/ <<name>>/*method,abstract*/: <<String>>/*type*/
  //       |    <<def>>/*keyword*/ <<age>>/*method,abstract*/: <<Int>>/*class,abstract*/
  //       |  }
  //       |
  //       |  <<val>>/*keyword*/ <<user>>/*variable,readonly*/ = <<null>>/*keyword*/.<<asInstanceOf>>/*method*/[<<User>>/*type*/]
  //       |  <<user>>/*variable,readonly*/.<<name>>/*method,abstract*/
  //       |  <<user>>/*variable,readonly*/.<<age>>/*method,abstract*/
  //       |
  //       |  <<val>>/*keyword*/ <<V>>/*variable,readonly*/: <<Object>>/*class,abstract*/ {
  //       |    <<def>>/*keyword*/ <<scalameta>>/*method,abstract*/: <<String>>/*type*/
  //       |  } = <<new>>/*keyword*/ {
  //       |    <<def>>/*keyword*/ <<scalameta>>/*method*/ = <<"4.0">>/*string*/
  //       |  }
  //       |  <<V>>/*variable,readonly*/.<<scalameta>>/*method,abstract*/
  //       |}
  //       |""".stripMargin,
  // )


  // check(
  //   "PatternMatching",
  //   s"""|
  //       |<<package>>/*keyword*/ <<example>>/*namespace*/
  //       |
  //       |<<class>>/*keyword*/ <<PatternMatching>>/*class*/ {
  //       |  <<val>>/*keyword*/ <<some>>/*variable,readonly*/ = <<Some>>/*class*/(<<1>>/*number*/)
  //       |  <<some>>/*variable,readonly*/ <<match>>/*keyword*/ {
  //       |    <<case>>/*keyword*/ <<Some>>/*class*/(<<number>>/*variable,readonly*/) <<=>>>/*operator*/
  //       |      <<number>>/*variable,readonly*/
  //       |  }
  //       |
  //       |  <<// tuple deconstruction>>/*comment*/
  //       |  <<val>>/*keyword*/ (<<left>>/*variable,readonly*/, <<right>>/*variable,readonly*/) = (<<1>>/*number*/, <<2>>/*number*/)
  //       |  (<<left>>/*variable,readonly*/, <<right>>/*variable,readonly*/)
  //       |
  //       |  <<// val deconstruction>>/*comment*/
  //       |  <<val>>/*keyword*/ <<Some>>/*class*/(<<number1>>/*variable,readonly*/) =
  //       |    <<some>>/*variable,readonly*/
  //       |  <<println>>/*method*/(<<number1>>/*variable,readonly*/)
  //       |
  //       |  <<def>>/*keyword*/ <<localDeconstruction>>/*method*/ = {
  //       |    <<val>>/*keyword*/ <<Some>>/*class*/(<<number2>>/*variable,readonly*/) =
  //       |      <<some>>/*variable,readonly*/
  //       |    <<number2>>/*variable,readonly*/
  //       |  }
  //       |}
  //       | 
  //       |""".stripMargin,
  // )

  // check(
  //   "package(nested)",
  //   s"""|
  //       |<<package>>/*keyword*/ <<example>>/*namespace*/
  //       |
  //       |<<package>>/*keyword*/ <<object>>/*keyword*/ <<nested>>/*class*/ {
  //       |
  //       |  <<class>>/*keyword*/ <<PackageObjectNestedClass>>/*class*/
  //       |
  //       |}
  //       |
  //       |<<class>>/*keyword*/ <<PackageObjectSibling>>/*class*/
  //       |
  //       |""".stripMargin,
  // )


  // check(
  //   "number literal, Static",
  //   s"""|
  //       |<<object>>/*keyword*/ <<ab>>/*class*/ {
  //       |  <<var>>/*keyword*/  <<iVar>>/*variable*/:<<Int>>/*class,abstract*/ = <<1>>/*number*/
  //       |  <<val>>/*keyword*/  <<iVal>>/*variable,readonly*/:<<Double>>/*class,abstract*/ = <<4.94065645841246544e-324d>>/*number*/
  //       |  <<val>>/*keyword*/  <<fVal>>/*variable,readonly*/:<<Float>>/*class,abstract*/ = <<1.40129846432481707e-45>>/*number*/
  //       |  <<val>>/*keyword*/  <<lVal>>/*variable,readonly*/:<<Long>>/*class,abstract*/ = <<9223372036854775807L>>/*number*/
  //       |}
  //       |
  //       |<<object>>/*keyword*/ <<sample10>>/*class*/ {
  //       |  <<def>>/*keyword*/ <<main>>/*method*/(<<args>>/*parameter*/: <<Array>>/*class*/[<<String>>/*type*/]) ={
  //       |    <<println>>/*method*/(
  //       |     (<<ab>>/*class*/.<<iVar>>/*variable*/ <<+>>/*method,abstract*/ <<ab>>/*class*/.<<iVal>>/*variable,readonly*/).<<toString>>/*method*/
  //       |    )
  //       |  }
  //       |}
  //       |""".stripMargin,
  // )


  def check(
      name: TestOptions,
      expected: String,
  ): Unit = {
    val fileContent =
      expected.replaceAll(raw"/\*[\w,]+\*/", "").replaceAll(raw"\<\<|\>\>", "")

    val fileName = "/a/src/main/scala/a/Main.scala"

    test(name) {
      for {
        // potentially we could derive input from
        _ <- initialize(
          s"""/metals.json
             |{"a":{}}
             |${fileName.trim()}
             |${fileContent}
             |""".stripMargin,
          expectError = true,
        )
        _ <- server.didOpen("a/src/main/scala/a/Main.scala")
        // _ = assertEmpty(client.workspaceDiagnostics)
        _ <- server.assertSemanticHighlight(
          "a/src/main/scala/a/Main.scala",
          expected,
          fileContent,
        )
      } yield ()
    }
  }

}
