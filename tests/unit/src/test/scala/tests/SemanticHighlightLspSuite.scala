package tests

import munit.TestOptions

/**
 * Test for request "textDocument/semanticTokens/full"
 * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_semanticTokens
 */
class SemanticHighlightLspSuite extends BaseLspSuite("SemanticHighlight") {


  check(
    "import(Out of File)",
    s"""|
        |object sample11 {
        |  def main(args: Array[String]) ={
        |    val name = "George"
        |    val outStr= s"Hello $name , Can you hear me ? "
        |    println(outStr)
        |  }
        |}
        |
        |
        |""".stripMargin,
  )

  // check(
  //   "String, Char",
  //   s"""|
  //       |object sample7 {
  //       |  def main(args: Array[String]) ={
  //       |
  //       |    val testStr1 : String = " Hello  "
  //       |    println(testStr1)
  //       |
  //       |    val testStr2 = """This is
  //       |    a multiline
  //       |    Test"""
  //       |    println(testStr2)
  //       |
  //       |    var testChar1 : Char =  'x'
  //       |     println(testChar1.toString())
  //       |
  //       |
  //       |  }
  //       |}
  //       |""".stripMargin
  // )

  // check(
  //   "enum",
  //   s"""|
  //       |
  //       |
  //       |
  //       |""".stripMargin
  // )

  // check(
  //   "Literal Identifer",
  //   s"""|
  //       |
  //       |
  //       |
  //       |""".stripMargin
  // )

  // check(
  //   "Template",
  //   s"""|
  //       |
  //       |
  //       |
  //       |""".stripMargin
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
        _ = assertEmpty(client.workspaceDiagnostics)
        _ <- server.assertSemanticHighlight(
          "a/src/main/scala/a/Main.scala",
          expected,
          fileContent,
        )
      } yield ()
    }
  }

}
