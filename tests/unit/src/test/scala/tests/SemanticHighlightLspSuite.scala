package tests

import munit.Location
import munit.TestOptions

/**
 * Test for request "textDocument/semanticTokens/full"
 * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_semanticTokens
 */
class SemanticHighlightLspSuite extends BaseLspSuite("SemanticHighlight") {

  check(
    "class, object, var, val, method, type, parameter,readonly",
    // note(@tgodzik) Looking at the token types it seems we don't need to include braces etc.
    s"""|
        |<<class>>/*keyword*/  <<Test>>/*class*/{
        |
        | <<var>>/*keyword*/ <<wkStr>>/*variable*/ = "Dog-"
        | <<val>>/*keyword*/ <<nameStr>>/*variable,readonly*/ = "Jack"
        |
        | <<def>>/*keyword*/ <<Main>>/*method*/={
        |
        |  <<val>>/*keyword*/ <<preStr>>/*variable,readonly*/= "I am "
        |  <<var>>/*keyword*/ <<postStr>>/*variable*/= "in a house. "
        |  <<wkStr>>/*variable*/=<<nameStr>>/*variable,readonly*/ <<+>>/*method*/ "Cat-"
        |
        |  <<testC>>/*class*/.<<bc>>/*method*/(<<preStr>>/*variable,readonly*/
        |    <<+>>/*method*/ <<wkStr>>/*variable*/
        |    <<+>>/*method*/ <<preStr>>/*variable,readonly*/)
        | }
        |}
        |
        |<<object>>/*keyword*/  <<testC>>/*class*/{
        |
        | <<def>>/*keyword*/ <<bc>>/*method*/(<<msg>>/*parameter*/:<<String>>/*type*/)={
        |   <<println>>/*method*/(<<msg>>/*parameter*/)
        | }
        |}
        |
        |""".stripMargin
  )

  // check(
  //   "Comment(Single-Line, Multi-Line)",
  //   s"""|
  //       |<<object>>/*keyword*/ <<Main>>/*class*/{
  //       |
  //       |   <</**>>/*comment*/
  //       |<<   * Test of Comment Block>>/*comment*/
  //       |<<   */>>/*comment*/  <<val>>/*keyword*/ <<x>>/*variable,readonly*/ = 1
  //       |
  //       |  <<def>>/*keyword*/ <<add>>/*method*/(<<a>>/*parameter*/ : <<Int>>/*class,abstract*/) = {
  //       |    <<// Single Line Comment>>/*comment*/
  //       |    <<a>>/*parameter*/ <<+>>/*method,abstract*/ 1 <<// com = 1>>/*comment*/
  //       |   }
  //       |}
  //       |
  //       |
  //       |""".stripMargin
  // )

  // check(
  //   "abstract, trait, import(Out of File)",
  //   s"""|
  //       |package SemanticHighlightLspSuite.tests
  //       |import java.util.logging.Logger
  //       |
  //       |trait Iterator[A] {
  //       |  def hasNext: Boolean
  //       |  def next(): A
  //       |}
  //       |
  //       |abstract class hasLogger {
  //       |  val logger = Logger.getLogger(classOf[This].getName)
  //       |}
  //       |
  //       |class IntIterator(to: Int) 
  //       |extends hasLogger with Iterator[Int]  {
  //       |  private var current = 0
  //       |  override def hasNext: Boolean = current < to
  //       |  override def next(): Int = {
  //       |    logger.info("next")
  //       |    if (hasNext) {
  //       |      val t = current
  //       |      current += 1
  //       |      t
  //       |    } else 0
  //       |  }
  //       |}
  //       |
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
