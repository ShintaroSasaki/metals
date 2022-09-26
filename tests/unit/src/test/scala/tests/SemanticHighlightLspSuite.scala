package tests

import munit.Location
import munit.TestOptions

/**
 * Test for request "textDocument/semanticTokens/full"
 * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_semanticTokens
 */
class SemanticHighlightLspSuite extends BaseLspSuite("SemanticHighlight") {

  // check(
  //   "class, object, var, val, method, type, parameter,readonly",
  //   // note(@tgodzik) Looking at the token types it seems we don't need to include braces etc.
  //   s"""|
  //       |<<class>>/*keyword*/  <<Test>>/*class*/{
  //       |
  //       | <<var>>/*keyword*/ <<wkStr>>/*variable*/ = "Dog-"
  //       | <<val>>/*keyword*/ <<nameStr>>/*variable,readonly*/ = "Jack"
  //       |
  //       | <<def>>/*keyword*/ <<Main>>/*method*/={
  //       |
  //       |  <<val>>/*keyword*/ <<preStr>>/*variable,readonly*/= "I am "
  //       |  <<var>>/*keyword*/ <<postStr>>/*variable*/= "in a house. "
  //       |  <<wkStr>>/*variable*/=<<nameStr>>/*variable,readonly*/ <<+>>/*method*/ "Cat-"
  //       |
  //       |  <<testC>>/*class*/.<<bc>>/*method*/(<<preStr>>/*variable,readonly*/
  //       |    <<+>>/*method*/ <<wkStr>>/*variable*/
  //       |    <<+>>/*method*/ <<preStr>>/*variable,readonly*/)
  //       | }
  //       |}
  //       |
  //       |<<object>>/*keyword*/  <<testC>>/*class*/{
  //       |
  //       | <<def>>/*keyword*/ <<bc>>/*method*/(<<msg>>/*parameter*/:<<String>>/*type*/)={
  //       |   <<println>>/*method*/(<<msg>>/*parameter*/)
  //       | }
  //       |}
  //       |
  //       |""".stripMargin
  // )

  //   check(
  //   "import(Out of File)",
  //   s"""|
  //       |import scala.math.sqrt
  //       |object sample3 {
  //       |
  //       |  def sqrtplus1(x: Int) 
  //       |     = sqrt(x).toString()
  //       |
  //       |  def main(args: Array[String]) ={
  //       |    println("Hello, world! : " + sqrtplus1(2))
  //       |  }
  //       |}
  //       |
  //       |""".stripMargin
  // )

  // check(
  //   "abstract, trait, type parameter",
  //   s"""|
  //       |package a.b
  //       |object Sample5 {
  //       |
  //       |  def main(args: Array[String]) ={
  //       |      val itr = new IntIterator(5)
  //       |      var str = itr.next().toString + ","+itr.next().toString
  //       |      println("count:"+str) 
  //       |  }
  //       |
  //       |  trait Iterator[A] {
  //       |    def next(): A
  //       |  }
  //       |
  //       |  abstract class hasLogger {
  //       |    def log(str:String) = {println(str)}
  //       |  }
  //       |
  //       |  class IntIterator(to: Int) 
  //       |  extends hasLogger with Iterator[Int]  {
  //       |    private var current = 0
  //       |    override def next(): Int = {
  //       |      if (current < to) {
  //       |        log("main")
  //       |        val t = current
  //       |        current = current + 1
  //       |        t
  //       |      } else 0
  //       |    }
  //       |  }
  //       |}
  //       |""".stripMargin
  // )

  
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

  check(
    "String, Char",
    s"""|
        |object sample7 {
        |  def main(args: Array[String]) ={
        |
        |    val testStr1 : String = " Hello  "
        |    println(testStr1)
        |
        |    val testStr2 = """This is
        |    a multiline
        |    Test"""
        |    println(testStr2)
        |
        |    var testChar1 : Char =  'x'
        |     println(testChar1.toString())
        |
        |
        |  }
        |}
        |""".stripMargin
  )

  // check(
  //   "enum",
  //   s"""|
  //       |
  //       |
  //       |
  //       |""".stripMargin
  // )

  //in referred to https://www.scala-lang.org/api/2.13.3/scala/deprecated.html
  check(
    "Deprecated",
    s"""|<<object>>/*keyword*/ <<sample9>>/*class*/ {
       |  <<@>>/*keyword*/<<deprecated>>/*class*/("this method will be removed", "FooLib 12.0")
       |  <<def>>/*keyword*/ <<oldMethod>>/*method,deprecated*/(<<x>>/*parameter*/: <<Int>>/*class,abstract*/) = <<x>>/*parameter*/
       |
       |  <<def>>/*keyword*/ <<main>>/*method*/(<<args>>/*parameter*/: <<Array>>/*class*/[String]) ={
       |    <<val>>/*keyword*/ <<str>>/*variable,readonly*/ = <<oldMethod>>/*method,deprecated*/(<<2>>/*number*/).<<toString>>/*method*/
       |    <<println>>/*method*/("Hello, world!"<<+>>/*method*/ <<str>>/*variable,readonly*/)
       |  }
       |}
       |""".stripMargin
  )


  check(
    "number literal, Static",
    s"""|
        |object ab {
        |  var  iVar:Int = 1
        |  val  iVal:Double = 4.94065645841246544e-324d
        |  val  fVal:Float = 1.40129846432481707e-45
        |  val  lVal:Long = 9223372036854775807L
        |}
        |
        |object sample10 {
        |  def main(args: Array[String]) ={
        |    println((ab.iVar + ab.iVal).toString)
        |  }
        |}
        |""".stripMargin
  )

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
      expected: String
  ) = {
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
          expectError = true
        )
        _ <- server.didOpen("a/src/main/scala/a/Main.scala")
        _ = assertEmpty(client.workspaceDiagnostics)
        _ <- server.assertSemanticHighlight(
          "a/src/main/scala/a/Main.scala",
          expected,
          fileContent
        )
      } yield ()
    }
  }

}
