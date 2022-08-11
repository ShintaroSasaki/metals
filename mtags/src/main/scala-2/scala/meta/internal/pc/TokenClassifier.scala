
package scala.meta.internal.pc

import scala.tools.nsc.ast.parser.Tokens
import org.eclipse.lsp4j.SemanticTokenTypes
import org.eclipse.lsp4j.SemanticTokenModifiers
import java.util.logging.Logger
import scala.meta.tokens._

/** Associate scala.tools.nsc.ast.parser.Tokens
 *  with LSP semanticToken types, and modifiers.
 *  https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_semanticTokens
 *
*/
class TokenClassifier {

}

object  TokenClassifier {

  val logger: Logger =Logger.getLogger(classOf[ScalaPresentationCompiler].getName)

  trait Typer[A]{
    def getTT(a : A, b:List[String]) : Integer
  }

  trait Additive[A] {
    def plus(a: A, b: A): A
    def zero: A
  }

  /** This function returns 0 when capable Type is nothing. */
  def getTokenType[B](token: B, capableTypes:List[String]):Integer = {
    
    // //sample
    // implicit object StringAdditive extends Additive[String] {
    //   def plus(a: String, b: String): String = a + b
    //   def zero: String = ""
    // }

    // implicit object IntAdditive extends Additive[Int] {
    //   def plus(a: Int, b: Int): Int = a + b
    //   def zero: Int = 0
    // }

    // def sum[A](lst: List[A])(implicit m: Additive[A]) = lst.foldLeft(m.zero)((x, y) => m.plus(x, y))
    // sum(List(1, 2, 3))// res7: Int = 6
    // sum(List("A", "B", "C"))

    implicit object KeywordTyper extends Typer[Token.Keyword] {
      def getTT(a:Token.Keyword, capableTypes:List[String]) : Integer 
        =  capableTypes.indexOf(SemanticTokenTypes.Keyword)
    }
    implicit object DefaultTyper extends Typer[Any] {
      def getTT(a:Any, capableTypes:List[String]) : Integer =  -1
    }
    
    def getTokenTypeSub[B](token : B, capableTypes:List[String])(implicit m: Typer[B]):Integer=
        m.getTT(token, capableTypes)
    

    getTokenTypeSub(token, capableTypes)()
    
  }
    

  /** This function returns 0 when capableModifier is nothing. */
  def getTokenModifier(token: Token, capableMods:List[String]):Integer={

    val place:Double = -1
  //  Tokens.ABSTRACT  => capableMods.indexOf(SemanticTokenModifiers.Abstract)
    var mods = 0
    if (place == -1 ){
      mods = 0
    } else {
      mods = mods + scala.math.pow(2, place).toInt //
    }
    // var strlog = ""
    // strlog ++=  "\n place :" + place.toString()
    // strlog ++=  "** mods :" + mods.toString()
    // strlog ++=  "** mods B :" + mods.toBinaryString
    // strlog ++=  "** mods I :" + mods.toBinaryString.toInt.toString()
    // logger.info(strlog)

    mods.toBinaryString.toInt
  } // end

}




