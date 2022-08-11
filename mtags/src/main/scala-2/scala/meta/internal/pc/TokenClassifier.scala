
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
  def getTokenType(tk:scala.meta.tokens.Token, capableTypes:List[String]):Integer = {
    tk match {
      case _ :Token.Ident => -1 //get information from tree
      
      // Alphanumeric keywords)
      // case _ :KwAbstract => capableTypes.indexOf(SemanticTokenTypes.ModifierKeyword)
      case _ :Token.KwCase => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwCatch => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwClass => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwDef => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwDo => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwElse => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwEnum => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwExport => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwExtends => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwFalse => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      // case _ :Token.KwFinal => capableTypes.indexOf(SemanticTokenTypes.ModifierKeyword)
      case _ :Token.KwFinally => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwFor => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwForsome => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwGiven => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwIf => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      // case _ :Token.KwImplicit => capableTypes.indexOf(SemanticTokenTypes.ModifierKeyword)
      case _ :Token.KwImport => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      // case _ :Token.KwLazy => capableTypes.indexOf(SemanticTokenTypes.ModifierKeyword)
      case _ :Token.KwMatch => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwMacro => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwNew => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwNull => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwObject => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      // case _ :Token.KwOverride => capableTypes.indexOf(SemanticTokenTypes.ModifierKeyword)
      case _ :Token.KwPackage => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      // case _ :Token.KwPrivate => capableTypes.indexOf(SemanticTokenTypes.ModifierKeyword)
      // case _ :Token.KwProtected => capableTypes.indexOf(SemanticTokenTypes.ModifierKeyword)
      case _ :Token.KwReturn => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      // case _ :Token.KwSealed => capableTypes.indexOf(SemanticTokenTypes.ModifierKeyword)
      case _ :Token.KwSuper => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwThen => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwThis => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwThrow => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwTrait => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwTrue => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwTry => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwType => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwVal => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwVar => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwWhile => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwWith => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.KwYield => capableTypes.indexOf(SemanticTokenTypes.Keyword)

      // extends Symbolic keywords
      case _ :Token.Hash => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.Colon => capableTypes.indexOf(SemanticTokenTypes.Operator)
      case _ :Token.Viewbound => capableTypes.indexOf(SemanticTokenTypes.Operator)
      case _ :Token.LeftArrow => capableTypes.indexOf(SemanticTokenTypes.Operator)
      case _ :Token.Subtype => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.Equals => capableTypes.indexOf(SemanticTokenTypes.Operator)
      case _ :Token.RightArrow => capableTypes.indexOf(SemanticTokenTypes.Operator)
      case _ :Token.Supertype => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.At => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.Underscore => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.TypeLambdaArrow => capableTypes.indexOf(SemanticTokenTypes.Operator)
      case _ :Token.ContextArrow => capableTypes.indexOf(SemanticTokenTypes.Operator)
      case _ :Token.MacroQuote => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _ :Token.MacroSplice => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      
      // // Delimiters
      // case _ :Token.LeftParen => capableTypes.indexOf(SemanticTokenTypes.???)
      // case _ :Token.RightParen => capableTypes.indexOf(SemanticTokenTypes.???)
      // case _ :Token.Comma => capableTypes.indexOf(SemanticTokenTypes.???)
      // case _ :Token.Dot => capableTypes.indexOf(SemanticTokenTypes.???)
      // case _ :Token.Semicolon => capableTypes.indexOf(SemanticTokenTypes.???)
      // case _ :Token.LeftBracket => capableTypes.indexOf(SemanticTokenTypes.???)
      // case _ :Token.RightBracket => capableTypes.indexOf(SemanticTokenTypes.???)
      // case _ :Token.LeftBrace => capableTypes.indexOf(SemanticTokenTypes.???)
      // case _ :Token.RightBrace => capableTypes.indexOf(SemanticTokenTypes.???)

      //Default
      case _ => -1
    }
    
  }
    

  /** This function returns 0 when capableModifier is nothing. */
  def getTokenModifier(tk: scala.meta.tokens.Token, capableMods:List[String]):Integer={

    val place:Double = tk match {
      case _ :Token.KwAbstract => capableMods.indexOf(SemanticTokenModifiers.Abstract)      
      case _ :Token.KwFinal => capableMods.indexOf(SemanticTokenModifiers.Modification)
      case _ :Token.KwImplicit => capableMods.indexOf(SemanticTokenModifiers.Modification)
      case _ :Token.KwLazy => capableMods.indexOf(SemanticTokenModifiers.Static)
      case _ :Token.KwOverride => capableMods.indexOf(SemanticTokenModifiers.Modification)
      case _ :Token.KwPrivate => capableMods.indexOf(SemanticTokenModifiers.Modification)
      case _ :Token.KwProtected => capableMods.indexOf(SemanticTokenModifiers.Modification)
      case _ :Token.KwSealed => capableMods.indexOf(SemanticTokenModifiers.Modification)
      case _ => -1
    }
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




