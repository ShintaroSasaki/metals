
package scala.meta.internal.pc

import scala.tools.nsc.ast.parser.Tokens
import org.eclipse.lsp4j.SemanticTokenTypes
import org.eclipse.lsp4j.SemanticTokenModifiers

/** Associate scala.tools.nsc.ast.parser.Tokens
 *  with LSP semanticToken types, and modifiers.
 *  https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_semanticTokens
 *
*/
class TokenClassifier {

}

object  TokenClassifier {

  def getTokenType(token: Int, capableTypes:List[String]):Integer={
    token match {
        // case Tokens.STRINGPART  => capableTypes.indexOf(SemanticTokenTypes.?)
        // case Tokens.SYMBOLLIT  => capableTypes.indexOf(SemanticTokenTypes.?)
        // case Tokens.INTERPOLATIONID  => capableTypes.indexOf(SemanticTokenTypes.?)

        /** identifiers */
        // case Tokens.IDENTIFIER  => capableTypes.indexOf(SemanticTokenTypes.?)
        // case Tokens.BACKQUOTED_IDENT  => capableTypes.indexOf(SemanticTokenTypes.?)

        /** modifiers */
        case Tokens.IMPLICIT  => capableTypes.indexOf(SemanticTokenTypes.Keyword)
        case Tokens.OVERRIDE  => capableTypes.indexOf(SemanticTokenTypes.Keyword)
        case Tokens.SEALED  => capableTypes.indexOf(SemanticTokenTypes.Keyword)
        case Tokens.LAZY  => capableTypes.indexOf(SemanticTokenTypes.Keyword)
        case Tokens.MACRO  => capableTypes.indexOf(SemanticTokenTypes.Keyword)

        /** templates */
        case Tokens.CASECLASS  => capableTypes.indexOf(SemanticTokenTypes.Class)
        case Tokens.OBJECT  => capableTypes.indexOf(SemanticTokenTypes.Class)
        case Tokens.CASEOBJECT  => capableTypes.indexOf(SemanticTokenTypes.Class)
        case Tokens.TRAIT  => capableTypes.indexOf(SemanticTokenTypes.Interface)
        case Tokens.WITH  => capableTypes.indexOf(SemanticTokenTypes.Keyword)
        case Tokens.TYPE  => capableTypes.indexOf(SemanticTokenTypes.Class)
        case Tokens.FORSOME  => capableTypes.indexOf(SemanticTokenTypes.Keyword)
        case Tokens.DEF  => capableTypes.indexOf(SemanticTokenTypes.Function)
        case Tokens.VAL  => capableTypes.indexOf(SemanticTokenTypes.Keyword)
        case Tokens.VAR  => capableTypes.indexOf(SemanticTokenTypes.Keyword)

        /** control structures */
        case Tokens.THEN  => capableTypes.indexOf(SemanticTokenTypes.Keyword)
        case Tokens.YIELD  => capableTypes.indexOf(SemanticTokenTypes.Keyword)
        case Tokens.MATCH  => capableTypes.indexOf(SemanticTokenTypes.Keyword)

        /** special symbols */
        // case Tokens.HASH  => capableTypes.indexOf(SemanticTokenTypes.?)
        // case Tokens.USCORE  => capableTypes.indexOf(SemanticTokenTypes.?)
        case Tokens.ARROW  => capableTypes.indexOf(SemanticTokenTypes.Operator)
        case Tokens.LARROW  => capableTypes.indexOf(SemanticTokenTypes.Operator)
        case Tokens.SUBTYPE  => capableTypes.indexOf(SemanticTokenTypes.Class)
        case Tokens.SUPERTYPE  => capableTypes.indexOf(SemanticTokenTypes.Class)
        // case Tokens.VIEWBOUND  => capableTypes.indexOf(SemanticTokenTypes.?)
        // case Tokens.NEWLINE  => capableTypes.indexOf(SemanticTokenTypes.?)
        // case Tokens.NEWLINES  => capableTypes.indexOf(SemanticTokenTypes.?)
        // case Tokens.XMLSTART  => capableTypes.indexOf(SemanticTokenTypes.?)

        /** for IDE only */
        case Tokens.COMMENT  => capableTypes.indexOf(SemanticTokenTypes.Comment)
        // case Tokens.WHITESPACE  => capableTypes.indexOf(SemanticTokenTypes.?)
        // case Tokens.IGNORE  => capableTypes.indexOf(SemanticTokenTypes.?)
        // case Tokens.ESCAPE  => capableTypes.indexOf(SemanticTokenTypes.?)

        /** special tokens */
        case Tokens.EMPTY  => capableTypes.indexOf(SemanticTokenTypes.Keyword)
        // case Tokens.UNDEF  => capableTypes.indexOf(SemanticTokenTypes.?)
        // case Tokens.ERROR  => capableTypes.indexOf(SemanticTokenTypes.?)
        // case Tokens.EOF  => capableTypes.indexOf(SemanticTokenTypes.?)

        /** literals */
        case Tokens.CHARLIT  => capableTypes.indexOf(SemanticTokenTypes.String)
        case Tokens.INTLIT  => capableTypes.indexOf(SemanticTokenTypes.Number)
        case Tokens.LONGLIT  => capableTypes.indexOf(SemanticTokenTypes.Number)
        case Tokens.FLOATLIT  => capableTypes.indexOf(SemanticTokenTypes.Number)
        case Tokens.DOUBLELIT  => capableTypes.indexOf(SemanticTokenTypes.Number)
        case Tokens.STRINGLIT  => capableTypes.indexOf(SemanticTokenTypes.Number)

        /** keywords */
        case Tokens.NEW  => capableTypes.indexOf(SemanticTokenTypes.Keyword)
        case Tokens.THIS  => capableTypes.indexOf(SemanticTokenTypes.Keyword)
        case Tokens.SUPER  => capableTypes.indexOf(SemanticTokenTypes.Keyword)

        case Tokens.NULL  => capableTypes.indexOf(SemanticTokenTypes.Keyword)
        case Tokens.TRUE  => capableTypes.indexOf(SemanticTokenTypes.Keyword)
        case Tokens.FALSE  => capableTypes.indexOf(SemanticTokenTypes.Keyword)

        /** modifiers */
        case Tokens.PROTECTED  => capableTypes.indexOf(SemanticTokenTypes.Keyword)
        case Tokens.PRIVATE  => capableTypes.indexOf(SemanticTokenTypes.Keyword)
        // case Tokens.ABSTRACT  => capableTypes.indexOf(SemanticTokenTypes.?)
        case Tokens.FINAL  => capableTypes.indexOf(SemanticTokenTypes.Keyword)

        /** templates */
        case Tokens.PACKAGE  => capableTypes.indexOf(SemanticTokenTypes.Keyword)
        case Tokens.IMPORT  => capableTypes.indexOf(SemanticTokenTypes.Keyword)
        case Tokens.CLASS  => capableTypes.indexOf(SemanticTokenTypes.Class)
        case Tokens.EXTENDS  => capableTypes.indexOf(SemanticTokenTypes.Keyword)

        /** control structures */
        case Tokens.IF  => capableTypes.indexOf(SemanticTokenTypes.Keyword)
        case Tokens.ELSE  => capableTypes.indexOf(SemanticTokenTypes.Keyword)
        case Tokens.WHILE  => capableTypes.indexOf(SemanticTokenTypes.Keyword)
        case Tokens.DO  => capableTypes.indexOf(SemanticTokenTypes.Keyword)
        case Tokens.FOR  => capableTypes.indexOf(SemanticTokenTypes.Keyword)
        case Tokens.THROW  => capableTypes.indexOf(SemanticTokenTypes.Keyword)
        case Tokens.TRY  => capableTypes.indexOf(SemanticTokenTypes.Keyword)
        case Tokens.CATCH  => capableTypes.indexOf(SemanticTokenTypes.Keyword)
        case Tokens.FINALLY  => capableTypes.indexOf(SemanticTokenTypes.Keyword)
        case Tokens.CASE  => capableTypes.indexOf(SemanticTokenTypes.Keyword)
        case Tokens.RETURN  => capableTypes.indexOf(SemanticTokenTypes.Keyword)

        /** parenthesis */
        // case Tokens.LPAREN  => capableTypes.indexOf(SemanticTokenTypes.?)
        // case Tokens.RPAREN  => capableTypes.indexOf(SemanticTokenTypes.?)
        // case Tokens.LBRACKET  => capableTypes.indexOf(SemanticTokenTypes.?)
        // case Tokens.RBRACKET  => capableTypes.indexOf(SemanticTokenTypes.?)
        // case Tokens.LBRACE  => capableTypes.indexOf(SemanticTokenTypes.?)
        // case Tokens.RBRACE  => capableTypes.indexOf(SemanticTokenTypes.?)

        /** special symbols */
        // case Tokens.COMMA  => capableTypes.indexOf(SemanticTokenTypes.?)
        // case Tokens.SEMI  => capableTypes.indexOf(SemanticTokenTypes.?)
        // case Tokens.DOT  => capableTypes.indexOf(SemanticTokenTypes.?)
        // case Tokens.COLON  => capableTypes.indexOf(SemanticTokenTypes.?)
        case Tokens.EQUALS  => capableTypes.indexOf(SemanticTokenTypes.Operator)
        // case Tokens.AT  => capableTypes.indexOf(SemanticTokenTypes.?)

        case _ => null
    }
}

  def getTokenModifier(token: Int, capableMods:List[String]):Integer={
    val place:Double = token match {
        // case Tokens.PROTECTED  => capableMods.indexOf(SemanticTokenModifiers.?)
        // case Tokens.PRIVATE  => capableMods.indexOf(SemanticTokenModifiers.?)
        case Tokens.ABSTRACT  => capableMods.indexOf(SemanticTokenModifiers.Abstract)
        // case Tokens.FINAL  => capableMods.indexOf(SemanticTokenModifiers.?)
        case _ => -1
    }
    var mods = 0
    if (place >= 0 ){
        mods = mods + scala.math.pow(2, place).toInt //
    }
    mods.toBinaryString.toInt
  }



}

