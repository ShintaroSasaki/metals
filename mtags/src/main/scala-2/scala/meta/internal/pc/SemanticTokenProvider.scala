package scala.meta.internal.pc
import org.eclipse.lsp4j.SemanticTokenModifiers
import org.checkerframework.common.returnsreceiver.qual.This
import scala.collection.mutable.ListBuffer
import java.util
import java.util.logging.Logger
import java.{util => ju}
import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.pc.VirtualFileParams
import org.eclipse.lsp4j.SemanticTokenTypes
import scala.meta.tokens._

// import scala.meta._

class SemanticTokenProvider (
      cp:MetalsGlobal,
        params: VirtualFileParams,
      capableTypes: util.List[String],
      capableModifiers: util.List[String]
){
  // logging parameter
  val logger = Logger.getLogger(classOf[This].getName)
  val strSep = ",  "
  val linSep = "\n"

  // main function
  // def provide(  ): Option[ju.List[Integer]] =  {
  def provide(  ): ju.List[Integer] =  {

    var logString = linSep + params.text()
    
    // get symbols from Presentation compiler via tree
    val unit = cp.addCompilationUnit(
      params.text(),
      params.uri().toString(),
      None
    )
    cp.typeCheck(unit) // a process such as initializing
    this.logger.info(treeDescriber(unit.lastBody) + linSep)
    import cp._

    implicit val allSymbols =unit.lastBody.collect {
      case df @ cp.DefDef(_, _, _, _, _, _) =>
        df.namePos.start -> df.symbol
      case id: cp.Ident =>
        id.pos.start -> id.symbol
      case sel: cp.Select =>
        sel.pos.start -> sel.symbol
    }.toMap


    // Loop by token
    import scala.meta._
    val compilerTokens = params.text().tokenize.toOption.get
    val buffer = ListBuffer.empty[Integer]
    var currentLine = 0
    var lastNewlineOffset = 0
    var lastAbsLine = 0
    var lastCharStartOffset = 0

    for (tk <- compilerTokens) yield {

      logString += tokenDescrier(tk)

      tk match {
        case _: Token.LF =>
          currentLine += 1
          lastNewlineOffset = tk.pos.end

        case _ =>
          val tokenType = getTokenType(tk)
          val tokeModifier = getTokenModifier(tk)

          logString ++= strSep + "tokenType : " + tokenType.toString()
          logString ++= strSep + "tokMeodifier : " + tokeModifier.toString()

          //Building Semantic Token
          if (tokenType == -1 && tokeModifier == 0) { /* I want to break from match-statement */
          } else {

            // convert lines and StartChar into "relative"
            val deltaLine = currentLine - lastAbsLine
            val absStartChar = tk.pos.start - lastNewlineOffset
            val deltaStartChar =
              if (deltaLine == 0) tk.pos.start - lastCharStartOffset
              else absStartChar
            val characterSize = tk.text.size

            // update counter for next loop
            lastAbsLine = currentLine
            lastCharStartOffset = tk.pos.start

            // List to return
            buffer.addAll(
              List(
                deltaLine, // 1
                deltaStartChar, // 2
                characterSize, // 3
                tokenType, // 4
                tokeModifier // 5
              )
            )
          }

        } // end match

      } // end for

      this.logger.info(logString)

      // Just adjust return type
      buffer.toList.asJava
      // Some(buffer.toList.asJava)

  }
//    override def hover(
//       params: OffsetParams
//   ): CompletableFuture[Optional[Hover]] =
//     compilerAccess.withNonInterruptableCompiler(
//       Optional.empty[Hover](),
//       params.token
//     ) { pc =>
//       Optional.ofNullable(
//         new HoverProvider(pc.compiler(), params).hover().orNull
//       )
//     }

//   def hover(): Option[Hover] = params match {
//     case range: RangeParams =>
//       range.trimWhitespaceInRange.flatMap(hoverOffset)
//     case _ if params.isWhitespace => None
//     case _ => hoverOffset(params)
//   }


  /** This function returns 0 when capable Type is nothing. */
  def getTokenType(
      tk: scala.meta.tokens.Token
      // ,capableTypes: List[String]
  )(implicit allSymbols:Map[Int, cp.Symbol]): Integer = {
    tk match {
      case _: Token.Ident => -1 // get information from tree
            tk match {
              case id: Token.Ident =>
                val sym = allSymbols(id.pos.start)
                if (sym.isClass)
                  capableTypes.indexOf(SemanticTokenTypes.Class)
                else
                  capableTypes.indexOf(SemanticTokenTypes.Type)

              case _ =>
                capableTypes.indexOf(SemanticTokenTypes.Type)
            }

      // Alphanumeric keywords)
      // case _ :KwAbstract => capableTypes.indexOf(SemanticTokenTypes.ModifierKeyword)
      case _: Token.KwCase => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwCatch => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwClass => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwDef => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwDo => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwElse => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwEnum => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwExport => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwExtends =>
        capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwFalse => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      // case _ :Token.KwFinal => capableTypes.indexOf(SemanticTokenTypes.ModifierKeyword)
      case _: Token.KwFinally =>
        capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwFor => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwForsome =>
        capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwGiven => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwIf => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      // case _ :Token.KwImplicit => capableTypes.indexOf(SemanticTokenTypes.ModifierKeyword)
      case _: Token.KwImport => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      // case _ :Token.KwLazy => capableTypes.indexOf(SemanticTokenTypes.ModifierKeyword)
      case _: Token.KwMatch => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwMacro => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwNew => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwNull => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwObject => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwOverride =>
        capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwPackage =>
        capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwPrivate =>
        capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwProtected =>
        capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwReturn => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      // case _ :Token.KwSealed => capableTypes.indexOf(SemanticTokenTypes.ModifierKeyword)
      case _: Token.KwSuper => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwThen => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwThis => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwThrow => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwTrait => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwTrue => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwTry => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwType => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwVal => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwVar => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwWhile => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwWith => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.KwYield => capableTypes.indexOf(SemanticTokenTypes.Keyword)

      // extends Symbolic keywords
      case _: Token.Hash => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.Colon => capableTypes.indexOf(SemanticTokenTypes.Operator)
      case _: Token.Viewbound =>
        capableTypes.indexOf(SemanticTokenTypes.Operator)
      case _: Token.LeftArrow =>
        capableTypes.indexOf(SemanticTokenTypes.Operator)
      case _: Token.Subtype => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.Equals => capableTypes.indexOf(SemanticTokenTypes.Operator)
      case _: Token.RightArrow =>
        capableTypes.indexOf(SemanticTokenTypes.Operator)
      case _: Token.Supertype =>
        capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.At => capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.Underscore =>
        capableTypes.indexOf(SemanticTokenTypes.Keyword)
      case _: Token.TypeLambdaArrow =>
        capableTypes.indexOf(SemanticTokenTypes.Operator)
      case _: Token.ContextArrow =>
        capableTypes.indexOf(SemanticTokenTypes.Operator)
      // case _ :Token.MacroQuote =>
      // case _ :Token.MacroSplice =>

      // // Delimiters
      // case _ :Token.LeftParen =>
      // case _ :Token.RightParen =>
      // case _ :Token.Comma =>
      // case _ :Token.Dot =>
      // case _ :Token.Semicolon =>
      // case _ :Token.LeftBracket =>
      // case _ :Token.RightBracket =>
      // case _ :Token.LeftBrace =>
      // case _ :Token.RightBrace =>

      // Default
      case _ => -1
    }

  }

  /** This function returns 0 when capableModifier is nothing. */
  def getTokenModifier(tk: scala.meta.tokens.Token): Integer = {
    /*
      We will need the typed tree for this to check symbol flags.
     */

    val place: Double = tk match {
      case _: Token.KwAbstract =>
        capableModifiers.indexOf(SemanticTokenModifiers.Abstract)
      case _: Token.KwFinal =>
        capableModifiers.indexOf(SemanticTokenModifiers.Modification)
      case _: Token.KwImplicit =>
        capableModifiers.indexOf(SemanticTokenModifiers.Modification)
      case _: Token.KwLazy => capableModifiers.indexOf(SemanticTokenModifiers.Static)
      case _: Token.KwSealed =>
        capableModifiers.indexOf(SemanticTokenModifiers.Modification)
      case _ => -1
    }
    //  Tokens.ABSTRACT  => capableMods.indexOf(SemanticTokenModifiers.Abstract)
    var mods = 0
    if (place == -1) {
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

  var counter = 0
  def treeDescriber(t: cp.Tree): String = {
      var ret = ""
      if (counter == 0) {
          ret += "\n NodesNum: " + t.id.toString
      }
      counter += 1
      ret += linSep

      ret += ("000" + counter.toString()).takeRight(3) + "  "
      try {
          ret += "pos:" + t.pos.start.toString() + strSep + t.pos.end.toString()
      } catch { case _ => }
      ret += strSep + "Childs:" + t.children.size.toString()
      try { ret += strSep + "sym:" + t.symbol.toString() }
      catch { case _ => }

      ret += strSep + "Typ:" + t.tpe.toString()
      ret += strSep + "smry:" + t.summaryString.toString()
      ret += strSep + "NumOfFree:("
      ret += t.freeTerms.size.toString()
      ret += strSep + t.freeTypes.size.toString()
      ret += strSep + t.freeSyms.size.toString() + ")"

      // recursive
      ret += t.children.map(treeDescriber(_)).mkString("\n")

      // end
      ret

  }

  def tokenDescrier(tk:scala.meta.tokens.Token): String={
    var logString = ""
    logString += linSep
    
    logString = logString + "token : " + tk.getClass.toString.substring(29)
    logString += strSep + "start : " + tk.pos.start.toString
    logString ++= strSep + "end : " + tk.pos.end.toString
    logString += strSep + "sttLn : " + tk.pos.startLine.toString
    logString += strSep + "endLn : " + tk.pos.endLine.toString
    logString = logString + strSep + "text : " + tk.text.toString()

    logString
  }
  

}
