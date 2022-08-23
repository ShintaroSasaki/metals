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


/**
 * Corresponds to tests.SemanticHighlightLspSuite
 */
class SemanticTokenProvider  (
    protected val cp:MetalsGlobal //  protected to avoid compile error
  , val params: VirtualFileParams
  , val capableTypes: util.List[String]
  , val capableModifiers: util.List[String]
)  {

  var tr:cp.Tree ={
      val unit = cp.addCompilationUnit(
        params.text(),
        params.uri().toString(),
        None
      )
      cp.typeCheck(unit) // initializing unit
      unit.lastBody
  }

  // logging parameter
  val logger = Logger.getLogger(classOf[This].getName)
  val strSep = ",  "
  val linSep = "\n"


  /** main method  */ 
  def provide(): ju.List[Integer] =  {

    var logString = linSep + params.text()
    this.logger.info(treeDescriber(tr) + linSep)

    // Loop by token
    import scala.meta._
    val buffer = ListBuffer.empty[Integer]
    var currentLine = 0
    var lasLine = 0
    var lastNewlineOffset = 0
    var lastCharStartOffset = 0

    for (tk <- params.text().tokenize.toOption.get) yield {

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
          if (tokenType == -1 && tokeModifier == 0) {
            /* I want to break from match-statement */
          } else {

            val characterSize = tk.text.size
            val absStartChar = tk.pos.start - lastNewlineOffset

            // convert currentline and StartChar into "relative"
            val deltaLine = currentLine - lasLine
            val deltaStartChar =
              if (deltaLine == 0) tk.pos.start - lastCharStartOffset
              else absStartChar

            // update controller for next loop
            lasLine = currentLine
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

      buffer.toList.asJava
      // Some(buffer.toList.asJava)

  }

  /** This function returns 0 when capable Type is nothing. */
  def getTokenType(
      tk: scala.meta.tokens.Token
      // ,capableTypes: List[String]
  ): Integer = {
    tk match {
      case _: Token.Ident => getIdentAttr(tr, tk.pos.start)

            // if (sym.isClass)
            //   capableTypes.indexOf(SemanticTokenTypes.Class)
            // else
            //   capableTypes.indexOf(SemanticTokenTypes.Type)

      // Alphanumeric keywords)
      case _: Token.KwAbstract => capableTypes.indexOf(SemanticTokenTypes.Keyword)
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

    mods.toInt
  } // end

  /**
    * looks up Ident symbol with @startPos in @t.
    * And returns the TokenType.
    */
  def getIdentAttr(t:cp.Tree, startPos: Int) :Int ={

    def getInd(p:String):Int =capableTypes.indexOf(p) //Alias
    def doRecursion():Int ={
          if (t.children.size == 0) -1
          else t.children.map(getIdentAttr(_,startPos))
                .max(Ordering[Int])
    }

    try {
      if (t.pos.start == startPos ) {
        if (t.symbol.isValueParameter ) getInd(SemanticTokenTypes.Parameter)
        else if (t.symbol.isClass) getInd(SemanticTokenTypes.Class)
        else doRecursion()
      }
      else doRecursion()
    }catch{
      // when hasSymbol==false , NoPosition==ture, and so on
      case _:Exception => doRecursion()
    }

    // import cp._
    // implicit val allSymbols =unit.lastBody.collect {
    //   case df @ cp.DefDef(_, _, _, _, _, _) =>
    //     df.namePos.start -> df.symbol
    //   case id: cp.Ident =>
    //     id.pos.start -> id.symbol
    //   case sel: cp.Select =>
    //     sel.pos.start -> sel.symbol
    // }.toMap

  }

  // def alterChildren(a:cp.Tree): List[cp.Tree] = {
  //     var builder: ListBuffer[cp.Tree] = null
  //     def subtrees(x: Any): Unit = x match {
  //       case b @ cp.EmptyTree =>
  //         logger.info("\n Empty:" + b.toString() + "\n")
  //         if (builder eq null) builder = new ListBuffer[cp.Tree]
  //         builder += cp.EmptyTree
  //       case t: cp.Tree =>
  //         if (builder eq null) builder = new ListBuffer[cp.Tree]
  //         builder += t
  //       case xs: List[_] => xs foreach subtrees
  //       case _ =>
  //     }
  //     a foreach subtrees
  //     if (builder eq null) Nil else builder.result()
  //   }


  var counter = 0
  /** makes string to logging tree construction. */
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
      
      try {
        ret += strSep + "sym:" + t.symbol.toString() 
        ret += strSep + "isClass:" + t.symbol.isClass.toString()
      } catch { case _ => }

      // ret += strSep + "Typ:" + t.tpe.toString()
      ret += strSep + "smry:" + t.summaryString.toString()

      // ret += strSep + "NumOfFree:("
      // ret += t.freeTerms.size.toString()
      // ret += strSep + t.freeTypes.size.toString()
      // ret += strSep + t.freeSyms.size.toString() + ")"

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
