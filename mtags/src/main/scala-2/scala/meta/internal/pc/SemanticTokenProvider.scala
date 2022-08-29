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
import org.scalameta.adt.none


/**
 * Corresponds to tests.SemanticHighlightLspSuite
 */
class SemanticTokenProvider  (
    protected val cp:MetalsGlobal // compiler
  , val params: VirtualFileParams
  , val capableTypes: util.List[String]
  , val capableModifiers: util.List[String]
)  {

  // initialize semantic tree
  var root:cp.Tree ={
      val unit = cp.addCompilationUnit(
        params.text(),
        params.uri().toString(),
        None
      )
      cp.typeCheck(unit) // initializing unit
      unit.lastBody
  }

  // alias for long notation
  def getTid(p:String):Int = capableTypes.indexOf(p) 
  def getMid(p:String):Int = capableModifiers.indexOf(p) 

  // logging parameter
  val logger = Logger.getLogger(classOf[This].getName)
  val strSep = ", "
  val linSep = "\n"

  /** main method  */
  def provide(): ju.List[Integer] =  {

    var logString = linSep + params.text()
    logger.info(treeDescriber(root) + linSep)

    // Loop by token
    import scala.meta._
    val buffer = ListBuffer.empty[Integer]
    var currentLine = 0
    var lastLine = 0
    var lastNewlineOffset = 0
    var lastCharStartOffset = 0

    for (tk <- params.text().tokenize.toOption.get) yield {

      if (tk.getClass.toString.substring(29)!="$Space"){
        logString += tokenDescrier(tk)
      }

      tk match {
        case _: Token.LF =>
          currentLine += 1
          lastNewlineOffset = tk.pos.end

        case _: Token.Space =>
          //pass

        case _ =>
          // val tokenType = getTokenType(tk)
          // val tokeModifier = getTokenModifier(tk)
          val (tokenType, tokeModifier,wkLog) = getSemanticTypeAndMod(tk)

          logString ++= strSep + "tokenType : " + tokenType.toString()
          logString ++= strSep + "tokMeodifier : " + tokeModifier.toString()
          logString ++= wkLog

          //Building Semantic Token
          if (tokenType == -1 && tokeModifier == 0) {
            /* I want to break from match-statement */
          } else {

            val characterSize = tk.text.size
            val absStartChar = tk.pos.start - lastNewlineOffset

            // convert currentline and StartChar into "relative"
            val deltaLine = currentLine - lastLine
            val deltaStartChar =
              if (deltaLine == 0) tk.pos.start - lastCharStartOffset
              else absStartChar

            // update controller for next loop
            lastLine = currentLine
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

  }

  /** This function returns 0 when capable Type is nothing. */
  private def typeOfNonIdentToken(
      tk: scala.meta.tokens.Token
  ): Integer = {
    tk match {
      // case _: Token.Ident => // tk shouldn't be Ident 

      // Alphanumeric keywords)
      case _: Token.KwAbstract => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwCase => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwCatch => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwClass => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwDef => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwDo => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwElse => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwEnum => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwExport => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwExtends =>getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwFalse => getTid(SemanticTokenTypes.Keyword)
      case _ :Token.KwFinal => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwFinally =>getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwFor => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwForsome =>getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwGiven => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwIf => getTid(SemanticTokenTypes.Keyword)
      case _ :Token.KwImplicit => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwImport => getTid(SemanticTokenTypes.Keyword)
      case _ :Token.KwLazy => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwMatch => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwMacro => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwNew => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwNull => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwObject => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwOverride =>getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwPackage =>getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwPrivate =>getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwProtected =>getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwReturn => getTid(SemanticTokenTypes.Keyword)
      case _ :Token.KwSealed => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwSuper => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwThen => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwThis => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwThrow => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwTrait => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwTrue => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwTry => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwType => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwVal => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwVar => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwWhile => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwWith => getTid(SemanticTokenTypes.Keyword)
      case _: Token.KwYield => getTid(SemanticTokenTypes.Keyword)

      // extends Symbolic keywords
      case _: Token.Hash => getTid(SemanticTokenTypes.Keyword)
      case _: Token.Colon => getTid(SemanticTokenTypes.Operator)
      case _: Token.Viewbound =>getTid(SemanticTokenTypes.Operator)
      case _: Token.LeftArrow =>getTid(SemanticTokenTypes.Operator)
      case _: Token.Subtype => getTid(SemanticTokenTypes.Keyword)
      case _: Token.Equals => getTid(SemanticTokenTypes.Operator)
      case _: Token.RightArrow =>getTid(SemanticTokenTypes.Operator)
      case _: Token.Supertype =>getTid(SemanticTokenTypes.Keyword)
      case _: Token.At => getTid(SemanticTokenTypes.Keyword)
      case _: Token.Underscore =>getTid(SemanticTokenTypes.Keyword)
      case _: Token.TypeLambdaArrow =>getTid(SemanticTokenTypes.Operator)
      case _: Token.ContextArrow =>getTid(SemanticTokenTypes.Operator)
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

  /**
    * get node which corresponds to @param tk from @param t 
    * # @param tk is assumed as Identifier
    */
  def getIdentNode(t:cp.Tree, tk:scala.meta.tokens.Token):cp.Tree={

    def doRecursion():cp.Tree ={
      if (t.children.size == 0) null
      else {
        val result = t.children.map(getIdentNode(_,tk)).filter(_!=null)
        if (result.size == 0) null else result(0)
      }
    }

    try {
      val wkNamePos = namePos(t)

      if (
        wkNamePos.start == tk.pos.start
        && wkNamePos.end == tk.pos.end
        && t.symbol.name.toString == tk.text
      ) t /* return */ else doRecursion()
    }catch{
      // e.g. hasSymbol==false, NoPosition==ture, and so on
      case _:Exception => doRecursion()
    }

  }

  /** This function returns 0 when capableModifier is nothing. */
  /**
    * returns (SemanticTokenType, SemanticTokenModifier) of @param tk
    */
  private def getSemanticTypeAndMod(tk:scala.meta.tokens.Token):(Int, Int,String) ={

    var logString = ""
    // whether token is identifier or not
    tk match {
      case _: Token.Ident => // continue this method 
      case _ => 
         //Non-Ident has no modifier.
         return (typeOfNonIdentToken(tk), 0, strSep + "Non-Ident") 
    }
    
    logString += linSep + linSep  + "  Start:Ident Part getSemanticTypeAndMod" 
    logString += linSep + "  " + tk.name
    
    //get node from semantic tree
    val node = getIdentNode(root, tk)
    if( node == null) return (-1,0,strSep + "Node-Nothing") // break

    logString += linSep + "  ** Got node" 
    logString += linSep + "  ** Keyword:" +keyword(node)

    //get type
    val typ = 
      if (node.symbol.isValueParameter ) getTid(SemanticTokenTypes.Parameter)
      else keyword(node) match {
          case kind.kType => getTid(SemanticTokenTypes.Type)
          case kind.KClass => getTid(SemanticTokenTypes.Class)
          case kind.kTrait => getTid(SemanticTokenTypes.Interface)
          case kind.kObject =>  getTid(SemanticTokenTypes.Class)
          case kind.kPackage => getTid(SemanticTokenTypes.Namespace)
          case kind.kVal => getTid(SemanticTokenTypes.Variable)
          case kind.kVar => getTid(SemanticTokenTypes.Variable)
          case kind.kDef => getTid(SemanticTokenTypes.Method)
          case _ => -1
      }         

    //get moodifier
    var mod:Int = 0
    def addPwrToMod(place:Int)={
      if (place != -1) mod += scala.math.pow(2, place).toInt 
    }

    if (node.symbol.isAbstract) addPwrToMod(getMid(SemanticTokenModifiers.Abstract))
    if (keyword(node)==kind.kVal) addPwrToMod(getMid(SemanticTokenModifiers.Readonly))
    //case _: Token.KwFinal =>getMid(SemanticTokenModifiers.Modification)


    //return
    return (typ,mod,logString) 
  }


  import scala.reflect.internal.util.Position
  private def namePos(t:cp.Tree): Position = {
    try {
      val wkStart = t.pos.point
      val wkEnd = wkStart + t.symbol.name.length() //- 1
      Position.range(t.pos.source, wkStart, wkStart, wkEnd)
    }catch {
      case _ => null
    }
  }

  private object kind extends Enumeration {
    val kType = "type"
    val KClass = "class"
    val kTrait = "trait"
    val kObject = "object"
    val kPackage = "package"
    val kVal = "val"
    val kVar = "var"
    val kDef = "def"
    val kOther =""
  }

  import cp._
  import scala.reflect.internal.ModifierFlags._
  private def keyword(t:cp.Tree):String = {
    t match {
      case TypeDef(_, _, _, _)      => kind.kType
      case ClassDef(mods, _, _, _)  => if (mods hasFlag TRAIT) kind.kTrait
                                          else kind.KClass
      case DefDef(_, _, _, _, _, _) => kind.kDef
      case ModuleDef(_, _, _)       => kind.kObject
      case PackageDef(_, _)         => kind.kPackage
      case ValDef(mods, _, _, _)    => if (mods hasFlag MUTABLE)  kind.kVar
                                          else kind.kVal
      case _ => kind.kOther
    }
  }

  


  var counter = 0
  /** makes string to logging tree construction. */
  def treeDescriber(t: cp.Tree): String = {
      var ret = ""
      if (counter == 0) ret += "\nNodesNum: " + t.id.toString

      counter += 1
      ret += linSep + ("000" + counter.toString()).takeRight(3) + "  "

      //Position
      try {
          ret += "pos(stt,end,point):(" + t.pos.start.toString() + strSep + t.pos.end.toString()
          ret += strSep + t.pos.point.toString()
          ret += ")"
          val wkNamePos= namePos(t)
          ret += strSep + "namePos:(" + wkNamePos.start.toString()
          // ret += "," + t.symbol.fullName.length()
          ret += "," + wkNamePos.end.toString() +")"
      } catch { case _ => }
      ret += strSep + "Childs:" + t.children.size.toString()

      ret += strSep + "smry:" + t.summaryString.toString()

      val wkStr = t match {
        case cp.Literal(const)     => "Liter"
        case cp.Ident(name)        => "Ident"
        case cp.Select(qual, name) => "Select(%s, %s)".format(qual.summaryString, name.decode)
        case t: cp.NameTree        => "NameTree"
        case t                  => "ShrtClass " +
          t.shortClass + (
            if (t.symbol != null && t.symbol != cp.NoSymbol)
              "(" + t.symbol + ")"
            else ""
          )

        }
      ret += strSep + "Extracted:" + wkStr
      ret += strSep + "\n   -> TreeCls : " + t.getClass.getName

      //symbol
      try {
        val sym = t.symbol
        ret += strSep + "sym:" + sym.toString()
        ret += strSep + "isClass:" + sym.isClass.toString()
        if (sym.isClass){
          ret += strSep + "\n   -> fullnm:" + sym.fullName
          ret += strSep + "\n   -> name : " + sym.nameString
          // ret += strSep + "keyStr:" + sym.keyString
          // ret += strSep + "isTerm:" + t.isTerm.toString()
          ret += strSep + "\n   -> SymCls : " + sym.getClass.getName
          }

      } catch { case _ => }

      ret += strSep + "\n   -> keyword:"+ keyword(t).toString()

      // recursive
      ret += t.children.map(treeDescriber(_)).mkString("\n")

      // end
      ret

  }

  def tokenDescrier(tk:scala.meta.tokens.Token): String={
    var logString = ""
    logString += linSep

    logString += "token: " + tk.getClass.toString.substring(29)
    logString += strSep + "text: " + tk.text.toString()
    logString += strSep + "stt,end:(" + tk.pos.start.toString
    logString += strSep  + tk.pos.end.toString + ")"
    logString += strSep + "sttLn: " + tk.pos.startLine.toString
    logString += strSep + "endLn: " + tk.pos.endLine.toString

    logString
  }


}
