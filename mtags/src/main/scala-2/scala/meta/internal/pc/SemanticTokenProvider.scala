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
import scala.meta.internal.semanticdb.SymbolInformation
import com.sourcegraph.semanticdb_javac.Semanticdb.Tree


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

    logger.info(linSep + linSep + params.text() + linSep)
    pprint.log(root)

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

      if (tk.getClass.toString.substring(29)!="$Space"
      && tk.getClass.toString.substring(29)!="$LF"
      ){
        logString += tokenDescriber(tk)
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

          // logString ++= strSep + "tokenType : " + tokenType.toString()
          // logString ++= strSep + "tokMeodifier : " + tokeModifier.toString()
          // logString ++= wkLog

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
      // case _: Token.Colon => getTid(SemanticTokenTypes.Operator)
      case _: Token.Viewbound =>getTid(SemanticTokenTypes.Operator)
      case _: Token.LeftArrow =>getTid(SemanticTokenTypes.Operator)
      case _: Token.Subtype => getTid(SemanticTokenTypes.Keyword)
      // case _: Token.Equals => getTid(SemanticTokenTypes.Operator)
      case _: Token.RightArrow =>getTid(SemanticTokenTypes.Operator)
      case _: Token.Supertype =>getTid(SemanticTokenTypes.Keyword)
      case _: Token.At => getTid(SemanticTokenTypes.Keyword)
      // case _: Token.Underscore =>getTid(SemanticTokenTypes.Keyword)
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

  def getIdentNodeList(t:cp.Tree, tk:scala.meta.tokens.Token):List[cp.Tree]={

    def doRecursion():List[cp.Tree] ={
      if (t.children.size == 0) null
      else {
        t.children.map(getIdentNodeList(_,tk))
        .filter(_!=null)
        .flatten
        // if (result.size == 0) null else result
      }
    }

    try {
      val wkNamePos = namePos(t)

      if (
        wkNamePos.start == tk.pos.start
        && wkNamePos.end == tk.pos.end
        && t.symbol.name.toString == tk.text
      ) List(t) /* return */ else doRecursion()
    }catch{
      // e.g. hasSymbol==false, NoPosition==ture, and so on
      case _:Exception => doRecursion()
    }

  }
        def traverse(
            highlights: Set[cp.Tree],
            tree: Tree
        ): Set[cp.Tree] = {
          tree match {
            /**
             * All indentifiers such as:
             * val a = <<b>>
             */
            case ident: cp.Ident if sought(ident.symbol) && ident.pos.isRange =>
              highlights + new DocumentHighlight(
                ident.pos.toLSP,
                DocumentHighlightKind.Read
              )
            /**
             * Needed for type trees such as:
             * type A = [<<b>>]
             */
            case tpe: TypeTree
                if tpe.original != null && sought(tpe.original.symbol) &&
                  tpe.pos.isRange =>
              highlights + new DocumentHighlight(
                typePos(tpe).toLSP,
                DocumentHighlightKind.Read
              )
            /**
             * All select statements such as:
             * val a = hello.<<b>>
             */
            case sel: Select if sought(sel.symbol) && sel.pos.isRange =>
              traverse(
                highlights + new DocumentHighlight(
                  sel.namePos.toLSP,
                  DocumentHighlightKind.Read
                ),
                sel.qualifier
              )
            /* all definitions:
             * def <<foo>> = ???
             * class <<Foo>> = ???
             * etc.
             */
            case df: Tree.MemberDef
                if sought(
                  df.symbol
                ) && df.pos.isRange =>
              (annotationChildren(df) ++ df.children).foldLeft(
                highlights + new DocumentHighlight(
                  df.namePos.toLSP,
                  DocumentHighlightKind.Write
                )
              )(traverse(_, _))
            /* Named parameters, since they don't show up in typed tree:
             * foo(<<name>> = "abc")
             * User(<<name>> = "abc")
             * etc.
             */
            case appl: Apply
                if owners(appl.symbol) || owners(appl.symbol.owner) =>
              val named = appl.args
                .flatMap { arg =>
                  namedArgCache.get(arg.pos.start)
                }
                .collectFirst {
                  case AssignOrNamedArg(i @ Ident(name), _)
                      if (sought.exists(sym => sym.name == name)) =>
                    new DocumentHighlight(
                      i.pos.toLSP,
                      DocumentHighlightKind.Read
                    )
                }
              tree.children.foldLeft(highlights ++ named)(traverse(_, _))

            /**
             * We don't automatically traverser types like:
             * val opt: Option[<<String>>] =
             */
            case tpe: TypeTree if tpe.original != null =>
              tpe.original.children.foldLeft(highlights)(traverse(_, _))
            /**
             * Some type trees don't have symbols attached such as:
             * type A = List[_ <: <<Iterable>>[Int]]
             */
            case id: Ident
                if id.symbol == NoSymbol && soughtNames.exists(_ == id.name) =>
              fallbackSymbol(id.name, id.pos) match {
                case Some(sym) if sought(sym) =>
                  highlights + new DocumentHighlight(
                    id.pos.toLSP,
                    DocumentHighlightKind.Read
                  )
                case _ => highlights
              }

            case df: MemberDef =>
              (tree.children ++ annotationChildren(df))
                .foldLeft(highlights)(traverse(_, _))
            case _ =>
              tree.children.foldLeft(highlights)(traverse(_, _))
          }
        }
        val all = traverse(Set.empty[cp.Tree], unit.lastBody)

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
    val sym = node.symbol
    val typ =  if (sym.isValueParameter ) getTid(SemanticTokenTypes.Parameter)
      // else if  (node.symbol.isMethod ) getTid(SemanticTokenTypes.Method)
      else node.symbol.keyString match {
          case kind.kDef => getTid(SemanticTokenTypes.Method)
              // if (sym.isGetter || sym.isSetter ) getTid(SemanticTokenTypes.Variable)
              // else getTid(SemanticTokenTypes.Method)

          case kind.kVal => getTid(SemanticTokenTypes.Variable)
          case kind.kVar => getTid(SemanticTokenTypes.Variable)
          case kind.KClass => getTid(SemanticTokenTypes.Class)
          case kind.kType => getTid(SemanticTokenTypes.Type)
          case kind.kTrait => getTid(SemanticTokenTypes.Interface)
          case kind.kObject =>  getTid(SemanticTokenTypes.Class) // treat object as class
          case kind.kPackage => getTid(SemanticTokenTypes.Namespace)
          case _ => -1
      }         

      // if (node.symbol.isValueParameter ) getTid(SemanticTokenTypes.Parameter)
      // else keyword(node) match {
      //     case kind.kType => getTid(SemanticTokenTypes.Type)
      //     case kind.KClass => getTid(SemanticTokenTypes.Class)
      //     case kind.kTrait => getTid(SemanticTokenTypes.Interface)
      //     case kind.kObject =>  getTid(SemanticTokenTypes.Class)
      //     case kind.kPackage => getTid(SemanticTokenTypes.Namespace)
      //     case kind.kVal => getTid(SemanticTokenTypes.Variable)
      //     case kind.kVar => getTid(SemanticTokenTypes.Variable)
      //     case kind.kDef => getTid(SemanticTokenTypes.Method)
      //     case _ => -1
      // }         

    //get moodifier
    var mod:Int = 0
    def addPwrToMod(place:Int)={
      if (place != -1) mod += scala.math.pow(2, place).toInt 
    }

    if (node.symbol.isAbstract) addPwrToMod(getMid(SemanticTokenModifiers.Abstract))
    if (node.symbol.keyString==kind.kVal) addPwrToMod(
          getMid(SemanticTokenModifiers.Readonly))

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
  // import scala.reflect.internal.ModifierFlags._
  import scala.reflect.internal.Flags._
  private def keyword(t:cp.Tree):String = {
    t match {
      case TypeDef(_, _, _, _)      => kind.kType
      case ClassDef(mods, _, _, _)  => if (mods hasFlag TRAIT) kind.kTrait
                                          else kind.KClass
      case DefDef(mods, _, _, _, _, _) => 
            if (mods hasFlag METHOD) kind.kDef
            else if  (mods hasFlag MUTABLE)  kind.kVar
            else  kind.kVal
            
      case ModuleDef(_, _, _)       => kind.kObject
      case PackageDef(_, _)         => kind.kPackage
      case ValDef(mods, _, _, _)    => if (mods hasFlag MUTABLE)  kind.kVar
                                          else kind.kVal
      case _ => kind.kOther
    }
  }

  


  var counter = 0
  /** makes string to logging tree construction. */
  def treeDescriber(t: cp.Tree, doRecurse:Boolean=true): String = {
      if (t == null)return  "  " + "Null Tree"
    //  t match {
    //   case _:Ident|_:Select|_:MemberDef|_:DefTree => //continue 
    //   case _ => return ""
    //  }

      // if(!t.hasSymbolField)return "" 

      var ret = ""
      if (counter == 0 && doRecurse) ret += "\nNodesNum: " + t.id.toString

      counter += 1
      ret += linSep +linSep + "  " + ("000" + counter.toString()).takeRight(3) + "  "

      //Position
      try {
          // ret += "pos(stt,end,point):(" + t.pos.start.toString() + strSep + t.pos.end.toString()
          // ret += strSep + t.pos.point.toString()
          // ret += ")"
          val wkNamePos= namePos(t)
          // ret += strSep
          ret += "namePos:(" + wkNamePos.start.toString()
          // ret += "," + t.symbol.fullName.length()
          ret += "," + wkNamePos.end.toString() +")"
      } catch { case _ => }
      // ret += strSep + "Childs:" + t.children.size.toString()
      // ret += strSep + "smry:" + t.summaryString.toString()

      // val wkStr = t match {
      //   case cp.Literal(const)     => "Liter"
      //   case cp.Ident(name)        => "Ident"
      //   case cp.Select(qual, name) => "Select(%s, %s)".format(qual.summaryString, name.decode)
      //   case t: cp.NameTree        => "NameTree"
      //   case t                  => "ShrtClass " +
      //     t.shortClass + (
      //       if (t.symbol != null && t.symbol != cp.NoSymbol)
      //         "(" + t.symbol + ")"
      //       else ""
      //     )

      //   }
      // ret += strSep + "Extracted:" + wkStr
      ret += strSep + "\n   -> TreeCls : " + t.getClass.getName.substring(29)


      //symbol
      try {
        val sym = t.symbol

        def SymtoStrtype: String = {
          import sym._
          val simplifyNames = !settings.isDebug
          if (isPackageObjectOrClass && simplifyNames) "1 - " // s"package object ${owner.decodedName}"
          else {
            val kind = kindString
            val _name: String =
              if (hasMeaninglessName) "2 - " //owner.decodedName + idString
              else if (simplifyNames && (kind == "variable" || kind == "value")) 
                "3 - " // unexpandedName.getterName.decode.toString // TODO: make condition less gross?
              else "4 - " // nameString

            //kind + " " + _name
            _name
          }
        }
        ret += strSep + " -> symtoStr:" + SymtoStrtype + sym.toString
        ret += strSep + "  keyStr:" + sym.keyString

        // symKd = sym.symbolKind match {
        //   case  kd :SymbolKind(accurate, _, _)  => accurate
        //   case _=>""
        // }
        ret += strSep + " SymKnd:" + sym.accurateKindString
        if (sym.isClass){
          // ret += strSep + "\n   -> fullnm:" + sym.fullName
          ret += strSep + "\n   -> name : " + sym.nameString
          // ret += strSep + "keyStr:" + sym.keyString
          // ret += strSep + "isTerm:" + t.isTerm.toString()
          ret += strSep + "\n   -> SymCls : " + sym.getClass.getName.substring(31)
          }

      } catch { case _ => return ""}


     val wkTreeType  = t match {
            case _:Ident => "Ident"
            case _:Select => "Select"
            case _:MemberDef => "MemberDef " + keyword(t).toString()
            case _:DefTree => "DefTree" //continue 
            case _:ValDef => "ValDef" //continue 
            case _:Assign => "Assign" //continue 
            case appl: Apply => "Assign" //continue 
            case _ => ""
          }
      ret += strSep + ", TreeType:" + wkTreeType
      // ret += strSep + "\n   -> keyword:"+ keyword(t).toString()

      // recursive
      if (doRecurse) ret += t.children
        .map(treeDescriber(_,true)).mkString("\n")

      // end
      ret + linSep

  }

  def tokenDescriber(tk:scala.meta.tokens.Token): String={
    
    var logString = ""
    logString += linSep

    logString += "token: " + tk.getClass.toString.substring(29)
    logString += strSep + "text: " + tk.text.toString()
    logString += strSep + "stt,end:(" + tk.pos.start.toString
    logString += strSep  + tk.pos.end.toString + ")"
    logString += strSep + "LnStt,End:(" + tk.pos.startLine.toString
    logString += "," + tk.pos.endLine.toString +")"

    counter=0
    logString +=getIdentNodeList(root,tk).map(treeDescriber(_,false)).mkString("")
    // logString +=treeDescriber(getIdentNode(root,tk))

    logString
  }


}
