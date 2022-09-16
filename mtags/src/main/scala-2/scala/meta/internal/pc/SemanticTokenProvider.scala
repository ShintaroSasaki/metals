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
import scala.reflect.internal.util.SourceFile


/**
 * Corresponds to tests.SemanticHighlightLspSuite
 */
class SemanticTokenProvider  (
    protected val cp:MetalsGlobal // compiler
  , val params: VirtualFileParams
  , val capableTypes: util.List[String]
  , val capableModifiers: util.List[String]
)  {

  // alias for long notation
  def getTypeId(p:String):Int = capableTypes.indexOf(p)
  def getModifierId(p:String):Int = capableModifiers.indexOf(p)

  val logger = Logger.getLogger(classOf[This].getName)
  val strSep = ", "
  val linSep = "\n"


  import cp._

  // initialize semantic tree
  val ( root:cp.Tree, 
        source:SourceFile
      )={
        val unit = cp.addCompilationUnit(
          params.text(),
          params.uri().toString(),
          None
        )
        cp.typeCheck(unit) // initializing unit

        ( unit.lastBody,
          unit.source
        )
      }

  val nodes:Set[NodeInfo]=traverser.traverse(Set.empty[NodeInfo], root) 

  


  /** main method  */
  def provide(): ju.List[Integer] =  {

    logger.info(linSep + linSep + params.text() + linSep)
    pprint.log(root)

    var logString = linSep + params.text()
    logger.info(treeDescriber(root) + linSep)

    logString += "\n\n nodes:" + nodes.size.toString()
    logString += nodes.toArray.sortBy(_.pos.start)
                  .map(n=>treeDescriber(n.tree,false))
                  .mkString("")

    val buffer = ListBuffer.empty[Integer]
    var currentLine = 0
    var lastLine = 0
    var lastNewlineOffset = 0
    var lastCharStartOffset = 0

    // Loop by token
    import scala.meta._
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

        case _: Token.Comment => 
          val lineCount = tk.text.count(_ == '\n')
          if (lineCount != 0) {
            currentLine += lineCount
            lastNewlineOffset = tk.pos.end -
                     (tk.text.size - tk.text.lastIndexOf("\n") - 1) 
          }

        case _ =>
          val (tokenType, tokeModifier,wkLog) = getTypeAndMod(tk)

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
      // case _: Token.Ident => // in case of Ident is 

      // Alphanumeric keywords)
      case _: Token.KwAbstract => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwCase => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwCatch => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwClass => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwDef => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwDo => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwElse => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwEnum => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwExport => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwExtends =>getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwFalse => getTypeId(SemanticTokenTypes.Keyword)
      case _ :Token.KwFinal => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwFinally =>getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwFor => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwForsome =>getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwGiven => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwIf => getTypeId(SemanticTokenTypes.Keyword)
      case _ :Token.KwImplicit => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwImport => getTypeId(SemanticTokenTypes.Keyword)
      case _ :Token.KwLazy => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwMatch => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwMacro => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwNew => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwNull => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwObject => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwOverride =>getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwPackage =>getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwPrivate =>getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwProtected =>getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwReturn => getTypeId(SemanticTokenTypes.Keyword)
      case _ :Token.KwSealed => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwSuper => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwThen => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwThis => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwThrow => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwTrait => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwTrue => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwTry => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwType => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwVal => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwVar => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwWhile => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwWith => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.KwYield => getTypeId(SemanticTokenTypes.Keyword)

      // extends Symbolic keywords
      case _: Token.Hash => getTypeId(SemanticTokenTypes.Keyword)
      // case _: Token.Colon => getTypeId(SemanticTokenTypes.Operator)
      case _: Token.Viewbound =>getTypeId(SemanticTokenTypes.Operator)
      case _: Token.LeftArrow =>getTypeId(SemanticTokenTypes.Operator)
      case _: Token.Subtype => getTypeId(SemanticTokenTypes.Keyword)
      // case _: Token.Equals => getTypeId(SemanticTokenTypes.Operator)
      case _: Token.RightArrow =>getTypeId(SemanticTokenTypes.Operator)
      case _: Token.Supertype =>getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.At => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.Underscore =>getTypeId(SemanticTokenTypes.Variable)
      case _: Token.TypeLambdaArrow =>getTypeId(SemanticTokenTypes.Operator)
      case _: Token.ContextArrow =>getTypeId(SemanticTokenTypes.Operator)

      // Default
      case _ => -1
    }

  }


  class NodeInfo (
    val tree: cp.Tree,
    val pos : scala.reflect.internal.util.Position
  )


  /**
    * was written in reference to PcDocumentHighlightProvider.
    */
  object traverser {

    /**
      * gathers all nodes inside given tree.
      * The nodes have symbol.
      */
    def traverse(
        nodes:Set[NodeInfo],
        tree: cp.Tree
    ): Set[NodeInfo] = {

     tree match {
        /**
         * All indentifiers such as:
         * val a = <<b>>
         */
        case ident: cp.Ident if ident.pos.isRange
          =>
            nodes + new NodeInfo(ident, ident.pos)
        /**
         * Needed for type trees such as:
         * type A = [<<b>>]
         */
        case tpe: cp.TypeTree 
        if tpe.original != null && tpe.pos.isRange
          =>
            nodes + new NodeInfo(tpe.original, typePos(tpe))

        /**
         * All select statements such as:
         * val a = hello.<<b>>
         */
        case sel: cp.Select if sel.pos.isRange =>
          traverse(
            nodes + new NodeInfo(sel, sel.namePos),
            sel.qualifier
          )
        /* all definitions:
          * def <<foo>> = ???
          * class <<Foo>> = ???
          * etc.
          */
        case df: cp.MemberDef if df.pos.isRange =>
          (annotationChildren(df) ++ df.children)
          .foldLeft(
            nodes + new NodeInfo(df, df.namePos)
          )(traverse(_, _))
        /* Named parameters, since they don't show up in typed tree:
          * foo(<<name>> = "abc")
          * User(<<name>> = "abc")
          * etc.
          */
        case appl: cp.Apply =>
          val named = appl.args
            .flatMap { arg =>
              namedArgCache.get(arg.pos.start)
            }
            .collectFirst {
              case cp.AssignOrNamedArg(i @ cp.Ident(name), _)
              =>
                new NodeInfo(i,i.pos)
            }

          tree.children.foldLeft(nodes ++ named)(traverse(_, _))

        /**
         * We don't automatically traverser types like:
         * val opt: Option[<<String>>] =
         */
        case tpe: cp.TypeTree if tpe.original != null =>
          tpe.original.children.foldLeft(nodes)(traverse(_, _))
        /**
         * Some type trees don't have symbols attached such as:
         * type A = List[_ <: <<Iterable>>[Int]]
         */
        case id: cp.Ident if id.symbol == NoSymbol =>
          fallbackSymbol(id.name, id.pos) match {
            case Some(sym) => nodes + new NodeInfo(id, id.pos)
            case _ => nodes
          }

        case df: cp.MemberDef =>
          (tree.children ++ annotationChildren(df))
            .foldLeft(nodes)(traverse(_, _))
        case _ =>
          if (tree==null)null
          else tree.children.foldLeft(nodes)(traverse(_, _))
      }
    }

    def fallbackSymbol(name: Name, pos: Position) = {
      val context = doLocateImportContext(pos)
      context.lookupSymbol(name, sym => sym.isType) match {
        case LookupSucceeded(_, symbol) =>
          Some(symbol)
        case _ => None
      }
    }
    private def annotationChildren(mdef: cp.MemberDef): List[cp.Tree] = {
      mdef.mods.annotations match {
        case Nil if mdef.symbol != null =>
          // After typechecking, annotations are moved from the modifiers
          // to the annotation on the symbol of the annotatee.
          mdef.symbol.annotations.map(_.original)
        case anns => anns
      }
    }

    private def typePos(tpe: cp.TypeTree) = {
      tpe.original match {
        case cp.AppliedTypeTree(tpt, _) => tpt.pos
        case sel: cp.Select => sel.namePos
        case _ => tpe.pos
      }
    }
    // We need to collect named params since they will not show on fully typed tree
    lazy val namedArgCache = {
      val parsedTree = cp.parseTree(source)
      parsedTree.collect { case arg @ cp.AssignOrNamedArg(_, rhs) =>
        rhs.pos.start -> arg
      }.toMap
    }
  }

  def pickFromTraversed(tk:scala.meta.tokens.Token):List[cp.Tree] ={
      val buffer = ListBuffer.empty[cp.Tree]
        
      for (node <- nodes){
        if( node.pos.start ==tk.pos.start &&
           node.pos.end ==tk.pos.end 
        ) buffer.addAll(List(node.tree))
      }

      buffer.toList
  }


  /**
    * returns (SemanticTokenType, SemanticTokenModifier) of @param tk
    */
  private def getTypeAndMod(tk:scala.meta.tokens.Token):(Int, Int,String) ={

    // whether token is identifier or not
    tk match {
      case _: Token.Ident => // continue this method
      case _ =>
         //Non-Ident has no modifier.
         return (typeOfNonIdentToken(tk), 0, strSep + "Non-Ident")
    }

    var logString = ""
    logString += linSep + linSep  + "  Start:Ident Part getSemanticTypeAndMod"
    logString += linSep + "  " + tk.name

    val nodeList = pickFromTraversed(tk)
    val node = if (nodeList.size ==0) null else nodeList(0)
    if( node == null) return (-1,0,strSep + "Node-Nothing") // break

    val sym = node.symbol
    import sym._

    // get type
    // see symbol.keystring about following logic.
    val typ = 
        if (isValueParameter ) getTypeId(SemanticTokenTypes.Parameter)
        // method
        else if ( isSourceMethod ) {
          if (isGetter || isSetter ) getTypeId(SemanticTokenTypes.Variable)
          else getTypeId(SemanticTokenTypes.Method)
        }
        // var 
        else if (isVariable)  getTypeId(SemanticTokenTypes.Variable)
        // val
        else if ((isTerm && (!isParameter || isParamAccessor))) 
          getTypeId(SemanticTokenTypes.Variable)
        // class
        else if (isClass) getTypeId(SemanticTokenTypes.Class) 
        // type
        else if (isType && !isParameter) getTypeId(SemanticTokenTypes.Type) //type
        // trait
        else if (isTrait) getTypeId(SemanticTokenTypes.Interface)
        // object (treat  as class)
        else if (isModule) getTypeId(SemanticTokenTypes.Class) 
        // package
        else if (hasPackageFlag) getTypeId(SemanticTokenTypes.Namespace)
        else  -1


    //get moodifier
    var mod:Int = 0
    def addPwrToMod(place:Int)={
      if (place != -1) mod += scala.math.pow(2, place).toInt
    }

    if (isAbstract)
      addPwrToMod(getModifierId(SemanticTokenModifiers.Abstract))

    // treat val as ReadOnly
    if ((isTerm && (!isParameter || isParamAccessor))) 
      addPwrToMod(getModifierId(SemanticTokenModifiers.Readonly))

    //return
    (typ,mod,logString)
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

  var counter = 0
  /** makes string to logging tree construction. */
  def treeDescriber(t: cp.Tree, doRecurse:Boolean=true): String = {
      if (t == null)return  "  " + "Null Tree"

      var ret = ""
      if (counter == 0 && doRecurse) ret += "\nNodesNum: " + t.id.toString

      counter += 1
      ret += linSep
      ret +=  "  " + ("000" + counter.toString()).takeRight(3) + "  "

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
      ret += strSep + "-> TreeCls:" + t.getClass.getName.substring(29)


      //symbol
      try {
        val sym = t.symbol

        def SymtoStrtype: String = {
          import sym._
          val simplifyNames = !settings.isDebug
          if (isPackageObjectOrClass && simplifyNames) "1-" // s"package object ${owner.decodedName}"
          else {
            val kind = kindString
            val _name: String =
              if (hasMeaninglessName) "2-" //owner.decodedName + idString
              else if (simplifyNames && (kind == "variable" || kind == "value"))
                "3-" // unexpandedName.getterName.decode.toString // TODO: make condition less gross?
              else "4-" // nameString

            //kind + " " + _name
            _name
          }
        }
        ret += strSep + "sym:" + SymtoStrtype + sym.toString
        ret += strSep + "keyStr:" + sym.keyString
        ret += strSep + "\n  name:" + sym.nameString
        ret += strSep + "SymCls:" + sym.getClass.getName.substring(31)
        ret += strSep + "SymKnd:" + sym.accurateKindString

      } catch { case _ => return ""}


     val wkTreeType  = t match {
            case _:Ident => "Ident"
            case _:Select => "Select"
            case mem:MemberDef => "MemberDef " + mem.keyword
            case _:DefTree => "DefTree" //continue
            case _:ValDef => "ValDef" //continue
            case _:Assign => "Assign" //continue
            case appl: Apply => "Assign" //continue
            case _ => ""
          }
      ret += strSep + "TreeTyp:" + wkTreeType
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

    val wkList = pickFromTraversed(tk)
    counter=0
    logString +=wkList.map(treeDescriber(_,false)).mkString("")

    logString
  }


}
