package scala.meta.internal.pc
import java.util
import java.util.logging.Logger
import java.{util => ju}

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.Position

import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.pc.VirtualFileParams
import scala.meta.tokens._
import scala.meta.internal.pc.SemanticTokenCapability._
import org.checkerframework.common.returnsreceiver.qual.This
import org.eclipse.lsp4j.SemanticTokenModifiers
import org.eclipse.lsp4j.SemanticTokenTypes
import scala.meta.pc.DefinitionResult

/**
 * Corresponds to tests.SemanticHighlightLspSuite
 */
final class SemanticTokenProvider(
    protected val cp: MetalsGlobal // compiler
    ,
    val params: VirtualFileParams,
) {
  val capableTypes= TokenTypes
  val capableModifiers = TokenModifiers

  // Alias for long notation
  val getTypeId :Map[String,Int] = capableTypes.zipWithIndex.toMap
  val getModifierId :Map[String,Int] = capableModifiers.zipWithIndex.toMap

  // Initialize Tree
  import cp._
  val unit: RichCompilationUnit = cp.addCompilationUnit(
    params.text(),
    params.uri().toString(),
    None
  )
  cp.typeCheck(unit) // initializing unit
  val (root, source) = (unit.lastBody, unit.source)

  def unitPos(offset: Int): Position = unit.position(offset)
  val nodes: List[NodeInfo] = traverser.traverse(List.empty[NodeInfo], root)
                                  .sortBy(_.pos.map(_.start))


  /**
   * main method
   */
  def provide(): ju.List[Integer] = {

      // logger.info(linSep + linSep + params.text() + linSep)
    // pprint.log(root)
    //  Thread.sleep(3000)
    // logString = linSep + params.text()
    // logString += "\n\n nodes:" + nodes.size.toString()
    // logString += nodes.map(n=>
    //     treeDescriber(n.tree.get,false)
    //     + "   "+  n.pos.get.start +"," + n.pos.get.end

    // ).mkString("")

    // Tools
    val buffer = ListBuffer.empty[Integer]

    /////////////////////////////
    // Loop by token
    /////////////////////////////
    // If a meta-Token is over multiline,
    // semantic-token is provided by each line.
    // For ecample, Comment or Literal String.
    import scala.meta._
    var cLine = Line(0,0) // Current Line
    var lastProvided = SingleLineToken(cLine,0,None)

      for (tk <- params.text().tokenize.toOption.get) yield {
        tk match {
          // case _:Token.LF =>
          //     logString += linSep + spcr + "Regular NewLine"
          //     cLine = Line(cLine.number + 1, tk.pos.end)

          // case _:Token.Space =>

          case _ =>

            tokenDescriber(tk)
            var cOffset = tk.pos.start // Current Offset
            var providing = SingleLineToken(cLine,cOffset,Some(lastProvided.copy()))
            // logString += linSep + spcr + "deltaLine" + providing.deltaLine
            // var nextStartOffset = 0
            // var nextStartLine = 0
            for (wkStr <- tk.text.toCharArray.toList.map(c => c.toString)) {
              

              cOffset += 1

              // Token Break
              if (wkStr == "\n" | cOffset == tk.pos.end) {
                val (tokenType, tokeModifier) = getTypeAndMod(tk)
                if (tokenType == -1 && tokeModifier == 0) {
                  // Go to next loop
                }else {
                  
                  providing.endOffset =
                      if (wkStr == "\n") cOffset - 1
                      else cOffset
                  buffer.++=(
                    List(
                      providing.deltaLine,
                      providing.deltaStartChar,
                      providing.charSize,
                      tokenType,
                      tokeModifier
                    )
                  )

                  lastProvided = providing
                }
                // Line Break
                if (wkStr == "\n") {
                  cLine = Line(cLine.number + 1, cOffset)
                }
                providing = SingleLineToken(cLine,cOffset,Some(lastProvided.copy()))


              }

            }

          }

        } // end for


    this.logger.info(logString) //Log
    buffer.toList.asJava

  }

  //single-LIne SemanticToken
  case class Line(
    val number:Int,
    val startOffset:Int
  )
  case class SingleLineToken (
    line:Line, // line which token on
    startOffset:Int, // Offset from start of file.
    lastToken:Option[SingleLineToken]
  ){
    var endOffset : Int = 0
    def charSize:Int = endOffset - startOffset
    def deltaLine: Int =
      line.number - this.lastToken.map(_.line.number).getOrElse(0)
      // line.number-lastLine.number
    def deltaStartChar: Int = {
      if (deltaLine == 0)
        startOffset - lastToken.map(_.startOffset).getOrElse(0)
      else
        startOffset - line.startOffset
    }
  }


  /**
   * This function returns -1 when capable Type is nothing.
   *  TokenTypes that can be on multilines are handled in another func.
   *  See Token.Comment in this file.
   */
  private def typeOfNonIdentToken(
      tk: scala.meta.tokens.Token
  ): Integer = {
    tk match {
      // case _: Token.Ident => // in case of Ident is

      // Alphanumeric keywords
      case _: Token.ModifierKeyword => getTypeId(SemanticTokenTypes.Modifier)
      case _: Token.Keyword => getTypeId(SemanticTokenTypes.Keyword)

      // extends Symbolic keywords
      case _: Token.Hash => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.Viewbound => getTypeId(SemanticTokenTypes.Operator)
      case _: Token.LeftArrow => getTypeId(SemanticTokenTypes.Operator)
      case _: Token.Subtype => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.RightArrow => getTypeId(SemanticTokenTypes.Operator)
      case _: Token.Supertype => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.At => getTypeId(SemanticTokenTypes.Keyword)
      case _: Token.Underscore => getTypeId(SemanticTokenTypes.Variable)
      case _: Token.TypeLambdaArrow => getTypeId(SemanticTokenTypes.Operator)
      case _: Token.ContextArrow => getTypeId(SemanticTokenTypes.Operator)

      // Constant
      case _: Token.Constant.Int | _: Token.Constant.Long |
          _: Token.Constant.Float | _: Token.Constant.Double =>
        getTypeId(SemanticTokenTypes.Number)
      case _: Token.Constant.String|_: Token.Constant.Char =>
         getTypeId(SemanticTokenTypes.String)
      case _: Token.Constant.Symbol =>getTypeId(SemanticTokenTypes.Property)

      // Comment
      case _:Token.Comment =>getTypeId(SemanticTokenTypes.Comment)

      // Default
      case _ => -1
    }

  }
  def pickFromTraversed(tk: scala.meta.tokens.Token): Option[NodeInfo] = {
    // logString += linSep + spcr + "Pick "
    // logString += linSep + spcr +tk.pos.start +","+tk.pos.end

    val buffer = ListBuffer.empty[NodeInfo]
    for (node <- nodes) {
      node.tree.get match {
        case imp: cp.Import =>
          // selector(imp, tk.pos.start) match {
          //   case Some(sym) => buffer.++=(List(NodeInfo(sym)))
          //   case None => // pass
          // }
          selector(imp, tk.pos.start)
            .map(sym=>buffer.++=(List(NodeInfo(sym))))

        case _ =>
          node.pos
            .filter(_.start == tk.pos.start)
            .filter(_.end == tk.pos.end)
            .map(_=>buffer.++=(List(node)))
      }
    }

    buffer.toList.headOption
    // if (nodeList.size == 0) None
    // else Some(nodeList(0))
  }

  case class NodeInfo(
    tree:Option[Tree],
    sym: Option[Symbol],
    pos: Option[scala.reflect.api.Position]
  )
  object NodeInfo {
    def apply(tree :Tree, pos:scala.reflect.api.Position): NodeInfo =
      new NodeInfo( Some(tree), Some(tree.symbol),Some(pos))

    def apply(imp: Import): NodeInfo  =
      new NodeInfo(Some(imp), None,None)

    def apply(sym:Symbol):NodeInfo =
      new NodeInfo(None,Some(sym),None)
  }

  /**
   * was written in reference to PcDocumentHighlightProvider.
   */
  import cp._
  object traverser {

    /**
     * gathers all nodes inside given tree.
     * The nodes have symbol.
     */
    def traverse(
        nodes: List[NodeInfo],
        tree: cp.Tree
    ): List[NodeInfo] = {

      tree match {
        /**
         * All indentifiers such as:
         * val a = <<b>>
         */
        case ident: cp.Ident if ident.pos.isRange =>
          nodes :+ NodeInfo(ident, ident.pos)
        /**
         * Needed for type trees such as:
         * type A = [<<b>>]
         */
        case tpe: cp.TypeTree if tpe.original != null && tpe.pos.isRange =>
          nodes :+ NodeInfo(tpe.original, typePos(tpe))

        /**
         * All select statements such as:
         * val a = hello.<<b>>
         */
        case sel: cp.Select if sel.pos.isRange =>
          traverse(
            nodes :+  NodeInfo(sel, sel.namePos),
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
              nodes :+  NodeInfo(df, df.namePos)
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
            .collectFirst { case cp.AssignOrNamedArg(i @ cp.Ident(_), _) =>
              NodeInfo(i, i.pos)
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
        case id: cp.Ident if id.symbol == cp.NoSymbol =>
          fallbackSymbol(id.name, id.pos) match {
            case Some(_) => nodes :+ NodeInfo(id, id.pos)
            case _ => nodes
          }

        case df: cp.MemberDef =>
          (tree.children ++ annotationChildren(df))
            .foldLeft(nodes)(traverse(_, _))
        /**
         * For traversing import selectors:
         * import scala.util.<<Try>>
         */
        case imp: cp.Import =>
          nodes :+ NodeInfo(imp)

        case _ =>
          if (tree == null) null
          else tree.children.foldLeft(nodes)(traverse(_, _))
      }
    }

    def fallbackSymbol(name: cp.Name, pos: cp.Position): Option[Symbol] = {
      val context = cp.doLocateImportContext(pos)
      context.lookupSymbol(name, sym => sym.isType) match {
        case cp.LookupSucceeded(_, symbol) =>
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
    lazy val namedArgCache: Map[Int, NamedArg] = {
      val parsedTree = cp.parseTree(source)
      parsedTree.collect { case arg @ cp.AssignOrNamedArg(_, rhs) =>
        rhs.pos.start -> arg
      }.toMap
    }
  }

  def selector(imp: cp.Import, startOffset: Int): Option[cp.Symbol] = {
    for {
      sel <- imp.selectors.reverseIterator
        .find(_.namePos <= startOffset)
    } yield imp.expr.symbol.info.member(sel.name)
  }

  /**
   * returns (SemanticTokenType, SemanticTokenModifier) of @param tk
   */
  private def getTypeAndMod(tk: scala.meta.tokens.Token): (Int, Int) = {

    tk match {
      case _: Token.Ident => IndentTypeAndMod(tk)
      case _ => (typeOfNonIdentToken(tk), 0)
    }
  }

  private def IndentTypeAndMod(tk: scala.meta.tokens.Token): (Int, Int) ={
    val default = (-1,0)

    val ret = for (
      nodeInfo <- pickFromTraversed(tk) ;
      sym <- nodeInfo.sym
    )yield {
      var mod: Int = 0
      def addPwrToMod(tokenID: String) = {
        val place: Int = getModifierId(tokenID)
        if (place != -1) mod += (1 << place)
      }

      // get Type
      val typ =
        if (sym.isValueParameter) getTypeId(SemanticTokenTypes.Parameter)
        else if (sym.isTypeParameter)
          getTypeId(SemanticTokenTypes.TypeParameter)
        else
        // See symbol.keystring about following conditions.
        if (sym.isJavaInterface)
          getTypeId(SemanticTokenTypes.Interface) // "interface"
        else if (sym.isTrait) getTypeId(SemanticTokenTypes.Interface) // "trait"
        else if (sym.isClass) getTypeId(SemanticTokenTypes.Class) // "class"
        else if (sym.isType && !sym.isParameter)
          getTypeId(SemanticTokenTypes.Type) // "type"
        else if (sym.isVariable) getTypeId(SemanticTokenTypes.Variable) // "var"
        else if (sym.hasPackageFlag)
          getTypeId(SemanticTokenTypes.Namespace) // "package"
        else if (sym.isModule) getTypeId(SemanticTokenTypes.Class) // "object"
        else if (sym.isSourceMethod)
          if (sym.isGetter | sym.isSetter)
            getTypeId(SemanticTokenTypes.Variable)
          else getTypeId(SemanticTokenTypes.Method) // "def"
        else if (sym.isTerm && (!sym.isParameter || sym.isParamAccessor)) {
          addPwrToMod(SemanticTokenModifiers.Readonly)
          getTypeId(SemanticTokenTypes.Variable) // "val"
        } else -1


      // Modifiers except by ReadOnly
      if (sym.isAbstract) addPwrToMod(SemanticTokenModifiers.Abstract)
      if (sym.isDeprecated) addPwrToMod(SemanticTokenModifiers.Deprecated)
      if (sym.owner.isModule) addPwrToMod(SemanticTokenModifiers.Static)

      (typ, mod)

    }

    ret.getOrElse(default)


    // def
    // nodeInfo.map(_.sym) match {
    //   case Some(sym:Symbol) =>
    //     // logString += linSep + "  ####  Symbol ###########"
    //     // initialize Mod
    //     var mod: Int = 0
    //     def addPwrToMod(tokenID: String) = {
    //       val place: Int = getModifierId(tokenID)
    //       if (place != -1) mod += (1 << place)
    //     }

    //     // get Type
    //     val typ =
    //       if (sym.isValueParameter) getTypeId(SemanticTokenTypes.Parameter)
    //       else if (sym.isTypeParameter)
    //         getTypeId(SemanticTokenTypes.TypeParameter)
    //       else
    //       // See symbol.keystring about following conditions.
    //       if (sym.isJavaInterface)
    //         getTypeId(SemanticTokenTypes.Interface) // "interface"
    //       else if (sym.isTrait) getTypeId(SemanticTokenTypes.Interface) // "trait"
    //       else if (sym.isClass) getTypeId(SemanticTokenTypes.Class) // "class"
    //       else if (sym.isType && !sym.isParameter)
    //         getTypeId(SemanticTokenTypes.Type) // "type"
    //       else if (sym.isVariable) getTypeId(SemanticTokenTypes.Variable) // "var"
    //       else if (sym.hasPackageFlag)
    //         getTypeId(SemanticTokenTypes.Namespace) // "package"
    //       else if (sym.isModule) getTypeId(SemanticTokenTypes.Class) // "object"
    //       else if (sym.isSourceMethod)
    //         if (sym.isGetter | sym.isSetter)
    //           getTypeId(SemanticTokenTypes.Variable)
    //         else getTypeId(SemanticTokenTypes.Method) // "def"
    //       else if (sym.isTerm && (!sym.isParameter || sym.isParamAccessor)) {
    //         addPwrToMod(SemanticTokenModifiers.Readonly)
    //         getTypeId(SemanticTokenTypes.Variable) // "val"
    //       } else -1


    //     // Modifiers except by ReadOnly
    //     if (sym.isAbstract) addPwrToMod(SemanticTokenModifiers.Abstract)
    //     if (sym.isDeprecated) addPwrToMod(SemanticTokenModifiers.Deprecated)
    //     if (sym.owner.isModule) addPwrToMod(SemanticTokenModifiers.Static)

    //     (typ, mod)
    //   case None =>
    //   // logString += linSep + "####  No Symbol ###########" + linSep

    //     (-1,0)
    // }

  }

  //////////////////////////////////////////////////
  // log tools
  //////////////////////////////////////////////////
  val logger: Logger = Logger.getLogger("SemanticTokenProvider")
  var logString:String=""
  val strSep = ", "
  val linSep = "\n"
  val spcr= "    "
  import scala.reflect.internal.util.Position
  private def namePos(t: cp.Tree): Position = {
    try {
      val wkStart = t.pos.point
      val wkEnd = wkStart + t.symbol.name.length() // - 1
      Position.range(t.pos.source, wkStart, wkStart, wkEnd)
    } catch {
      case _ => null
    }
  }

  var counter = 0

  /** makes string to logging tree construction. */
  def treeDescriber(t: cp.Tree, doRecurse: Boolean = true): String = {
    if (t == null) return "  " + "Null Tree"

    var ret = ""
    if (counter == 0 && doRecurse) ret += "\nNodesNum: " + t.id.toString

    counter += 1
    ret += linSep
    ret += "  " + ("000" + counter.toString()).takeRight(3) + "  "

    // Position
    try {
      val wkNamePos = namePos(t)
      ret += "namePos:(" + wkNamePos.start.toString()
      ret += "," + wkNamePos.end.toString() + ")"
    } catch { case _ => }
    ret += strSep + "-> TreeCls:" + t.getClass.getName.substring(29)

    // symbol
    try {
      ret = ret + SymDescriber(t.symbol)

    } catch { case _ => return "" }

    // recursive
    if (doRecurse)
      ret += t.children
        .map(treeDescriber(_, true))
        .mkString("\n")

    // end
    ret + linSep

  }
  def SymDescriber(sym: cp.Symbol): String = {
    var ret = ""

    ret += strSep + "sym:" + sym.toString
    ret += strSep + "keyStr:" + sym.keyString
    ret += strSep + "\n  name:" + sym.nameString
    ret += strSep + "SymCls:" + sym.getClass.getName.substring(31)
    ret += strSep + "SymKnd:" + sym.accurateKindString

    ret

  }

  def tokenDescriber(tk: scala.meta.tokens.Token) : Unit= {
    // tk match {
    //   case _:Token.Ident =>
    //   case _ => return
    // }

    logString += linSep + linSep

    logString += "token: " + tk.getClass.toString.substring(29)
    logString += strSep + "text: " + tk.text.toString()
    logString += strSep + "stt,end:(" + tk.pos.start.toString
    logString += strSep + tk.pos.end.toString + ")"
    logString += strSep + "LnStt,End:(" + tk.pos.startLine.toString
    logString += "," + tk.pos.endLine.toString + ")"

    counter = 0
    val nodeInfo = pickFromTraversed(tk)
    if (nodeInfo != None){
      nodeInfo.get.sym match {
        case Some(symbol) =>
          logString = logString + SymDescriber(symbol) + linSep
        case None =>
      }
    }

  }



}
