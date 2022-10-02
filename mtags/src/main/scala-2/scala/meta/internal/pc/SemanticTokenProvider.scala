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

  // alias for long notation
  val getTypeId :Map[String,Int] = capableTypes.zipWithIndex.toMap
  val getModifierId :Map[String,Int] = capableModifiers.zipWithIndex.toMap

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

  /** main method */
  def provide(): ju.List[Integer] = {

    val buffer = ListBuffer.empty[Integer]
    var currentLine = 0
    var lastLine = 0
    var lastNewlineOffset = 0
    var lastCharStartOffset = 0

    def deltaLine(): Int = currentLine - lastLine
    def deltaStartChar(startPos: Int): Int = {
      if (deltaLine() == 0) startPos - lastCharStartOffset
      else startPos - lastNewlineOffset
    }
    def addTokenToBuffer(
        startPos: Int,
        charSize: Int,
        tokenType: Int,
        tokeModifier: Int
    ): Unit = {
      if (tokenType == -1 && tokeModifier == 0) return

      buffer.++=(
        List(
          deltaLine(), // 1
          deltaStartChar(startPos), // 2
          charSize, // 3
          tokenType, // 4
          tokeModifier // 5
        )
      )
      lastLine = currentLine
      lastCharStartOffset = startPos
    }

    // Loop by token
    import scala.meta._
    for (tk <- params.text().tokenize.toOption.get) yield {

      tk match {
        case _: Token.LF =>
          currentLine += 1
          lastNewlineOffset = tk.pos.end

        case _: Token.Space =>

        // deals multi-line token
        case _: Token.Comment | _: Token.Constant.String =>
          var wkStartPos = tk.pos.start
          var wkCurrentPos = tk.pos.start
          val tokenType = tk match {
            case _: Token.Comment => getTypeId(SemanticTokenTypes.Comment)
            case _ => getTypeId(SemanticTokenTypes.String)
          }
          val tokeModifier = 0

          for (wkStr <- tk.text.toCharArray.toList.map(c => c.toString)) {
            wkCurrentPos += 1

            // Add token to Buffer
            if (wkStr == "\n" | wkCurrentPos == tk.pos.end) {
              val adjustedCurrentPos =
                if (wkStr == "\n") wkCurrentPos - 1
                else wkCurrentPos
              val charSize = adjustedCurrentPos - wkStartPos

              addTokenToBuffer(
                wkStartPos,
                charSize,
                tokenType,
                tokeModifier
              )
              wkStartPos = wkCurrentPos
            }

            // Count such as Token.LF
            if (wkStr == "\n") {
              currentLine += 1
              lastNewlineOffset = wkCurrentPos
            }

          }

        case _ =>
          val (tokenType, tokeModifier) = getTypeAndMod(tk)
          addTokenToBuffer(
            tk.pos.start,
            tk.text.size,
            tokenType,
            tokeModifier
          )
      } // end match

    } // end for


    buffer.toList.asJava

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
      case _: Token.Constant.Char => getTypeId(SemanticTokenTypes.String)

      // Default
      case _ => -1
    }

  }

  case class NodeInfo(
    tree:Option[Tree],
    sym: Option[Symbol],
    pos: Option[scala.reflect.api.Position]
  ){
    def find(tk:scala.meta.tokens.Token):Option[NodeInfo]={
      this.tree match {
        case imp:Import =>
          selector(imp, tk.pos.start).map(NodeInfo(_))

        case _ =>
          this.pos
            .filter(_.start ==tk.pos.start )
            .filter(_.end ==tk.pos.end )
            .map(_=>this)
      }
    }

  }
  object NodeInfo {
    def apply(tree :Tree, pos:scala.reflect.api.Position): NodeInfo =
      new NodeInfo( Some(tree), None,Some(pos))

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

    // whether token is identifier or not
    tk match {
      case _: Token.Ident | _: Token.Constant.Symbol =>
      // continue this method.
      // Constant.Symbol means literal symbol with backticks.
      // e.g. which is `yield` of such as Thread.`yield`().
      case _ =>
        // Non-Ident has no modifier.
        return (typeOfNonIdentToken(tk), 0)
    }

    val nodeInfo = nodes.map(_.find(tk)).headOption.get

    nodeInfo.map(_.sym) match {
      case sym:Symbol =>
        // initialize Mod
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
      case _ =>
        (-1,0)
    }

  }

}
