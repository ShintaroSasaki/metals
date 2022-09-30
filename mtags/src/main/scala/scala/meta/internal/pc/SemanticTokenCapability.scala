package scala.meta.internal.pc

import scala.collection.JavaConverters._

import org.eclipse.lsp4j._

object SemanticTokenCapability {
  var TokenTypes: List[String] = List(
    SemanticTokenTypes.Type,
    SemanticTokenTypes.Class,
    SemanticTokenTypes.Enum, // 3
    SemanticTokenTypes.Interface, // 4
    SemanticTokenTypes.Struct, // 5
    SemanticTokenTypes.TypeParameter, // 6
    SemanticTokenTypes.Parameter, // 7
    SemanticTokenTypes.Variable, // 8
    SemanticTokenTypes.Property, // 9
    SemanticTokenTypes.EnumMember, // 10
    SemanticTokenTypes.Event, // 11
    SemanticTokenTypes.Function, // 12
    SemanticTokenTypes.Method, // 13
    SemanticTokenTypes.Macro, // 14
    SemanticTokenTypes.Keyword, // 15
    SemanticTokenTypes.Modifier,
    SemanticTokenTypes.Comment,
    SemanticTokenTypes.String,
    SemanticTokenTypes.Number,
    SemanticTokenTypes.Regexp,
    SemanticTokenTypes.Operator,
    SemanticTokenTypes.Decorator,
  )

  var TokenModifiers: List[String] = List(
    SemanticTokenModifiers.Declaration,
    SemanticTokenModifiers.Definition,
    SemanticTokenModifiers.Readonly,
    SemanticTokenModifiers.Static,
    SemanticTokenModifiers.Deprecated,
    SemanticTokenModifiers.Abstract,
    SemanticTokenModifiers.Async,
    SemanticTokenModifiers.Modification,
    SemanticTokenModifiers.Documentation,
    SemanticTokenModifiers.DefaultLibrary,
  )

}
