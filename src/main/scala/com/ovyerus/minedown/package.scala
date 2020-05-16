package com.ovyerus

import io.circe.Json

package object minedown {
  sealed trait Node {
    val name: String
  }

  sealed trait ValueNode extends Node {
    val value: String
  }

  sealed trait StructureNode extends Node {
    val nodes: List[Node]
  }

  case class RootNode(nodes: List[Node], name: String = "Root")
    extends StructureNode
  case class TextNode(value: String, name: String = "Text") extends ValueNode
  case class ItalicNode(nodes: List[Node], name: String = "Italic")
    extends StructureNode
  case class BoldNode(nodes: List[Node], name: String = "Bold")
    extends StructureNode
  case class BoldItalicNode(nodes: List[Node], name: String = "BoldItalic")
    extends StructureNode
  case class UnderlineNode(nodes: List[Node], name: String = "Underline")
    extends StructureNode
  case class StrikethroughNode(
                                nodes: List[Node],
                                name: String = "Strikethrough"
                              ) extends StructureNode
  case class SpoilerNode(nodes: List[Node], name: String = "Spoiler")
    extends StructureNode

  type NodeCtor[T <: Node] = (List[Node], String) => T
  type NodeTuple[T <: Node] = (Int, T)

  case class Token(
                    name: String,
                    var value: String,
                    var position: Option[Int] = None
                  )

  type TokenTuple = (Int, Option[Token])

  def minedown(text: String, asJson: Boolean = false): String = {
    val tokens = Tokeniser(text)
    val ast = Parser(tokens)

    if (!asJson)
      StringEmitter.emit(ast)
    else
      JsonEmitter.emit(ast).noSpaces
  }
}
