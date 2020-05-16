package com.ovyerus.minedown

import sun.reflect.generics.reflectiveObjects.NotImplementedException
import scala.collection.mutable.ListBuffer

/**
 * Parser for transforming a list of [[com.ovyerus.minedown.Token]] into an AST.
 */
object Parser {
  private def findNextOfType(tokens: List[Token], curr: Token) =
    tokens.find(
      token => token.name == curr.name && token.position != curr.position
    )

  private def parseClosable[T <: Node](
                                        ctor: NodeCtor[T]
                                      )(tokens: List[Token], curr: Int): NodeTuple[T] = {
    var pointer = curr
    val token = tokens({pointer += 1; pointer - 1})
    val closer = findNextOfType(tokens, token)
    val nodes = ListBuffer[Node]()

    closer match {
      case Some(tk) =>
        var next: Option[Token] = tokens.lift(pointer)

        while (next.isDefined && next.get.position != tk.position) {
          var (newPointer, node) = parseToken(tokens, pointer)

          nodes += node
          pointer = newPointer
          next = tokens.lift(pointer)
        }
      case None => throw new NotImplementedException()
    }

    (pointer + 1, ctor(nodes))
  }

  private def parseText(tokens: List[Token], curr: Int): NodeTuple[TextNode] =
    (curr + 1, TextNode(tokens(curr).value))

  private val parseItalic = parseClosable(ItalicNode)
  private val parseBold = parseClosable(BoldNode)
  private val parseBoldItalic = parseClosable(BoldItalicNode)
  private val parseUnderline = parseClosable(UnderlineNode)
  private val parseStrikethrough = parseClosable(StrikethroughNode)
  private val parseSpoiler = parseClosable(SpoilerNode)

  private def parseToken(tokens: List[Token], curr: Int): NodeTuple[Node] = {
    val token = tokens(curr)

    token.name match {
      case "text" => parseText(tokens, curr)
      case "italic" => parseItalic(tokens, curr)
      case "bold" => parseBold(tokens, curr)
      case "boldItalic" => parseBoldItalic(tokens, curr)
      case "underline" => parseUnderline(tokens, curr)
      case "strikethrough" => parseStrikethrough(tokens, curr)
      case "spoiler" => parseSpoiler(tokens, curr)
      case _ => throw new IllegalArgumentException(s"Unknown token ${token.name}")
    }
  }

  /**
   * Parses a list of [[com.ovyerus.minedown.Token]] representing their formatting into an AST relating them to each other.
   *
   * @param tokens Input tokens to parse.
   * @return The root node of an AST representing the formatting of the original string.
   */
  def apply(tokens: List[Token]): RootNode = {
    var current = 0;
    val nodes = ListBuffer[Node]()

    while (current < tokens.length) {
      val (newCurr, node) = parseToken(tokens, current)

      nodes += node
      current = newCurr
    }

    RootNode(nodes.toList)
  }
}
