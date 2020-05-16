package com.ovyerus.minedown

import scala.collection.mutable.ListBuffer
import io.circe._, io.circe.generic.auto._, io.circe.syntax._

/**
 * Outputs a string representing Markdown using Minecraft's custom formatting scheme.
 * @see See [[https://minecraft.gamepedia.com/Formatting_codes]] for the formatting codes.
 */
object StringEmitter {
  private val RESET = "§r"
  private val ITALIC = "§o"
  private val BOLD = "§l"
  private val UNDERLINE = "§n"
  private val STRIKETHROUGH = "§m"
  private val OBFUSCATED = "§k"

  def apply(root: RootNode): String = {
    var emitted = ""
    val context = ListBuffer[String]()

    def addNode[T <: StructureNode](prepend: String, node: T): Unit = {
      emitted += prepend
      context += prepend

      if (node.nodes.nonEmpty) node.nodes.foreach { walk }
      if (context.nonEmpty) context.dropRight(1)

      emitted += RESET
      context.foreach {emitted += _}
    }

    def walk(node: Node): Unit = {
      node match {
        case txt: TextNode => emitted += txt.value
        case n: ItalicNode => addNode(ITALIC, n)
        case n: BoldNode => addNode(BOLD, n)
        case n: BoldItalicNode => addNode(BOLD + ITALIC, n)
        case n: UnderlineNode => addNode(UNDERLINE, n)
        case n: StrikethroughNode => addNode(STRIKETHROUGH, n)
        case n: SpoilerNode => addNode(OBFUSCATED, n)
        case _ => throw new IllegalArgumentException(s"Cannot walk node ${node.name}")
      }
    }

    root.nodes.foreach {walk}

    emitted
  }
}

/**
 * Outputs a JSON tree representing Markdown using Minecraft's raw JSON scheme.
 * @see See [[https://minecraft.gamepedia.com/Commands#Raw_JSON_text]] for the structure of this raw JSON.
 */
object JsonEmitter {
  private type ChatMap = Map[String, Any]

  private def mapNodes(nodes: List[Node]) = nodes.map(apply)
  private def mapNodes(nodes: List[Node], asSingular: true) = Map("extra" -> mapNodes(nodes))

  private def emitNest[T <: StructureNode](base: T => ChatMap)(node: T): ChatMap =
    node.nodes.lift(0) match {
      case Some(txt: TextNode) => base(node) ++ Map("text" -> txt.value)
      case _ => base(node) ++ Map("extra" -> mapNodes(node.nodes))
    }

  private def emitRoot(node: RootNode) = mapNodes(node.nodes)
  private def emitText(node: TextNode) = Map("text" -> node.value)

  private val emitItalic = emitNest[ItalicNode](_ => Map("italic" -> true))
  private val emitBold = emitNest[BoldNode](_ => Map("bold" -> true))
  private val emitBoldItalic = emitNest[BoldItalicNode](_ => Map("italic" -> true, "bold" -> true))
  private val emitUnderline = emitNest[UnderlineNode](_ => Map("underlined" -> true))
  private val emitStrikethrough = emitNest[StrikethroughNode](_ => Map("strikethrough" -> true))
  private val emitSpoiler = emitNest[SpoilerNode](node => Map(
    "obfuscated" -> true,
    "hoverEvent" -> Map("action" -> "show_text", "value" -> mapNodes(node.nodes, true))
  ))

  def apply(node: Node): ChatMap = node match {
//    case n: RootNode => emitRoot(n)
    case n: TextNode => emitText(n)
    case n: ItalicNode => emitItalic(n)
    case n: BoldNode => emitBold(n)
    case n: BoldItalicNode => emitBoldItalic(n)
    case n: UnderlineNode => emitUnderline(n)
    case n: StrikethroughNode => emitStrikethrough(n)
    case n: SpoilerNode => emitSpoiler(n)
  }

  def apply(root: RootNode): Json = emitRoot(root).asJson
}