package com.ovyerus.minedown

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

/**
 * Tokeniser for a string of markdown-like text.
 */
object Tokeniser {
  private def tokeniseChar(
                    name: String,
                    value: Char
                  )(input: String, current: Int): TokenTuple =
    if (input(current) == value) (1, Some(Token(name, value.toString)))
    else (0, None)

  private def tokeniseGroup(
                     name: String,
                     value: String
                   )(input: String, current: Int): TokenTuple = {
    val end = current + value.length
    val slice = input.slice(current, end)

    if (slice == value) (value.length, Some(Token(name, value)))
    else (0, None)
  }

  private type PartialTokeniser = (String, Int) => TokenTuple

  // TODO: escaping
  // TODO: italic_star, italic_underscore
  private val tokeniseItalicStar = tokeniseChar("italic", '*')
  private val tokeniseItalicUnderscore = tokeniseChar("italic", '_')
  private val tokeniseBold = tokeniseGroup("bold", "**")
  private val tokeniseBoldItalic = tokeniseGroup("boldItalic", "***")
  private val tokeniseUnderline = tokeniseGroup("underline", "__")
  private val tokeniseStrikethrough = tokeniseGroup("strikethrough", "~~")
  private val tokeniseSpoiler = tokeniseGroup("spoiler", "||")

  private val tokenisers = List[PartialTokeniser](
    tokeniseUnderline,
    tokeniseBoldItalic,
    tokeniseBold,
    tokeniseItalicStar,
    tokeniseItalicUnderscore,
    tokeniseStrikethrough,
    tokeniseSpoiler
  )

  /**
   * Tokenises a markdown-like string into a list of tokens representing their formatting.
   *
   * @param input The input string to tokenise.
   * @return A list of [[com.ovyerus.minedown.Token]]s.
   */
  def apply(input: String): List[Token] = {
    var current = 0
    val tokens = ListBuffer[Token]()

    while (current < input.length) {
      var tokenised = false

      breakable {
        tokenisers.foreach { fn =>
          if (tokenised) break

          val (consumed, token) = fn(input, current)

          if (consumed != 0) {
            tokenised = true
            current += consumed
          }

          token match {
            case Some(tk) =>
              tk.position = Some(current - 1)
              tokens += tk
          }
        }
      }

      if (!tokenised) {
        def addNew(): Unit = tokens += Token("text", input({current += 1; current - 1}).toString, Some(current - 1))

        tokens.lastOption match {
          case Some(prev) => if (prev.name == "text") prev.value += input({current += 1; current -1}) else addNew()
          case None => addNew()
        }
      }
    }

    tokens.toList
  }
}
