package com.github.robfitzgerald.markmark.presentation.codec

import scala.util.matching.Regex

import com.github.robfitzgerald.markmark.presentation.model.Content
import com.github.robfitzgerald.markmark.typeclass.Codec

trait MarkdownCodec {

  val MarkdownCodecInstance: Codec[Content] = new Codec[Content] {

    def encode(a: Content): Either[Error, String] = Right(a.toMarkdown())

    def decode(s: String): Either[Error, List[Content]] = s.parseMarkdown

  }

  implicit class MarkdownDecoderExtension(s: String) {
    val TopMatterRegex: Regex  = "# (.+)\\n\\n(.+)\\n\\n(.+)\\n([\\S\\n\\t\\v ]*)".r
    val SectionRegex: Regex    = "## (.+)\\n+([\\S\\n\\t\\v ]*)".r
    val SubSectionRegex: Regex = "### (.+)\\n+([\\S\\n\\t\\v ]*)".r
    val FrameRegex: Regex      = "#### (.+)\\n([\\s\\S]*?)(?=\\n.*?#|$)([\\S\\n\\t\\v ]*)".r
    val FinalFrameRegex: Regex = "#### (.+)(\\n+[\\S\\n\\t\\v ]*)?".r
    val UnorderedRegex: Regex  = "( *)- (.+)(\\n+[\\S\\n\\t\\v ]*)?".r
    val OrderedRegex: Regex    = "( *)\\d+[.] (.+)(\\n+[\\S\\n\\t\\v ]*)?".r

    def parseMarkdown: Either[Error, List[Content]] = {

      def _parse(
        maybeRemaining: String,
        depth: Int = 0,
        result: List[Content] = List.empty,
        blocks: Map[Int, Content] = Map.empty
      ): Either[Error, List[Content]] = {
        Option(maybeRemaining) match {
          case None =>
            Right(result)
          case Some(remainingString) =>
            val remainingTrimmed = remainingString.dropWhile(_ == '\n')
            remainingTrimmed match {
              case x if x.trim.isEmpty =>
                Right(result)
              case TopMatterRegex(title, author, institute, remaining) =>
                val topMatter = Content.TopMatter(title, author, institute)
                _parse(remaining, result = topMatter +: result)
              case SectionRegex(title, remaining) =>
                val section = Content.Section(title)
                _parse(remaining, result = section +: result)
              case SubSectionRegex(title, remaining) =>
                val subSection = Content.SubSection(title)
                _parse(remaining, result = subSection +: result)
              case FrameRegex(title, content, remaining) =>
                _parse(content, depth) match {
                  case Left(error) =>
                    Left(error)
                  case Right(parsed) =>
                    _parse(remaining, depth, Content.Frame(title, parsed) +: result)
                }
              case FinalFrameRegex(title, content) =>
                _parse(content, depth) match {
                  case Left(error) =>
                    Left(error)
                  case Right(parsed) =>
                    Right(Content.Frame(title, parsed) +: result)
                }
              case UnorderedRegex(spacer, line, remaining) =>
                val newDepth    = spacer.length / 2
                val lineContent = Content.Item(line)
                // if we are at the same depth as before, we parse and attach our result
                // to the container in the result, which is either empty or is a single
                // Content.Unordered
                if (newDepth == depth) {
                  val thisBlock = blocks.getOrElse(newDepth, Content.Unordered())
                  thisBlock match {
                    case Content.Unordered(prevContent) =>
                      val nextBlocks =
                        blocks.updated(newDepth, Content.Unordered(lineContent +: prevContent))
                      _parse(remaining, depth, result, nextBlocks)
                    case other =>
                      Left(new Error(s"attempting to update block with ordered line $lineContent but block is $other"))
                  }
                } else if (newDepth > depth) {
//                  val updatedResult = List(Content.Unordered(lineContent))
                  val updatedBlocks = blocks.updated(newDepth, Content.Unordered(lineContent))
                  _parse(remaining, newDepth, result, updatedBlocks)
                } else {}

                if (newDepth == depth) {
                  val updatedContainer = result match {
                    case (container: Content.Unordered) :: _ =>
                      Content.Unordered(lineContent +: container.content)
                    case _ =>
                      Content.Unordered(lineContent)
                  }
                  val recursed = _parse(remaining, newDepth)
                  recursed.map { siblings =>
                    val contentReverse = (siblings ::: updatedContainer.content).reverse
                    List(Content.Unordered(contentReverse))
                  //                    List(Content.Unordered(contentReverse))
                  }
                } else if (newDepth > depth) {
                  // if we are at a deeper depth, we start a result with one entry
                  // and parse the remaining
                  val updatedResult = List(Content.Unordered(lineContent))
                  _parse(remaining, newDepth, updatedResult)
                } else {
                  // if we are less deep, then we take what's stored in our result
                  // and return our stuff at the same depth as it
                  val updatedResult = lineContent +: result
                  Right(updatedResult)
                }
              case OrderedRegex(spacer, line, remaining) =>
                val newDepth    = spacer.length / 2
                val lineContent = Content.Item(line)
                // if we are at the same depth as before, we parse and attach our result
                // to the container in the result, which is either empty or is a single
                // Content.Unordered
                if (newDepth == depth) {
                  val updatedContainer = result match {
                    case (container: Content.Ordered) :: _ =>
                      Content.Ordered(lineContent +: container.content)
                    case _ =>
                      Content.Ordered(lineContent)
                  }

                  _parse(remaining, newDepth)
                    .map { siblings =>
                      val contentReverse = (siblings ::: updatedContainer.content).reverse
                      List(Content.Ordered(contentReverse))
//                      List(Content.Ordered(siblings.reverse))
                    }
                } else if (newDepth > depth) {
                  // if we are at a deeper depth, we start a result which is empty
                  // and parse the remaining
                  val updatedResult = List(Content.Ordered(lineContent))
                  _parse(remaining, newDepth, updatedResult)
                } else {
                  // if we are less deep, then we take what's stored in our result
                  // and return our stuff at the same depth as it
                  val updatedResult = lineContent +: result
                  Right(updatedResult)
                }
              case other =>
                Left(new Error(s"match error on input $other"))
            }
        }
      }

      _parse(s).map { _.reverse }
    }
  }

  implicit class MarkdownEncoderExtension(c: Content) {

    def toMarkdown(depth: Int = 0, spaces: Int = 2): String = c match {
      case Content.TopMatter(title, author, institute) =>
        s"""# $title
           |
           |$author
           |
           |$institute
           |
           |""".stripMargin
      case Content.Section(title)    => s"## $title\n\n"
      case Content.SubSection(title) => s"### $title\n\n"
      case Content.Frame(title, content) =>
        val inner = content.map { _.toMarkdown(depth) }.mkString("\n")
        s"""#### $title
           |
           |$inner
           |
           |""".stripMargin
      case Content.Item(content) =>
        content
      case Content.Unordered(content) =>
        val buffer = (" " * spaces) * depth
        content
          .map {
            case item: Content.Item =>
              s"$buffer- ${item.toMarkdown(depth + 1)}"
            case other => other.toMarkdown(depth + 1)
          }
          .mkString("\n")
      case Content.Ordered(content) =>
        val buffer = (" " * spaces) * depth
        content.zipWithIndex
          .map {
            case (item: Content.Item, idx) =>
              s"$buffer$idx. ${item.toMarkdown(depth + 1)}"
            case (other, _) => other.toMarkdown(depth + 1)
          }
          .mkString("\n")
    }
  }

}
