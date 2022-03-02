package com.github.robfitzgerald.markmark.presentation.codec

import scala.util.matching.Regex

import com.github.robfitzgerald.markmark.typeclass.Codec
import com.github.robfitzgerald.markmark.presentation.model._
import com.github.robfitzgerald.markmark.presentation.model.Content._

trait BeamerCodec {

  val BeamerCodecInstance: Codec[Content] = new Codec[Content] {

    def encode(a: Content): Either[Error, String] = Right(a.toLatex())

    def decode(s: String): Either[Error, List[Content]] = s.parseLatex

  }

  implicit class StringExtensionMethods(s: String) {
    val ItemFinalRegex: Regex         = "\\\\item (.+)".r
    val ItemWithRemainingRegex: Regex = "\\\\item (.+)\n(.+)".r
    val EnvFinalRegex: Regex          = "(?s)\\\\begin\\{(\\w+)} *(\\{\\w+})? *(.+?)\\\\end\\{(\\w+)}".r
    val EnvWithRemainingRegex: Regex  = "(?s)\\\\begin\\{(\\w+)} *(\\{\\w+})? *(.+?)\\\\end\\{(\\w+)} *(\\.+)?".r

    def parseLatex: Either[Error, List[Content]] = {
      def _parse(remaining: String, result: List[Content] = List.empty): Either[Error, List[Content]] = {
        if (remaining.isEmpty) Right(result)
        else {
          val remainingTrimmed = remaining.trim
          remainingTrimmed match {
            case "" => Right(result)
            case ItemFinalRegex(item) =>
              Right(Content.Item(item) +: result)
            case ItemWithRemainingRegex(item, nextRemaining) =>
              _parse(nextRemaining, result).map { Content.Item(item) +: _ }

            case EnvFinalRegex(start, title, env, end) =>
              if (start.trim != end.trim) {
                Left(new Error(s"environment has mismatched tags, start: $start; end: $end"))
              } else {
                val thisEnvironment: Either[Error, Content] = _parse(env).flatMap { innerContent =>
                  start match {
                    case "itemize" =>
                      Right(Content.Unordered(innerContent))
                    case "enumerate" =>
                      Right(Content.Ordered(innerContent))
                    case "frame" =>
                      Right(Content.Frame(title, innerContent))
                    case other =>
                      Left(new Error(s"unknown LaTeX environment type $other"))
                  }
                }
                thisEnvironment match {
                  case Left(error)      => Left(error)
                  case Right(envParsed) => Right(envParsed +: result)
                }
              }

            case EnvWithRemainingRegex(start, title, env, end, nextRemaining) =>
              if (start.trim != end.trim) {
                Left(new Error(s"environment has mismatched tags, start: $start; end: $end"))
              } else {
                val thisEnvironment: Either[Error, Content] = _parse(env).flatMap { innerContent =>
                  start match {
                    case "itemize" =>
                      Right(Content.Unordered(innerContent))
                    case "enumerate" =>
                      Right(Content.Ordered(innerContent))
                    case "frame" =>
                      Right(Content.Frame(title, innerContent))
                    case other =>
                      Left(new Error(s"unknown LaTeX environment type $other"))
                  }
                }
                thisEnvironment match {
                  case Left(error)      => Left(error)
                  case Right(envParsed) => _parse(nextRemaining, envParsed +: result)
                }
              }

          }
        }
      }

      _parse(s)
    }
  }

  implicit class ContentExtensionMethods(c: Content) {

    def toLatex(depth: Int = 0, spaces: Int = 4): String = c match {
      case TopMatter(title, author, institute) =>
        s"""\\title{$title}
           |
           |\\author[$author]{$author}
           |
           |\\institute[$institute]{$institute}
           |
           |""".stripMargin
      case Section(title)    => s"""\\section{$title}\n"""
      case SubSection(title) => s"""\\subsection{$title}\n"""
      case Frame(title, content) =>
        val frameContent = content.map { _.toLatex(depth + 1, spaces) }.mkString("\n")
        s"""\\begin{frame}{$title}
           |$frameContent
           |\\end{frame}\n""".stripMargin
      case Item(content) =>
        val buffer = (" " * spaces) * depth
        s"""$buffer\\item $content"""
      case Unordered(content) =>
        val buffer = (" " * spaces) * depth
        val items  = content.map { _.toLatex(depth + 1, spaces) }.mkString("\n")
        s"""$buffer\\begin{itemize}
           |$items
           |$buffer\\end{itemize}""".stripMargin
      case Ordered(content) =>
        val buffer = (" " * spaces) * depth
        val items  = content.map { _.toLatex(depth + 1, spaces) }.mkString("\n")
        s"""$buffer\\begin{enumerate}
           |$items
           |$buffer\\end{enumerate}""".stripMargin
    }
  }
}
