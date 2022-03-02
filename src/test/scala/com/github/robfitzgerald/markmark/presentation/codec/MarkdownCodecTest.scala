package com.github.robfitzgerald.markmark.presentation.codec

import com.github.robfitzgerald.markmark.presentation.model.Content
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest._
import matchers.should._

class MarkdownCodecTest extends AnyWordSpec with Matchers with MarkdownCodec {
  "Markdown" when {
    "encode" when {
      "encoding a frame" should {
        "make it" in {
          val frame = Content.Frame(
            "my frame is cool",
            List(
              Content.Ordered(
                Content.Item("first ordered item"),
                Content.Unordered(
                  Content.Item("first unordered item"),
                  Content.Item("second unordered item")
                ),
                Content.Item("third ordered item")
              )
            )
          )
          println(frame.toMarkdown())
        }
      }
    }
    "decode" when {
      "called with top matter" should {
        "produce content" in {
          val input =
            """# awesome sauce
              |
              |Rob Fitzgerald
              |
              |University of Cookieville, Monsterhio
              |
              |""".stripMargin
          MarkdownCodecInstance.decode(input) match {
            case Left(value) => fail(value)
            case Right(value) =>
              print(value)
              succeed
          }
        }
      }
      "called with one frame" should {
        "produce content" in {
          val input =
            """#### frame title
              |- first outer line
              |  - first inner line
              |- second outer line
              |""".stripMargin
          MarkdownCodecInstance.decode(input) match {
            case Left(value) => fail(value)
            case Right(value) =>
              print(value)
              succeed
          }
        }
      }
      "trouble" in {
        val trouble =
          """
            |## future work
            |
            |#### motivation
            |
            |- our work shows promise that an algorithm which can capture the externalities of SO agent route assignments can overcome
            |""".stripMargin

        MarkdownCodecInstance.decode(trouble) match {
          case Left(value) => fail(value)
          case Right(value) =>
            print(value)
            succeed
        }
      }
      "called with two frames" should {
        "produce content" in {
          val input =
            """#### frame title
              |- first outer line
              |  1. first inner line
              |- second outer line
              |
              |#### frame two
              |- yo
              |  - foog
              |    - dooga
              |""".stripMargin
          MarkdownCodecInstance.decode(input) match {
            case Left(value) => fail(value)
            case Right(value) =>
              print(value)
              succeed
          }
        }
      }
    }
  }
}
