package com.github.robfitzgerald.markmark.presentation.codec

import com.github.robfitzgerald.markmark.presentation.model.Content
import org.scalatest._
import matchers.should._
import org.scalatest.wordspec.AnyWordSpec

class BeamerCodecTest extends AnyWordSpec with Matchers with BeamerCodec {
  "Content" when {
    "decode" when {
      "called on an item" should {
        "produce Content" in {
          val input = """\item foo"""
          BeamerCodecInstance.decode(input) should equal(Right(List(Content.Item("foo"))))
        }
      }
      "called on an itemize environment" should {
        "produce Content" in {
          val input =
            """\begin{itemize}
              |  \item big dookie
              |\end{itemize}
              |""".stripMargin
          BeamerCodecInstance.decode(input) should equal(Right(List(Content.Unordered(Content.Item("big dookie")))))
        }
      }
      "called on an itemize environment with multiple items" should {
        "produce Content" in {
          val input =
            """\begin{itemize}
              |  \item big dookie
              |  \item little flookie
              |\end{itemize}
              |""".stripMargin
          val expectedContent = List(
            Content.Unordered(
              Content.Item("big dookie"),
              Content.Item("little flookie")
            )
          )
          BeamerCodecInstance.decode(input) should equal(Right(expectedContent))
        }
      }
    }
    "encode" when {
      "called on a frame" should {
        "produce the equivalent latex" in {
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
          println(frame.toLatex())
        }
      }
      "called on an ordered list" should {
        "produce the equivalent latex" in {
          val content = Content.Ordered(
            Content.Item("foo"),
            Content.Item("bar"),
            Content.Item("baz")
          )
          val result = content.toLatex()
          println(result)
        }
      }
    }
  }
}
