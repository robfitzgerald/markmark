package com.github.robfitzgerald.markmark.presentation.codec

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest._
import matchers.should._

class MarkdownBeamerTest extends AnyWordSpec with Matchers with MarkdownCodec with BeamerCodec {
  "test" in {
    val dir      = "/Users/robertfitzgerald/Documents/robfitzgerald/UCDenver/doctoral/Research/work/journal"
    val filename = "2022-02-25-presentation-outline.md"
    val source   = io.Source.fromFile(f"$dir/$filename")
    val lines    = source.getLines.mkString("\n")
    source.close()

    val result = for {
      content <- MarkdownCodecInstance.decode(lines)
    } yield {
      content.flatMap { s => BeamerCodecInstance.encode(s).toOption }.foreach(println)
    }

    result match {
      case Left(value) =>
        fail(value)
      case Right(value) =>
        println(value)
        succeed
    }
  }
}
