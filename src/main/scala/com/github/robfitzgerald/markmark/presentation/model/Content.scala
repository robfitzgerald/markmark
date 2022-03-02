package com.github.robfitzgerald.markmark.presentation.model

sealed trait Content

object Content {

  // structural content
  case class TopMatter(title: String, author: String, institute: String) extends Content
  case class Section(title: String)                                      extends Content
  case class SubSection(title: String)                                   extends Content
  case class Frame(title: String, content: List[Content])                extends Content

  // inner content
  case class Item(content: String)             extends Content
  case class Unordered(content: List[Content]) extends Content
  case class Ordered(content: List[Content])   extends Content

  object Unordered {
    def apply(content: Content*): Unordered = new Unordered(content.toList)
  }

  object Ordered {
    def apply(content: Content*): Ordered = new Ordered(content.toList)
  }

}
