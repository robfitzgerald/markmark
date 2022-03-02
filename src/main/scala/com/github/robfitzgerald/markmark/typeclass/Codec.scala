package com.github.robfitzgerald.markmark.typeclass

trait Codec[A] {
  def encode(a: A): Either[Error, String]
  def decode(s: String): Either[Error, List[A]]
}
