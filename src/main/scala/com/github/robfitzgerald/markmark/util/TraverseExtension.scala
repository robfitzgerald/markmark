package com.github.robfitzgerald.markmark.util

import scala.annotation.tailrec

trait TraverseExtension {

  implicit class TraverseExtensionForSeq[A](seq: Seq[A]) {

    def traverse[B, C](fn: A => Either[B, C]): Either[B, Seq[C]] = {
      @tailrec
      def _inner(remaining: Seq[A], solution: Seq[C]): Either[B, Seq[C]] = {
        remaining.headOption match {
          case None => Right(solution)
          case Some(a) =>
            fn(a) match {
              case Left(b) => Left(b)
              case Right(c) =>
                _inner(remaining.tail, solution.appended(c))
            }
        }
      }
      _inner(seq, Seq.empty)
    }
  }
}
