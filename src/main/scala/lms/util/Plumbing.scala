package lms.util

object Plumbing {
  def onLeft[A1,B,A2](f: A1 => A2)(x: A1, y: B): (A2, B) = (f(x), y)
  def onRight[A,B1,B2](f: B1 => B2)(x: A, y: B1): (A, B2) = (x, f(y))
}
