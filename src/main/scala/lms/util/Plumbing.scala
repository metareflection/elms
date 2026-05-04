package lms.util

import scala.math.Ordering.Implicits._

object Plumbing {
  def onLeft[A1, B, A2](f: A1 => A2)(x: A1, y: B): (A2, B) = (f(x), y)
  def onRight[A, B1, B2](f: B1 => B2)(x: A, y: B1): (A, B2) = (x, f(y))

  extension [A, B] (xy: (A, B))
    def mapLeft[A2](f: A => A2): (A2, B) = onLeft(f)(xy._1, xy._2)
    def mapRight[B2](f: B => B2): (A, B2) = onRight(f)(xy._1, xy._2)

  extension [A] (xs: Seq[A])
    def filterMap[B](f: A => Option[B]): Seq[B] =
      xs.map(f).filter(_.isDefined).map(_.get)

    def argmin[B](cost: A => B)(using Ordering[B]): Option[A] =
      xs.foldLeft[Option[(A, B)]](None) {
        case (None, x) => Some((x, cost(x)))
        case (acc @ Some((y, best)), x) => {
          val xcost = cost(x)
          if xcost < best then Some((x, xcost)) else acc
        }
      }.map(_._1)

  extension [A] (xs: Seq[Option[A]])
    def traverse: Option[Seq[A]] =
      xs.foldRight(Option(Seq.empty)) { case (mx, macc) => for {
        x <- mx
        acc <- macc
      } yield x +: acc }
}
