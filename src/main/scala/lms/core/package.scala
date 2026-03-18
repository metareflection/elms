package lms

import scala.Conversion

package object core {
  given typableOfLiftable[A]: Conversion[Liftable[A], Typable[A]] with
    def apply(x: Liftable[A]): Typable[A] = Typable.ofLiftable(x)
}
