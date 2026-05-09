package lms.util.typeclasses

trait Nameable[A] {
  extension (x: A) def render: String
}

given Nameable[String] {
  extension (x: String) def render = x
}
