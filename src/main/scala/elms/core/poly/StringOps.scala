package elms.core.poly

trait StringOps extends PrimitiveOps {
  extension (s: Rep[String])
    def take(x: Rep[Int]): Rep[String]
    def drop(x: Rep[Int]): Rep[String]
    def startsWith(haystack: Rep[String]): Rep[Boolean]
    def endsWith(haystack: Rep[String]): Rep[Boolean]
    def charAt(i: Rep[Int]): Rep[Char]
    def substring(st: Rep[Int], end: Rep[Int]): Rep[String]

  given RepApply1[String, Int, Char] with
    def run(s: Rep[String], i: Rep[Int]): Rep[Char] = s.charAt(i)

  given stringLength: RepLength[String]
}
