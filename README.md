# ELMS: Equality-guided Lightweight Modular Staging

## Quickstart

```
import lms.helpers.{SimpleDriver, DslOps}

@virtualize
trait Dsl extends DslOps {
  // define your Rep functions here...
}

@virtualize
object Snippet extends SimpleDriver[Int, Int] with Dsl {
  override def snippet(x: Int): Int = // ...
}

@main def main(): Unit = println(Snippet.code)
```

To use the optimizing backend, instead use `OptimizingDriver`:

```
import lms.pipeline.eqsat.*
import lms.helpers.OptimizingDriver

val rules = Seq(
  // define rules here, see Pattern.scala
)

// ...

@virtualize
object Snippet extends OptimizingDriver[Int, Int] with Dsl {
  // ...
}
```

Example:
```
@virtualize
trait Dsl extends DslOps {
  def fact: Rep[Int => Int] = fun { (x: Rep[Int]) =>
    if x === 0 then unit(1) else x * fact(x - 1)
  }
}

object Playground extends OptimizingDriver[Int, Int](Seq()) with Dsl {
  def snippet(v: Rep[Int]): Rep[Int] = { fact(v) }
}

@main
def main() = {
  println(Playground.code())
}
```
