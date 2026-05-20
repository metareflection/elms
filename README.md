# ELMS: Equality-guided Lightweight Modular Staging

ELMS is a framework for runtime code generation in Scala. It provides the core
abstractions needed to construct staged languages, embedded compilers, and
high-performance program generators.

ELMS builds on ideas from the original
[Lightweight Modular Staging (LMS)](https://scala-lms.github.io/) library,
but is a ground-up redesign rather than a continuation of the original codebase.

## Quickstart

Import `elms.prelude`:

```scala
import elms.prelude.{given, *}
```

Make sure to import `elms.prelude.given`! Many of ELMS's DSL type inference
won't work. We also strongly recommend `scala.language.implicitConversions` and
`scala.language.strictEquality`.

Extend `DslOps` to gain access to the `Rep` type constructor. This trait should
have the `@virtualize` annotation applied to ensure that control flow is properly
lifted:

```scala
@virtualize
trait Dsl extends DslOps {
  def pow(x: Rep[Int], n: Int): Rep[Int] =
    if n == 0 then 1 else x * pow(x, n-1)
}
```

Mix it into an `OptimizingSnippetDriver` and define a `snippet` method:

```scala
class PowSnippet(n: Int) extends OptimizingSnippetDriver[Int, Int] with Dsl {
  def snippet(x: Rep[Int]): Rep[Int] = pow(x, n)
}
```

Finally, print the `code`:

```scala
@main
def main: Unit = print(PowSnippet(4).code)
```

All together:

```scala
import scala.language.implicitConversions
import scala.language.strictEquality

import elms.prelude.{given, *}

@virtualize
trait Dsl extends DslOps {
  def pow(x: Rep[Int], n: Int): Rep[Int] =
    if n == 0 then 1 else x * pow(x, n-1)
}

class PowSnippet(n: Int) extends OptimizingSnippetDriver[Int, Int] with Dsl {
  def snippet(x: Rep[Int]): Rep[Int] = pow(x, n)
}

@main
def main: Unit = print(PowSnippet(4).code)
```

## Advanced usage

### `DslOps`

The core operations exposed by ELMS are exposed by the traits `elms.core.Base`
and `elms.core.PrimitiveOps`. The mixin `elms.helpers.DslOps` provides these
traits along with several useful type-specific operations. In general, an
ELMS program will consist of a "core functionality" trait extending `DslOps`
and a `Driver` to actually perform code generation (next section).

Any significant operations on `Rep` types should be wrapped in the `@virtualize`
macro, usually applied to the top-level `DslOps` trait:
```scala
@virtualize
trait MyDsl extends DslOps {
  // Dsl code here...
}
```

This macro rewrites Scala-level control flow into internal ELMS functions for
building the IR.

A top-level residue functions are defined using the `fun` combinator. It has a
few variations, but the most straightforward follows the pattern

```scala
  def myfun: Rep[A, B] = fun[A, B]("myfun") { (x: Rep[A] =>
    // ...
  }
```

**Make sure to use** `def`, **not `val`**! ELMS evaluates the bodies of top-level
functions on instantiation, which can cause crashes if `fun` is called before
ELMS is fully initialized.

### Drivers

To actually instantiate a `DslOps` trait, mix it into a `Driver`:

```scala
class MyDriver extends Driver with MyDsl
  val builder: elms.pipeline.Builder = // ...
```

By default, ELMS provides two builders:

- `simple.Builder` performs no optimizations, producing as direct a recreation
  of the staged code as possible. Good for simple applications and debugging.
- `eqsat.Builder` is a more traditional optimizing middle-end. Like the name
  suggests, this pipeline uses an equality graph to perform algebraic rewrites
  and code motion on the IR before rendering.

Once a `Driver` has been created, produce the code via the `extract()` method.
This produces a value of type `elms.core.tree.Program`, which can be further
analyzed or transformed as necessary.

### Code generation

To actually render a `Program` into standalone source code, it should be fed to
a `elms.codegen.Backend`. Currently, `ELMS` provides `ScalaCodegen` and `CCodegen`,
producing Scala and C code respectively. Not all features are supported by all
code generation backends.

If you have used the old LMS, you may find the abstract class `lms.helpers.SnippetDriver`
to be more familiar. An instance of `SnippetDriver` uses its `builder` and
`codegen` members to abstract the above process away and produce at least one
top-level function from its abstract `snippet` method.
