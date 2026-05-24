package elms.test

import elms.prelude.*
import elms.prelude.given

abstract class DslDriver[A: Typable, B: Typable]
    extends OptimizingSnippetDriver[A, B](Seq()) with DslOps
