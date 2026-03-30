package lms.helpers

import lms.core._

trait DslOps
    extends Base
    with Builtins
    with PrimitiveOps
    with BooleanOps
    with IntegerOps
    with RangeOps
    with ArrayOps
    with StringOps
