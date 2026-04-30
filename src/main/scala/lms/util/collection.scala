package lms.util.collection

import scala.collection.mutable

extension [T](seq: mutable.Seq[T])
  def clear(): Unit = seq.drop(seq.size)

extension [T](stk: mutable.Stack[T])
  def peek: Option[T] = if stk.isEmpty then None else Some(stk.top)
  def popSafe(): Option[T] = if stk.isEmpty then None else Some(stk.pop)
