package lms.util.collection

import scala.collection.mutable

extension [T](seq: Seq[T])
  def uncons: Option[(T, Seq[T])] = seq match {
    case head +: tail => Some(head, tail)
    case _            => None
  }

extension [T](seq: mutable.Seq[T]) def clear(): Unit = seq.drop(seq.size)

extension [T](stk: mutable.Stack[T])
  def peek: Option[T] = if stk.isEmpty then None else Some(stk.top)
  def popSafe(): Option[T] = if stk.isEmpty then None else Some(stk.pop)

extension [K, V](m: Map[K, V])
  def setOrRemove(k: K, mv: Option[V]): Map[K, V] = mv match {
    case Some(v) => m + (k -> v)
    case None    => m
  }
