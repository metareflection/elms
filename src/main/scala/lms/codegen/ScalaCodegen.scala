package elms.codegen

import elms.core.*
import elms.core.Op.*
import elms.core.Name
import elms.core.tree.untyped as ast
import elms.core.tree.untyped.View
import elms.util.IndentedWriter
import elms.runtime.Log

class ScalaCodegen(cfg: Config = Config.scalaDefault) extends Backend(cfg) {
  import ast._

  def emit(prog: Program, out: java.io.PrintStream): Unit = {
    val w = makeIndentedWriter(out)
    prog.staticData.foreach { (name, data) => w.emitNamedStaticData(name, data) }
    prog.functions.foreach { (fname, fdef) => w.emitFunction(fname, fdef) }
  }

  private def renderArgs(name: Name, ty: Type): String =
    s"${name.render(cfg.varPrefix)}: ${ty.render}"

  extension [A: Primitive](x: A)
    def render: String = summon[Primitive[A]] match {
      case UNIT   => s"()"
      case INT    => s"$x"
      case BOOL   => s"$x"
      case CHAR   => s"'$x'"
      case STRING => s"\"$x\""
    }

  extension (ty: Type)
    private def render: String = ty match {
      case UNIT     => "Unit"
      case INT      => "Int"
      case BOOL     => "Boolean"
      case CHAR     => "Char"
      case STRING   => "String"
      case ARRAY(t) => s"Array[${t.render}]"
    }

  extension (out: IndentedWriter)
    private def invalidTerm(msg: String): Unit = {
      Log.error(msg)
      out.emit("???")
    }

    private def emitFunction(fname: Name, fdef: Function): Unit = {
      val Function(arg, inty, outty, body) = fdef

      val argsS = renderArgs(arg, inty)
      val header = s"def ${fname.render(cfg.varPrefix)}($argsS): ${outty.render} = {"
      out.emitln(header)
      out.indented { out.emitTerm(body) }
      out.emitln("")
      out.emitln("}\n")
    }

    private def emitMaybeParenthesized(t: Term): Unit = {
      val (l, r) = t match {
        case V(_) | E(_: Const[_], Nil) => ("", "")
        case Let(_,_,_) => ("{", "}")
        case _ => ("(", ")")
      }
      out.emit(l)
      out.emitTerm(t)
      out.emit(r)
    }

    private def emitAsExpr(t: Term): Unit = {
      val (l, r) = t match {
        case Let(_,_,_) => ("{", "}")
        case _ => ("", "")
      }
      out.emit(l)
      out.emitTerm(t)
      out.emit(r)
    }

    private def emitTerm(term: Term): Unit = term match {
      case V(name)                    => out.emit(name.render(cfg.varPrefix))
      case View.Let(x, mutTy, e1, e2) => {
        mutTy match {
          case Some(ty) => out.emit(s"var ${x.render(cfg.varPrefix)}: ${ty.render} = ")
          case None     => out.emit(s"val ${x.render(cfg.varPrefix)} = ")
        }
        out.emitAsExpr(e1)
        out.emitln("")
        out.emitTerm(e2)
      }
      case Let(x, e1, e2) => Log
          .error("BUG: Got `Let` without matching `View.Let` (should be impossible)")
      case Function(arg, inty, _outty, body) => {
        out.emit("(")
        out.emit(renderArgs(arg, inty))
        out.emitln(") => {")
        out.indented { out.emitTerm(body) }
        out.emitln("}")
      }
      case View.Const(const) => out.emit(const.value.render(using const.prim))

      // CR-soon cwong: This is likely subtly broken in the case that the `Var`
      // under inspection comes from something like a `Rep[Array[Var[T]]]`.
      case View.VarGet(x)    => out.emitTerm(x)
      case View.VarSet(x, v) => {
        out.emitTerm(x)
        out.emit(" = ")
        out.emitMaybeParenthesized(v)
      }
      case View.IfThenElse(guard, tthen, telse) => {
        out.emit("if ")
        out.emitMaybeParenthesized(guard)
        out.emitln(" then {")
        out.indented { out.emitTerm(tthen) }
        out.emitln("")
        out.emitln("} else {")
        out.indented { out.emitTerm(telse) }
        out.emitln("")
        out.emit("}")
      }
      case View.App(f, args) => {
        out.emitMaybeParenthesized(f)
        out.emitArgTerms(args)
      }
      case View.Negate(t) => {
        out.emit("-")
        out.emitMaybeParenthesized(t)
      }
      case View.Plus(x, y)   => out.emitBinop("+", x, y)
      case View.Times(x, y)  => out.emitBinop("*", x, y)
      case View.Minus(x, y)  => out.emitBinop("-", x, y)
      case View.Equals(x, y) => out.emitBinop("==", x, y)
      case View.Lt(x, y)     => out.emitBinop("<", x, y)
      case View.Gt(x, y)     => out.emitBinop(">", x, y)
      case View.Le(x, y)     => out.emitBinop("<=", x, y)
      case View.Ge(x, y)     => out.emitBinop(">=", x, y)
      case View.And(x, y)    => out.emitBinop("&&", x, y)
      case View.Or(x, y)     => out.emitBinop("||", x, y)
      case View.Not(t)       => {
        out.emit("!")
        out.emitMaybeParenthesized(t)
      }
      case View.Range(x, y)   => out.emitBinop("until", x, y)
      case View.RangeStart(t) => {
        out.emitMaybeParenthesized(t)
        out.emit(".start")
      }
      case View.RangeEnd(t) => {
        out.emitMaybeParenthesized(t)
        out.emit(".end")
      }
      case View.While(guard, body) => {
        out.emitln(s"while {")
        out.indented { out.emitTerm(guard) }
        out.emitln("")
        out.emitln("} do {")
        out.indented { out.emitTerm(body) }
        out.emitln("")
        out.emitln("}")
      }
      case View.RangeForEach(name, st, end, body) => {
        out.emit(s"for (${name.render(cfg.varPrefix)} <- ")
        out.emitMaybeParenthesized(st)
        out.emit(" until ")
        out.emitMaybeParenthesized(end)
        out.emitln(") {")
        out.indented { out.emitTerm(body) }
        out.emitln("")
        out.emitln("}")
      }
      case View.ArrayNew(ty, t) => {
        out.emit(s"new Array[${ty.render}](")
        out.emitTerm(t)
        out.emit(")")
      }
      case View.ArrayGet(arr, i) => {
        out.emitMaybeParenthesized(arr)
        out.emit("(")
        out.emitTerm(i)
        out.emit(")")
      }
      case View.ArraySet(arr, i, x) => {
        out.emitMaybeParenthesized(arr)
        out.emit("(")
        out.emitTerm(i)
        out.emit(") = ")
        out.emitTerm(x)
      }
      case View.ArrayLength(arr) => {
        out.emitMaybeParenthesized(arr)
        out.emit(".length")
      }
      case View.StringLength(s) => {
        out.emitMaybeParenthesized(s)
        out.emit(".length")
      }
      case View.StringTake(s, n) => {
        out.emitMaybeParenthesized(s)
        out.emit(".take(")
        out.emitMaybeParenthesized(n)
        out.emit(")")
      }
      case View.StringDrop(s, n) => {
        out.emitMaybeParenthesized(s)
        out.emit(".drop(")
        out.emitMaybeParenthesized(n)
        out.emit(")")
      }
      case View.StringStartsWith(s, prefix) => {
        out.emitMaybeParenthesized(s)
        out.emit(".startsWith(")
        out.emitMaybeParenthesized(prefix)
        out.emit(")")
      }
      case View.StringCharAt(s, i) => {
        out.emitMaybeParenthesized(s)
        out.emit(".charAt(")
        out.emitMaybeParenthesized(i)
        out.emit(")")
      }
      case View.StringEndsWith(s, suffix) => {
        out.emitMaybeParenthesized(s)
        out.emit(".endsWith(")
        out.emitMaybeParenthesized(suffix)
        out.emit(")")
      }
      case View.StringSubstring(s, start, end) => {
        out.emitMaybeParenthesized(s)
        out.emit(".substring(")
        out.emitMaybeParenthesized(start)
        out.emit(", ")
        out.emitMaybeParenthesized(end)
        out.emit(")")
      }
      case View.StructGet(repr, t, field) => {
        out.emitMaybeParenthesized(t)
        out.emit(s".$field")
      }
      case View.StructSet(t, field, v) => {
        out.emitMaybeParenthesized(t)
        out.emit(".$field = ")
        out.emitTerm(v)
      }
      case E(_, _) => out.invalidTerm(s"Got invalid term: $term")
    }

    private def emitArgTerms(args: Seq[Term]): Unit = {
      out.emit("(")
      args.zipWithIndex.foreach {
        case (t, i) => {
          if i != 0 then out.emit(", ")
          out.emitTerm(t)
        }
      }
      out.emit(")")
    }

    private def emitBinop(sym: String, x: Term, y: Term): Unit = {
      out.emitMaybeParenthesized(x)
      out.emit(s" $sym ")
      out.emitMaybeParenthesized(y)
    }

    private def emitNamedStaticData(name: Name, data: StaticData): Unit = {
      out.emitln(s"val ${name.render(cfg.varPrefix)} = ${renderStaticData(data)}")
    }

    private def renderStaticData(data: StaticData): String = data match {
      case s @ Scalar(x)         => x.render(using s.prim)
      case SArray(elemTy, elems) => {
        val renderedElems = elems.map(renderStaticData).mkString(",")
        s"Array[${elemTy.render}]($renderedElems)"
      }
    }
}
