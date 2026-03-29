package lms.codegen

import lms.core.{Op, Type}, Type._, Op._
import lms.codegen.ast, ast._
import lms.util.IndentedWriter
import lms.runtime.Log

class ScalaCodegen(cfg: Config = Config.scalaDefault) extends Backend(cfg) {
  def emit(prog: Program, out: java.io.PrintStream): Unit = {
    val w = makeIndentedWriter(out)
    prog.functions.foreach { (fname, fdef) => w.emitFunction(fname, fdef) }
  }

  private def renderArgs(args: Seq[(String, Type)]): String = args.map { (name, ty) =>
    s"$name: ${ty.render}"
  }.mkString(",")

  extension [A](c: Const[A]) def render: String = s"${c.v}"

  extension (ty: Type)
    private def render: String = ty match {
      case UNIT     => "Unit"
      case INT      => "Int"
      case BOOL     => "Boolean"
      case CHAR     => "Char"
      case STRING   => "String"
      case ARRAY(t) => s"Array[${t.render}]"
    }

  extension (t: Term)
    private def isCompound: Boolean = t match {
      case V(_) | E(_: Const[_], Nil) => false
      case _                          => true
    }

  extension (out: IndentedWriter)
    private def emitFunction(fname: String, fdef: Function): Unit = {
      val Function(args, outty, body) = fdef

      val argsS = renderArgs(args)
      val header = s"def $fname($argsS): ${outty.render} = {"
      out.emitln(header)
      out.indented { out.emitTerm(body) }
      out.emitln("")
      out.emitln("}\n")
    }

    private def emitTerm(t: Term): Unit = t match {
      case E(op, children) => out.emitCompound(op, children)
      case V(name)         => out.emit(name)
      case Let(x, e1, e2)  => {
        out.emit(s"val $x = ")
        out.emitTerm(e1)
        out.emitln("")
        out.emitTerm(e2)
      }
      case Function(args, _outty, body) => {
        out.emit("(")
        out.emit(renderArgs(args))
        out.emitln(") => {")
        out.indented { out.emitTerm(body) }
        out.emitln("}")
      }
    }

    private def emitMaybeParenthesized(t: Term): Unit = {
      if t.isCompound then out.emit("(")
      out.emitTerm(t)
      if t.isCompound then out.emit(")")
    }

    private def emitCompound(op: Op, children: Seq[Term]): Unit = op match {
      case c: Const[_] => out.emit(c.render)
      case IfThenElse  => children match {
          case Seq(guard, tthen, telse) => {
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
          case _ => {
            Log.error(s"BUG: IfThenElse should have exactly 3 children")
            out.emit("???")
          }
        }
      case While => children match {
          case Seq(guard, body) => {
            out.emit("while ")
            out.emitMaybeParenthesized(guard)
            out.emit(" do {")
            out.indented { out.emitTerm(body) }
            out.emit("}")
          }
          case _ => {
            Log.error(s"BUG: While should have exactly 2 children")
            out.emit("???")
          }
        }
      case App => children match {
          case f +: args => {
            out.emitMaybeParenthesized(f)
            out.emitArgTerms(args)
          }
          case _ => {
            Log.error(s"BUG: function application with no children")
            out.emit("???")
          }
        }
      case Plus         => out.emitBinop("+", children)
      case Times        => out.emitBinop("*", children)
      case Minus        => out.emitBinop("-", children)
      case Equals       => out.emitBinop("==", children)
      case And          => out.emitBinop("&&", children)
      case Or           => out.emitBinop("||", children)
      case ArrayNew(ty) => {
        out.emit(s"new Array[${ty.render}]")
        out.emitArgTerms(children)
      }
      case ArrayGet => children match {
          case Seq(arr, i) => {
            out.emitTerm(arr)
            out.emit("(")
            out.emitTerm(i)
            out.emit(")")
          }
          case _ => {
            Log.error(s"BUG: ArrayGet not enough children")
            out.emit("???")
          }
        }
      case ArraySet => children match {
          case Seq(arr, i, x) => {
            out.emitTerm(arr)
            out.emit("(")
            out.emitTerm(i)
            out.emit(") = ")
            out.emitTerm(x)
          }
          case _ => {
            Log.error(s"BUG: ArraySet not enough children")
            out.emit("???")
          }
        }
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

    private def emitBinop(sym: String, args: Seq[Term]): Unit = args match {
      case Seq(x, y) => {
        out.emitMaybeParenthesized(x)
        out.emit(s" $sym ")
        out.emitMaybeParenthesized(y)
      }
      case _ => {
        Log.error(s"BUG: attempted to render binary op with len(args) != 2")
        out.emit(sym)
        emitArgTerms(args)
      }
    }
}
