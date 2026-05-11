package lms.codegen

import lms.core.{Op, Type}, Type._, Op._
import lms.pipeline.tree as ast
import lms.pipeline.Name
import lms.util.IndentedWriter
import lms.runtime.Log

class ScalaCodegen(cfg: Config = Config.scalaDefault) extends Backend(cfg) {
  import ast._

  def emit(prog: Program, out: java.io.PrintStream): Unit = {
    val w = makeIndentedWriter(out)
    prog.functions.foreach { (fname, fdef) => w.emitFunction(fname, fdef) }
  }

  private def renderArgs(args: Seq[(Name, Type)]): String = args.map { (name, ty) =>
    s"${name.render(cfg.varPrefix)}: ${ty.render}"
  }.mkString(",")

  extension [A](c: Const[A])
    def render: String = c.v match {
      case s: String => s"\"$s\""
      case x         => s"$x"
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

  extension (t: Term)
    private def isCompound: Boolean = t match {
      case V(_) | E(_: Const[_], Nil) => false
      case _                          => true
    }

  extension (out: IndentedWriter)
    private def invalidTerm(msg: String): Unit = {
      Log.error(msg)
      out.emit("???")
    }

    private def emitFunction(fname: Name, fdef: Function): Unit = {
      val Function(args, outty, body) = fdef

      val argsS = renderArgs(args)
      val header = s"def ${fname.render(cfg.varPrefix)}($argsS): ${outty.render} = {"
      out.emitln(header)
      out.indented { out.emitTerm(body) }
      out.emitln("")
      out.emitln("}\n")
    }

    private def emitTerm(t: Term): Unit = t match {
      case E(op, children) => out.emitCompound(op, children)
      case V(name)         => out.emit(name.render(cfg.varPrefix))
      case Let(x, e1, e2)  => {
        out.emit(s"val ${x.render(cfg.varPrefix)} = ")
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
          case _ => out.invalidTerm("BUG: IfThenElse should have exactly 3 children")
        }
      case App => children match {
          case f +: args => {
            out.emitMaybeParenthesized(f)
            out.emitArgTerms(args)
          }
          case _ => out.invalidTerm("BUG: function application with no children")
        }
      case Negate => children match {
        case Seq(t) => {
          out.emit("-")
          out.emitMaybeParenthesized(t)
        }
        case _ => out.invalidTerm("BUG: negate invalid children")
      }
      case Plus       => out.emitBinop("+", children)
      case Times      => out.emitBinop("*", children)
      case Minus      => out.emitBinop("-", children)
      case Equals     => out.emitBinop("==", children)
      case Lt         => out.emitBinop("<", children)
      case Gt         => out.emitBinop(">", children)
      case Le         => out.emitBinop("<=", children)
      case Ge         => out.emitBinop(">=", children)
      case And        => out.emitBinop("&&", children)
      case Or         => out.emitBinop("||", children)
      case Range      => out.emitBinop("until", children)
      case RangeStart => children match {
          case Seq(arg) => {
            out.emitTerm(arg)
            out.emit(".start")
          }
          case _ => out.invalidTerm(s"BUG: RangeStart invalid children")
        }
      case RangeEnd => children match {
          case Seq(arg) => {
            out.emitTerm(arg)
            out.emit(".end")
          }
          case _ => out.invalidTerm(s"BUG: RangeEnd invalid children")
        }
      case RangeForEach(name) => children match {
          case Seq(st, end, body) => {
            out.emit(s"for (${name.render(cfg.varPrefix)} <- ")
            out.emitMaybeParenthesized(st)
            out.emit(" until ")
            out.emitMaybeParenthesized(end)
            out.emitln(") {")
            out.indented { out.emitTerm(body) }
            out.emitln("")
            out.emitln("}")
          }
          case Seq(_, _, _, _) => out
              .invalidTerm("BUG: RangeForEach first child should be a Var")
          case _ => out.invalidTerm("BUG: RangeForEach invalid children")
        }
      case ArrayNew(ty) => {
        out.emit(s"new Array[${ty.render}]")
        out.emitArgTerms(children)
      }
      case ArrayInit(vals) => {
        out.emit(s"Array(")
        vals.zipWithIndex.foreach { case (v, i) =>
          if i != 0 then out.emit(", ")
          out.emit(Const(v).render)
        }
        out.emit(")")
      }
      case ArrayGet => children match {
          case Seq(arr, i) => {
            out.emitTerm(arr)
            out.emit("(")
            out.emitTerm(i)
            out.emit(")")
          }
          case _ => out.invalidTerm("BUG: ArrayGet invalid children")
        }
      case ArraySet => children match {
          case Seq(arr, i, x) => {
            out.emitTerm(arr)
            out.emit("(")
            out.emitTerm(i)
            out.emit(") = ")
            out.emitTerm(x)
          }
          case _ => out.invalidTerm(s"BUG: ArraySet invalid children")
        }
      case ArrayLength => children match {
          case Seq(arr) => {
            out.emitTerm(arr)
            out.emit(".length")
          }
          case _ => out.invalidTerm("BUG: ArrayLength invalid children")
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
