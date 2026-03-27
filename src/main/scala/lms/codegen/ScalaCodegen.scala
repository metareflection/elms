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

  extension (c: Const)
    def render: String = c.typ match { case UNIT | INT | BOOL | STRING | CHAR => s"${c.v}" }

  extension (ty: Type)
    private def render: String = ty match {
      case UNIT   => "Unit"
      case INT    => "Int"
      case BOOL   => "Boolean"
      case CHAR => "Char"
      case STRING => "String"
    }

  extension (t: Term)
    private def isCompound: Boolean = t match {
      case V(_) | E(_: Const, Nil) => false
      case _                       => true
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
      case Function(args, outty, body) => {
        // CR cwong: TODO
        Log.error("TODO: lambdas")
      }
    }

    private def emitMaybeParenthesized(t: Term): Unit = {
      if t.isCompound then out.emit("(")
      out.emitTerm(t)
      if t.isCompound then out.emit(")")
    }

    private def emitKnownFunctionCall(name: String, args: Seq[Term]): Unit = {
      out.emit(name)
      out.emitArgTerms(args)
    }

    private def emitCompound(op: Op, children: Seq[Term]): Unit = op match {
      case c: Const   => out.emit(c.render)
      case IfThenElse => children match {
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
            Log.error(s"BUG: attempted to render function application with no children")
            out.emit("???")
          }
        }
      case Plus   => out.emitBinop("+", children)
      case Times  => out.emitBinop("*", children)
      case Minus  => out.emitBinop("-", children)
      case Equals => out.emitBinop("==", children)
      case And    => out.emitBinop("&&", children)
      case Or     => out.emitBinop("||", children)
      case StringLength => out.emitKnownFunctionCall("String.length", children)
      case StringTake => out.emitKnownFunctionCall("String.take", children)
      case StringDrop => out.emitKnownFunctionCall("String.drop", children)
      case StringStartsWith => out.emitKnownFunctionCall("String.startsWith", children)
      case StringEndsWith => out.emitKnownFunctionCall("String.endsWith", children)
      case StringCharAt => out.emitKnownFunctionCall("String.charAt", children)
      case StringSubstring => out.emitKnownFunctionCall("String.substring", children)
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
