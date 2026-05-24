package elms.codegen

import elms.core.*
import elms.core.Op.*
import elms.core.Name
import elms.core.tree as ast
import elms.core.tree.View
import elms.core.given
import elms.util.IndentedWriter
import elms.util.collection.*
import elms.runtime.Log

class CCodegen(cfg: Config = Config.cDefault) extends Backend(cfg) {
  import ast._

  def emit(prog: Program, out: java.io.PrintStream): Unit = {
    val w = makeIndentedWriter(out)

    w.emitln("#include <stdbool.h>")
    w.emitln("#include <stdlib.h>")
    w.emitln("")

    prog.staticData.foreach { (name, data) => w.emitNamedStaticData(name, data) }
    if prog.staticData.nonEmpty then w.emitln("")

    val topEnv: Env = prog.functions.map { (fname, fdef) =>
      fname -> functionType(fdef)
    }.toMap

    prog.functions.foreach { (fname, fdef) => w.emitFunction(topEnv)(fname, fdef) }
  }

  private def renderArgs(args: Seq[(Name, Type)]): String = args.map { (name, ty) =>
    s"${ty.renderParam} ${name.render(cfg.varPrefix)}"
  }.mkString(", ")

  extension [A: Primitive](x: A)
    def render: String = summon[Primitive[A]] match {
      case UNIT   => s"/* unit */"
      case INT    => s"$x"
      case BOOL   => if x.asInstanceOf[Boolean] then "true" else "false"
      case CHAR   => s"'${escapeChar(x.asInstanceOf[Char])}'"
      case STRING => s"\"${escapeString(x.asInstanceOf[String])}\""
    }

  private def escapeChar(c: Char): String = c match {
    case '\n'             => "\\n"
    case '\r'             => "\\r"
    case '\t'             => "\\t"
    case '\b'             => "\\b"
    case '\f'             => "\\f"
    case '\''             => "\\'"
    case '"'              => "\\\""
    case '\\'             => "\\\\"
    case c if c.isControl => f"\\x${c.toInt}%02x"
    case c                => c.toString
  }

  private def escapeString(s: String): String = s.flatMap {
    case '\n'             => "\\n"
    case '\r'             => "\\r"
    case '\t'             => "\\t"
    case '\b'             => "\\b"
    case '\f'             => "\\f"
    case '"'              => "\\\""
    case '\\'             => "\\\\"
    case c if c.isControl => f"\\x${c.toInt}%02x"
    case c                => c.toString
  }

  extension (ty: Type)
    private def render: String = ty match {
      case UNIT         => "void"
      case INT          => "int"
      case BOOL         => "bool"
      case CHAR         => "char"
      case STRING       => "const char *"
      case ARRAY(t)     => s"${t.render} *"
      case STRUCT(repr) => s"struct ${repr.name} *"
    }

    private def renderParam: String = ty.render

    private def renderElement: String = ty match {
      case ARRAY(t) => t.render
      case _        => ty.render
    }

  extension (t: Term)
    private def isSimpleExpr: Boolean = t match {
      case V(_) | E(_: Const[_], Nil) => true
      case View.ArrayGet(_, _)        => true
      case View.ArrayLength(_)        => true
      case View.App(_, _)             => true
      case _                          => false
    }

  private type Env = Map[Name, Type]

  // CR-soon cwong: We can probably perform `inferType` at the same time
  // we walk the tree to print it. This would be a quadratic speedup in term
  // size, but I'm reasonably sure that it won't be noticable in practice.

  private def inferType(env: Env)(term: Term): Option[Type] = term match {
    case V(name) => env.get(name)

    case View.Const(const) => Some(const.prim)

    case Let(x, e1, e2) =>
      inferType(env)(e1).flatMap { ty1 => inferType(env + (x -> ty1))(e2) }

    case View.IfThenElse(_, tthen, telse) =>
      (inferType(env)(tthen), inferType(env)(telse)) match {
        case (Some(a), Some(b)) if a == b => Some(a)
        case _                            => None
      }

    case View.Negate(_)                                        => Some(INT)
    case View.Plus(_, _) | View.Times(_, _) | View.Minus(_, _) => Some(INT)
    case View.Equals(_, _) | View.Lt(_, _) | View.Gt(_, _) | View.Le(_, _) | View
          .Ge(_, _) | View.And(_, _) | View.Or(_, _) => Some(BOOL)
    case View.Range(_, _)                      => None
    case View.RangeStart(_) | View.RangeEnd(_) => Some(INT)

    case View.VarNew(ty, t) => Some(ty)
    case View.VarGet(t)     => inferType(env)(t)
    case View.VarSet(_, _)  => Some(UNIT)

    case View.RangeForEach(_, _, _, _) => Some(UNIT)
    case View.While(_, _)              => Some(UNIT)

    case View.ArrayNew(ty, _) => Some(ARRAY(ty))

    case View.ArrayGet(arr, _) => inferType(env)(arr) match {
        case Some(ARRAY(elemTy)) => Some(elemTy)
        case _                   => None
      }
    case View.ArraySet(_, _, _)         => Some(UNIT)
    case View.ArrayLength(_)            => Some(INT)
    case View.StructGet(repr, _, field) => repr.get(field)
    case View.StructSet(_, _, _)        => Some(UNIT)

    case View.App(f, x) => inferType(env)(f) match {
        case Some(ARROW(_, out)) => Some(out)
        case _                   => None
      }

    case Function(args, outty, _) => Some(ARROW(args.map(_._2), outty))

    case E(_, _) => None
  }

  private def functionType(fdef: Function): Type =
    ARROW(fdef.args.map(_._2), fdef.outty)

  extension (out: IndentedWriter)
    private def invalidTerm(msg: String): Unit = {
      Log.error(msg)
      out.emit("/* ERROR: ")
      out.emit(msg.replace("*/", "* /"))
      out.emit(" */")
    }

    private def emitFunction(topEnv: Env)(fname: Name, fdef: Function): Unit = {
      val Function(args, outty, body) = fdef

      val env = args.toMap ++ topEnv
      val argsS = renderArgs(args)

      out.emitln(s"${outty.render} ${fname.render(cfg.varPrefix)}($argsS) {")
      out.indented {
        if outty == UNIT then out.emitStmt(env)(body)
        else out.emitReturnTerm(env, outty)(body)
      }
      out.emitln("}")
      out.emitln("")
    }

    private def emitMaybeParenthesizedExpr(env: Env)(t: Term): Unit = {
      if t.isSimpleExpr then out.emitExpr(env)(t)
      else {
        out.emit("(")
        out.emitExpr(env)(t)
        out.emit(")")
      }
    }

    private def emitAssign(env: Env)(x: Name, e: Term): Option[Type] = {
      val ty = inferType(env)(e)

      ty match {
        case Some(UNIT) => out.emitStmt(env)(e)
        case Some(ty)   => {
          out.emit(s"${ty.render} ${x.render(cfg.varPrefix)} = ")
          out.emitExpr(env)(e)
          out.emitln(";")
        }
        case None => {
          out.invalidTerm(s"Could not infer C type for let-bound term: $e")
          out.emitln(";")
        }
      }

      ty
    }

    private def emitExpr(env: Env)(term: Term): Unit = term match {
      case V(name) => out.emit(name.render(cfg.varPrefix))

      case View.Const(const) => out.emit(const.value.render(using const.prim))

      case View.App(f, args) => {
        out.emitMaybeParenthesizedExpr(env)(f)
        out.emitArgTerms(env)(args)
      }

      case View.Negate(t) => {
        out.emit("-")
        out.emitMaybeParenthesizedExpr(env)(t)
      }

      case View.Plus(x, y)   => out.emitBinop(env)("+", x, y)
      case View.Times(x, y)  => out.emitBinop(env)("*", x, y)
      case View.Minus(x, y)  => out.emitBinop(env)("-", x, y)
      case View.Equals(x, y) => out.emitBinop(env)("==", x, y)
      case View.Lt(x, y)     => out.emitBinop(env)("<", x, y)
      case View.Gt(x, y)     => out.emitBinop(env)(">", x, y)
      case View.Le(x, y)     => out.emitBinop(env)("<=", x, y)
      case View.Ge(x, y)     => out.emitBinop(env)(">=", x, y)
      case View.And(x, y)    => out.emitBinop(env)("&&", x, y)
      case View.Or(x, y)     => out.emitBinop(env)("||", x, y)

      case View.IfThenElse(guard, tthen, telse) => {
        out.emit("(")
        out.emitExpr(env)(guard)
        out.emit(" ? ")
        out.emitExpr(env)(tthen)
        out.emit(" : ")
        out.emitExpr(env)(telse)
        out.emit(")")
      }

      case View.VarNew(ty, t) => out.emitExpr(env)(t)
      case View.VarGet(t)     => out.emitExpr(env)(t)
      case View.VarSet(_, _)  => {
        out.emit("({")
        out.emitStmt(env)(term)
        out.emit("})")
      }

      case View.ArrayNew(ty, t) => {
        out.emit(s"(${ARRAY(ty).render})malloc(sizeof(${ty.render}) * ")
        out.emitExpr(env)(t)
        out.emit(")")
      }

      case View.ArrayGet(arr, i) => {
        out.emitExpr(env)(arr)
        out.emit("[")
        out.emitExpr(env)(i)
        out.emit("]")
      }

      case View.ArraySet(_, _, _) => {
        out.emit("({")
        out.emitStmt(env)(term)
        out.emit("})")
      }

      case View.ArrayLength(arr) => out.invalidTerm(
          s"C backend cannot emit array length without explicit length metadata: $arr"
        )

      case View.StructGet(repr, t, field) => {
        out.emitMaybeParenthesizedExpr(env)(t)
        out.emit(s"->$field")
      }

      case View.StructSet(_, _, _) => {
        out.emit("({")
        out.emitStmt(env)(term)
        out.emit("})")
      }

      case View.RangeStart(t) => out
          .invalidTerm(s"C backend has no first-class range value: $t")

      case View.RangeEnd(t) => out
          .invalidTerm(s"C backend has no first-class range value: $t")

      case View.Range(_, _) => out.invalidTerm(
          s"C backend only supports ranges directly in foreach loops: $term"
        )

      case Let(x, e1, e2) => out.emitLetExpr(env)(x, e1, e2)

      case View.RangeForEach(_, _, _, _) => out
          .invalidTerm(s"for-loop cannot be emitted as a C expression: $term")

      case View.While(_, _) => out
          .invalidTerm(s"while-loop cannot be emitted as a C expression: $term")

      case Function(_, _, _) => out
          .invalidTerm(s"C backend does not support anonymous functions/lambdas: $term")

      case E(_, _) => out.invalidTerm(s"Got invalid expression term: $term")
    }

    // CR-someday cwong: There is a decent amount of duplication when emitting
    // the same term in statement or expr position.

    private def emitStmt(env: Env)(term: Term): Unit = term match {
      case Let(x, e1, e2) => {
        val ty = emitAssign(env)(x, e1)
        out.emitStmt(env.setOrRemove(x, ty))(e2)
      }

      case View.IfThenElse(guard, tthen, telse) => {
        out.emit("if (")
        out.emitExpr(env)(guard)
        out.emitln(") {")
        out.indented { out.emitStmt(env)(tthen) }
        out.emitln("} else {")
        out.indented { out.emitStmt(env)(telse) }
        out.emitln("}")
      }

      case View.RangeForEach(name, st, end, body) => {
        out.emit(s"for (int ${name.render(cfg.varPrefix)} = ")
        out.emitExpr(env)(st)
        out.emit("; ")
        out.emit(name.render(cfg.varPrefix))
        out.emit(" < ")
        out.emitExpr(env)(end)
        out.emit("; ")
        out.emit(name.render(cfg.varPrefix))
        out.emitln("++) {")
        out.indented { out.emitStmt(env + (name -> INT))(body) }
        out.emitln("}")
      }

      case View.While(guard, body) => {
        out.emit("while (")
        out.emitExpr(env)(guard)
        out.emitln(") {")
        out.indented { out.emitStmt(env)(body) }
        out.emitln("}")
      }

      case View.ArraySet(arr, i, x) => {
        out.emitExpr(env)(arr)
        out.emit("[")
        out.emitExpr(env)(i)
        out.emit("] = ")
        out.emitExpr(env)(x)
        out.emitln(";")
      }

      case View.VarSet(x, v) => {
        out.emitExpr(env)(x)
        out.emit(" = ")
        out.emitExpr(env)(v)
        out.emit(";")
      }

      case View.StructSet(x, field, v) => {
        out.emitMaybeParenthesizedExpr(env)(x)
        out.emit(s"->$field = ")
        out.emitExpr(env)(v)
        out.emit(";")
      }

      case View.App(_, _) => {
        out.emitExpr(env)(term)
        out.emitln(";")
      }

      case View.Const[Unit](const) => out.emitln(";")

      case Function(_, _, _) => {
        out
          .invalidTerm(s"C backend does not support anonymous functions/lambdas: $term")
        out.emitln(";")
      }

      case _ => {
        out.emitExpr(env)(term)
        out.emitln(";")
      }
    }

    private def emitReturnTerm(env: Env, outty: Type)(term: Term): Unit = term match {
      case Let(x, e1, e2) => {
        val ty = emitAssign(env)(x, e1).getOrElse(UNIT)
        out.emitReturnTerm(env + (x -> ty), outty)(e2)
      }

      case View.IfThenElse(guard, tthen, telse) => {
        out.emit("if (")
        out.emitExpr(env)(guard)
        out.emitln(") {")
        out.indented { out.emitReturnTerm(env, outty)(tthen) }
        out.emitln("} else {")
        out.indented { out.emitReturnTerm(env, outty)(telse) }
        out.emitln("}")
      }

      case View.RangeForEach(_, _, _, _) => {
        out.invalidTerm(s"Cannot return the result of a for-loop in C: $term")
        out.emitln(";")
        out.emitReturnFallback(outty)
      }

      case View.While(_, _) => {
        out.invalidTerm(s"Cannot return the result of a while-loop in C: $term")
        out.emitln(";")
        out.emitReturnFallback(outty)
      }

      case Function(_, _, _) => {
        out
          .invalidTerm(s"C backend does not support anonymous functions/lambdas: $term")
        out.emitln(";")
        out.emitReturnFallback(outty)
      }

      case _ => {
        out.emit("return ")
        out.emitExpr(env)(term)
        out.emitln(";")
      }
    }

    private def emitReturnFallback(outty: Type): Unit = outty match {
      case UNIT       => out.emitln("return;")
      case INT | CHAR => out.emitln("return 0;")
      case BOOL       => out.emitln("return false;")
      case STRING     => out.emitln("return NULL;")
      case ARRAY(_)   => out.emitln("return NULL;")
      case STRUCT(_)  => out.emitln("return NULL;")
    }

    private def emitLetExpr(env: Env)(x: Name, e1: Term, e2: Term): Unit = {
      out.emitln("({")
      out.indented {
        val ty = emitAssign(env)(x, e1)
        out.emitExprResult(env.setOrRemove(x, ty))(e2)
      }
      out.emit("})")
    }

    private def emitExprResult(env: Env)(term: Term): Unit = term match {
      case Let(x, e1, e2) => {
        val ty = emitAssign(env)(x, e1)
        out.emitExprResult(env.setOrRemove(x, ty))(e2)
      }

      case View.IfThenElse(guard, tthen, telse) => {
        out.emit("if (")
        out.emitExpr(env)(guard)
        out.emitln(") {")
        out.indented { out.emitExprResult(env)(tthen) }
        out.emitln("} else {")
        out.indented { out.emitExprResult(env)(telse) }
        out.emitln("}")
      }

      case View.RangeForEach(name, st, end, body) => {
        out.emit(s"for (int ${name.render(cfg.varPrefix)} = ")
        out.emitExpr(env)(st)
        out.emit("; ")
        out.emit(name.render(cfg.varPrefix))
        out.emit(" < ")
        out.emitExpr(env)(end)
        out.emit("; ")
        out.emit(name.render(cfg.varPrefix))
        out.emitln("++) {")
        out.indented { out.emitStmt(env + (name -> INT))(body) }
        out.emitln("}")
        out.emitln("/* unit */;")
      }

      case View.While(guard, body) => {
        out.emit("while (")
        out.emitExpr(env)(guard)
        out.emitln(") {")
        out.indented { out.emitStmt(env)(body) }
        out.emitln("}")
        out.emitln("/* unit */;")
      }

      case View.ArraySet(arr, i, x) => {
        out.emitExpr(env)(arr)
        out.emit("[")
        out.emitExpr(env)(i)
        out.emit("] = ")
        out.emitExpr(env)(x)
        out.emitln(";")
      }

      case Function(_, _, _) => {
        out
          .invalidTerm(s"C backend does not support anonymous functions/lambdas: $term")
        out.emitln(";")
      }

      case _ => {
        out.emitExpr(env)(term)
        out.emitln(";")
      }
    }

    private def emitArgTerms(env: Env)(args: Seq[Term]): Unit = {
      out.emit("(")
      args.zipWithIndex.foreach { case (t, i) =>
        if i != 0 then out.emit(", ")
        out.emitExpr(env)(t)
      }
      out.emit(")")
    }

    private def emitBinop(env: Env)(sym: String, x: Term, y: Term): Unit = {
      out.emitMaybeParenthesizedExpr(env)(x)
      out.emit(s" $sym ")
      out.emitMaybeParenthesizedExpr(env)(y)
    }

    private def emitNamedStaticData(name: Name, data: StaticData): Unit = data match {
      case s @ Scalar(x) =>
        val ty = s.prim
        out.emitln(s"static const ${ty.render} ${name.render(cfg.varPrefix)} = ${x
            .render(using s.prim)};")

      case SArray(elemTy, elems) =>
        out.emit(s"static const ${elemTy.render} ${name.render(cfg.varPrefix)}[] = ")
        out.emit(renderStaticDataInitializer(data))
        out.emitln(";")
    }

    private def renderStaticDataInitializer(data: StaticData): String = data match {
      case s @ Scalar(x) => x.render(using s.prim)

      case SArray(_, elems) => elems.map(renderStaticDataInitializer)
          .mkString("{", ", ", "}")
    }
}
