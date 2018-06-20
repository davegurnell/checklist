package checklist

import scala.reflect.macros.blackbox

class RuleMacros(val c: blackbox.Context) {
  import c.universe._

  def field[A: c.WeakTypeTag, B: c.WeakTypeTag](accessor: c.Tree)(rule: c.Tree): c.Tree = {
    val q"($param) => $rhs" = accessor
    val a = weakTypeOf[A]
    val b = weakTypeOf[B]
    val path = accessorPrefix(accessor)
    val lens = q"""monocle.macros.GenLens[$a].apply[$b]($accessor)"""
    q"${c.prefix}.field($path, $lens)($rule)"
  }

  def fieldWith[A: c.WeakTypeTag, B: c.WeakTypeTag](accessor: c.Tree)(builder: c.Tree): c.Tree = {
    val a = weakTypeOf[A]
    val b = weakTypeOf[B]
    val path = accessorPrefix(accessor)
    val lens = q"""monocle.macros.GenLens[$a].apply[$b]($accessor)"""
    q"${c.prefix}.fieldWith($path, $lens)($builder)"
  }

  private def accessorPrefix(accessor: c.Tree): Tree = {
    def fail = c.abort(c.enclosingPosition, errorMessage(s"Argument is not an accessor function literal."))

    @scala.annotation.tailrec
    def unpack(expr: Tree, accum: List[String]): Tree =
      expr match {
        case Ident(_)               => accum.foldRight(q"_root_.checklist.PNil" : Tree)((a, b) => q"$a :: $b")
        case Select(a, TermName(b)) => unpack(a, b :: accum)
        case _                      => fail
      }

    accessor match {
      case q"($param) => $rhs" => unpack(rhs, Nil)
      case other               => fail
    }
  }

  private def errorMessage(prefix: String) =
    s"""
     |$prefix
     |
     |The argument must be a function literal of the form `_.field`.
     |Alternatively use the `rule.field(path, lens)(rule)` method,
     |which allows you to specify the field name manually.
     """.stripMargin
}
