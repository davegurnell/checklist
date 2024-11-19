package checklist

import monocle.{Focus, Lens}
import scala.quoted.*

object RuleMacros {
  def field[A: Type, B: Type](self: Expr[Rule[A, A]], accessor: Expr[A => B], rule: Expr[Rule[B, B]])(using Quotes): Expr[Rule[A, A]] =
    new RuleMacrosImpl().field(self, accessor, rule)

  def fieldWith[A: Type, B: Type](self: Expr[Rule[A, A]], accessor: Expr[A => B], builder: Expr[A => Rule[B, B]])(using Quotes): Expr[Rule[A, A]] =
    new RuleMacrosImpl().fieldWith(self, accessor, builder)
}

class RuleMacrosImpl()(using quotes: Quotes) {
  import quotes.reflect.*

  def field[A: Type, B: Type](self: Expr[Rule[A, A]], accessor: Expr[A => B], rule: Expr[Rule[B, B]]): Expr[Rule[A, A]] = {
    val path = accessorToPath(accessor.asTerm)
    val lens = '{ Focus[A](${accessor}) }.asInstanceOf[Expr[Lens[A, B]]]
    '{ $self.field($path, $lens)($rule) }
  }

  def fieldWith[A: Type, B: Type](self: Expr[Rule[A, A]], accessor: Expr[A => B], builder: Expr[A => Rule[B, B]]): Expr[Rule[A, A]] = {
    val path = accessorToPath(accessor.asTerm)
    val lens = '{ Focus[A](${accessor}) }.asInstanceOf[Expr[Lens[A, B]]]
    '{ $self.fieldWith($path, $lens)($builder) }
  }

  private def accessorToPath(accessor: Term): Expr[Path] =
    accessor match {
      case Inlined(_, _, Block(List(DefDef(_, _, _, Some(body))), _)) => selectToPath(body)
      // case Lambda(List(ValDef(_, _, _)), body) => selectToPath(body)
      case other => report.throwError(s"Argument is not an accessor function literal: ${other}")
    }

  private def selectToPath(select: Term): Expr[Path] =
    select match {
      case Ident(name) => '{ PNil }
      case Select(qualifier, name) => '{ ${selectToPath(qualifier)} ++ PField(${Expr(name)}) }
      case other => report.throwError(s"Unsupported tree structure for selector: ${other}")
    }
}
