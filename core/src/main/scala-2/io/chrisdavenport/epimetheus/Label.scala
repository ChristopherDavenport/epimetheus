package io.chrisdavenport.epimetheus

import cats.syntax.all._

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

final class Label private[epimetheus](val getLabel: String) extends AnyVal {
  def ++(that: Label): Label = new Label(getLabel |+| that.getLabel)
  def suffix(s: Label.Suffix): Label = new Label(getLabel |+| s.getSuffix)
}

object Label extends LabelCommons {

  private[epimetheus] class Macros(val c: whitebox.Context) {
    import c.universe._
    def labelLiteral(s: c.Expr[String]): Tree =
      s.tree match {
        case Literal(Constant(s: String))=>
          Label.impl(s)
            .fold(
              e => c.abort(c.enclosingPosition, e.getMessage),
              _ =>
                q"""
                @SuppressWarnings(Array("org.wartremover.warts.Throw"))
                val label = _root_.io.chrisdavenport.epimetheus.Label.impl($s).fold(throw _, _root_.scala.Predef.identity)
                label
                """
            )
        case _ =>
          c.abort(
            c.enclosingPosition,
            s"This method uses a macro to verify that a Label literal is valid. Use Label.impl if you have a dynamic value you want to parse as a label."
          )
      }
  }

  def apply(s: String): Label = macro Macros.labelLiteral

  final class Suffix private[epimetheus] (val getSuffix: String) extends AnyVal {
    def ++(that: Suffix): Suffix = new Suffix(getSuffix |+| that.getSuffix)
  }
  object Suffix extends SuffixCommons {

    private[Suffix] class Macros(val c: whitebox.Context) {
      import c.universe._
      def suffixLiteral(s: c.Expr[String]): Tree =
        s.tree match {
          case Literal(Constant(s: String))=>
            impl(s)
              .fold(
                e => c.abort(c.enclosingPosition, e.getMessage),
                _ =>
                  q"""
                  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
                  val suffix = _root_.io.chrisdavenport.epimetheus.Label.Suffix.impl($s).fold(throw _, _root_.scala.Predef.identity)
                  suffix
                  """
              )
          case _ =>
            c.abort(
              c.enclosingPosition,
              s"This method uses a macro to verify that a Label.Suffix literal is valid. Use Label.Suffix.impl if you have a dynamic value you want to parse as a suffix."
            )
        }
    }

    def apply(s: String): Suffix = macro Macros.suffixLiteral
  }

}



