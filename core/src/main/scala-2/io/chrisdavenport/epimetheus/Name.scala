package io.chrisdavenport.epimetheus

import cats.syntax.all._

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

final class Name private[epimetheus] (val getName: String) extends AnyVal {
  def ++(that: Name): Name = new Name(getName |+| that.getName)
  def suffix(s: Name.Suffix): Name = new Name(getName |+| s.getSuffix)
}

object Name extends NameCommons {

  private[epimetheus] class Macros(val c: whitebox.Context) {
    import c.universe._
    def nameLiteral(s: c.Expr[String]): Tree =
      s.tree match {
        case Literal(Constant(s: String))=>
          Name.impl(s)
            .fold(
              e => c.abort(c.enclosingPosition, e.getMessage),
              _ =>
                q"""
                @SuppressWarnings(Array("org.wartremover.warts.Throw"))
                val name = _root_.io.chrisdavenport.epimetheus.Name.impl($s).fold(throw _, _root_.scala.Predef.identity)
                name
                """
            )
        case _ =>
          c.abort(
            c.enclosingPosition,
            s"This method uses a macro to verify that a Name literal is valid. Use Name.impl if you have a dynamic value you want to parse as a name."
          )
      }
  }

  def apply(s: String): Name = macro Macros.nameLiteral

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
                  val suffix = _root_.io.chrisdavenport.epimetheus.Name.Suffix.impl($s).fold(throw _, _root_.scala.Predef.identity)
                  suffix
                  """
              )
          case _ =>
            c.abort(
              c.enclosingPosition,
              s"This method uses a macro to verify that a Name.Suffix literal is valid. Use Name.Suffix.impl if you have a dynamic value you want to parse as a suffix."
            )
        }
    }

    def apply(s: String): Suffix = macro Macros.suffixLiteral
  }

}
