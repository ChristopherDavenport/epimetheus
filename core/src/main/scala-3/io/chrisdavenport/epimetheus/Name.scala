package io.chrisdavenport.epimetheus

import cats.syntax.all.*

import scala.quoted.*

final class Name private[epimetheus] (val getName: String) extends AnyVal {
  def ++(that: Name): Name = new Name(getName |+| that.getName)
  def suffix(s: Name.Suffix): Name = new Name(getName |+| s.getSuffix)
}

object Name extends NameCommons {

  private[epimetheus] object Macros {
    def nameLiteral(s: Expr[String])(using q: Quotes): Expr[Name] =
      s.value match {
        case Some(s) =>
          Name.impl(s).fold(throw _, _ => '{val name = Name.impl(${Expr(s)}).fold(throw _, identity); name})
        case None =>
          q.reflect.report.error("This method uses a macro to verify that a Name literal is valid. Use Name.impl if you have a dynamic value you want to parse as a name.")
          '{???}
      }
  }

  inline def apply(inline s: String): Name = ${Macros.nameLiteral('s)}

  final class Suffix private[epimetheus] (val getSuffix: String) extends AnyVal {
    def ++(that: Suffix): Suffix = new Suffix(getSuffix |+| that.getSuffix)
  }
  object Suffix extends SuffixCommons {

    private[epimetheus] object Macros {
      def suffixLiteral(s: Expr[String])(using q: Quotes): Expr[Suffix] =
        s.value match {
          case Some(s) =>
            Suffix.impl(s).fold(throw _, _ => '{val suffix = Name.Suffix.impl(${Expr(s)}).fold(throw _, identity); suffix})
          case None =>
            q.reflect.report.error("This method uses a macro to verify that a Name.Suffix literal is valid. Use Name.Suffix.impl if you have a dynamic value you want to parse as a name.")
            '{???}
        }
    }

    inline def apply(inline s: String): Suffix = ${Macros.suffixLiteral('s)}
  }

}
