package io.chrisdavenport.epimetheus

import cats.syntax.all.*

import scala.quoted.*

final class Label private[epimetheus] (val getLabel: String) extends AnyVal {
  def ++(that: Label): Label = new Label(getLabel |+| that.getLabel)
  def suffix(s: Label.Suffix): Label = new Label(getLabel |+| s.getSuffix)
}

object Label extends LabelCommons {

  private[epimetheus] object Macros {
    def labelLiteral(s: Expr[String])(using q: Quotes): Expr[Label] =
      s.value match {
        case Some(s) =>
          Label.impl(s).fold(throw _, _ => '{val label = Label.impl(${Expr(s)}).fold(throw _, identity); label})
        case None =>
          q.reflect.report.error("This method uses a macro to verify that a Label literal is valid. Use Label.impl if you have a dynamic value you want to parse as a label.")
          '{???}
      }
  }

  inline def apply(inline s: String): Label = ${Macros.labelLiteral('s)}

  final class Suffix private[epimetheus] (val getSuffix: String) extends AnyVal {
    def ++(that: Suffix): Suffix = new Suffix(getSuffix |+| that.getSuffix)
  }
  object Suffix extends SuffixCommons {

    private[epimetheus] object Macros {
      def suffixLiteral(s: Expr[String])(using q: Quotes): Expr[Suffix] =
        s.value match {
          case Some(s) =>
            Suffix.impl(s).fold(throw _, _ => '{val suffix = Label.Suffix.impl(${Expr(s)}).fold(throw _, identity); suffix})
          case None =>
            q.reflect.report.error("This method uses a macro to verify that a Label.Suffix literal is valid. Use Label.Suffix.impl if you have a dynamic value you want to parse as a name.")
            '{???}  
        }
    }

    inline def apply(inline s: String): Suffix = ${Macros.suffixLiteral('s)}
  }

}



