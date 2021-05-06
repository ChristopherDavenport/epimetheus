package io.chrisdavenport.epimetheus

import cats._
import cats.implicits._

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

final class Name private(val getName: String) extends AnyVal {
  def ++(that: Name): Name = new Name(getName |+| that.getName)
  def suffix(s: Name.Suffix): Name = new Name(getName |+| s.getSuffix)
}

object Name {
  implicit val nameInstances: Show[Name] with Semigroup[Name] with Eq[Name] with Order[Name] =
    new Show[Name] with Semigroup[Name] with Eq[Name] with Order[Name]{
      // Members declared in cats.Show.ContravariantShow
      def show(t: Name): String = t.getName
      // Members declared in cats.kernel.Semigroup
      def combine(x: Name, y: Name): Name = x ++ y
      // Members declared in cats.kernel.Order
      override def compare(x: Name, y: Name): Int =
        Order[String].compare(x.getName, y.getName)
      // Members declared in cats.kernel.Eq
      override def eqv(x: Name, y: Name): Boolean =
        Eq[String].eqv(x.getName, y.getName)
    }

  private val reg = "([a-zA-Z_:][a-zA-Z0-9_:]*)".r

  private[Name] class Macros(val c: whitebox.Context) {
    import c.universe._
    def nameLiteral(s: c.Expr[String]): Tree =
      s.tree match {
        case Literal(Constant(s: String))=>
            impl(s)
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

  def impl(s: String): Either[IllegalArgumentException, Name] = s match {
    case reg(string) => Either.right(new Name(string))
    case _ => Either.left(
      new IllegalArgumentException(
        s"Input String - $s does not match regex - ([a-zA-Z_:][a-zA-Z0-9_:]*)"
      )
    )
  }
  def implF[F[_]: ApplicativeThrow](s: String): F[Name] = {
    impl(s).liftTo[F]
  }

  def apply(s: String): Name = macro Macros.nameLiteral

  final class Suffix private(val getSuffix: String) extends AnyVal {
    def ++(that: Suffix): Suffix = new Suffix(getSuffix |+| that.getSuffix)
  }
  object Suffix {

    implicit val nameInstances: Show[Suffix] with Semigroup[Suffix] =
      new Show[Suffix] with Semigroup[Suffix]{
        // Members declared in cats.Show.ContravariantShow
        def show(t: Suffix): String = t.getSuffix
        // Members declared in cats.kernel.Semigroup
        def combine(x: Suffix, y: Suffix): Suffix = x ++ y
      }

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

    private val sufreg = "([a-zA-Z0-9_:]*)".r

    def impl(s: String): Either[IllegalArgumentException, Suffix] = s match {
      case sufreg(string) => Either.right(new Suffix(string))
      case _ => Either.left(
        new IllegalArgumentException(
          s"Input String - $s does not match regex - ([a-zA-Z0-9_:]*)"
        )
      )
    }

    def implF[F[_]: ApplicativeThrow](s: String): F[Suffix] = {
      impl(s).liftTo[F]
    }

    def apply(s: String): Suffix = macro Macros.suffixLiteral

  }

}
