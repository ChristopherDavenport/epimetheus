package io.chrisdavenport.epimetheus

import cats._
import cats.implicits._

// Mixed into Scala version-specific companion objects
trait NameCommons {
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

  trait SuffixCommons {
    import Name.Suffix

    implicit val nameInstances: Show[Suffix] with Semigroup[Suffix] =
      new Show[Suffix] with Semigroup[Suffix]{
        // Members declared in cats.Show.ContravariantShow
        def show(t: Suffix): String = t.getSuffix
        // Members declared in cats.kernel.Semigroup
        def combine(x: Suffix, y: Suffix): Suffix = x ++ y
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
  }

}
