package io.chrisdavenport.epimetheus

import cats._
import cats.implicits._

trait LabelCommons {
  implicit val labelInstances: Show[Label] with Semigroup[Label] with Eq[Label] with Order[Label] =
    new Show[Label] with Semigroup[Label] with Eq[Label] with Order[Label]{
      // Members declared in cats.Show.ContravariantShow
      def show(t: Label): String = t.getLabel
      // Members declared in cats.kernel.Semigroup
      def combine(x: Label, y: Label): Label = x ++ y
      // Members declared in cats.kernel.Order
      override def compare(x: Label, y: Label): Int =
        Order[String].compare(x.getLabel, y.getLabel)
      // Members declared in cats.kernel.Eq
      override def eqv(x: Label, y: Label): Boolean =
        Eq[String].eqv(x.getLabel, y.getLabel)
    }

  /** See [[https://prometheus.io/docs/concepts/data_model/#metric-names-and-labels]] */
  private val reg = "([a-zA-Z_][a-zA-Z0-9_]*)".r

  def impl(s: String): Either[IllegalArgumentException, Label] = s match {
    case reg(string) => Either.right(new Label(string))
    case _ => Either.left(
      new IllegalArgumentException(
        s"Input String - $s does not match regex - $reg"
      )
    )
  }
  def implF[F[_]: ApplicativeThrow](s: String): F[Label] = {
    impl(s).liftTo[F]
  }


  trait SuffixCommons {

    import Label.Suffix

    implicit val labelInstances: Show[Suffix] with Semigroup[Suffix] =
      new Show[Suffix] with Semigroup[Suffix]{
        // Members declared in cats.Show.ContravariantShow
        def show(t: Suffix): String = t.getSuffix
        // Members declared in cats.kernel.Semigroup
        def combine(x: Suffix, y: Suffix): Suffix = x ++ y
      }

    private val sufreg = "([a-zA-Z0-9_]*)".r

    def impl(s: String): Either[IllegalArgumentException, Suffix] = s match {
      case sufreg(string) => Either.right(new Suffix(string))
      case _ => Either.left(
        new IllegalArgumentException(
          s"Input String - $s does not match regex - $sufreg"
        )
      )
    }

    def implF[F[_]: ApplicativeThrow](s: String): F[Suffix] = {
      impl(s).liftTo[F]
    }
  }

}
