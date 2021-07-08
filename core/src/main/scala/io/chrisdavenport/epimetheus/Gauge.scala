package io.chrisdavenport.epimetheus

import cats._
import cats.implicits._
import cats.effect._
import io.prometheus.client.{Gauge => JGauge}
import shapeless._

/**
 * Gauge metric, to report instantaneous values.
 *
 * Gauges can go both up and down.
 *
 * An Example With No Labels:
 * {{{
 *  for {
 *    cr <- CollectorRegistry.build
 *    gauge <- Gauge.noLabels(cr, "gauge_value", "Gauge Help")
 *    _ <- gauge.inc
 *    _ <- gauge.dec
 *  } yield ()
 * }}}
 *
 * An Example With Labels:
 * {{{
 *  for {
 *    cr <- CollectorRegistry.build
 *    gauge <- Gauge.labelled(cr, "gauge_value", "Gauge Help", Sized("foo"), {s: String => Sized(s)})
 *    _ <- gauge.label("bar").inc
 *    _ <- gauge.label("bar").dec
 *    _ <- gauge.label("baz").inc
 *    _ <- gauge.label("baz").dec
 * }}}
 *
 * These can be aggregated and processed together much more easily in the Prometheus
 * server than individual metrics for each labelset.
 */
sealed abstract class Gauge[F[_]]{

  /**
   * Access to the current value of this [[Gauge]].
   */
  def get: F[Double]

  /**
   * Decrement the value of this [[Gauge]] by 1.
   */
  def dec: F[Unit]

  /**
   * Decrement the value of this gauge by the provided value.
   *
   * @param d The value to decrease the [[Gauge]] by.
   *
   */
  def decBy(d: Double): F[Unit]

  /**
   * Increment the value of this [[Gauge]] by 1.
   */
  def inc: F[Unit]

  /**
   * Increment the value of this gauge by the provided value.
   *
   * @param d The value to increase the [[Gauge]] by.
   *
   */
  def incBy(d: Double): F[Unit]

  def set(d: Double): F[Unit]

  def mapK[G[_]](fk: F ~> G): Gauge[G] = new Gauge.MapKGauge[F, G](this, fk)
}

/**
 * Gauge Constructors, and Unsafe Gauge Access
 */
object Gauge {

  // Convenience
  def incIn[F[_], A](g: Gauge[F], fa: F[A])(implicit C: MonadCancel[F, _]): F[A] =
    C.bracket(g.inc)(_ => fa)(_ => g.dec)

  def incByIn[F[_], A](g: Gauge[F], fa: F[A], i: Double)(implicit C: MonadCancel[F, _]): F[A] =
    C.bracket(g.incBy(i))(_ => fa)(_ => g.decBy(i))

  def decIn[F[_], A](g: Gauge[F], fa: F[A])(implicit C: MonadCancel[F, _]): F[A] =
    C.bracket(g.dec)(_ => fa)(_ => g.inc)

  def decByIn[F[_], A](g: Gauge[F], fa: F[A], i: Double)(implicit C: MonadCancel[F, _]): F[A] =
    C.bracket(g.decBy(i))(_ => fa)(_ => g.incBy(i))

  // Constructors

  /**
   * Constructor for a [[Gauge]] with no labels.
   *
   * @param cr CollectorRegistry this [[Gauge]] will be registered with
   * @param name The name of the Gauge
   * @param help The help string of the metric
   */
  def noLabels[F[_]: Sync](cr: CollectorRegistry[F], name: Name, help: String): F[Gauge[F]] = for {
    c <- Sync[F].delay(JGauge.build().name(name.getName).help(help))
    out <- Sync[F].delay(c.register(CollectorRegistry.Unsafe.asJava(cr)))
  } yield new NoLabelsGauge[F](out)

  /**
   * Constructor for a labelled [[Gauge]].
   *
   * This generates a specific number of labels via `Sized`, in combination with a function
   * to generate an equally `Sized` set of labels from some type. Values are applied by position.
   *
   * This counter needs to have a label applied to the [[UnlabelledGauge]] in order to
   * be measureable or recorded.
   *
   * @param cr CollectorRegistry this [[Gauge]] will be registred with
   * @param name The name of the [[Gauge]].
   * @param help The help string of the metric
   * @param labels The name of the labels to be applied to this metric
   * @param f Function to take some value provided in the future to generate an equally sized list
   *  of strings as the list of labels. These are assigned to labels by position.
   */
  def labelled[F[_]: Sync, A, N <: Nat](
    cr: CollectorRegistry[F],
    name: Name,
    help: String,
    labels: Sized[IndexedSeq[Label], N],
    f: A => Sized[IndexedSeq[String], N]
  ): F[UnlabelledGauge[F, A]] = for {
      c <- Sync[F].delay(JGauge.build().name(name.getName).help(help).labelNames(labels.map(_.getLabel):_*))
    out <- Sync[F].delay(c.register(CollectorRegistry.Unsafe.asJava(cr)))
  } yield new UnlabelledGaugeImpl[F, A](out, f.andThen(_.unsized))


  private final class NoLabelsGauge[F[_]: Sync] private[Gauge] (
    private[Gauge] val underlying: JGauge
  ) extends Gauge[F] {
    override def get: F[Double] = Sync[F].delay(underlying.get())

    override def dec: F[Unit] = Sync[F].delay(underlying.dec())
    override def decBy(d: Double): F[Unit] = Sync[F].delay(underlying.dec(d))

    override def inc: F[Unit] = Sync[F].delay(underlying.inc())
    override def incBy(d: Double): F[Unit] = Sync[F].delay(underlying.inc(d))

    override def set(d: Double): F[Unit] = Sync[F].delay(underlying.set(d))
  }

  private final class LabelledGauge[F[_]: Sync] private[Gauge] (
    private val underlying: JGauge.Child
  ) extends Gauge[F] {
    override def get: F[Double] = Sync[F].delay(underlying.get())

    override def dec: F[Unit] = Sync[F].delay(underlying.dec())
    override def decBy(d: Double): F[Unit] = Sync[F].delay(underlying.dec(d))

    override def inc: F[Unit] = Sync[F].delay(underlying.inc())
    override def incBy(d: Double): F[Unit] = Sync[F].delay(underlying.inc(d))

    override def set(d: Double): F[Unit] = Sync[F].delay(underlying.set(d))
  }

  private final class MapKGauge[F[_], G[_]](private[Gauge] val base: Gauge[F], fk: F ~> G) extends Gauge[G]{
    override def get: G[Double] = fk(base.get)

    override def dec: G[Unit] = fk(base.dec)
    override def decBy(d: Double): G[Unit] = fk(base.decBy(d))

    override def inc: G[Unit] = fk(base.inc)
    override def incBy(d: Double): G[Unit] = fk(base.incBy(d))

    override def set(d: Double): G[Unit] = fk(base.set(d))
  }


  sealed trait UnlabelledGauge[F[_], A]{
    def label(a: A): Gauge[F]
    def mapK[G[_]](fk: F ~> G): UnlabelledGauge[G, A] = new MapKUnlabelledGauge[F, G, A](this, fk)
    def collector: Collector
  }

  /**
   * Generic Unlabeled Gauge
   *
   * It is necessary to apply a value of type `A` to this
   * gauge to be able to take any measurements.
   */
  final private class UnlabelledGaugeImpl[F[_]: Sync, A] private[epimetheus](
    private[Gauge] val underlying: JGauge,
    private val f: A => IndexedSeq[String],
  ) extends UnlabelledGauge[F, A] {
    override def label(a: A): Gauge[F] =
      new LabelledGauge[F](underlying.labels(f(a):_*))

    override def collector: Collector = Collector.Unsafe.fromJava(underlying)
  }

  final private class MapKUnlabelledGauge[F[_], G[_], A](private[Gauge] val base: UnlabelledGauge[F, A], fk: F ~> G) extends UnlabelledGauge[G, A]{
    override def label(a: A): Gauge[G] = base.label(a).mapK(fk)

    override def collector: Collector = base.collector
  }


  object Unsafe {
    def asJavaUnlabelled[F[_], A](g: UnlabelledGauge[F, A]): JGauge = g match {
      case x: UnlabelledGaugeImpl[_, _] => x.underlying
      case x: MapKUnlabelledGauge[_, _, _] => asJavaUnlabelled(x.base)
    }
    def asJava[F[_]: ApplicativeThrow](c: Gauge[F]): F[JGauge] = c match {
      case _: LabelledGauge[F] => ApplicativeThrow[F].raiseError(new IllegalArgumentException("Cannot Get Underlying Parent with Labels Applied"))
      case n: NoLabelsGauge[F] => n.underlying.pure[F]
      case m: MapKGauge[_, _] => asJava(m.base)
    }
  }
}
