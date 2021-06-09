package io.chrisdavenport.epimetheus

import cats._
import cats.effect._
import cats.implicits._
import io.prometheus.client.{Histogram => JHistogram}

import scala.concurrent.duration._

/**
 * Histogram metric, to track distributions of events.
 *
 * Note: Each bucket is one timeseries. Many buckets and/or many dimensions with labels
 * can produce large amount of time series, that may cause performance problems.
 *
 * The default buckets are intended to cover a typical web/rpc request from milliseconds to seconds.
 *
 */
sealed abstract class Histogram[F[_]]{

  /**
   * Persist an observation into this [[Histogram]]
   *
   * @param d The observation to persist
   */
  def observe(d: Double): F[Unit]

  def mapK[G[_]](fk: F ~> G): Histogram[G] = new Histogram.MapKHistogram[F, G](this, fk)

}

/**
 * Histogram Constructors, Convenience Methods and Unsafe Histogram Access
 *
 * Convenience function exposed here will also be exposed as implicit syntax
 * enhancements on the Histogram
 */
object Histogram {

  // Convenience ----------------------------------------------------
  // Since these methods are not ex

  /**
   * Persist a timed value into this [[Histogram]]
   *
   * @param h Histogram
   * @param fa The action to time
   * @param unit The unit of time to observe the timing in. Default Histogram buckets
   *  are optimized for `SECONDS`.
   */
  def timed[F[_] : Clock, A](h: Histogram[F], fa: F[A], unit: TimeUnit)(implicit C: MonadCancel[F, _]): F[A] =
    C.bracket(Clock[F].monotonic)((_: FiniteDuration) => fa) { (start: FiniteDuration) =>
      Clock[F].monotonic.flatMap(now => h.observe((now - start).toUnit(unit)))
    }

  /**
   * Persist a timed value into this [[Histogram]] in unit Seconds. This is exposed.
   * since default buckets are in seconds it makes sense the general case will be to
   * match the default buckets.
   *
   * @param h Histogram
   * @param fa The action to time
   */
  def timedSeconds[F[_] : Clock, A](h: Histogram[F], fa: F[A])(implicit C: MonadCancel[F, _]): F[A] =
    timed(h, fa, SECONDS)

  // Constructors ---------------------------------------------------
  /**
   * Default Buckets
   *
   * Intended to cover a typical web/rpc request from milliseconds to seconds.
   */
  val defaults = List(.005, .01, .025, .05, .075, .1, .25, .5, .75, 1, 2.5, 5, 7.5, 10)

  /**
   * Constructor for a [[Histogram]] with no labels. Default buckets are [[defaults]]
   * and are intended to cover a typical web/rpc request from milliseconds to seconds.
   *
   * @param cr CollectorRegistry this [[Histogram]] will be registered with
   * @param name The name of the Histogram
   * @param help The help string of the metric
   */
  def noLabels[F[_]: Sync](
    cr: CollectorRegistry[F],
    name: Name,
    help: String
  ): F[Histogram[F]] =
    noLabelsBuckets(cr, name, help, defaults:_*)

  /**
   * Constructor for a [[Histogram]] with no labels. Default buckets are [[defaults]]
   * and are intended to cover a typical web/rpc request from milliseconds to seconds.
   *
   * @param cr CollectorRegistry this [[Histogram]] will be registered with
   * @param name The name of the Gauge
   * @param help The help string of the metric
   * @param buckets The buckets to measure observations by.
   */
  def noLabelsBuckets[F[_]: Sync](
    cr: CollectorRegistry[F],
    name: Name,
    help: String,
    buckets: Double*
  ): F[Histogram[F]] = for {
    c <- Sync[F].delay(
      JHistogram.build()
      .name(name.getName)
      .help(help)
      .buckets(buckets:_*)
    )
    out <- Sync[F].delay(c.register(CollectorRegistry.Unsafe.asJava(cr)))
  } yield new NoLabelsHistogram[F](out)

  def noLabelsLinearBuckets[F[_]: Sync](
    cr: CollectorRegistry[F],
    name: Name,
    help: String,
    start: Double,
    factor: Double,
    count: Int
  ): F[Histogram[F]] = for {
    c <- Sync[F].delay(
      JHistogram.build()
      .name(name.getName)
      .help(help)
      .linearBuckets(start, factor, count)
    )
    out <- Sync[F].delay(c.register(CollectorRegistry.Unsafe.asJava(cr)))
  } yield new NoLabelsHistogram[F](out)

  def noLabelsExponentialBuckets[F[_]: Sync](
    cr: CollectorRegistry[F],
    name: Name, help: String,
    start: Double,
    factor: Double,
    count: Int
  ): F[Histogram[F]] = for {
    c <- Sync[F].delay(
      JHistogram.build()
      .name(name.getName)
      .help(help)
      .exponentialBuckets(start, factor, count)
    )
    out <- Sync[F].delay(c.register(CollectorRegistry.Unsafe.asJava(cr)))
  } yield new NoLabelsHistogram[F](out)

  /**
   * Constructor for a labelled [[Histogram]]. Default buckets are [[defaults]]
   * and are intended to cover a typical web/rpc request from milliseconds to seconds.
   *
   * This generates a specific number of labels via `Sized`, in combination with a function
   * to generate an equally `Sized` set of labels from some type. Values are applied by position.
   *
   * This counter needs to have a label applied to the [[UnlabelledHistogram]] in order to
   * be measureable or recorded.
   *
   * @param cr CollectorRegistry this [[Histogram]] will be registred with
   * @param name The name of the [[Histogram]].
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
  ): F[UnlabelledHistogram[F, A]] =
    labelledBuckets(cr, name, help, labels, f, defaults:_*)

  /**
   * Constructor for a labelled [[Histogram]]. Default buckets are [[defaults]]
   * and are intended to cover a typical web/rpc request from milliseconds to seconds.
   *
   * This generates a specific number of labels via `Sized`, in combination with a function
   * to generate an equally `Sized` set of labels from some type. Values are applied by position.
   *
   * This counter needs to have a label applied to the [[UnlabelledHistogram]] in order to
   * be measureable or recorded.
   *
   * @param cr CollectorRegistry this [[Histogram]] will be registred with
   * @param name The name of the [[Histogram]].
   * @param help The help string of the metric
   * @param labels The name of the labels to be applied to this metric
   * @param f Function to take some value provided in the future to generate an equally sized list
   *  of strings as the list of labels. These are assigned to labels by position.
   * @param buckets The buckets to measure observations by.
   */
  def labelledBuckets[F[_]: Sync, A, N <: Nat](
    cr: CollectorRegistry[F],
    name: Name,
    help: String,
    labels: Sized[IndexedSeq[Label], N],
    f: A => Sized[IndexedSeq[String], N],
    buckets: Double*
  ): F[UnlabelledHistogram[F, A]] = for {
    c <- Sync[F].delay(
      JHistogram.build()
      .name(name.getName)
      .help(help)
      .labelNames(labels.unsized.map(_.getLabel):_*)
      .buckets(buckets:_*)
    )
    out <- Sync[F].delay(c.register(CollectorRegistry.Unsafe.asJava(cr)))
  } yield new UnlabelledHistogramImpl[F, A](out, f.andThen(_.unsized))

  def labelledLinearBuckets[F[_]: Sync, A, N <: Nat](
    cr: CollectorRegistry[F],
    name: Name,
    help: String,
    labels: Sized[IndexedSeq[Label], N],
    f: A => Sized[IndexedSeq[String], N],
    start: Double,
    factor: Double,
    count: Int
  ): F[UnlabelledHistogram[F, A]] = for {
    c <- Sync[F].delay(
      JHistogram.build()
      .name(name.getName)
      .help(help)
      .labelNames(labels.unsized.map(_.getLabel):_*)
      .linearBuckets(start, factor, count)
    )
    out <- Sync[F].delay(c.register(CollectorRegistry.Unsafe.asJava(cr)))
  } yield new UnlabelledHistogramImpl[F, A](out, f.andThen(_.unsized))

  def labelledExponentialBuckets[F[_]: Sync, A, N <: Nat](
    cr: CollectorRegistry[F],
    name: Name,
    help: String,
    labels: Sized[IndexedSeq[Label], N],
    f: A => Sized[IndexedSeq[String], N],
    start: Double,
    factor: Double,
    count: Int
  ): F[UnlabelledHistogram[F, A]] = for {
    c <- Sync[F].delay(
      JHistogram.build()
      .name(name.getName)
      .help(help)
      .labelNames(labels.unsized.map(_.getLabel):_*)
      .exponentialBuckets(start, factor, count)
    )
    out <- Sync[F].delay(c.register(CollectorRegistry.Unsafe.asJava(cr)))
  } yield new UnlabelledHistogramImpl[F, A](out, f.andThen(_.unsized))

  private final case class NoLabelsHistogram[F[_]: Sync] private[Histogram] (
    private[Histogram] val underlying: JHistogram
  ) extends Histogram[F] {
    def observe(d: Double): F[Unit] = Sync[F].delay(underlying.observe(d))

  }

  private final case class LabelledHistogram[F[_]: Sync] private[Histogram] (
    private val underlying: JHistogram.Child
  ) extends Histogram[F] {
    def observe(d: Double): F[Unit] = Sync[F].delay(underlying.observe(d))
  }

  private final case class MapKHistogram[F[_], G[_]](private[Histogram] val base: Histogram[F], fk: F ~> G) extends Histogram[G]{
    def observe(d: Double): G[Unit] = fk(base.observe(d))
  }

  /**
   * Generic UnlabelledHistorgram
   *
   * It is necessary to apply a value of type `A` to this
   * histogram to be able to take any measurements.
   */
  sealed trait UnlabelledHistogram[F[_], A]{
    def label(a: A): Histogram[F]
    def mapK[G[_]](fk: F ~> G): UnlabelledHistogram[G, A] = new MapKUnlabelledHistogram[F, G, A](this, fk)
  }

  final private[epimetheus] case class UnlabelledHistogramImpl[F[_]: Sync, A] private[epimetheus] (
    private[Histogram] val underlying: JHistogram,
    private val f: A => IndexedSeq[String]
  ) extends UnlabelledHistogram[F, A]{
    def label(a: A): Histogram[F] =
      new LabelledHistogram[F](underlying.labels(f(a):_*))
  }

  final private case class MapKUnlabelledHistogram[F[_], G[_], A](private[Histogram] val base: UnlabelledHistogram[F, A], fk: F ~> G) extends UnlabelledHistogram[G, A]{
    def label(a: A): Histogram[G] = base.label(a).mapK(fk)
  }

  object Unsafe {
    def asJavaUnlabelled[F[_], A](h: UnlabelledHistogram[F, A]): JHistogram = h match {
      case h: UnlabelledHistogramImpl[F, A] => h.underlying
      case h: MapKUnlabelledHistogram[f, _, a] => asJavaUnlabelled(h.base)
    }
    def asJava[F[_]: ApplicativeThrow](c: Histogram[F]): F[JHistogram] = c match {
      case _: LabelledHistogram[F] => ApplicativeThrow[F].raiseError(new IllegalArgumentException("Cannot Get Underlying Parent with Labels Applied"))
      case NoLabelsHistogram(underlying) => underlying.pure[F]
      case MapKHistogram(base, _) => asJava(base.asInstanceOf[Histogram[F]])
    }
  }

}
