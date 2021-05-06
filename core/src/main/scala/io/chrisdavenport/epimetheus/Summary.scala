package io.chrisdavenport.epimetheus

import cats._
import cats.implicits._
import cats.effect._
import io.prometheus.client.{Summary => JSummary}
import scala.concurrent.duration._
import shapeless._

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
 * Summary metric, to track the size of events.
 *
 * The quantiles are calculated over a sliding window of time. There are two options to configure this time window:
 *
 * maxAgeSeconds: Long -  Set the duration of the time window is, i.e. how long observations are kept before they are discarded.
 * Default is 10 minutes.
 *
 * ageBuckets: Int - Set the number of buckets used to implement the sliding time window. If your time window is 10 minutes, and you have ageBuckets=5,
 * buckets will be switched every 2 minutes. The value is a trade-off between resources (memory and cpu for maintaining the bucket)
 * and how smooth the time window is moved. Default value is 5.
 *
 * See https://prometheus.io/docs/practices/histograms/ for more info on quantiles.
 */
sealed abstract class Summary[F[_]]{

  /**
   * Persist an observation into this [[Summary]]
   *
   * @param d The observation to persist
   */
  def observe(d: Double): F[Unit]

  def mapK[G[_]](fk: F ~> G): Summary[G] = new Summary.MapKSummary[F, G](this, fk)
}


/**
 * Summary Constructors, and Unsafe Summary Access
 */
object Summary {

  // Convenience ----------------------------------------------------

  /**
   * Persist a timed value into this [[Summary]]
   *
   * @param s The summary to persist into.
   * @param fa The action to time
   * @param unit The unit of time to observe the timing in.
   */
  def timed[F[_] : Clock, A](s: Summary[F], fa: F[A], unit: TimeUnit)(implicit C: MonadCancel[F, _]): F[A] =
    C.bracket(Clock[F].monotonic)
    {_: FiniteDuration => fa}
    {start: FiniteDuration => Clock[F].monotonic.flatMap(now => s.observe((now - start).toUnit(unit)))}

  /**
   * Persist a timed value into this [[Summary]] in unit Seconds. Since the default
   * buckets for histogram are in seconds and Summary are in some ways counterparts
   * to histograms, this exposes convenience function.
   *
   * @param s The summary to persist to
   * @param fa The action to time
   */
  def timedSeconds[F[_] : Clock, A](s: Summary[F], fa: F[A])(implicit C: MonadCancel[F, _]): F[A] =
    timed(s, fa, SECONDS)

  // Constructors ---------------------------------------------------
  val defaultMaxAgeSeconds = 600L
  val defaultAgeBuckets = 5

  /**
   * Safe Constructor for Literal Quantiles
   *
   * If you want to construct a dynamic quantile use the [[Quantile.impl safe constructor]]
   */
  def quantile(quantile: Double, error: Double): Quantile = macro Quantile.Macros.quantileLiteral

  /**
   * Default Constructor for a [[Summary]] with no labels.
   *
   * maxAgeSeconds is set to [[defaultMaxAgeSeconds]] which is 10 minutes.
   *
   * ageBuckets is the number of buckets for the sliding time window, set to [[defaultAgeBuckets]] which is 5.
   *
   * If you want to exert control, use the full constructor [[Summary.noLabelsQuantiles noLabelsQuantiles]]
   *
   * @param cr CollectorRegistry this [[Summary]] will be registered with
   * @param name The name of the Summary
   * @param help The help string of the metric
   * @param quantiles The measurements to track for specifically over the sliding time window.
   */
  def noLabels[F[_]: Sync](
    cr: CollectorRegistry[F],
    name: Name,
    help: String,
    quantiles: Quantile*
  ): F[Summary[F]] =
    noLabelsQuantiles(cr, name, help, defaultMaxAgeSeconds, defaultAgeBuckets, quantiles:_*)

  /**
   * Constructor for a [[Summary]] with no labels.
   *
   * maxAgeSeconds is set to [[defaultMaxAgeSeconds]] which is 10 minutes.
   *
   * ageBuckets is the number of buckets for the sliding time window, set to [[defaultAgeBuckets]] which is 5.
   *
   * If you want to exert control, use the full constructor [[Summary.noLabelsQuantiles noLabelsQuantiles]]
   *
   * @param cr CollectorRegistry this [[Summary]] will be registered with
   * @param name The name of the Summary
   * @param help The help string of the metric
   * @param maxAgeSeconds Set the duration of the time window is,
   *  i.e. how long observations are kept before they are discarded.
   * @param ageBuckets Set the number of buckets used to implement the sliding time window. If your time window is 10 minutes, and you have ageBuckets=5,
   *  buckets will be switched every 2 minutes. The value is a trade-off between resources (memory and cpu for maintaining the bucket)
   *  and how smooth the time window is moved.
   * @param quantiles The measurements to track for specifically over the sliding time window.
   */
  def noLabelsQuantiles[F[_]: Sync](
    cr: CollectorRegistry[F],
    name: Name,
    help: String,
    maxAgeSeconds: Long,
    ageBuckets: Int,
    quantiles: Quantile*
  ): F[Summary[F]] = for {
    c1 <- Sync[F].delay(
      JSummary.build()
      .name(name.getName)
      .help(help)
      .maxAgeSeconds(maxAgeSeconds)
      .ageBuckets(ageBuckets)
    )
    c <- Sync[F].delay(quantiles.foldLeft(c1){ case (c, q) => c.quantile(q.quantile, q.error)})
    out <- Sync[F].delay(c.register(CollectorRegistry.Unsafe.asJava(cr)))
  } yield new NoLabelsSummary[F](out)

  /**
   * Default Constructor for a labelled [[Summary]].
   *
   * maxAgeSeconds is set to [[defaultMaxAgeSeconds]] which is 10 minutes.
   *
   * ageBuckets is the number of buckets for the sliding time window, set to [[defaultAgeBuckets]] which is 5.
   *
   * This generates a specific number of labels via `Sized`, in combination with a function
   * to generate an equally `Sized` set of labels from some type. Values are applied by position.
   *
   * This counter needs to have a label applied to the [[UnlabelledSummary]] in order to
   * be measureable or recorded.
   *
   * @param cr CollectorRegistry this [[Summary]] will be registred with
   * @param name The name of the [[Summary]].
   * @param help The help string of the metric
   * @param labels The name of the labels to be applied to this metric
   * @param f Function to take some value provided in the future to generate an equally sized list
   *  of strings as the list of labels. These are assigned to labels by position.
   * @param quantiles The measurements to track for specifically over the sliding time window.
   */
  def labelled[F[_]: Sync, A, N <: Nat](
    cr: CollectorRegistry[F],
    name: Name,
    help: String,
    labels: Sized[IndexedSeq[Label], N],
    f: A => Sized[IndexedSeq[String], N],
    quantiles: Quantile*
  ): F[UnlabelledSummary[F, A]] =
    labelledQuantiles(cr, name, help, defaultMaxAgeSeconds, defaultAgeBuckets, labels, f, quantiles:_*)

  /**
   * Constructor for a labelled [[Summary]].
   *
   * maxAgeSeconds is set to [[defaultMaxAgeSeconds]] which is 10 minutes.
   *
   * ageBuckets is the number of buckets for the sliding time window, set to [[defaultAgeBuckets]] which is 5.
   *
   * This generates a specific number of labels via `Sized`, in combination with a function
   * to generate an equally `Sized` set of labels from some type. Values are applied by position.
   *
   * This counter needs to have a label applied to the [[UnlabelledSummary]] in order to
   * be measureable or recorded.
   *
   * @param cr CollectorRegistry this [[Summary]] will be registred with
   * @param name The name of the [[Summary]].
   * @param help The help string of the metric
   * @param maxAgeSeconds Set the duration of the time window is,
   *  i.e. how long observations are kept before they are discarded.
   * @param ageBuckets Set the number of buckets used to implement the sliding time window.
   *  If your time window is 10 minutes, and you have ageBuckets=5,
   *  buckets will be switched every 2 minutes.
   *  The value is a trade-off between resources (memory and cpu for maintaining the bucket)
   *  and how smooth the time window is moved.
   * @param labels The name of the labels to be applied to this metric
   * @param f Function to take some value provided in the future to generate an equally sized list
   *  of strings as the list of labels. These are assigned to labels by position.
   * @param quantiles The measurements to track for specifically over the sliding time window.
   */
  def labelledQuantiles[F[_]: Sync, A, N <: Nat](
    cr: CollectorRegistry[F],
    name: Name,
    help: String,
    maxAgeSeconds: Long,
    ageBuckets: Int,
    labels: Sized[IndexedSeq[Label], N],
    f: A => Sized[IndexedSeq[String], N],
    quantiles: Quantile*
  ): F[UnlabelledSummary[F, A]] = for {
    c1 <- Sync[F].delay(
      JSummary.build()
      .name(name.getName)
      .help(help)
      .maxAgeSeconds(maxAgeSeconds)
      .ageBuckets(ageBuckets)
      .labelNames(labels.map(_.getLabel):_*)
    )
    c <- Sync[F].delay(quantiles.foldLeft(c1){ case (c, q) => c.quantile(q.quantile, q.error)})
    out <- Sync[F].delay(c.register(CollectorRegistry.Unsafe.asJava(cr)))
  } yield new UnlabelledSummaryImpl[F, A](out, f.andThen(_.unsized))

  final private class NoLabelsSummary[F[_]: Sync] private[Summary] (
    private[Summary] val underlying: JSummary
  ) extends Summary[F] {
    def observe(d: Double): F[Unit] = Sync[F].delay(underlying.observe(d))
  }
  final private class LabelledSummary[F[_]: Sync] private[Summary] (
    private val underlying: JSummary.Child
  ) extends Summary[F] {
    def observe(d: Double): F[Unit] = Sync[F].delay(underlying.observe(d))
  }

  final private class MapKSummary[F[_], G[_]](private[Summary] val base: Summary[F], fk: F ~> G) extends Summary[G]{
    def observe(d: Double): G[Unit] = fk(base.observe(d))
  }

  /**
   * Generic Unlabeled Summary
   *
   * Apply a label to be able to measure events.
   */
  sealed trait UnlabelledSummary[F[_], A]{
    def label(a: A): Summary[F]
    def mapK[G[_]](fk: F ~> G): UnlabelledSummary[G, A] = new MapKUnlabelledSummary[F,G, A](this, fk)
  }
  final private class UnlabelledSummaryImpl[F[_]: Sync, A] private[epimetheus](
    private[Summary] val underlying: JSummary,
    private val f: A => IndexedSeq[String]
  ) extends UnlabelledSummary[F,A]{
    def label(a: A): Summary[F] =
      new LabelledSummary[F](underlying.labels(f(a):_*))
  }

  final private class MapKUnlabelledSummary[F[_], G[_], A](private[Summary] val base: UnlabelledSummary[F,A], fk: F ~> G) extends UnlabelledSummary[G, A]{
    def label(a: A): Summary[G] = base.label(a).mapK(fk)
  }
  /**
   * The percentile and tolerated error to be observed
   *
   * There is a [[Quantile.impl safe constructor]], and a [[Quantile.quantile macro constructor]] which can
   * statically verify these values if they are known at compile time.
   *
   *
   * `Quantile.quantile(0.5, 0.05)` - 50th percentile (= median) with 5% tolerated error
   *
   * `Quantile.quantile(0.9, 0.01)` - 90th percentile with 1% tolerated error
   *
   * `Quantile.quantile(0.99, 0.001)` - 99th percentile with 0.1% tolerated error
   */
  final class Quantile private(val quantile: Double, val error: Double)
  object Quantile {
    private[Summary] class Macros(val c: whitebox.Context) {
      import c.universe._
      def quantileLiteral(quantile: c.Expr[Double], error: c.Expr[Double]): Tree =
        (quantile.tree, error.tree) match {
          case (Literal(Constant(q: Double)), Literal(Constant(e: Double))) =>
              impl(q, e)
              .fold(
                e => c.abort(c.enclosingPosition, e.getMessage),
                _ =>
                  q"_root_.io.chrisdavenport.epimetheus.Summary.Quantile.impl($q, $e).fold(throw _, _root_.scala.Predef.identity)"
              )
          case _ =>
            c.abort(
              c.enclosingPosition,
              s"This method uses a macro to verify that a Quantile literal is valid. Use Quantile.impl if you have a dynamic set that you want to parse as a Quantile."
            )
        }
    }

    /**
     * Safe Constructor of a Quantile valid values for both values are greater than 0
     * but less than 1.
     */
    def impl(quantile: Double, error: Double): Either[IllegalArgumentException, Quantile] = {
      if (quantile < 0.0 || quantile > 1.0) Either.left(new IllegalArgumentException("Quantile " + quantile + " invalid: Expected number between 0.0 and 1.0."))
      else if (error < 0.0 || error > 1.0) Either.left(new IllegalArgumentException("Error " + error + " invalid: Expected number between 0.0 and 1.0."))
      else Either.right(new Quantile(quantile, error))
    }

    def implF[F[_]: ApplicativeThrow](quantile: Double, error: Double): F[Quantile] =
      impl(quantile, error).liftTo[F]

    def quantile(quantile: Double, error: Double): Quantile = macro Macros.quantileLiteral
  }

  object Unsafe {
    def asJavaUnlabelled[F[_], A](g: UnlabelledSummary[F, A]): JSummary = g match {
      case a: UnlabelledSummaryImpl[_, _] => a.underlying
      case a: MapKUnlabelledSummary[_, _, _] => asJavaUnlabelled(a.base)
    }
    def asJava[F[_]: ApplicativeThrow](c: Summary[F]): F[JSummary] = c match {
      case _: LabelledSummary[F] => ApplicativeThrow[F].raiseError(new IllegalArgumentException("Cannot Get Underlying Parent with Labels Applied"))
      case n: NoLabelsSummary[F] => n.underlying.pure[F]
      case b: MapKSummary[_, _] => asJava(b.base)
    }
  }
}
