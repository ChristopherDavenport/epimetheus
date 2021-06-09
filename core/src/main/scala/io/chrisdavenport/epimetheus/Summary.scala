package io.chrisdavenport.epimetheus

import cats._
import cats.implicits._
import cats.effect._
import io.prometheus.client.{Summary => JSummary}
import scala.concurrent.duration._

/**
 * Summary Constructors, and Unsafe Summary Access
 */
trait SummaryCommons {

  // Convenience ----------------------------------------------------

  /**
   * Persist a timed value into this [[Summary]]
   *
   * @param s The summary to persist into.
   * @param fa The action to time
   * @param unit The unit of time to observe the timing in.
   */
  def timed[F[_] : Clock, A](s: Summary[F], fa: F[A], unit: TimeUnit)(implicit C: MonadCancel[F, _]): F[A] =
    C.bracket(Clock[F].monotonic)((_: FiniteDuration) => fa) { (start: FiniteDuration) =>
      Clock[F].monotonic.flatMap(now => s.observe((now - start).toUnit(unit)))
    }

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
    quantiles: Summary.Quantile*
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
    quantiles: Summary.Quantile*
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
    quantiles: Summary.Quantile*
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
    quantiles: Summary.Quantile*
  ): F[UnlabelledSummary[F, A]] = for {
    c1 <- Sync[F].delay(
      JSummary.build()
      .name(name.getName)
      .help(help)
      .maxAgeSeconds(maxAgeSeconds)
      .ageBuckets(ageBuckets)
      .labelNames(labels.unsized.map(_.getLabel):_*)
    )
    c <- Sync[F].delay(quantiles.foldLeft(c1){ case (c, q) => c.quantile(q.quantile, q.error)})
    out <- Sync[F].delay(c.register(CollectorRegistry.Unsafe.asJava(cr)))
  } yield new UnlabelledSummaryImpl[F, A](out, f.andThen(_.unsized))

  final private[epimetheus] case class NoLabelsSummary[F[_]: Sync] private[epimetheus] (
    private[epimetheus] val underlying: JSummary
  ) extends Summary[F] {
    def observe(d: Double): F[Unit] = Sync[F].delay(underlying.observe(d))
  }
  final private[epimetheus] case class LabelledSummary[F[_]: Sync] private[epimetheus] (
    private[epimetheus] val underlying: JSummary.Child
  ) extends Summary[F] {
    def observe(d: Double): F[Unit] = Sync[F].delay(underlying.observe(d))
  }

  final private[epimetheus] case class MapKSummary[F[_], G[_]](private[epimetheus] val base: Summary[F], fk: F ~> G) extends Summary[G]{
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
  final private[epimetheus] class UnlabelledSummaryImpl[F[_]: Sync, A] private[epimetheus](
    private[epimetheus] val underlying: JSummary,
    private val f: A => IndexedSeq[String]
  ) extends UnlabelledSummary[F,A]{
    def label(a: A): Summary[F] =
      new LabelledSummary[F](underlying.labels(f(a):_*))
  }

  final private[epimetheus] class MapKUnlabelledSummary[F[_], G[_], A](private[epimetheus] val base: UnlabelledSummary[F,A], fk: F ~> G) extends UnlabelledSummary[G, A]{
    def label(a: A): Summary[G] = base.label(a).mapK(fk)
  }

  trait QuantileCommons {
    /**
     * Safe Constructor of a Quantile valid values for both values are greater than 0
     * but less than 1.
     */
    def impl(quantile: Double, error: Double): Either[IllegalArgumentException, Summary.Quantile] = {
      if (quantile < 0.0 || quantile > 1.0) Either.left(new IllegalArgumentException("Quantile " + quantile + " invalid: Expected number between 0.0 and 1.0."))
      else if (error < 0.0 || error > 1.0) Either.left(new IllegalArgumentException("Error " + error + " invalid: Expected number between 0.0 and 1.0."))
      else Either.right(new Summary.Quantile(quantile, error))
    }

    def implF[F[_]: ApplicativeThrow](quantile: Double, error: Double): F[Summary.Quantile] =
      impl(quantile, error).liftTo[F]
  }

  object Unsafe {
    def asJavaUnlabelled[F[_], A](g: UnlabelledSummary[F, A]): JSummary = g match {
      case a: UnlabelledSummaryImpl[F, A] => a.underlying
      case a: MapKUnlabelledSummary[f, _, a] => asJavaUnlabelled(a.base)
    }
    def asJava[F[_]: ApplicativeThrow](c: Summary[F]): F[JSummary] = c match {
      case _: LabelledSummary[F] => ApplicativeThrow[F].raiseError(new IllegalArgumentException("Cannot Get Underlying Parent with Labels Applied"))
      case NoLabelsSummary(underlying) => underlying.pure[F]
      case MapKSummary(base, _) => asJava(base.asInstanceOf[Summary[F]])
    }
  }
}
