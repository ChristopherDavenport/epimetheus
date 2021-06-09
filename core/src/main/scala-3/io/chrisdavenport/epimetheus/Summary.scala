package io.chrisdavenport.epimetheus

import cats.*

import scala.quoted.*

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
abstract class Summary[F[_]]{

  /**
   * Persist an observation into this [[Summary]]
   *
   * @param d The observation to persist
   */
  def observe(d: Double): F[Unit]

  def mapK[G[_]](fk: F ~> G): Summary[G] = new Summary.MapKSummary[F, G](this, fk)
}

object Summary extends SummaryCommons {

  /**
   * Safe Constructor for Literal Quantiles
   *
   * If you want to construct a dynamic quantile use the [[Quantile.impl safe constructor]]
   */
  inline def quantile(inline quantile: Double, inline error: Double): Quantile = ${Quantile.Macros.quantileLiteral('quantile, 'error)}

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
  final class Quantile private[epimetheus] (val quantile: Double, val error: Double)
  object Quantile extends QuantileCommons {
    private[Summary] object Macros {
      def quantileLiteral(quantile: Expr[Double], error: Expr[Double])(using quotes: Quotes): Expr[Quantile] =
        (quantile.value, error.value) match {
          case (Some(q), Some(e)) =>
            Quantile.impl(q, e).fold(throw _, _ => '{val quantile = Quantile.impl(${Expr(q)}, ${Expr(e)}).fold(throw _, identity); quantile})
          case _ =>
            quotes.reflect.report.error("This method uses a macro to verify that a Quantile literal is valid. Use Quantile.impl if you have a dynamic set that you want to parse as a Quantile.")
            '{???}
        }
    }

    inline def quantile(inline quantile: Double, inline error: Double): Quantile = ${Quantile.Macros.quantileLiteral('quantile, 'error)}
  }

}
