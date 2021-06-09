package io.chrisdavenport

package object epimetheus extends ShapelessPolyfill {
  type UnlabelledCounter[F[_], A] = Counter.UnlabelledCounter[F, A]
  type UnlabelledGauge[F[_], A] = Gauge.UnlabelledGauge[F, A]
  type UnlabelledHistogram[F[_], A] = Histogram.UnlabelledHistogram[F, A]
  type UnlabelledSummary[F[_], A] = Summary.UnlabelledSummary[F, A]
}
