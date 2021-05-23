package io.chrisdavenport.epimetheus

import cats.effect._
import shapeless._

class HistogramSpec extends munit.CatsEffectSuite {
  test("Histogram No Labels: Register cleanly in the collector") {
    val test = for {
      cr <- CollectorRegistry.build[IO]
      h <- Histogram.noLabelsBuckets[IO](cr, Name("boo"), "Boo ", 0.1, 0.2, 0.3, 0.4)
    } yield h

    test.attempt.map(_.isRight).assert
  }

  test("Histogram Labelled: Register cleanly in the collector") {
    val test = for {
      cr <- CollectorRegistry.build[IO]
      h <- Histogram.labelledBuckets(cr, Name("boo"), "Boo ", Sized(Label("boo")), { s: String => Sized(s) }, 0.1, 0.2, 0.3, 0.4)
    } yield h

    test.attempt.map(_.isRight).assert
  }
}
