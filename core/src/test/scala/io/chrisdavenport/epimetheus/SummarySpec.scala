package io.chrisdavenport.epimetheus

import cats.effect._

class SummarySpec extends munit.CatsEffectSuite {
  test("Summary No Labels: Register cleanly in the collector") {
    val test = for {
      cr <- CollectorRegistry.build[IO]
      s <- Summary.noLabels[IO](cr, Name("boo"), "Boo ", Summary.quantile(0.5, 0.05))
    } yield s

    test.attempt.map(_.isRight).assert
  }

  test("Summary Labelled: Register cleanly in the collector") {
    val test = for {
      cr <- CollectorRegistry.build[IO]
      s <- Summary.labelled(cr, Name("boo"), "Boo ", Sized(Label("boo")), { (s: String) => Sized(s) }, Summary.quantile(0.5, 0.05))
    } yield s

    test.attempt.map(_.isRight).assert
  }

  test("Summary.Quantile: Compilation fails for invalid values") {
    val good = Summary.quantile(0.5, 0.05)
    assert(compileErrors("Summary.quantile(2.0, 0.05)").nonEmpty)
  }
}
