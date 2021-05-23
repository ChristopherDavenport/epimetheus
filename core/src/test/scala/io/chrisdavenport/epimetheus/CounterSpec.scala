package io.chrisdavenport.epimetheus

import cats.effect._
import shapeless._

class CounterSpec extends munit.CatsEffectSuite {

    test("Counter No Labels: Register cleanly in the collector") {
      val test = for {
        cr <- CollectorRegistry.build[IO]
        counter <- Counter.noLabels[IO](cr, Name("boo"), "Boo Counter")
      } yield counter

      test.attempt.map(_.isRight).assert
    }

    test("Counter No Labels: Increase correctly") {
      val test = for {
        cr <- CollectorRegistry.build[IO]
        counter <- Counter.noLabels[IO](cr, Name("boo"), "Boo Counter")
        _ <- counter.inc
        out <- counter.get
      } yield out

      test.assertEquals(1D)
    }

    test("Counter Labelled: Register cleanly in the collector") {
      val test = for {
        cr <- CollectorRegistry.build[IO]
        counter <- Counter.labelled(cr, Name("boo"), "Boo Counter", Sized(Label("foo")), {s: String => Sized(s)})
      } yield counter

      test.attempt.map(_.isRight).assert
    }

    test("Counter Labelled: Increase correctly") {
      val test = for {
        cr <- CollectorRegistry.build[IO]
        counter <- Counter.labelled(cr, Name("boo"), "Boo Counter", Sized(Label("foo")), {s: String => Sized(s)})
        _ <- counter.label("foo").inc
        out <- counter.label("foo").get
      } yield out

      test.assertEquals(1D)
    }

}
