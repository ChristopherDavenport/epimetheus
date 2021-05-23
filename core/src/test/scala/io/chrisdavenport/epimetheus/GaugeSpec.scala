package io.chrisdavenport.epimetheus

import cats.effect._
import shapeless._
import io.chrisdavenport.epimetheus.implicits._
import scala.concurrent.duration._

class GaugeSpec extends munit.CatsEffectSuite {

  test("Gauge No Labels: Register cleanly in the collector") {
    val test = for {
      cr <- CollectorRegistry.build[IO]
      gauge <- Gauge.noLabels[IO](cr, Name("boo"), "Boo Gauge")
    } yield gauge

    test.attempt.map(_.isRight).assert
  }

  test("Gauge No Labels: Increase correctly") {
    val test = for {
      cr <- CollectorRegistry.build[IO]
      gauge <- Gauge.noLabels[IO](cr, Name("boo"), "Boo Gauge")
      _ <- gauge.inc
      out <- gauge.get
    } yield out

    test.assertEquals(1D)
  }

  test("Gauge No Labels: Decrease correctly") {
    val test = for {
      cr <- CollectorRegistry.build[IO]
      gauge <- Gauge.noLabels[IO](cr, Name("boo"), "Boo Gauge")
      _ <- gauge.inc
      _ <- gauge.dec
      out <- gauge.get
    } yield out

    test.assertEquals(0D)
  }

  test("Gauge No Labels: Set correctly") {
    val set = 52D
    val test = for {
      cr <- CollectorRegistry.build[IO]
      gauge <- Gauge.noLabels[IO](cr, Name("boo"), "Boo Gauge")
      _ <- gauge.set(set)
      out <- gauge.get
    } yield out

    test.assertEquals(set)
  }

  test("Gauge Labelled: Register cleanly in the collector") {
    val test = for {
      cr <- CollectorRegistry.build[IO]
      gauge <- Gauge.labelled(cr, Name("boo"), "Boo Gauge", Sized(Label("boo")), { s: String => Sized(s) })
    } yield gauge

    test.attempt.map(_.isRight).assert
  }

  test("Gauge Labelled: Increase correctly") {
    val test = for {
      cr <- CollectorRegistry.build[IO]
      gauge <- Gauge.labelled(cr, Name("boo"), "Boo Gauge", Sized(Label("boo")), { s: String => Sized(s) })
      _ <- gauge.label("boo").inc
      out <- gauge.label("boo").get
    } yield out

    test.assertEquals(1D)
  }

  test("Gauge Labelled: Decrease correctly") {
    val test = for {
      cr <- CollectorRegistry.build[IO]
      gauge <- Gauge.labelled(cr, Name("boo"), "Boo Gauge", Sized(Label("boo")), { s: String => Sized(s) })
      _ <- gauge.label("boo").inc
      _ <- gauge.label("boo").dec
      out <- gauge.label("boo").get
    } yield out

    test.assertEquals(0D)
  }

  test("Gauge Labelled: Set correctly") {
    val set = 52D
    val test = for {
      cr <- CollectorRegistry.build[IO]
      gauge <- Gauge.labelled(cr, Name("boo"), "Boo Gauge", Sized(Label("boo")), { s: String => Sized(s) })
      _ <- gauge.label("boo").set(set)
      out <- gauge.label("boo").get
    } yield out

    test.assertEquals(set)
  }

  test("Gauge Convenience: incIn an operation succesfully") {
    val test = for {
      cr <- CollectorRegistry.build[IO]
      gauge <- Gauge.noLabels[IO](cr, Name("boo"), "Boo Gauge")
      defer <- Deferred[IO, Unit]
      fib <- gauge.incIn(defer.get).start
      _ <- IO.sleep(1.second)
      current <- gauge.get
      _ <- defer.complete(())
      _ <- fib.join
      after <- gauge.get
    } yield (current, after)

    test.assertEquals((1D, 0D))
  }

  test("Gauge Convenience: incByIn an operation succesfully") {
    val test = for {
      cr <- CollectorRegistry.build[IO]
      gauge <- Gauge.noLabels[IO](cr, Name("boo"), "Boo Gauge")
      defer <- Deferred[IO, Unit]
      fib <- gauge.incByIn(defer.get, 10).start
      _ <- IO.sleep(1.second)
      current <- gauge.get
      _ <- defer.complete(())
      _ <- fib.join
      after <- gauge.get
    } yield (current, after)

    test.assertEquals((10D, 0D))
  }

  test("Gauge Convenience: decIn an operation succesfully") {
    val test = for {
      cr <- CollectorRegistry.build[IO]
      gauge <- Gauge.noLabels[IO](cr, Name("boo"), "Boo Gauge")
      _ <- gauge.inc
      defer <- Deferred[IO, Unit]
      fib <- gauge.decIn(defer.get).start
      _ <- IO.sleep(1.second)
      current <- gauge.get
      _ <- defer.complete(())
      _ <- fib.join
      after <- gauge.get
    } yield (current, after)

    test.assertEquals((0D, 1D))
  }

  test("Gauge Convenience: decByIn an operation succesfully") {
    val test = for {
      cr <- CollectorRegistry.build[IO]
      gauge <- Gauge.noLabels[IO](cr, Name("boo"), "Boo Gauge")
      _ <- gauge.incBy(10)
      defer <- Deferred[IO, Unit]
      fib <- gauge.decByIn(defer.get, 10).start
      _ <- IO.sleep(1.second)
      current <- gauge.get
      _ <- defer.complete(())
      _ <- fib.join
      after <- gauge.get
    } yield (current, after)

    test.assertEquals((0D, 10D))
  }


}
