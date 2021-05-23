package io.chrisdavenport.epimetheus

class NameSpec extends munit.CatsEffectSuite {
    test("Name: Return the input string if valid") {
      assertEquals(Name("L").getName, "L")
    }

    test("Suffix: Return the input string if valid") {
      assertEquals(Name.Suffix("0").getSuffix, "0")
    }

  object Compile {
    val good = Name("asdf_basrr")
    // val badNumberFirst = Name("0asdfa")
    val goodIfNumberSecond = Name("a0asdfa")
    // val badAscii = Name("^")

    val goodSuffix = Name.Suffix("0asdfa")
    // val failAscci = Name.Suffix("^")
  }
}
