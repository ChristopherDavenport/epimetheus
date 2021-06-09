package io.chrisdavenport.epimetheus

class LabelSpec extends munit.CatsEffectSuite {

  test("Label: Return the input string if valid") {
    assertEquals(Label("L").getLabel, "L")
  }

  test("Label.Suffix: Return the input string if valid") {
    assertEquals(Label.Suffix("0").getSuffix, "0")
  }

  test("Label: compile-time check fails for invalid values") {
    val good = Label("asdf_basrr")
    assert(compileErrors("""Label("0asdfa")""").nonEmpty)
    val goodIfNumberSecond = Label("a0asdfa")
    assert(compileErrors("""Label("^")""").nonEmpty)

    val goodSuffix = Label.Suffix("0asdfa")
    assert(compileErrors("""Label.Suffix("^")""").nonEmpty)
  }
}

