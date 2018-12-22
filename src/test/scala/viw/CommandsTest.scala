package viw

import org.scalatest.{BeforeAndAfter, FunSuite}

class CommandsTest extends FunSuite with ViwTest with BeforeAndAfter {

  before {
    // setup your test
  }

  after {
    // cleanup your history
  }

  val sourceText =
    """Lorem ipsum dolor sit ame#t#, consectetur adipiscing elit.
      |Cras quis massa eu ex commodo imperdiet.
      |Curabitur auctor tellus at justo malesuada, at ornare mi tincidunt.""".stripMargin

  test("Repeat delete") {
    viwTrue(
      "x.",
      "Test this re#p#eat",
      "Test this re#a#t"
    )
  }

  test("Multiple repeats") {
    viwTrue(
      "x" ++ "." * 10,
      "this is a tes#t#",
      "th#i#"
    )
  }

  test("Repeat join line") {
    viwTrue(
      "J.",
      """tes#t#
        |test
        |test
        |test""".stripMargin,
      """test test# #test
        |test""".stripMargin
    )
  }

  test("Delete and paste") {
    viwTrue(
      "xp",
      "ab#c#d",
      "abd#c#"
    )
  }

  test("Delete and paste behind") {
    viwTrue(
      "xP",
      "ab#c#d",
      "ab#c#d"
    )
  }
}