package viw

import org.scalatest.{BeforeAndAfter, FunSuite}

class ExitViwCommandsTest extends FunSuite with ViwTest with BeforeAndAfter {

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

  test("Insert into normal mode") {
    viwFalse(
      "i",
      """function(test) #{# return 'hello'; }""",
      """function(test) #{# return 'hello'; }"""
    )
  }

  test("Append after character") {
    viwFalse(
      "a",
      """function(test) #{# return 'hello'; }""",
      """function(test) {# #return 'hello'; }"""
    )
  }

  test("Append after character at last character of the line") {
    viwFalse(
      "a",
      """function(test) { return 'hello'; #}#
        | test""".stripMargin,
      """function(test) { return 'hello'; }##
        | test""".stripMargin,
    )
  }

  test("Open newline") {
    viwFalse(
      "o",
      "function(test) #{# return 'hello'; }",
      "function(test) { return 'hello'; }\n##"
    )
  }

  test("Open newline multiple lines") {
    viwFalse(
      "o",
      "function(test) #{# return 'hello'; }\n test",
      "function(test) { return 'hello'; }\n" ++ "##\n" ++
        " test"
    )
  }

  test("Open newline multiple times") {
    viwFalse(
      "oo",
      "function(test) #{# return 'hello'; }\n test",
      "function(test) { return 'hello'; }\n" ++ "\n" ++ "##\n" ++
        " test"
    )
  }

  test("Substitute") {
    viwFalse(
      "s",
      "function(test) #{# return 'hello'; }",
      "function(test) # #return 'hello'; }"
    )
  }

  test("Substitute empty") {
    viwFalse(
      "s",
      "##",
      "##"
    )
  }

  test("Substitute last char of line") {
    viwFalse(
      "s",
      "function(test) { return 'hello'; #}#",
      "function(test) { return 'hello';# #"
    )
  }

  test("Go") {
    viwFalse(
      "G",
      """Lorem ipsum dolor sit ame#t#, consectetur adipiscing elit.
        |Cras quis massa eu ex commodo imperdiet.
        |Curabitur auctor tellus at justo malesuada, at ornare mi tincidunt.""".stripMargin,
      """Lorem ipsum dolor sit amet, consectetur adipiscing elit.
        |Cras quis massa eu ex commodo imperdiet.
        |#C#urabitur auctor tellus at justo malesuada, at ornare mi tincidunt.""".stripMargin
    )
  }

  test("Go single line") {
    viwFalse(
      "G",
      """Lorem ipsum dolor sit ame#t#, consectetur adipiscing elit.""".stripMargin,
      """#L#orem ipsum dolor sit amet, consectetur adipiscing elit.""".stripMargin
    )
  }

  test("Insert in line") {
    viwFalse(
      "I",
      "Lorem ipsum dolor sit ame#t#, consectetur adipiscing elit",
      "#L#orem ipsum dolor sit amet, consectetur adipiscing elit"
    )
  }

  test("Insert in line empty line") {
    viwFalse(
      "I",
      "##",
      "##"
    )
  }

  test("Insert after line") {
    viwFalse(
      "A",
      "Lorem ipsum dolor sit ame#t#, consectetur adipiscing elit",
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit##"
    )
  }

  test("Change to the end of the line") {
    viwFalse(
      "C",
      "Lorem ipsum dolor sit ame#t#, consectetur adipiscing elit",
      "Lorem ipsum dolor sit ame##"
    )
  }

  test("Change to the end of the line at end of line") {
    viwFalse(
      "C",
      "Lorem ipsum dolor sit amet, consectetur adipiscing eli#t#",
      "Lorem ipsum dolor sit amet, consectetur adipiscing eli##"
    )
  }
}