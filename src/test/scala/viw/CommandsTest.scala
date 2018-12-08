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

  test("Moving left and right moves the cursor (example for multiple commands)") {
    viwTrue(
      "hhhll",
      sourceText,
      """Lorem ipsum dolor sit am#e#t, consectetur adipiscing elit.
        |Cras quis massa eu ex commodo imperdiet.
        |Curabitur auctor tellus at justo malesuada, at ornare mi tincidunt.""".stripMargin
    )
  }

  test("Moving left moves the cursor") {
    viwTrue(
      "h",
      sourceText,
      """Lorem ipsum dolor sit am#e#t, consectetur adipiscing elit.
        |Cras quis massa eu ex commodo imperdiet.
        |Curabitur auctor tellus at justo malesuada, at ornare mi tincidunt.""".stripMargin
    )
  }

  test("Moving left at start of the line") {
    viwTrue(
      "h",
      """abc
        |#a#b""".stripMargin,
      """abc
        |#a#b""".stripMargin
    )
  }

  test("Moving up moves the cursor") {
    viwTrue(
      "k",
      """Lorem ipsum dolor sit amet, consectetur adipiscing elit.
        |Cras quis massa eu ex commod#o# imperdiet.
        |Curabitur auctor tellus at justo malesuada, at ornare mi tincidunt.""".stripMargin,
      """Lorem ipsum dolor sit amet, #c#onsectetur adipiscing elit.
        |Cras quis massa eu ex commodo imperdiet.
        |Curabitur auctor tellus at justo malesuada, at ornare mi tincidunt.""".stripMargin
    )
  }

  test("Moving up on first line") {
    viwTrue(
      "k",
      """a#b#c
        |abc""".stripMargin,
      """a#b#c
        |abc""".stripMargin,
    )
  }

  test("Moving down moves the cursor") {
    viwTrue(
      "j",
      sourceText,
      """Lorem ipsum dolor sit amet, consectetur adipiscing elit.
        |Cras quis massa eu ex com#m#odo imperdiet.
        |Curabitur auctor tellus at justo malesuada, at ornare mi tincidunt.""".stripMargin
    )
  }

  test("Moving down on last line") {
    viwTrue(
      "j",
      """abc
        |a#b#c""".stripMargin,
      """abc
        |a#b#c""".stripMargin,
    )
  }

  test("Moving right moves the cursor ") {
    viwTrue(
      "l",
      sourceText,
      """Lorem ipsum dolor sit amet#,# consectetur adipiscing elit.
        |Cras quis massa eu ex commodo imperdiet.
        |Curabitur auctor tellus at justo malesuada, at ornare mi tincidunt.""".stripMargin
    )
  }

  test("Moving right at end of the line") {
    viwTrue(
      "l",
      """ab#c#
        |abc
      """.stripMargin,
      """ab#c#
        |abc
      """.stripMargin
    )
  }

  test("Next word") {
    viwTrue(
      "w",
      """Lorem #i#psum dolor sit amet, consectetur adipiscing elit.""",
      """Lorem ipsum #d#olor sit amet, consectetur adipiscing elit."""
    )
  }

  test("Back word") {
    viwTrue(
      "b",
      """Lorem #i#psum dolor sit amet, consectetur adipiscing elit.""",
      """#L#orem ipsum dolor sit amet, consectetur adipiscing elit."""
    )
  }

  test("End of word") {
    viwTrue(
      "e",
      """Lorem ipsum #d#olor sit amet, consectetur adipiscing elit.""",
      """Lorem ipsum dolo#r# sit amet, consectetur adipiscing elit."""
    )
  }

  test("End of line") {
    viwTrue(
      "$",
      """Lorem ipsum #d#olor sit amet, consectetur adipiscing elit.""",
      """Lorem ipsum dolor sit amet, consectetur adipiscing elit#.#"""
    )
  }

  test("Start of line") {
    viwTrue(
      "0",
      """Lorem ipsum #d#olor sit amet, consectetur adipiscing elit.""",
      """#L#orem ipsum dolor sit amet, consectetur adipiscing elit."""
    )
  }

  test("Match brackets") {
    viwTrue(
      "%",
      """function(test) #{# return 'hello'; }""",
      """function(test) { return 'hello'; #}#"""
    )
  }

  test("Delete") {
    viwTrue(
      "x",
      """function(test) #{# return 'hello'; }""",
      """function(test) # #return 'hello'; }"""
    )
  }

  test("Delete backwards") {
    viwTrue(
      "X",
      """function(test) #{# return 'hello'; }""",
      """function(test)# # return 'hello'; }"""
    )
  }

  test("Delete line") {
    viwTrue(
      "D",
      """function(test) #{# return 'hello'; }""",
      """function(test)# #"""
    )
  }

  test("Join line") {
    viwTrue(
      "J",
      """Lorem ipsum dolor sit ame#t#, consectetur adipiscing elit.
      |Cras quis massa eu ex commodo imperdiet.
      |Curabitur auctor tellus at justo malesuada, at ornare mi tincidunt.""".stripMargin,
      """Lorem ipsum dolor sit amet, consectetur adipiscing elit.# #Cras quis massa eu ex commodo imperdiet.
      |Curabitur auctor tellus at justo malesuada, at ornare mi tincidunt.""".stripMargin
    )
  }

  test("Insert into normal mode") {
    viwFalse(
      "i",
      """function(test) #{# return 'hello'; }""",
      """function(test) #{# return 'hello'; }"""
    )
  }

  test("Insert after character") {
    viwFalse(
      "a",
      """function(test) #{# return 'hello'; }""",
      """function(test) {# #return 'hello'; }"""
    )
  }

  test("Open newline") {
    viwFalse(
      "o",
      "function(test) #{# return 'hello'; }",
      "function(test) { return 'hello'; }\n##"
    )
  }

  test("Substitute") {
    viwFalse(
      "s",
      "function(test) #{# return 'hello'; }",
      "function(test) # #return 'hello'; }"
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

  test("Insert in line") {
    viwFalse(
      "I",
      "Lorem ipsum dolor sit ame#t#, consectetur adipiscing elit",
      "#L#orem ipsum dolor sit amet, consectetur adipiscing elit"
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
}
