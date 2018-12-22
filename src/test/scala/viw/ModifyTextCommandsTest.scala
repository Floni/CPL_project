package viw

import org.scalatest.{BeforeAndAfter, FunSuite}

class ModifyTextCommandsTest extends FunSuite with ViwTest with BeforeAndAfter {

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

  test("Delete") {
    viwTrue(
      "x",
      """function(test) #{# return 'hello'; }""",
      """function(test) # #return 'hello'; }"""
    )
  }

  test("Delete last character on line") {
    viwTrue(
      "x",
      """function(test) { return 'hello'; #}#""",
      """function(test) { return 'hello';# #"""
    )
  }

  test("Delete empty") {
    viwTrue(
      "x",
      "##",
      "##"
    )
  }


  test("Delete backwards") {
    viwTrue(
      "X",
      """function(test) #{# return 'hello'; }""",
      """function(test)# # return 'hello'; }"""
    )
  }

  test("Delete backwards start of line") {
    viwTrue(
      "X",
      """#f#unction(test) { return 'hello'; }""",
      """#u#nction(test) { return 'hello'; }"""
    )
  }

  test("Delete line") {
    viwTrue(
      "D",
      """function(test) #{# return 'hello'; }""",
      """function(test)# #"""
    )
  }

  test("Delete full line") {
    viwTrue(
      "D",
      """#f#unction(test) { return 'hello'; }""",
      """##"""
    )
  }

  test("Delete empty line") {
    viwTrue(
      "D",
      """##""",
      """##"""
    )
  }

  test("Delete line from end of line") {
    viwTrue(
      "D",
      """function(test) { return 'hello'; #}#""",
      """function(test) { return 'hello';# #"""
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

  test("Join line last line") {
    viwTrue(
      "J",
      """Lorem ipsum dolor sit amet, consectetur adipiscing elit.
        |Cras quis massa eu ex commodo imperdiet.
        |C#u#rabitur auctor tellus at justo malesuada, at ornare mi tincidunt.""".stripMargin,
      """Lorem ipsum dolor sit amet, consectetur adipiscing elit.
        |Cras quis massa eu ex commodo imperdiet.
        |C#u#rabitur auctor tellus at justo malesuada, at ornare mi tincidunt.""".stripMargin
    )
  }
}