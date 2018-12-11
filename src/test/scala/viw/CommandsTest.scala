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

  test("Next word middle word") {
    viwTrue(
      "w",
      """Lorem i#p#sum dolor sit amet, consectetur adipiscing elit.""",
      """Lorem ipsum #d#olor sit amet, consectetur adipiscing elit."""
    )
  }

  test("Next word on next line") {
    viwTrue(
      "w",
      """Lorem ipsum dolor sit amet, consectetur adipiscing e#l#it.
        |Cras quis massa eu ex commodo imperdiet.
        |Curabitur auctor tellus at justo malesuada, at ornare mi tincidunt.""".stripMargin,
    """Lorem ipsum dolor sit amet, consectetur adipiscing elit.
      |#C#ras quis massa eu ex commodo imperdiet.
      |Curabitur auctor tellus at justo malesuada, at ornare mi tincidunt.""".stripMargin
    )
  }

  test("Next work on next line indented") {
    viwTrue(
      "w",
      """Lorem ipsum dolor sit amet, consectetur adipiscing e#l#it.
        |  Cras quis massa eu ex commodo imperdiet.
        |Curabitur auctor tellus at justo malesuada, at ornare mi tincidunt.""".stripMargin,
      """Lorem ipsum dolor sit amet, consectetur adipiscing elit.
        |  #C#ras quis massa eu ex commodo imperdiet.
        |Curabitur auctor tellus at justo malesuada, at ornare mi tincidunt.""".stripMargin
    )
  }

  test("Next word last word") {
    viwTrue(
      "w",
      """Lorem ipsum dolor sit amet, consectetur adipiscing elit.
        |Cras quis massa eu ex commodo imperdiet.
        |Curabitur auctor tellus at justo malesuada, at ornare mi t#i#ncidunt.""".stripMargin,
      """Lorem ipsum dolor sit amet, consectetur adipiscing elit.
        |Cras quis massa eu ex commodo imperdiet.
        |Curabitur auctor tellus at justo malesuada, at ornare mi t#i#ncidunt.""".stripMargin
    )
  }


  test("Back word") {
    viwTrue(
      "b",
      """Lorem #i#psum dolor sit amet, consectetur adipiscing elit.""",
      """#L#orem ipsum dolor sit amet, consectetur adipiscing elit."""
    )
  }

  test("Back word in middle of word") {
    viwTrue(
      "b",
      """test a te#s#t""",
      """test a #t#est"""
    )
  }

  test("Back word in spaces") {
    viwTrue(
      "b",
      """test a  # #  test""",
      """test #a#     test"""
    )
  }

  test("Back word on previous line") {
    viwTrue(
      "b",
      """Lorem ipsum dolor sit amet, consectetur adipiscing elit.
        |#C#ras quis massa eu ex commodo imperdiet.
        |Curabitur auctor tellus at justo malesuada, at ornare mi tincidunt.""".stripMargin,
      """Lorem ipsum dolor sit amet, consectetur adipiscing #e#lit.
        |Cras quis massa eu ex commodo imperdiet.
        |Curabitur auctor tellus at justo malesuada, at ornare mi tincidunt.""".stripMargin,
    )
  }

  test("Back word indented") {
    viwTrue(
      "b",
      """Lorem ipsum dolor sit amet, consectetur adipiscing elit.
        |    #C#ras quis massa eu ex commodo imperdiet.
        |Curabitur auctor tellus at justo malesuada, at ornare mi tincidunt.""".stripMargin,
      """Lorem ipsum dolor sit amet, consectetur adipiscing #e#lit.
        |    Cras quis massa eu ex commodo imperdiet.
        |Curabitur auctor tellus at justo malesuada, at ornare mi tincidunt.""".stripMargin,
    )
  }

  test("Back word first word") {
    viwTrue(
      "b",
      """#L#orem ipsum dolor sit amet, consectetur adipiscing elit.
        |Cras quis massa eu ex commodo imperdiet.
        |Curabitur auctor tellus at justo malesuada, at ornare mi tincidunt.""".stripMargin,
      """#L#orem ipsum dolor sit amet, consectetur adipiscing elit.
        |Cras quis massa eu ex commodo imperdiet.
        |Curabitur auctor tellus at justo malesuada, at ornare mi tincidunt.""".stripMargin,
    )
  }

  test("End of word") {
    viwTrue(
      "e",
      """Lorem ipsum #d#olor sit amet, consectetur adipiscing elit.""",
      """Lorem ipsum dolo#r# sit amet, consectetur adipiscing elit."""
    )
  }

  test("End of word on the end of a word") {
    viwTrue(
      "e",
      """Lorem ipsum dolo#r# sit amet, consectetur adipiscing elit.""",
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

  test("End of line at line end") {
    viwTrue(
      "$",
      """Lorem ipsum dolor sit amet, consectetur adipiscing elit#.#""",
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

  test("Start of line at line start") {
    viwTrue(
      "0",
      """#L#orem ipsum dolor sit amet, consectetur adipiscing elit.""",
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

  test("Match brackets nested brackets") {
    viwTrue(
      "%",
      """function(test) #{# {{return 'hello';}} }""",
      """function(test) { {{return 'hello';}} #}#"""
    )
  }

  test("Match brackets over multiple lines") {
    viwTrue(
      "%",
      """public void fizzbuzz(Int max) {
        |  for(int i = 0; i < max; i++) #{#
        |    if(i % 15 == 0) {
        |      printf("FizzBuzz\n");
        |    } else if(i % 3) {
        |      printf("Fizz\n");
        |    } else if(i % 5) {
        |      printf("Buzz\n");
        |    } else {
        |      printf("%d", i);
        |    }
        |  }
        |}""".stripMargin,
      """public void fizzbuzz(Int max) {
        |  for(int i = 0; i < max; i++) {
        |    if(i % 15 == 0) {
        |      printf("FizzBuzz\n");
        |    } else if(i % 3) {
        |      printf("Fizz\n");
        |    } else if(i % 5) {
        |      printf("Buzz\n");
        |    } else {
        |      printf("%d", i);
        |    }
        |  #}#
        |}""".stripMargin,
    )
  }

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

  test("Insert after character at last character of the line") {
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

  test("Delete movement right") {
    viwTrue(
      "dl",
      "ab#c#d",
      "ab#c#"
    )
  }

  test("Delete movement left") {
    viwTrue(
      "dh",
      "ab#c#d",
      "a#c#d"
    )
  }

  test("Delete movement down") {
    viwTrue(
      "dj",
      """abc
        |d#e#f
        |ghi""".stripMargin,
      """abc
        |d#e#i""".stripMargin
    )
  }

  test("Delete movement up") {
    viwTrue(
      "dk",
      """abc
        |d#e#f
        |ghi""".stripMargin,
      """a#e#f
        |ghi""".stripMargin
    )
  }

  test("Delete movement delete movement deletes line") {
    viwTrue(
      "dd",
      """abc
        |d#e#f
        |ghi""".stripMargin,
      """abc
        |#g#hi""".stripMargin
    )
  }

  //TODO: more test with more complicated movements (end line, bracket match etc)
  //TODO: change tests
}
