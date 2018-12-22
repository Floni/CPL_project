package viw

import org.scalatest.{BeforeAndAfter, FunSuite}

class SuspendableCommandsTest extends FunSuite with ViwTest with BeforeAndAfter {

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

  test("Delete movement right") {
    viwTrue(
      "dl",
      "ab#c#d",
      "ab#d#"
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
        |d#h#i""".stripMargin
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

  test("Delete movement next word") {
    viwTrue(
      "dw",
      "abc d#e#f ghi",
      "abc d#g#hi"
    )
  }

  test("Delete movement end word") {
    viwTrue(
      "de",
      "abc d#e#fghi",
      "abc d#i#"
    )
  }

  test("Delete movement end word new word") {
    viwTrue(
      "de",
      "ab#c# defghi",
      "ab#i#"
    )
  }

  test("Delete back word same word") {
    viwTrue(
      "db",
      "abc defg#h#i",
      "abc #h#i"
    )
  }

  test("Delete back word new word") {
    viwTrue(
      "db",
      "abc #d#efghi",
      "#d#efghi"
    )
  }

  test("Delete to end of line") {
    viwTrue(
      "d$",
      """ab#c# defghi
        |test""".stripMargin,
      """ab#i#
        |test""".stripMargin
    )
  }

  test("Delete to start of line") {
    viwTrue(
      "d0",
      """abc defg#h#i
        |test""".stripMargin,
      """#h#i
        |test""".stripMargin
    )
  }

  test("Delete match bracket to first bracket on line") {
    viwTrue(
      "d%",
      """ab(cd#e#
        |test""".stripMargin,
      """ab#e#
        |test""".stripMargin
    )
  }

  test("Delete match bracket on other line") {
    viwTrue(
      "d%",
      """function() #{#
        | {}
        | ([])
        | blabla {
        | test
        | }
        |}
        |test""".stripMargin,
      """function() #}#
        |test""".stripMargin
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

  test("Change command delete right") {
    viwFalse(
      "cl",
      "ab#c#d",
      "ab#d#"
    )
  }

  test("Change command change command deletes line") {
    viwFalse(
      "cc",
      """abc
        |d#e#f
        |ghi""".stripMargin,
      """abc
        |#g#hi""".stripMargin
    )
  }

  test("Yank right and paste") {
    viwTrue(
      "ylp",
      "abcd#e#fghi",
      "abcde#e#fghi"
    )
  }

  test("Yank left and paste") {
    viwTrue(
      "yhp",
      "abcd#e#fghi",
      "abcde#d#fghi"
    )
  }

  test("Yank down and paste") {
    viwTrue(
      "yjp",
      """abc de#f# ghi
        |test a test
        |test""".stripMargin,
      """abc deff ghi
        |test #a# ghi
        |test a test
        |test""".stripMargin
    )
  }

  test("Yank up and paste") {
    viwTrue(
      "ykp",
      """abc def ghi
        |test a test
        |te#s#t""".stripMargin,
      """abc def ghi
        |test a test
        |tesst a test
        |t#e#t""".stripMargin
    )
  }

  test("Yank whole line and paste") {
    viwTrue(
      "yyp",
      """abc def ghi
        |test #a# test
        |test""".stripMargin,
      """abc def ghi
        |test atest a tes#t#
        | test
        |test""".stripMargin
    )
  }

  test("Count command move right") {
    viwTrue(
      "3l",
      "this is #a# test",
      "this is a t#e#st"
    )
  }

  test("Count command move right more than length") {
    viwTrue(
      "9l",
      "this is #a# test",
      "this is a tes#t#"
    )
  }

  test("Count command delete 3 to the right") {
    viwTrue(
      "d3l",
      "this is #a# test",
      "this is #e#st"
    )
  }

  test("Repeat delete count command command 3 to the right") {
    viwTrue(
      "d3l.",
      "thi#s# is a test",
      "thi# #test"
    )
  }

  test("Count command delete 11 to the right") {
    viwTrue(
      "d11l",
      "t#h#is is a test",
      "t#s#t"
    )
  }

  test("Count command delete 10 to the right") {
    viwTrue(
      "d10l",
      "t#h#is is a test",
      "t#e#st"
    )
  }


  test("Indent current line") {
    viwTrue(
      ">>",
      "this is a tes#t#",
      "  this is a tes#t#"
    )
  }

  test("Indent current line repeat") {
    viwTrue(
      ">>.",
      "this is a tes#t#",
      "    this is a tes#t#"
    )
  }


  test("Indent up") {
    viwTrue(
      ">k",
      """  test a test
        |test #a# test
        |test""".stripMargin,
      """    test a test
        |  test #a# test
        |test""".stripMargin
    )
  }

  test("Indent match brackets") {
    viwTrue(
      ">%",
      """function(arg) {
        |if(bla) #{#
        |  qlzdqlzdqlz
        |  lzqdqldlqd
        |  qzdlqzldql
        |  {}
        |}
        |}""".stripMargin,
      """function(arg) {
        |  if(bla) #{#
        |    qlzdqlzdqlz
        |    lzqdqldlqd
        |    qzdlqzldql
        |    {}
        |  }
        |}""".stripMargin
    )
  }

  test("Dec indent line") {
    viwTrue(
      "<<",
      "   test this lin#e#",
      " test this lin#e#"
    )
  }

  test("Dec indent line repeat") {
    viwTrue(
      "<<.",
      "   test this lin#e#",
      "test this lin#e#"
    )
  }
}