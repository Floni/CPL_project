package viw

import org.scalatest.{Assertion, Assertions}
import viw.ViwTest.TestData
import viw.internals.State

trait ViwTest extends Assertions {

  implicit class StateCursoredText(state: State) {
    import state._

    def putStringAt(contentLines: Seq[String],
                    position: State.Position,
                    string: String) = {
      val c = position.character
      val l = position.line
      val line = contentLines(l)
      val newLine = line.slice(0, c) + string + line.slice(c, Int.MaxValue)
      contentLines.slice(0, l) ++ (newLine +: contentLines.slice(l + 1,
                                                                 Int.MaxValue))
    }

    /**
      * text representation of the contents and cursor position
      */
    lazy val cursoredText = {
      val contentLinesRaw = content.split('\n')
      val contentLines =
        if (contentLinesRaw.length - 1 < position.line) contentLinesRaw :+ ""
        else contentLinesRaw

      val makeCursorText = {
        val firstCursor = putStringAt(contentLines, position, "#")
        putStringAt(firstCursor,
                    position.copy(character = position.character + 2),
                    "#")
      }

      makeCursorText.mkString("\n")
    }
  }

  /**
    * Assumes viw-mode to be on at the start and off at the end
    *
    * @param commandStack
    * @param in
    * @param expected
    * @return assertion that checks if the proper commands have been executed
    */
  def viwFalse(commandStack: String, in: String, expected: String): Assertion =
    viwMode(commandStack, in, expected, false)

  /**
    * Assumes viw-mode to be on at the start and at the end
    *
    * @param commandStack
    * @param in
    * @param expected
    * @return assertion that checks if the proper commands have been executed
    */
  def viwTrue(commandStack: String, in: String, expected: String): Assertion =
    viwMode(commandStack, in, expected, true)

  def viwMode(commandStack: String,
              in: String,
              expected: String,
              expectedMode: Boolean): Assertion =
    viwTest(commandStack, TestData(in, true), TestData(expected, expectedMode))

  def viwTest(commandStack: String,
              in: TestData,
              expected: TestData): Assertion = {

    val endState = commandStack.foldLeft(in.state) { (state, char) =>
      Viw.processKey(char.toString, state).getOrElse(state)
    }

    assert(endState.cursoredText === expected.text)
    assert(endState.mode === expected.mode)
  }

}

object ViwTest {
  case class TestData(
      text: String,
      mode: Boolean = true,
      selection: Option[(State.Position, State.Position)] = None) {
    val state = State.fromCursoredText(text, mode)
  }
}
