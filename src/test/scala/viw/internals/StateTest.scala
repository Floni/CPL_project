package viw.internals

import org.scalatest.FunSuite
import viw.ViwTest

class StateTest extends FunSuite with ViwTest {

  test("State from cursoredText should be equal to its source") {
    val sourceText =
      """Lore#m# ipsum dolor sit amet, consectetur adipiscing elit.
        |Donec eu mi commodo, consequat purus vitae, facilisis nunc.
      """.stripMargin

    val state = State.fromCursoredText(sourceText)

    assert(state.cursoredText === sourceText)
    assert(state.position.character === 4)
    assert(state.position.line === 0)
  }

    test("State from cursoredText should be equal to its source with an empty cursor") {
      val sourceText =
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit.\n##"

      val state = State.fromCursoredText(sourceText)

      assert(state.cursoredText === sourceText)
      assert(state.position.character === 0)
      assert(state.position.line === 1)
  }

}
