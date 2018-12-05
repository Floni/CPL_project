package viw

import viw.internals.State
import viw.internals.State.Position

object Viw {
  def processKey(key: String, state: State) : Option[State] = key match{
    case k if k == "h" => CursorMover.moveCursorLeft(state)
    case k if k == "l" => CursorMover.moveCursorRight(state)
    case _ => None
  }
}

object CursorMover {
  def moveCursorLeft(state: State) : Option[State] = {
    if (state.position.character > 0) {
      Option(State(state.content, Position(state.position.line, state.position.character - 1), None, state.mode))
    } else if (state.position.line > 0) {
      val newLine = state.position.line - 1
      Option(State(state.content, Position(newLine, state.contentLines(newLine).length), None, state.mode))
    } else {
      Option(state)
    }
  }

  def moveCursorRight(state: State) : Option[State] = {
    if (state.position.character < state.contentLines(state.position.line).length) {
      Option(State(state.content, Position(state.position.line, state.position.character + 1), None, state.mode))
    } else if (state.position.line < state.contentLines.length) {
      val newLine = state.position.line + 1
      Option(State(state.content, Position(newLine, state.contentLines(newLine).length), None, state.mode))
    } else {
      Option(state)
    }
  }


}